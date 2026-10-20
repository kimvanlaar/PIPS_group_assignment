# ---------------------------------------------------------------
# analysis/stats_analysis.R
# Research questions:
#   1. More social jet lag, more tiredness?
#   2. More social jet lag, lower mood?
# Both controlling for age, sex and sleep duration.
# Data: NHANES 2017 - March 2020.
#
# Part 0: packages
# Part 1: functions
# Part 2: run the analysis (run from top to bottom, or click Source)
# ---------------------------------------------------------------


# ===============================================================
# PART 0: PACKAGES
# Installs any missing packages, then loads them.
# ===============================================================

required_packages <- c("dplyr", "ggplot2", "patchwork", "plotly")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

library(dplyr)
library(ggplot2)
library(patchwork)
library(plotly)


# ===============================================================
# PART 1: FUNCTIONS
# ===============================================================

# Labels and titles for each outcome, used in the plots
OUTCOME_INFO <- list(
  phq_tired = list(label    = "Mean tiredness score (0-3)",
                   title    = "More social jet lag, more tiredness?",
                   question = "Feeling tired or having little energy"),
  phq_down  = list(label    = "Mean low mood score (0-3)",
                   title    = "More social jet lag, lower mood?",
                   question = "Feeling down, depressed or hopeless")
)

# Meaning of the 0-3 answer scale, shown under the plots
SCALE_NOTE <- paste0("Scale: 0 = not at all, 1 = several days, ",
                     "2 = more than half the days, 3 = nearly every day")

#' Get the labels for an outcome
#' @param outcome Outcome column name.
#' @return List with label, title and question. Falls back to the
#'   column name if the outcome is not in OUTCOME_INFO.
get_outcome_info <- function(outcome) {
  info <- OUTCOME_INFO[[outcome]]
  if (is.null(info)) info <- list(label = outcome, title = outcome, question = outcome)
  info
}

#' Load the NHANES data
#' @param path Path to the .rds file, relative to the top folder of the repo.
#' @return Data frame with one row per participant.
load_nhanes <- function(path = "C:/Users/User/Documents/ResMas Psy UvA/PIPS/group_assignment/sleep-app/nhanes_ref.rds") {
  if (!file.exists(path)) {
    stop("Data file not found: ", path,
         "\nCheck that your working directory is the top folder of the repository.")
  }
  readRDS(path)
}

#' Prepare the data for the analysis
#' Keeps adults with an answer on the outcome and a plausible social jet lag.
#' @param data Data frame from load_nhanes().
#' @param outcome Outcome column: "phq_tired" or "phq_down".
#' @param max_sjl Largest social jet lag (hours) we keep. Higher values are
#'   treated as implausible (for example shift work).
#' @param min_age Youngest age kept. The mood questions were only asked from age 18.
#' @return Data frame without missing values on the analysis variables.
prepare_analysis_data <- function(data, outcome = "phq_tired",
                                  max_sjl = 6, min_age = 18) {
  needed <- c(outcome, "sjl", "age", "sex", "sd_week", "w")
  missing_cols <- setdiff(needed, names(data))
  if (length(missing_cols) > 0) {
    stop("These columns are missing from the data: ",
         paste(missing_cols, collapse = ", "))
  }

  n_start <- nrow(data)
  out <- data |>
    filter(age >= min_age, sjl <= max_sjl) |>
    select(all_of(needed)) |>
    filter(if_all(everything(), ~ !is.na(.x))) |>
    mutate(sex = factor(sex, levels = c("F", "M"), labels = c("Female", "Male")))

  message(sprintf("Kept %d of %d participants (age >= %d, social jet lag <= %g h, no missing values).",
                  nrow(out), n_start, min_age, max_sjl))
  out
}

#' Fit the regression model
#' @param data Data frame from prepare_analysis_data().
#' @param outcome Outcome column.
#' @param covariates Control variables added next to social jet lag.
#' @param weighted If TRUE, uses the NHANES survey weights (column w).
#' @return An lm model.
fit_sjl_model <- function(data, outcome = "phq_tired",
                          covariates = c("age", "sex", "sd_week"),
                          weighted = FALSE) {
  model_formula <- reformulate(c("sjl", covariates), response = outcome)
  if (weighted) {
    lm(model_formula, data = data, weights = w)
  } else {
    lm(model_formula, data = data)
  }
}

#' Summarise the model as a readable table
#' @param model An lm model from fit_sjl_model().
#' @return Data frame with estimate, 95% confidence interval and p-value per predictor.
summarise_model <- function(model) {
  coefs <- summary(model)$coefficients
  ci <- confint(model)
  data.frame(
    predictor = rownames(coefs),
    estimate  = round(coefs[, "Estimate"], 3),
    ci_lower  = round(ci[, 1], 3),
    ci_upper  = round(ci[, 2], 3),
    p_value   = signif(coefs[, "Pr(>|t|)"], 3),
    row.names = NULL
  )
}

#' Plot the outcome by social jet lag group
#' Shows the mean score per group with 95% confidence intervals.
#' The y-axis starts at 0 so small differences are not exaggerated.
#' @param data Data frame from prepare_analysis_data().
#' @param outcome Outcome column.
#' @param breaks Cut points (hours) for the social jet lag groups.
#' @return A ggplot object.
plot_sjl_outcome <- function(data, outcome = "phq_tired",
                             breaks = c(0, 1, 2, 3, Inf)) {
  info <- get_outcome_info(outcome)

  group_labels <- ifelse(is.infinite(breaks[-1]),
                         paste0(breaks[-length(breaks)], "+ h"),
                         paste0(breaks[-length(breaks)], "-", breaks[-1], " h"))

  summary_df <- data |>
    mutate(sjl_group = cut(sjl, breaks = breaks, labels = group_labels,
                           right = FALSE, include.lowest = TRUE)) |>
    group_by(sjl_group) |>
    summarise(n = n(),
              mean = mean(.data[[outcome]]),
              se = sd(.data[[outcome]]) / sqrt(n),
              .groups = "drop") |>
    mutate(lower = mean - 1.96 * se,
           upper = mean + 1.96 * se,
           tooltip = paste0("Social jet lag: ", sjl_group,
                            "<br>People: ", n,
                            "<br>Mean score: ", round(mean, 2),
                            "<br>95% CI: ", round(lower, 2), " to ", round(upper, 2)))

  ggplot(summary_df, aes(x = sjl_group, y = mean, group = 1, text = tooltip)) +
    geom_line(colour = "grey60") +
    geom_pointrange(aes(ymin = lower, ymax = upper), colour = "#165E83") +
    expand_limits(y = 0) +
    labs(x = "Social jet lag (hours)",
         y = info$label,
         title = info$title,
         subtitle = paste0("\"", info$question, "\"\nN = ", sum(summary_df$n))) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(size = 13, face = "bold"))
}

#' Plot answer distribution by social jet lag group
plot_sjl_distribution <- function(data, outcome = "phq_tired",
                                  breaks = c(0:6)) {
  info <- get_outcome_info(outcome)
  
  group_labels <- c(
    paste0(0:5, "-", 1:6, " h")
  )
  
  plot_data <- data |>
    mutate(
      sjl_group = cut(
        sjl,
        breaks = breaks,
        labels = group_labels,
        right = FALSE,
        include.lowest = TRUE
      ),
      answer = factor(
        .data[[outcome]],
        levels = 0:3,
        labels = c(
          "0 - Not at all",
          "1 - Several days",
          "2 - More than half the days",
          "3 - Nearly every day"
        )
      )
    ) |>
    filter(!is.na(sjl_group), !is.na(answer)) |>
    count(sjl_group, answer, .drop = FALSE) |>
    group_by(sjl_group) |>
    mutate(percentage = n / sum(n) * 100) |>
    ungroup()
  
  ggplot(plot_data,
         aes(x = sjl_group, y = percentage, fill = answer)) +
    geom_col(width = 0.75) +
    scale_fill_brewer(palette = "Blues", name = "Answer") +
    scale_y_continuous(
      limits = c(0, 100),
      breaks = seq(0, 100, 20),
      labels = function(x) paste0(x, "%"),
      expand = c(0, 0)
    ) +
    labs(
      x = "Social jet lag (hours)",
      y = "Percentage of participants",
      title = info$question,
      subtitle = paste("N =", nrow(data))
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 13, face = "bold"),
      legend.position = "bottom"
    )
}

#' Put several outcome plots side by side (static version)
#' @param data_list List of data frames, one per outcome.
#' @param outcomes Vector of outcome column names, same order as data_list.
#' @param y_max Top of the y-axis, the same for all plots so they are comparable.
#' @return A patchwork object.
plot_outcomes_static <- function(data_list, outcomes, y_max = 1) {
  if (length(data_list) != length(outcomes)) {
    stop("data_list and outcomes must have the same length.")
  }
  plots <- lapply(seq_along(outcomes), function(i) {
    plot_sjl_outcome(data_list[[i]], outcomes[[i]])
  })
  wrap_plots(plots, nrow = 1) &
    expand_limits(y = c(0, y_max))
}

#' Put several outcome plots side by side (interactive version)
#' Hover over the points to see the exact numbers. Opens in the Viewer tab.
#' @param data_list List of data frames, one per outcome.
#' @param outcomes Vector of outcome column names, same order as data_list.
#' @param y_max Top of the y-axis, the same for all plots so they are comparable.
#' @return A plotly object.
plot_outcomes_interactive <- function(data_list, outcomes, y_max = 1) {
  if (length(data_list) != length(outcomes)) {
    stop("data_list and outcomes must have the same length.")
  }
  n_plots <- length(outcomes)

  plots <- lapply(seq_along(outcomes), function(i) {
    p <- plot_sjl_outcome(data_list[[i]], outcomes[[i]]) +
      expand_limits(y = c(0, y_max))
    ggplotly(p, tooltip = "text")
  })

  # One title above each plot
  plot_titles <- lapply(seq_len(n_plots), function(i) {
    list(text = get_outcome_info(outcomes[[i]])$title,
         x = (i - 0.5) / n_plots, y = 1.02,
         xref = "paper", yref = "paper",
         xanchor = "center", yanchor = "bottom",
         showarrow = FALSE, font = list(size = 14))
  })

  subplot(plots, nrows = 1, titleX = TRUE, titleY = TRUE, margin = 0.07) |>
    layout(title = list(text = "Hover over the points to see the exact numbers",
                        font = list(size = 12), y = 0.99),
           annotations = plot_titles,
           margin = list(t = 80),
           showlegend = FALSE)
}


# ===============================================================
# PART 2: RUN THE ANALYSIS
# (This part will move to the main file later.)
# ===============================================================

nhanes <- load_nhanes()

# ---- Question 1: social jet lag and tiredness ----
dat_tired <- prepare_analysis_data(nhanes, outcome = "phq_tired")
model_tired <- fit_sjl_model(dat_tired, outcome = "phq_tired")
print(summarise_model(model_tired))

# ---- Question 2: social jet lag and mood ----
dat_down <- prepare_analysis_data(nhanes, outcome = "phq_down")
model_down <- fit_sjl_model(dat_down, outcome = "phq_down")
print(summarise_model(model_down))

# ---- Model checks (residual plots) ----
par(mfrow = c(2, 2))
plot(model_tired)
plot(model_down)
par(mfrow = c(1, 1))

# ---- Main plot: static, side by side (Plots tab) ----
static_plot <- plot_outcomes_static(
  data_list = list(dat_tired, dat_down),
  outcomes  = c("phq_tired", "phq_down")
) +
  plot_annotation(caption = paste0("Points show group means with 95% confidence intervals.\n",
                                   SCALE_NOTE))
print(static_plot)

# ---- Main plot: interactive, side by side (Viewer tab) ----
interactive_plot <- plot_outcomes_interactive(
  data_list = list(dat_tired, dat_down),
  outcomes  = c("phq_tired", "phq_down")
)
print(interactive_plot)


# ---- Stacked bar plots: answer distribution ----

distribution_tired <- plot_sjl_distribution(
  dat_tired, outcome = "phq_tired"
)

distribution_down <- plot_sjl_distribution(
  dat_down, outcome = "phq_down"
)

distribution_plot <- distribution_tired + distribution_down +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

print(distribution_plot)
