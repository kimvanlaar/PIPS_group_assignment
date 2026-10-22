# ---------------------------------------------------------------
# analysis/stats_analysis.R
# Research questions:
#   1. More social jet lag, more tiredness?
#   2. More social jet lag, lower mood?
# Both controlling for age, sex and sleep duration.
# Data: NHANES 2017 - March 2020.
#

# ===============================================================
# RUN THE ANALYSIS
# ===============================================================

source(here::here("src", "R", "helpers.R"))
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

# ---------------------------------------------------------------
# Continuous social jet lag plots
# Option 1: smooth GAM curve (original version)
# Option 2: unadjusted straight-line fit using geom_smooth(method = "lm")
# Paste PART 1C with the other functions in stats_analysis.R,
# and PART 2C at the end (needs dat_tired and dat_down from Part 2).
# Uses: get_outcome_info(), SCALE_NOTE, patchwork, ggplot2
# Extra package: mgcv (ships with R, so usually already installed)
# ---------------------------------------------------------------

if (!requireNamespace("mgcv", quietly = TRUE)) install.packages("mgcv")

# ===============================================================
# PART 1C: FUNCTIONS
# ===============================================================

#' Plot the outcome against continuous social jet lag
#' Draws a fitted line or curve with a 95% confidence band.
#' A rug at the bottom shows where the data are.
#'
#' adjusted = FALSE: fit the raw scores using the selected method.
#'   unadjusted_method = "gam": flexible smooth curve (Option 1).
#'   unadjusted_method = "lm": straight-line fit (Option 2).
#' adjusted = TRUE: use the original adjusted GAM, with numeric
#'   covariates held at their mean and factors at their first level.
#'   The unadjusted_method argument does not change the adjusted model.
#'
#' @param data Data frame from prepare_analysis_data().
#' @param outcome Outcome column: "phq_tired" or "phq_down".
#' @param adjusted If TRUE, adjust for the covariates using a GAM.
#' @param covariates Control variables (only used when adjusted = TRUE).
#' @param y_max Top of the y-axis, the same for all plots.
#' @param k Flexibility of the GAM curve (maximum basis functions).
#' @param unadjusted_method "gam" for a curve or "lm" for a straight line.
#' @return A ggplot object.
plot_sjl_continuous <- function(data, outcome = "phq_tired",
                                adjusted = FALSE,
                                covariates = c("age", "sex", "sd_week"),
                                y_max = 1, k = 5,
                                unadjusted_method = c("gam", "lm")) {
  unadjusted_method <- match.arg(unadjusted_method)
  info <- get_outcome_info(outcome)
  
  if (!adjusted) {
    p <- ggplot(data, aes(x = sjl, y = .data[[outcome]]))
    
    if (unadjusted_method == "lm") {
      p <- p + geom_smooth(
        method = "lm",
        formula = y ~ x,
        se = TRUE,
        level = 0.95,
        colour = "#165E83",
        fill = "#165E83",
        alpha = 0.2
      )
    } else {
      p <- p + geom_smooth(
        method = "gam",
        formula = as.formula(paste0("y ~ s(x, k = ", k, ")")),
        se = TRUE,
        level = 0.95,
        colour = "#165E83",
        fill = "#165E83",
        alpha = 0.2
      )
    }
  } else {
    rhs <- c(paste0("s(sjl, k = ", k, ")"), covariates)
    model <- mgcv::gam(reformulate(rhs, response = outcome), data = data)
    
    # Grid of social jet lag values, other variables held fixed
    grid <- data.frame(sjl = seq(min(data$sjl), max(data$sjl), length.out = 100))
    for (v in covariates) {
      grid[[v]] <- if (is.numeric(data[[v]])) {
        mean(data[[v]])
      } else {
        factor(levels(data[[v]])[1], levels = levels(data[[v]]))
      }
    }
    pr <- predict(model, newdata = grid, se.fit = TRUE)
    grid$fit   <- pr$fit
    grid$lower <- pr$fit - 1.96 * pr$se.fit
    grid$upper <- pr$fit + 1.96 * pr$se.fit
    
    p <- ggplot(grid, aes(x = sjl, y = fit)) +
      geom_ribbon(aes(ymin = lower, ymax = upper),
                  fill = "#165E83", alpha = 0.2) +
      geom_line(colour = "#165E83", linewidth = 1)
  }
  
  p +
    geom_rug(data = data, aes(x = sjl), inherit.aes = FALSE,
             sides = "b", alpha = 0.05) +
    scale_x_continuous(breaks = 0:6) +
    coord_cartesian(ylim = c(0, y_max)) +
    labs(x = "Social jet lag (hours)",
         y = info$label,
         title = info$title,
         subtitle = paste0("\"", info$question, "\"\nN = ", nrow(data))) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(size = 13, face = "bold"))
}

#' Put several continuous outcome plots side by side
#' @param data_list List of data frames, one per outcome.
#' @param outcomes Vector of outcome column names, same order as data_list.
#' @param adjusted Passed to plot_sjl_continuous().
#' @param y_max Top of the y-axis, the same for all plots.
#' @param unadjusted_method "gam" (Option 1) or "lm" (Option 2).
#' @return A patchwork object.
plot_outcomes_continuous <- function(data_list, outcomes,
                                     adjusted = FALSE, y_max = 1,
                                     unadjusted_method = c("gam", "lm")) {
  unadjusted_method <- match.arg(unadjusted_method)
  if (length(data_list) != length(outcomes)) {
    stop("data_list and outcomes must have the same length.")
  }
  plots <- lapply(seq_along(outcomes), function(i) {
    plot_sjl_continuous(data_list[[i]], outcomes[[i]],
                        adjusted = adjusted, y_max = y_max,
                        unadjusted_method = unadjusted_method)
  })
  wrap_plots(plots, nrow = 1)
}

# ===============================================================
# PART 2C: RUN
# ===============================================================


# ---- Option 1: Unadjusted straight-line fit ----
continuous_linear_plot <- plot_outcomes_continuous(
  data_list = list(dat_tired, dat_down),
  outcomes = c("phq_tired", "phq_down"),
  adjusted = FALSE,
  unadjusted_method = "lm"
) +
  plot_annotation(caption = paste0("Linear fit with 95% confidence band. Unadjusted. ",
                                   "Ticks at the bottom show the data.\n",
                                   SCALE_NOTE))
print(continuous_linear_plot)
#not super informative so trying the curve!

# ---- Option 2: Unadjusted smooth GAM curve ----
continuous_plot <- plot_outcomes_continuous(
  data_list = list(dat_tired, dat_down),
  outcomes = c("phq_tired", "phq_down"),
  unadjusted_method = "gam"
) +
  plot_annotation(caption = paste0("Smooth curve (GAM) with 95% confidence band. Unadjusted. ",
                                   "Ticks at the bottom show the data.\n",
                                   SCALE_NOTE))
print(continuous_plot)

# ---- Adjusted GAM: age, sex and weekly sleep duration as covariates considered----
continuous_adj_plot <- plot_outcomes_continuous(
  data_list = list(dat_tired, dat_down),
  outcomes = c("phq_tired", "phq_down"),
  adjusted = TRUE
) +
  plot_annotation(caption = paste0("Adjusted for age, sex and weekly sleep duration ",
                                   "(age and sleep at their mean, sex = Female).\n",
                                   "Smooth curve (GAM) with 95% confidence band. ",
                                   SCALE_NOTE))
print(continuous_adj_plot)

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
