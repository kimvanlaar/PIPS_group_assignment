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
# Option 1: Unadjusted straight-line fit using geom_smooth(method = "lm")
# Option 2: Unadjusted smooth GAM curve 
# Option 3: Adjusted GAM curve
# Uses: get_outcome_info(), SCALE_NOTE, patchwork, ggplot2
# Extra package: mgcv (ships with R, so usually already installed)
# ---------------------------------------------------------------

if (!requireNamespace("mgcv", quietly = TRUE)) install.packages("mgcv")

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
