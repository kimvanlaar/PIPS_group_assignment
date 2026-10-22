# ---------------------------------------------------------------
# R/helpers.R
# Shared by app.R (auto-sourced by Shiny) and data-raw/build_reference.R.
# Using ONE set of formulas for the user AND the NHANES reference
# guarantees an apples-to-apples comparison.
#
# Notation follows the Munich ChronoType Questionnaire (MCTQ):
#   so = sleep onset, se = sleep end (wake), sd = sleep duration,
#   ms = mid-sleep, _w = work days, _f = free days
# ---------------------------------------------------------------

# "07:30" -> 7.5 (hours). Vectorised; NA for blanks / invalid strings.
parse_hm <- function(x) {
  x   <- trimws(as.character(x))
  ok  <- !is.na(x) & grepl("^\\d{1,2}:\\d{2}$", x)
  out <- rep(NA_real_, length(x))
  h   <- suppressWarnings(as.numeric(sub(":.*", "", x[ok])))
  m   <- suppressWarnings(as.numeric(sub(".*:", "", x[ok])))
  v   <- h + m / 60
  v[h > 23 | m > 59] <- NA_real_
  out[ok] <- v
  out
}

# 7.5 -> "07:30" (vectorised)
fmt_hm <- function(h) {
  out  <- rep("\u2013", length(h))
  ok   <- !is.na(h)
  mins <- round((h[ok] %% 24) * 60) %% 1440
  out[ok] <- sprintf("%02d:%02d", as.integer(mins %/% 60), as.integer(mins %% 60))
  out
}

# 2.25 -> "2h 15min"
fmt_dur <- function(h) {
  mins <- round(abs(h) * 60)
  sprintf("%dh %02dmin", as.integer(mins %/% 60), as.integer(mins %% 60))
}

# Circular mean of clock times in hours (23:30 and 00:30 -> 00:00)
circ_mean <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_real_)
  a <- x / 24 * 2 * pi
  (atan2(mean(sin(a)), mean(cos(a))) / (2 * pi) * 24) %% 24
}

# Shortest distance between two clock times (hours)
circ_diff <- function(a, b) {
  d <- abs(a - b) %% 24
  pmin(d, 24 - d)
}

# Put mid-sleep on a linear scale for plotting / percentiles
# (e.g. 23:30 -> -0.5, so very early sleepers don't land at the far right)
wrap_ms <- function(x) ifelse(x > 14, x - 24, x)

# Weighted percentile: % of (weighted) reference at or below value v
w_pct <- function(x, w, v) {
  ok <- !is.na(x) & !is.na(w)
  100 * sum(w[ok & x <= v]) / sum(w[ok])
}

season_of <- function(date, lat) {
  m <- as.integer(format(date, "%m"))
  north <- c("Winter","Winter","Spring","Spring","Spring","Summer",
             "Summer","Summer","Autumn","Autumn","Autumn","Winter")
  south <- c("Summer","Summer","Autumn","Autumn","Autumn","Winter",
             "Winter","Winter","Spring","Spring","Spring","Summer")
  if (lat >= 0) north[m] else south[m]
}

# ---- MCTQ core (vectorised; works on one person or a whole data frame) ----
# n_w = number of work days per week (MCTQ default: 5)
# sd_week may be supplied directly (used for the app, where we know the real week)
mctq_core <- function(sd_w, sd_f, ms_w, ms_f, n_w = 5, sd_week = NULL) {
  if (is.null(sd_week)) sd_week <- (sd_w * n_w + sd_f * (7 - n_w)) / 7
  # Sleep-corrected mid-sleep on free days (removes the "catch-up sleep" effect)
  msf_sc <- ifelse(!is.na(sd_f) & !is.na(sd_w) & sd_f > sd_w,
                   (ms_f - (sd_f - sd_week) / 2) %% 24,
                   ms_f)
  list(
    sd_w = sd_w, sd_f = sd_f, sd_week = sd_week,
    ms_w = ms_w, ms_f = ms_f,
    msf_sc = msf_sc,
    sjl = circ_diff(ms_f, ms_w)         # social jet lag
  )
}

# Metrics from the user's 7-day diary (data frame with work, sd, ms)
user_metrics <- function(d) {
  d  <- d[!is.na(d$sd), ]
  wd <- d[d$work, ]
  fd <- d[!d$work, ]
  core <- mctq_core(
    sd_w = if (nrow(wd)) mean(wd$sd) else NA_real_,
    sd_f = if (nrow(fd)) mean(fd$sd) else NA_real_,
    ms_w = if (nrow(wd)) circ_mean(wd$ms) else NA_real_,
    ms_f = if (nrow(fd)) circ_mean(fd$ms) else NA_real_,
    sd_week = mean(d$sd)
  )
  c(core, list(n_w = nrow(wd), n_f = nrow(fd)))
}

