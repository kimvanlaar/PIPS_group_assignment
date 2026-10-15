# ---------------------------------------------------------------
# Sleep, light & mood week-diary  |  Shiny app  (v0.1)
# ---------------------------------------------------------------
# install.packages(c("shiny","bslib","dplyr","ggplot2","lubridate",
#                    "suncalc","tidygeocoder","lutz"))
# Optional later: install.packages("mctq")
# ---------------------------------------------------------------

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(lubridate)
library(suncalc)        # sunrise / sunset from lat, lon, date
library(tidygeocoder)   # place name -> lat / lon (OpenStreetMap)
library(lutz)           # lat / lon -> time zone

# ------------------------- helpers -------------------------------

# "07:30" -> 7.5 (hours). Returns NA if it can't be parsed.
parse_hm <- function(x) {
  x <- trimws(x)
  if (is.null(x) || is.na(x) || !grepl("^\\d{1,2}:\\d{2}$", x)) return(NA_real_)
  p <- as.numeric(strsplit(x, ":")[[1]])
  if (p[1] > 23 || p[2] > 59) return(NA_real_)
  p[1] + p[2] / 60
}

fmt_hm <- function(h) {
  if (length(h) == 0 || is.na(h)) return("–")
  h <- h %% 24
  sprintf("%02d:%02d", floor(h), round((h - floor(h)) * 60) %% 60)
}

# circular mean of clock times (hours), so 23:30 and 00:30 average to 00:00
circ_mean <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_real_)
  a <- x / 24 * 2 * pi
  (atan2(mean(sin(a)), mean(cos(a))) / (2 * pi) * 24) %% 24
}

# smallest distance between two clock times (hours)
circ_diff <- function(a, b) {
  d <- abs(a - b) %% 24
  pmin(d, 24 - d)
}

season_of <- function(date, lat) {
  m <- month(date)
  north <- c("Winter","Winter","Spring","Spring","Spring","Summer",
             "Summer","Summer","Autumn","Autumn","Autumn","Winter")
  south <- c("Summer","Summer","Autumn","Autumn","Autumn","Winter",
             "Winter","Winter","Spring","Spring","Spring","Summer")
  if (lat >= 0) north[m] else south[m]
}

# MCTQ-style metrics from the 7-day diary (follows Roenneberg et al. 2003/2019)
# Work days = days flagged as work days; free days = the rest.
mctq_metrics <- function(d) {
  wd <- d %>% filter(work)
  fd <- d %>% filter(!work)
  n_w <- nrow(wd); n_f <- nrow(fd)

  SDw <- if (n_w) mean(wd$sd) else NA_real_
  SDf <- if (n_f) mean(fd$sd) else NA_real_
  MSW <- if (n_w) circ_mean(wd$ms) else NA_real_
  MSF <- if (n_f) circ_mean(fd$ms) else NA_real_
  SDweek <- mean(d$sd)

  # sleep-corrected mid-sleep on free days (chronotype proxy)
  MSFsc <- if (is.na(MSF)) NA_real_ else if (!is.na(SDf) && !is.na(SDw) && SDf > SDw) {
    (MSF - (SDf - SDweek) / 2) %% 24
  } else MSF

  SJL <- if (is.na(MSW) || is.na(MSF)) NA_real_ else circ_diff(MSF, MSW)

  list(SDw = SDw, SDf = SDf, SDweek = SDweek, MSW = MSW, MSF = MSF,
       MSFsc = MSFsc, SJL = SJL, n_w = n_w, n_f = n_f)
}

# ------------------- reference population ------------------------
# !! PLACEHOLDER !! Simulated data loosely shaped like published MCTQ norms.
# Replace this function later with a real dataset (e.g. the large MCTQ / EUCLOCK
# data, or a cleaned file read from CSV). It only needs these columns:
#   msfsc (clock hours, e.g. 4.5 = 04:30), sd_week (h), sjl (h), age, sex
load_reference <- function(n = 5000, seed = 1) {
  set.seed(seed)
  tibble(
    age   = round(runif(n, 16, 80)),
    sex   = sample(c("F", "M"), n, replace = TRUE),
    msfsc = rnorm(n, 4.6, 1.3),
    sd_week = pmin(pmax(rnorm(n, 7.4, 0.9), 4.5), 11),
    sjl   = pmin(rgamma(n, shape = 2.2, scale = 0.45), 5)
  )
}
REF <- load_reference()

# ------------------------------ UI -------------------------------

day_row <- function(i) {
  fluidRow(
    column(1, strong(paste("Day", i)), style = "padding-top:28px;"),
    column(2, checkboxInput(paste0("work", i), "Work day", value = i <= 5)),
    column(2, textInput(paste0("bed", i), "Went to bed (night before)", "23:30")),
    column(2, textInput(paste0("wake", i), "Woke up", if (i <= 5) "07:00" else "09:00")),
    column(2, textInput(paste0("start", i), "Work starts", if (i <= 5) "09:00" else "")),
    column(3, sliderInput(paste0("tired", i), "Tiredness (1–10)", 1, 10, 5, ticks = FALSE),
           sliderInput(paste0("mood", i), "Mood (1–10)", 1, 10, 6, ticks = FALSE))
  )
}

ui <- page_navbar(
  title = "Sleep, light & mood diary",
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  nav_panel(
    "1 · Your week",
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("Where & when"),
        textInput("place", "Place", placeholder = "e.g. Berlin, Germany"),
        actionButton("geocode", "Look up location", class = "btn-primary"),
        numericInput("lat", "Latitude", NA, step = 0.01),
        numericInput("lon", "Longitude", NA, step = 0.01),
        dateInput("date", "First day of your diary week", value = Sys.Date()),
        textOutput("season_txt"),
        hr(),
        checkboxInput("manual_sun", "I want to enter sunrise/sunset myself"),
        conditionalPanel(
          "input.manual_sun",
          textInput("sunrise_in", "Sunrise (HH:MM)", "07:30"),
          textInput("sunset_in", "Sunset (HH:MM)", "17:00")
        ),
        textOutput("sun_txt")
      ),
      card(
        card_header("Your 7 days (use 24h format HH:MM; leave 'Work starts' empty on free days)"),
        lapply(1:7, day_row)
      )
    )
  ),

  nav_panel(
    "2 · Results",
    layout_columns(
      fill = FALSE,
      value_box("Avg sleep duration", textOutput("vb_sd"), theme = "primary"),
      value_box("Chronotype (MSFsc)", textOutput("vb_msfsc"), theme = "info"),
      value_box("Social jet lag", textOutput("vb_sjl"), theme = "warning"),
      value_box("Avg mood / tiredness", textOutput("vb_mt"), theme = "success")
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(card_header("Sleep vs. daylight"), plotOutput("p_timeline", height = 380)),
      card(card_header("Mood & tiredness"), plotOutput("p_moodtired", height = 380))
    ),
    card(card_header("How you compare to the reference population"),
         layout_columns(
           plotOutput("p_ref_msfsc", height = 260),
           plotOutput("p_ref_sd", height = 260),
           plotOutput("p_ref_sjl", height = 260)
         ),
         uiOutput("ref_note"))
  )
)

# ---------------------------- server -----------------------------

server <- function(input, output, session) {

  # ---- location ----
  observeEvent(input$geocode, {
    req(nzchar(input$place))
    res <- tryCatch(
      tidygeocoder::geo(input$place, method = "osm", quiet = TRUE),
      error = function(e) NULL
    )
    if (is.null(res) || is.na(res$lat[1])) {
      showNotification("Couldn't find that place – enter lat/lon manually.", type = "error")
    } else {
      updateNumericInput(session, "lat", value = round(res$lat[1], 4))
      updateNumericInput(session, "lon", value = round(res$long[1], 4))
    }
  })

  tz <- reactive({
    req(input$lat, input$lon)
    tryCatch(lutz::tz_lookup_coords(input$lat, input$lon, method = "fast", warn = FALSE),
             error = function(e) "UTC")
  })

  output$season_txt <- renderText({
    req(input$lat, input$date)
    paste("Season:", season_of(input$date, input$lat))
  })

  # ---- sunrise / sunset for each of the 7 days (hours, local clock time) ----
  sun <- reactive({
    dates <- as.Date(input$date) + 0:6
    if (isTRUE(input$manual_sun)) {
      return(tibble(sunrise = parse_hm(input$sunrise_in), sunset = parse_hm(input$sunset_in)) %>%
               slice(rep(1, 7)))
    }
    req(input$lat, input$lon)
    st <- suncalc::getSunlightTimes(date = dates, lat = input$lat, lon = input$lon,
                                    keep = c("sunrise", "sunset"), tz = tz())
    tibble(
      sunrise = hour(st$sunrise) + minute(st$sunrise) / 60,
      sunset  = hour(st$sunset)  + minute(st$sunset)  / 60
    )
  })

  output$sun_txt <- renderText({
    s <- sun()
    paste0("Sunrise ≈ ", fmt_hm(s$sunrise[1]), " · Sunset ≈ ", fmt_hm(s$sunset[1]),
           " (first day)")
  })

  # ---- diary as a tidy data frame ----
  diary <- reactive({
    s <- sun()
    tibble(day = 1:7) %>%
      rowwise() %>%
      mutate(
        date  = as.Date(input$date) + day - 1,
        work  = isTRUE(input[[paste0("work", day)]]),
        bed   = parse_hm(input[[paste0("bed", day)]]),
        wake  = parse_hm(input[[paste0("wake", day)]]),
        start = parse_hm(input[[paste0("start", day)]]),
        tired = input[[paste0("tired", day)]],
        mood  = input[[paste0("mood", day)]]
      ) %>%
      ungroup() %>%
      mutate(
        sd = (wake - bed) %% 24,
        ms = (bed + sd / 2) %% 24,
        sunrise = s$sunrise,
        sunset  = s$sunset,
        wake_after_sunrise = ((wake - sunrise + 12) %% 24) - 12
      )
  })

  metrics <- reactive({
    d <- diary() %>% filter(!is.na(sd))
    validate(need(nrow(d) > 0, "Please enter valid bed/wake times (HH:MM)."))
    mctq_metrics(d)
  })

  # ---- value boxes ----
  output$vb_sd     <- renderText(sprintf("%.1f h", metrics()$SDweek))
  output$vb_msfsc  <- renderText(fmt_hm(metrics()$MSFsc))
  output$vb_sjl    <- renderText(if (is.na(metrics()$SJL)) "need work + free days" else sprintf("%.1f h", metrics()$SJL))
  output$vb_mt     <- renderText({
    d <- diary(); sprintf("%.1f / %.1f", mean(d$mood), mean(d$tired))
  })

  # ---- plots ----
  output$p_timeline <- renderPlot({
    d <- diary() %>%
      filter(!is.na(sd)) %>%
      mutate(
        bed_x  = ifelse(bed > 12, bed - 24, bed),   # evening bedtimes drawn left of midnight
        wake_x = ifelse(wake < bed_x, wake + 24, wake),
        lbl = paste0("Day ", day, " (", format(date, "%a"), ")")
      )
    ggplot(d, aes(y = reorder(lbl, -day))) +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "grey97") +
      geom_rect(aes(xmin = sunrise, xmax = sunset,
                    ymin = as.numeric(reorder(lbl, -day)) - 0.42,
                    ymax = as.numeric(reorder(lbl, -day)) + 0.42),
                fill = "#FFE9A8", alpha = .8) +
      geom_segment(aes(x = bed_x, xend = wake_x, yend = lbl),
                   linewidth = 6, colour = "#2C3E70", lineend = "round") +
      geom_point(aes(x = start, shape = "Work starts"), size = 3.5, colour = "#E74C3C", na.rm = TRUE) +
      scale_x_continuous(breaks = seq(-6, 24, 3),
                         labels = function(x) sprintf("%02d:00", x %% 24),
                         limits = c(-6, 24)) +
      scale_shape_manual(NULL, values = c("Work starts" = 17)) +
      labs(x = NULL, y = NULL,
           subtitle = "Dark bar = sleep · yellow band = daylight (sunrise → sunset)") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom", panel.grid.minor = element_blank())
  })

  output$p_moodtired <- renderPlot({
    diary() %>%
      select(day, Mood = mood, Tiredness = tired) %>%
      tidyr::pivot_longer(-day) %>%
      ggplot(aes(day, value, colour = name)) +
      geom_line(linewidth = 1.2) + geom_point(size = 3) +
      scale_x_continuous(breaks = 1:7) +
      scale_y_continuous(limits = c(1, 10), breaks = 1:10) +
      scale_colour_manual(NULL, values = c(Mood = "#27AE60", Tiredness = "#E67E22")) +
      labs(x = "Day", y = "Score") +
      theme_minimal(base_size = 13) + theme(legend.position = "bottom")
  })

  # generic "you vs population" histogram
  ref_plot <- function(ref_vals, you, xlab, fmt = function(x) x) {
    validate(need(!is.na(you), "Not enough data yet"))
    pct <- round(100 * mean(ref_vals <= you))
    ggplot(tibble(x = ref_vals), aes(x)) +
      geom_histogram(bins = 40, fill = "grey75", colour = "white") +
      geom_vline(xintercept = you, colour = "#E74C3C", linewidth = 1.4) +
      labs(x = xlab, y = NULL,
           title = paste0("You: ", fmt(you), "  (", pct, "th percentile)")) +
      theme_minimal(base_size = 12) +
      theme(axis.text.y = element_blank(), panel.grid = element_blank())
  }

  output$p_ref_msfsc <- renderPlot({
    you <- metrics()$MSFsc
    if (!is.na(you) && you > 14) you <- you - 24   # keep on same scale as ref
    ref_plot(REF$msfsc, you, "Chronotype: MSFsc (clock hours after midnight)",
             function(x) fmt_hm(x))
  })
  output$p_ref_sd <- renderPlot(
    ref_plot(REF$sd_week, metrics()$SDweek, "Average weekly sleep duration (h)",
             function(x) sprintf("%.1f h", x))
  )
  output$p_ref_sjl <- renderPlot(
    ref_plot(REF$sjl, metrics()$SJL, "Social jet lag (h)",
             function(x) sprintf("%.1f h", x))
  )

  output$ref_note <- renderUI({
    tags$p(class = "text-muted small",
           "⚠️ The reference population is currently SIMULATED placeholder data. ",
           "Swap in a real dataset by editing load_reference(). ",
           "Mood and tiredness are not part of MCTQ, so they aren't compared yet.")
  })
}

shinyApp(ui, server)
