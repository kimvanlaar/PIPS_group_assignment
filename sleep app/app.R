# ---------------------------------------------------------------
# Sleep, light & mood diary  |  Shiny app  (v0.2)
# Reference population: NHANES 2017 - March 2020 (run data-raw/build_reference.R once)
#
# install.packages(c("shiny","bslib","bsicons","dplyr","tidyr","tibble","ggplot2",
#                    "suncalc","tidygeocoder","lutz","haven"))
# ---------------------------------------------------------------

library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(suncalc)        # sunrise / sunset
library(tidygeocoder)   # place -> lat / lon
library(lutz)           # lat / lon -> time zone
# R/helpers.R is sourced automatically by Shiny

# ------------------------- look & feel ---------------------------

pal <- list(night = "#1B2A4A", sleep = "#3B4F9A", day = "#FFE29A", sun = "#F2A93B",
            you = "#E4572E", pop = "#C9D1E3", good = "#2E9E83", mute = "#6C7A96")

# >>> CHANGE FONTS HERE (any Google Font name works) <<<
FONT_BODY    <- "Nunito"
FONT_HEADING <- "Poppins"

app_theme <- bs_theme(
  version = 5,
  bg = "#F7F5F0", fg = pal$night,
  primary = pal$sleep, secondary = pal$mute,
  success = pal$good, warning = pal$sun, info = "#5B8DEF",
  base_font = font_google(FONT_BODY, local = FALSE),
  heading_font = font_google(FONT_HEADING, local = FALSE)
) |>
  bs_add_rules("
    .navbar { background: #1B2A4A !important; }
    .navbar .navbar-brand, .navbar .nav-link { color: #F7F5F0 !important; }
    .navbar .nav-link.active { border-bottom: 2px solid #F2A93B; }
    .card { border: 0; border-radius: 16px; box-shadow: 0 2px 14px rgba(27,42,74,.07); }
    .card-header { background: transparent; border-bottom: 0; 
                   font-size: 1.15rem; padding-bottom: 0; }
    .bslib-value-box { border-radius: 16px; }
    .week-wrap { overflow-x: auto; }
    .week-grid { display: grid; min-width: 760px;
                 grid-template-columns: 96px 64px repeat(3, minmax(96px, 1fr)) repeat(2, minmax(120px, 1.3fr));
                 gap: .35rem .75rem; align-items: center; }
    .wk-head { font-size: .72rem; text-transform: uppercase; letter-spacing: .06em; color: #6C7A96; }
    .wk-day  { font-weight: 600; }
    .week-grid .form-group, .week-grid .shiny-input-container { margin-bottom: 0; width: 100%; }
    .week-grid .irs-min, .week-grid .irs-max { display: none; }
    .week-grid .irs { height: 34px; }
    .insight-list li { padding: .45rem 0; border-bottom: 1px solid #ECE8DF; }
    .insight-list li:last-child { border-bottom: 0; }
    .insight-list svg { color: #F2A93B; margin-right: .35rem; }
    .pill { display: inline-block; padding: .2rem .7rem; border-radius: 999px;
            background: #FFF1C9; color: #8A5A00; font-size: .85rem; margin-right: .4rem; }
    .hint { color: #6C7A96; font-size: .85rem; }
  ")

theme_sleep <- function(base = 12) {
  theme_minimal(base_size = base, base_family = "") +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.y = element_line(colour = "#ECE8DF"),
          panel.grid.major.x = element_line(colour = "#ECE8DF"),
          plot.title = element_text(face = "bold", colour = pal$night),
          plot.subtitle = element_text(colour = pal$mute),
          axis.title = element_text(colour = pal$mute),
          legend.position = "bottom",
          plot.background = element_rect(fill = NA, colour = NA))
}

# ------------------- reference population ------------------------

simulate_reference <- function(n = 5000, seed = 1) {
  set.seed(seed)
  sd_w <- pmin(pmax(rnorm(n, 7.0, 1.1), 3.5), 11)
  sd_f <- pmin(pmax(sd_w + rnorm(n, 0.8, 0.9), 3.5), 12)
  ms_w <- (rnorm(n, 3.6, 1.2)) %% 24
  ms_f <- (ms_w + rgamma(n, 2, 3)) %% 24
  core <- mctq_core(sd_w, sd_f, ms_w, ms_f)
  tibble(age = round(runif(n, 16, 80)), sex = sample(c("F", "M"), n, TRUE), w = 1,
         sd_w, sd_f, sd_week = core$sd_week, ms_w, ms_f, msf_sc = core$msf_sc, sjl = core$sjl,
         phq_down = sample(0:3, n, TRUE, c(.65, .22, .07, .06)),
         phq_tired = sample(0:3, n, TRUE, c(.35, .38, .13, .14)))
}

REF_REAL <- file.exists("data/nhanes_ref.rds")
REF <- if (REF_REAL) readRDS("data/nhanes_ref.rds") else simulate_reference()

PHQ_LABELS <- c("Not at all", "Several days", "More than half the days", "Nearly every day")

# ------------------------------ UI -------------------------------

time_input <- function(id, value = "") {
  tags$input(type = "time", id = id, value = value,
             class = "form-control form-control-sm native-time")
}

day_cells <- function(i) {
  tagList(
    div(class = "wk-day", textOutput(paste0("dlab", i), inline = TRUE)),
    input_switch(paste0("work", i), label = NULL, value = i <= 5),
    time_input(paste0("so", i), if (i <= 5) "23:30" else "00:30"),
    time_input(paste0("se", i), if (i <= 5) "07:00" else "09:00"),
    time_input(paste0("start", i), if (i <= 5) "09:00" else ""),
    sliderInput(paste0("tired", i), NULL, 1, 10, 5, step = 1, ticks = FALSE, width = "100%"),
    sliderInput(paste0("mood", i), NULL, 1, 10, 6, step = 1, ticks = FALSE, width = "100%")
  )
}

js_time <- "
$(document).on('change input', 'input.native-time', function() {
  Shiny.setInputValue(this.id, this.value);
});
$(document).on('shiny:connected', function() {
  $('input.native-time').each(function() { Shiny.setInputValue(this.id, this.value); });
});
"

ui <- page_navbar(
  title = tagList(bs_icon("moon-stars-fill"), " Sleep & light diary"),
  theme = app_theme,
  header = tags$head(tags$script(HTML(js_time))),
  fillable = FALSE,

  # ---------------- TAB 1: input ----------------
  nav_panel(
    "1 \u00b7 Your week", icon = bs_icon("pencil-square"),
    layout_columns(
      col_widths = c(4, 8),
      div(
        card(
          card_header("About you"),
          textInput("pid", "Participant ID", placeholder = "e.g. P001"),
          layout_columns(
            numericInput("age", "Age", 30, min = 16, max = 100),
            selectInput("sex", "Sex", c("Female" = "F", "Male" = "M"))
          ),
          p(class = "hint", "Age and sex are only used to pick a matching comparison group."),
          hr(),
          h6("Over the past two weeks, how often have you been bothered by…"),
          p(class = "hint", "Same wording as the NHANES questionnaire, so we can compare you to a real population."),
          radioButtons("phq_down", "Feeling down, depressed or hopeless",
                       choices = setNames(0:3, PHQ_LABELS), selected = 0, inline = TRUE),
          radioButtons("phq_tired", "Feeling tired or having little energy",
                       choices = setNames(0:3, PHQ_LABELS), selected = 1, inline = TRUE)
        ),
        card(
          card_header("Where & when"),
          textInput("place", NULL, placeholder = "City, country (e.g. Berlin, Germany)"),
          actionButton("geocode", "Find location", icon = icon("location-dot"), class = "btn-primary btn-sm"),
          layout_columns(
            numericInput("lat", "Latitude", NA, step = 0.01),
            numericInput("lon", "Longitude", NA, step = 0.01)
          ),
          dateInput("date", "First day of your diary week", value = Sys.Date()),
          uiOutput("season_badge"),
          hr(),
          input_switch("manual_sun", "Enter sunrise / sunset myself", value = FALSE),
          conditionalPanel(
            "input.manual_sun",
            layout_columns(
              div(tags$label("Sunrise", class = "form-label"), time_input("sunrise_in", "07:30")),
              div(tags$label("Sunset", class = "form-label"), time_input("sunset_in", "17:00"))
            )
          )
        )
      ),
      card(
        card_header("Your 7 days"),
        p(class = "hint",
          "Enter when you actually ", strong("fell asleep"), " (not when you got into bed) and when you woke up. ",
          "Switch 'Work' off for free days; leave 'Work starts' empty then."),
        div(class = "week-wrap",
            div(class = "week-grid",
                div(class = "wk-head", "Day"), div(class = "wk-head", "Work"),
                div(class = "wk-head", "Fell asleep"), div(class = "wk-head", "Woke up"),
                div(class = "wk-head", "Work starts"),
                div(class = "wk-head", "Tiredness (1\u201310)"), div(class = "wk-head", "Mood (1\u201310)"),
                lapply(1:7, day_cells)))
      )
    )
  ),

  # ---------------- TAB 2: results ----------------
  nav_panel(
    "2 \u00b7 Results", icon = bs_icon("graph-up"),
    uiOutput("pid_banner"),
    layout_columns(
      fill = FALSE,
      value_box("Average sleep", textOutput("vb_sd"), showcase = bs_icon("moon-stars"), theme = "primary"),
      value_box(
        title = tagList("Body clock type ",
                        tooltip(bs_icon("info-circle"),
                                "Whether your body clock runs early or late. We use the middle of your sleep on free days, adjusted for catch-up sleep. A lower time means earlier. (Scientific name: chronotype, MSFsc.)")),
        value = textOutput("vb_msfsc"),
        textOutput("vb_chrono_label"),
        showcase = bs_icon("clock-history"), theme = "info"),
      value_box(
        title = tagList("Work vs. free-day shift ",
                        tooltip(bs_icon("info-circle"),
                                "How much the middle of your sleep moves between work days and free days. A big shift feels like jet lag. (Scientific name: social jet lag.)")),
        value = textOutput("vb_sjl"),
        showcase = bs_icon("airplane"), theme = "warning"),
      value_box("Mood \u00b7 tiredness", textOutput("vb_mt"), showcase = bs_icon("emoji-smile"), theme = "success")
    ),
    layout_columns(
      col_widths = c(8, 4),
      card(card_header("Your sleep vs. daylight"), plotOutput("p_timeline", height = 390)),
      card(card_header("What stands out"), uiOutput("insights"))
    ),
    card(
      card_header("How you compare to the US population"),
      layout_columns(
        col_widths = c(8, 4),
        radioButtons("ref_group", NULL, inline = TRUE,
                     choices = c("Everyone" = "all", "My age group (\u00b110 y)" = "age", "My sex" = "sex")),
        div(class = "text-end", uiOutput("ref_source"))
      ),
      layout_columns(plotOutput("p_ref_msfsc", height = 250),
                     plotOutput("p_ref_sd", height = 250),
                     plotOutput("p_ref_sjl", height = 250))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Mood & tiredness across your week"), plotOutput("p_moodtired", height = 300)),
      card(card_header("Past two weeks vs. population"),
           layout_columns(plotOutput("p_phq_down", height = 300), plotOutput("p_phq_tired", height = 300)))
    )
  ),

  # ---------------- TAB 3: about ----------------
  nav_panel(
    "About the data", icon = bs_icon("info-circle"),
    card(
      card_header("Where the comparison comes from"),
      p("The reference group is the ", strong("NHANES 2017\u2013March 2020 pre-pandemic"), " sample (US, nationally representative, ages 16+). ",
        "Its sleep questionnaire was adapted from the Munich ChronoType Questionnaire: participants report when they usually fall asleep and wake up ",
        "on workdays and on non-workdays. From these we compute the same MCTQ measures we compute for you."),
      tags$ul(
        tags$li(strong("Average sleep"), ": time between falling asleep and waking up, averaged over your work and free days."),
        tags$li(strong("Body clock type"), " (chronotype): the middle point of your sleep on free days, adjusted for catch-up sleep. ",
                "The label compares you with the selected NHANES group: earliest 10% = strong morning lark, next 20% = morning lark, ",
                "middle 40% = in between, next 20% = night owl, latest 10% = strong night owl. These cut-offs are this app's choice, not an official MCTQ classification."),
        tags$li(strong("Work vs. free-day shift"), " (social jet lag): how far the middle of your sleep moves between work days and free days."),
        tags$li(strong("Mood & tiredness"), ": two PHQ-9 items (feeling down; feeling tired / little energy) over the past 2 weeks.")
      ),
      h5("Caveats"),
      tags$ul(
        tags$li("NHANES collects usual times only: no alarm-clock use, sleep latency or number of work days. We assume 5 work days for the reference."),
        tags$li("Participants who don't work, or who work shifts, answer the same 'weekday/workday' question; we can't separate them."),
        tags$li("Percentiles use the NHANES interview survey weights, but no confidence intervals."),
        tags$li("Your 1\u201310 daily mood/tiredness scores have no NHANES equivalent; only the two past-2-weeks questions are compared."),
        tags$li("Body clock type varies with age, sex, latitude and season, so use the age / sex filters.")
      )
    )
  )
)

# ---------------------------- server -----------------------------

server <- function(input, output, session) {

  # ---- day labels ----
  lapply(1:7, function(i) {
    output[[paste0("dlab", i)]] <- renderText(format(as.Date(input$date) + i - 1, "%a %d %b"))
  })

  # ---- location ----
  observeEvent(input$geocode, {
    req(nzchar(input$place))
    res <- tryCatch(tidygeocoder::geo(input$place, method = "osm", quiet = TRUE),
                    error = function(e) NULL)
    if (is.null(res) || is.na(res$lat[1])) {
      showNotification("Couldn't find that place \u2013 enter latitude/longitude manually.", type = "error")
    } else {
      updateNumericInput(session, "lat", value = round(res$lat[1], 4))
      updateNumericInput(session, "lon", value = round(res$long[1], 4))
    }
  })

  has_loc <- reactive(isTRUE(!is.na(input$lat) && !is.na(input$lon)))

  tz <- reactive({
    req(has_loc())
    tryCatch(lutz::tz_lookup_coords(input$lat, input$lon, method = "fast", warn = FALSE),
             error = function(e) "UTC")
  })

  # ---- sunrise / sunset (local clock hours) for the 7 days ----
  sun <- reactive({
    if (isTRUE(input$manual_sun)) {
      req(input$sunrise_in, input$sunset_in)
      return(tibble(sunrise = rep(parse_hm(input$sunrise_in), 7),
                    sunset  = rep(parse_hm(input$sunset_in), 7)))
    }
    validate(need(has_loc(), "Add your location on the 'Your week' tab (or enter sunrise/sunset manually)."))
    req(input$date)
    st <- suncalc::getSunlightTimes(date = as.Date(input$date) + 0:6,
                                    lat = input$lat, lon = input$lon,
                                    keep = c("sunrise", "sunset"), tz = tz())
    hr <- function(t) as.numeric(format(t, "%H")) + as.numeric(format(t, "%M")) / 60
    tibble(sunrise = hr(st$sunrise), sunset = hr(st$sunset))
  })

  output$season_badge <- renderUI({
    if (!has_loc() || is.null(input$date)) return(p(class = "hint", "Add a location to see the season and day length."))
    s <- sun()
    dl <- s$sunset[1] - s$sunrise[1]
    sn <- season_of(as.Date(input$date), input$lat)
    emoji <- c(Spring = "\U0001F338", Summer = "\u2600\ufe0f", Autumn = "\U0001F342", Winter = "\u2744\ufe0f")[[sn]]
    div(span(class = "pill", paste(emoji, sn)),
        if (!is.na(dl)) span(class = "pill", paste0("\u2600 ", fmt_dur(dl), " of daylight")),
        if (!is.na(s$sunrise[1])) span(class = "pill", paste0(fmt_hm(s$sunrise[1]), " \u2192 ", fmt_hm(s$sunset[1]))))
  })

  # ---- the diary as a tidy data frame ----
  diary <- reactive({
    req(input$date)
    s <- sun()
    gc <- function(id) { v <- input[[id]]; if (is.null(v)) NA_character_ else v }
    gn <- function(id) { v <- input[[id]]; if (is.null(v)) NA_real_ else as.numeric(v) }
    tibble(day = 1:7) %>%
      mutate(
        date  = as.Date(input$date) + day - 1,
        work  = vapply(day, function(i) isTRUE(input[[paste0("work", i)]]), logical(1)),
        so    = vapply(day, function(i) parse_hm(gc(paste0("so", i))), numeric(1)),
        se    = vapply(day, function(i) parse_hm(gc(paste0("se", i))), numeric(1)),
        start = vapply(day, function(i) parse_hm(gc(paste0("start", i))), numeric(1)),
        tired = vapply(day, function(i) gn(paste0("tired", i)), numeric(1)),
        mood  = vapply(day, function(i) gn(paste0("mood", i)), numeric(1)),
        start = ifelse(work, start, NA_real_),
        sd    = (se - so) %% 24,
        ms    = (so + sd / 2) %% 24,
        sunrise = s$sunrise,
        sunset  = s$sunset,
        wake_vs_sunrise = ((se - sunrise + 12) %% 24) - 12,
        sleep_vs_sunset = ((so - sunset + 12) %% 24) - 12
      )
  })

  metrics <- reactive({
    d <- diary() %>% filter(!is.na(sd))
    validate(need(nrow(d) > 0, "Please enter valid sleep and wake times."))
    user_metrics(d)
  })

  output$pid_banner <- renderUI({
    if (!is.null(input$pid) && nzchar(trimws(input$pid)))
      h4(paste0("Participant ", trimws(input$pid)), class = "mb-3")
  })

  # ---- value boxes ----
  output$vb_sd    <- renderText(fmt_dur(metrics()$sd_week))
  output$vb_msfsc <- renderText({
    m <- metrics(); if (is.na(m$msf_sc)) "needs free days" else fmt_hm(m$msf_sc)
  })
  output$vb_sjl   <- renderText({
    m <- metrics(); if (is.na(m$sjl)) "needs both day types" else fmt_dur(m$sjl)
  })
  output$vb_mt    <- renderText({
    d <- diary(); sprintf("%.1f \u00b7 %.1f", mean(d$mood, na.rm = TRUE), mean(d$tired, na.rm = TRUE))
  })

  # ---- insights ----
  output$insights <- renderUI({
    d <- diary() %>% filter(!is.na(sd)); m <- metrics()
    items <- list()
    add <- function(icon, ...) items[[length(items) + 1]] <<- tags$li(bs_icon(icon), ...)

    r <- mean(d$wake_vs_sunrise, na.rm = TRUE)
    if (is.finite(r)) {
      add("sunrise", "You wake ", strong(fmt_dur(r)), if (r >= 0) " after" else " before", " sunrise on average",
          if (any(d$wake_vs_sunrise < 0, na.rm = TRUE))
            paste0(" (before sunrise on ", sum(d$wake_vs_sunrise < 0, na.rm = TRUE), " of ", nrow(d), " days)."), ".")
    }
    ws <- d %>% filter(work, !is.na(start), !is.na(sunrise))
    if (nrow(ws) && any(ws$start < ws$sunrise))
      add("briefcase", "Work starts before sunrise on ", strong(sum(ws$start < ws$sunrise)), " work day(s).")
    r2 <- mean(d$sleep_vs_sunset, na.rm = TRUE)
    if (is.finite(r2))
      add("sunset", "You fall asleep ", strong(fmt_dur(r2)), if (r2 >= 0) " after" else " before", " sunset on average.")
    if (!is.na(m$sjl))
      add("arrow-left-right", "Your sleep midpoint shifts by ", strong(fmt_dur(m$sjl)),
          " between work and free days",
          if (m$sjl >= 1) " \u2013 that's a big shift, a bit like jet lag." else ".")
    if (!is.na(m$sd_w) && !is.na(m$sd_f) && m$sd_f - m$sd_w >= 1)
      add("moon", "You sleep ", strong(fmt_dur(m$sd_f - m$sd_w)), " longer on free days: a sign you may be short on sleep during the week.")
    if (!length(items)) items <- list(tags$li("Fill in your week to see insights."))
    tags$ul(class = "list-unstyled insight-list", items)
  })

  # ---- timeline plot ----
  output$p_timeline <- renderPlot({
    d <- diary() %>% filter(!is.na(sd)) %>%
      mutate(so_x = ifelse(so > 12, so - 24, so),
             se_x = ifelse(se < so_x, se + 24, se),
             y = 8 - day)
    ggplot(d) +
      geom_rect(aes(xmin = sunrise, xmax = sunset, ymin = y - .42, ymax = y + .42),
                fill = pal$day, alpha = .75, na.rm = TRUE) +
      geom_vline(xintercept = 0, linetype = "dashed", colour = pal$mute) +
      geom_segment(aes(x = so_x, xend = se_x, y = y, yend = y),
                   linewidth = 7, colour = pal$sleep, lineend = "round") +
      geom_point(aes(x = start, y = y, shape = "Work starts"),
                 size = 3.8, colour = pal$you, na.rm = TRUE) +
      scale_y_continuous(breaks = d$y, labels = format(d$date, "%a %d")) +
      scale_x_continuous(breaks = seq(-6, 24, 3), labels = function(x) sprintf("%02d:00", as.integer(x %% 24))) +
      scale_shape_manual(NULL, values = c("Work starts" = 17)) +
      coord_cartesian(xlim = c(-6, 24)) +
      labs(x = NULL, y = NULL, subtitle = "Blue = sleep \u00b7 yellow = daylight \u00b7 dashed line = midnight") +
      theme_sleep()
  })

  output$p_moodtired <- renderPlot({
    diary() %>% select(date, Mood = mood, Tiredness = tired) %>%
      pivot_longer(-date) %>%
      ggplot(aes(date, value, colour = name)) +
      geom_line(linewidth = 1.3) + geom_point(size = 3.2) +
      scale_x_date(date_labels = "%a") +
      scale_y_continuous(limits = c(1, 10), breaks = 1:10) +
      scale_colour_manual(NULL, values = c(Mood = pal$good, Tiredness = pal$sun)) +
      labs(x = NULL, y = NULL) + theme_sleep()
  })

  # ---- reference population ----
  ref <- reactive({
    r <- switch(input$ref_group,
                all = REF,
                age = REF[abs(REF$age - input$age) <= 10, ],
                sex = REF[REF$sex == input$sex, ])
    validate(need(nrow(r) > 50, "Too few reference participants in this group."))
    r
  })

  # Chronotype label, based on where you fall in the (filtered) NHANES distribution
  output$vb_chrono_label <- renderText({
    m <- metrics()
    if (is.na(m$msf_sc)) return("")
    r <- ref()
    pct <- w_pct(wrap_ms(r$msf_sc), r$w, wrap_ms(m$msf_sc))
    lab <- if (pct < 10) "Strong morning lark"
           else if (pct < 30) "Morning lark"
           else if (pct <= 70) "In between"
           else if (pct <= 90) "Night owl"
           else "Strong night owl"
    paste0(lab, " \u00b7 later than ", round(pct), "% of people")
  })

  output$ref_source <- renderUI({
    if (REF_REAL) span(class = "pill", paste0("NHANES 2017\u20132020 \u00b7 n = ", format(nrow(ref()), big.mark = ",")))
    else span(class = "pill", style = "background:#FBD5CC;color:#8A2A12;",
              "\u26a0 simulated placeholder \u2013 run data-raw/build_reference.R")
  })

  ref_plot <- function(col, you, xlab, fmt, word, clock = FALSE) {
    r <- ref()
    x <- r[[col]]
    if (clock) { x <- wrap_ms(x); you <- wrap_ms(you) }
    validate(need(!is.na(you), "Needs both work and free days."))
    pct <- round(w_pct(x, r$w, you))
    p <- ggplot(data.frame(x = x, w = r$w), aes(x, weight = w)) +
      geom_histogram(bins = 40, fill = pal$pop, colour = "white") +
      geom_vline(xintercept = you, colour = pal$you, linewidth = 1.4) +
      labs(x = xlab, y = NULL,
           title = paste0("You: ", fmt(you)),
           subtitle = paste0(word, " than ", pct, "% of people")) +
      theme_sleep(11) + theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank())
    if (clock) p <- p + scale_x_continuous(labels = function(v) fmt_hm(v))
    p
  }

  output$p_ref_msfsc <- renderPlot(
    ref_plot("msf_sc", metrics()$msf_sc, "Body clock type (middle of sleep on free days)", fmt_hm, "Later", clock = TRUE))
  output$p_ref_sd <- renderPlot(
    ref_plot("sd_week", metrics()$sd_week, "Average sleep per night (h)", fmt_dur, "Longer"))
  output$p_ref_sjl <- renderPlot(
    ref_plot("sjl", metrics()$sjl, "Work vs. free-day shift (h)", fmt_dur, "More"))

  phq_plot <- function(col, you, title) {
    r <- ref(); r <- r[!is.na(r[[col]]), ]
    validate(need(nrow(r) > 0, "No PHQ-9 data in the reference file."))
    tot <- tapply(r$w, factor(r[[col]], levels = 0:3), sum)
    tot[is.na(tot)] <- 0
    df <- data.frame(resp = factor(PHQ_LABELS, levels = PHQ_LABELS),
                     p = as.numeric(tot / sum(tot)), you = 0:3 == as.integer(you))
    ggplot(df, aes(resp, p, fill = you)) +
      geom_col(width = .7) +
      geom_text(aes(label = scales::percent(p, accuracy = 1)), vjust = -.4, size = 3.6, colour = pal$night) +
      scale_fill_manual(values = c(`TRUE` = pal$you, `FALSE` = pal$pop), guide = "none") +
      scale_x_discrete(labels = function(x) vapply(strwrap(x, 10, simplify = FALSE),
                                                   paste, character(1), collapse = "\n")) +
      scale_y_continuous(expand = expansion(mult = c(0, .15)), labels = NULL) +
      labs(x = NULL, y = NULL, title = title, subtitle = "Red = your answer") +
      theme_sleep(10.5) + theme(panel.grid.major = element_blank())
  }
  output$p_phq_down  <- renderPlot(phq_plot("phq_down",  input$phq_down,  "Feeling down"))
  output$p_phq_tired <- renderPlot(phq_plot("phq_tired", input$phq_tired, "Tired / low energy"))
}

shinyApp(ui, server)
