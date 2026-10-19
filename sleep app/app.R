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

# Load the shared helper functions explicitly (don't rely on Shiny's automatic R/ loading)
helpers_path <- c("R/helpers.R", "helpers.R")
helpers_path <- helpers_path[file.exists(helpers_path)][1]
if (is.na(helpers_path))
  stop("Can't find helpers.R. It must be in a folder called 'R' next to app.R (sleep-app/R/helpers.R), ",
       "and R must be running from the sleep-app folder. Current folder: ", getwd())
source(helpers_path)

# ------------------------- look & feel ---------------------------

# Traditional Japanese colours (approximate hex values)
#   kon 紺 (deep navy)      ai 藍 (indigo)        asagi 浅葱 (light teal)
#   yamabuki 山吹 (gold)    shu 朱 (vermilion)    wakatake 若竹 (young bamboo)
#   rokusho 緑青 (verdigris) shironeri 白練 (off-white)  shirafuji 白藤 (pale wisteria)
pal <- list(
  night = "#0F2540",   # kon
  sleep = "#165E83",   # ai
  teal  = "#0E8A96",   # asagi (deepened)
  day   = "#FCD575",   # tamago-ish gold for daylight
  sun   = "#F8B500",   # yamabuki
  you   = "#EB6101",   # shu
  pop   = "#C5C6DE",   # shirafuji
  good  = "#3B7960",   # rokusho
  mute  = "#7A7F72",   # rikyu-nezumi
  paper = "#FAF8F2",   # shironeri
  white = "#FFFFFF",
  ink   = "#2A2B33",   # body text
  line  = "#ECE8DF",   # card dividers, chart grid lines
  field = "#E4DFD3",   # input borders
  field_bg = "#FEFDFB",
  pill_bg = "#F8EBC9", pill_fg = "#6B4E00",
  tired = "#E0A100",   # tiredness line
  warn_bg = "#FBD5CC", warn_fg = "#8A2A12"
)

# Every colour is defined ONCE, in `pal` above. The CSS below uses {{name}} tokens
# (e.g. {{night}}) and {{name@0.3}} for a transparent version; fill_pal() swaps them in.
fill_pal <- function(css, pal) {
  tokens <- unique(regmatches(css, gregexpr("\\{\\{[a-z_]+(@[0-9.]+)?\\}\\}", css))[[1]])
  for (t in tokens) {
    parts <- strsplit(gsub("[{}]", "", t), "@", fixed = TRUE)[[1]]
    hex <- pal[[parts[1]]]
    if (is.null(hex)) stop("Unknown colour in CSS: ", t)
    val <- if (length(parts) == 2) {
      rgb <- col2rgb(hex)
      sprintf("rgba(%d,%d,%d,%s)", rgb[1], rgb[2], rgb[3], parts[2])
    } else hex
    css <- gsub(t, val, css, fixed = TRUE)
  }
  css
}

# Small "i" icon with a hover explanation, and a label that carries one
info <- function(text) tooltip(bs_icon("info-circle", size = "0.9em"), text)
with_info <- function(label, text) tagList(label, " ", info(text))

# >>> CHANGE FONTS HERE (any Google Font name works) <<<
FONT_BODY    <- "Open Sauce Sans"   # loaded from the Fontsource CDN (see @font-face rules in the CSS)
FONT_HEADING <- "Montserrat"        # Google Font
FONT_BODY_STACK <- paste0("'", FONT_BODY,    "', system-ui, -apple-system, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif")
FONT_HEAD_STACK <- paste0("'", FONT_HEADING, "', 'Segoe UI', system-ui, Helvetica, Arial, sans-serif")

app_theme <- bs_theme(
  version = 5,
  bg = pal$paper, fg = pal$ink,
  primary = pal$sleep, secondary = pal$mute,
  success = pal$good, warning = pal$sun, info = pal$teal, danger = pal$you,
  heading_font = font_google(FONT_HEADING, wght = c(500, 600, 700), local = FALSE)
) |>
  bs_add_rules(fill_pal(r"---(
    @font-face { font-family: 'Open Sauce Sans'; font-style: normal; font-weight: 400; font-display: swap;
      src: url(https://cdn.jsdelivr.net/fontsource/fonts/open-sauce-sans@5.3.0/latin-400-normal.woff2) format('woff2'); }
    @font-face { font-family: 'Open Sauce Sans'; font-style: normal; font-weight: 500; font-display: swap;
      src: url(https://cdn.jsdelivr.net/fontsource/fonts/open-sauce-sans@5.3.0/latin-500-normal.woff2) format('woff2'); }
    @font-face { font-family: 'Open Sauce Sans'; font-style: normal; font-weight: 700; font-display: swap;
      src: url(https://cdn.jsdelivr.net/fontsource/fonts/open-sauce-sans@5.3.0/latin-700-normal.woff2) format('woff2'); }

    body { background: {{paper}}; font-family: {{font_body}}; }
    .form-control, .form-select, .btn, .nav-link, .tooltip, .popover { font-family: {{font_body}}; }
    h1, h2, h3, h4, h5, h6, .navbar-brand, .card-header, .wk-head, .hero h2,
    .bslib-value-box .value-box-title { font-family: {{font_head}}; }
    .vb-sub { font-size: .8rem; opacity: .88; margin: 0 0 .25rem; }

    /* ---- navbar ---- */
    .navbar { background: linear-gradient(100deg, {{night}} 0%, {{sleep}} 100%) !important;
              border-bottom: 3px solid {{sun}}; }
    .navbar .navbar-brand { color: {{paper}} !important; font-family: {{font_head}};
                            font-weight: 700; letter-spacing: .02em; }
    .navbar .nav-link { color: {{paper@.75}} !important; padding-bottom: .55rem; }
    .navbar .nav-link:hover { color: {{paper}} !important; }
    .navbar .nav-link.active { color: {{paper}} !important; border-bottom: 2px solid {{sun}}; }

    /* ---- hero ---- */
    .hero { position: relative; overflow: hidden; color: {{paper}}; border-radius: 18px;
            background: linear-gradient(120deg, {{night}} 0%, {{sleep}} 100%);
            padding: 1.6rem 2rem; margin-bottom: 1.1rem; }
    .hero::after { content: ""; position: absolute; right: -50px; top: -50px; width: 210px; height: 210px;
                   border-radius: 50%;
                   background: radial-gradient(circle, {{sun}} 0 42%, {{sun@.38}} 43% 60%,
                                               {{sun@.16}} 61% 78%, transparent 79%); }
    .hero h2 { font-size: 1.75rem; margin: 0 0 .35rem; color: {{paper}}; }
    .hero p  { margin: 0; max-width: 36rem; opacity: .88; }

    /* ---- cards ---- */
    .card { background: {{white}}; border: 1px solid {{night@.06}}; border-radius: 16px;
            box-shadow: 0 6px 22px {{night@.07}}; }
    .card-header { background: transparent; border-bottom: 0; padding-bottom: 0;
                   font-family: {{font_head}}; font-weight: 700; font-size: 1.15rem;
                   color: {{night}}; }
    .card-header::before { content: ""; display: inline-block; width: 4px; height: 1.05em;
                           background: {{you}}; border-radius: 2px; margin-right: .6rem; vertical-align: -.15em; }

    /* ---- value boxes ---- */
    .bslib-value-box { border-radius: 16px; border: 0; box-shadow: 0 6px 22px {{night@.12}}; }
    .bslib-value-box .value-box-title { font-size: .82rem; letter-spacing: .05em; text-transform: uppercase; opacity: .92; }
    .bslib-value-box .value-box-value { font-family: {{font_head}}; font-weight: 700; }

    /* ---- forms ---- */
    .form-control, .form-select { border-radius: 10px; border-color: {{field}}; background-color: {{field_bg}}; }
    .form-control:focus, .form-select:focus { border-color: {{sleep}}; box-shadow: 0 0 0 .2rem {{sleep@.15}}; }
    .btn { border-radius: 999px; }
    .shiny-input-container > label, .control-label { font-weight: 500; color: {{ink}}; }

    /* ---- weekly grid ---- */
    .week-wrap { overflow-x: auto; }
    .week-grid { display: grid; min-width: 900px;
                 grid-template-columns: 100px repeat(2, minmax(100px, 1fr)) minmax(130px, 1.1fr) minmax(100px, 1fr) repeat(2, minmax(120px, 1.3fr));
                 gap: .4rem .75rem; align-items: center; }
    .wk-head { font-size: .72rem; text-transform: uppercase; letter-spacing: .07em; color: {{mute}};
               padding-bottom: .3rem; border-bottom: 1px solid {{line}}; }
    .wk-day  { font-weight: 700; color: {{sleep}}; }
    .week-grid .form-group, .week-grid .shiny-input-container { margin-bottom: 0; width: 100%; }
    .week-grid .irs-min, .week-grid .irs-max { display: none; }
    .week-grid .irs { height: 34px; }

    /* ---- misc ---- */
    .insight-list li { padding: .55rem 0; border-bottom: 1px solid {{line}}; }
    .insight-list li:last-child { border-bottom: 0; }
    .insight-list svg { color: {{you}}; margin-right: .4rem; }
    .pill { display: inline-block; padding: .22rem .75rem; border-radius: 999px;
            background: {{pill_bg}}; color: {{pill_fg}}; font-size: .85rem; margin: 0 .4rem .3rem 0; }
    .hint { color: {{mute}}; font-size: .85rem; }
    h4, h5, h6 { color: {{night}}; }
  
    .card-header svg, .wk-head svg, .form-label svg, .control-label svg {
      opacity: .65; margin-left: .1rem; cursor: help; vertical-align: -.1em; }
    .wk-head svg { text-transform: none; }
    .week-grid select { padding-right: 2rem; }
    div > .card + .card { margin-top: 1rem; }
    input.native-time:disabled, select:disabled { background: {{line}}; opacity: .6; }
  )---", c(pal, list(font_body = FONT_BODY_STACK, font_head = FONT_HEAD_STACK))))

theme_sleep <- function(base = 12) {
  theme_minimal(base_size = base) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.y = element_line(colour = pal$line),
          panel.grid.major.x = element_line(colour = pal$line),
          plot.title = element_text(face = "bold", colour = pal$night),
          plot.subtitle = element_text(colour = pal$mute),
          axis.title = element_text(colour = pal$ink, face = "bold"),
          axis.text = element_text(colour = pal$ink),
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

time_input <- function(id, value = "", disabled = FALSE) {
  tags$input(type = "time", id = id, value = value,
             disabled = if (disabled) NA else NULL,
             class = "form-control form-control-sm native-time")
}

# One row of the weekly table. `date` decides the default: Sat/Sun start as free days.
day_cells <- function(i, date) {
  free <- format(date, "%u") %in% c("6", "7")
  tagList(
    div(class = "wk-day", textOutput(paste0("dlab", i), inline = TRUE)),
    time_input(paste0("so", i), if (free) "00:30" else "23:30"),
    time_input(paste0("se", i), if (free) "09:00" else "07:00"),
    selectInput(paste0("work", i), NULL, width = "100%", selectize = FALSE,
                choices = c("Work day" = "work", "Free day" = "free"),
                selected = if (free) "free" else "work"),
    time_input(paste0("start", i), if (free) "" else "09:00", disabled = free),
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
// Free day -> 'Work starts' is greyed out and emptied; Work day -> it comes back.
function applyDayType(i, free) {
  var el = $('#start' + i);
  el.prop('disabled', free);
  if (free) { el.val(''); Shiny.setInputValue('start' + i, ''); }
  else if (!el.val()) { el.val('09:00'); Shiny.setInputValue('start' + i, '09:00'); }
}
$(document).on('change', 'select[id^=\"work\"]', function() {
  applyDayType(this.id.replace('work', ''), this.value === 'free');
});
$(function() {   // also used when the server changes a day type (e.g. after changing the end date)
  Shiny.addCustomMessageHandler('daytype', function(m) { applyDayType(m.i, m.free); });
});"

# The UI is built when someone opens the app, so 'today' is always today
build_ui <- function() {
  end_date   <- Sys.Date()
  week_dates <- end_date - 6:0
  page_navbar(
    id = "main_nav",
    title = tagList(bs_icon("moon-stars-fill"), " Sleep & light diary"),
    theme = app_theme,
    header = tags$head(tags$script(HTML(js_time))),
    fillable = FALSE,
    
    # ---------------- TAB 1: input ----------------
    nav_panel(
      "1 \u00b7 Your week", value = "input", icon = bs_icon("pencil-square"),
      div(class = "hero",
          h2("Your week, your body clock"),
          p("Tell us how you slept and felt over the last 7 days. We'll show how your sleep lines up with daylight",
            " and how you compare with thousands of people in a US national health survey.")),
      layout_columns(
        col_widths = c(4, 8),
        div(
          card(
            card_header("About you"),
            textInput("pid", with_info("Participant ID", "Any code you like (for example P001) to label this diary."), placeholder = "e.g. P001"),
            layout_columns(
              numericInput("age", with_info("Age", "Used to compare you with people of a similar age."), 30, min = 16, max = 100),
              selectInput("sex", "Sex", c("Female" = "F", "Male" = "M"))
            ),
            p(class = "hint", "Age and sex are only used to pick a matching comparison group.")
          ),
          card(
            card_header("Where & when"),
            textInput("place", NULL, placeholder = "City, country (e.g. Berlin, Germany)"),
            actionButton("geocode", "Find location", icon = icon("location-dot"), class = "btn-primary btn-sm"),
            layout_columns(
              numericInput("lat", with_info("Latitude", "How far north or south you are. It fills in when you search for your place, and lets us work out sunrise and sunset."), NA, step = 0.01),
              numericInput("lon", with_info("Longitude", "How far east or west you are. It fills in when you search for your place."), NA, step = 0.01)
            ),
            dateInput("date", with_info("Last day of your diary", "Your diary covers the 7 days up to and including this date. It starts as today."),
                      value = end_date, max = end_date),
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
        div(
          card(
            card_header(with_info("Your last 7 days", "The 7 days ending on the date you chose on the left. Day names are shown in each row.")),
            p(class = "hint",
              "Fill in one row per day. Choose ", strong("Free day"), " for days you didn't have to get up for work or other commitments ",
              "(the weekend is pre-selected, but change it if your week was different)."),
            div(class = "week-wrap",
                div(class = "week-grid",
                    div(class = "wk-head", "Day"),
                    div(class = "wk-head", with_info("Fell asleep", "The time you actually fell asleep, not when you got into bed.")),
                    div(class = "wk-head", with_info("Woke up", "The time you woke up for the last time that morning.")),
                    div(class = "wk-head", with_info("Type of day", "Work day: you had to get up for work, school or other fixed commitments. Free day: you could sleep as long as you liked.")),
                    div(class = "wk-head", with_info("Work starts", "Optional. Greys out by itself on free days.")),
                    div(class = "wk-head", with_info("Tiredness", "1 = fully rested, 10 = exhausted.")),
                    div(class = "wk-head", with_info("Mood", "1 = very low, 10 = great.")),
                    lapply(1:7, function(i) day_cells(i, week_dates[i]))))
          ),
          card(
            card_header(with_info("Past two weeks", "These two questions come from a standard health questionnaire (the PHQ-9) used in a large US survey, so we can compare your answers with theirs. They are not a diagnosis.")),
            h6("How often have you been bothered by…"),
            p(class = "hint", "Same wording as the US survey, so your answers can be compared."),
            radioButtons("phq_down", "Feeling down, depressed or hopeless",
                         choices = setNames(0:3, PHQ_LABELS), selected = 0, inline = TRUE),
            radioButtons("phq_tired", "Feeling tired or having little energy",
                         choices = setNames(0:3, PHQ_LABELS), selected = 1, inline = TRUE)
          ),
          div(class = "text-end mt-3",
              actionButton("go_results", "See my results", icon = icon("arrow-right"),
                           class = "btn-primary btn-lg"))
        )
      )
    ),
    
    # ---------------- TAB 2: results ----------------
    nav_panel(
      "2 \u00b7 Results", value = "results", icon = bs_icon("graph-up"),
      uiOutput("pid_banner"),
      layout_columns(
        fill = FALSE,
        value_box(
          title = with_info("Average sleep", "Hours you slept per night this week, with your work days and free days counted together."),
          value = textOutput("vb_sd"), showcase = bs_icon("moon-stars"), theme = "primary"),
        value_box(
          title = tagList("Body clock type ",
                          tooltip(bs_icon("info-circle"),
                                  "Whether you naturally sleep early or late. The time shown is the point halfway between falling asleep and waking up on free days, adjusted for catch-up sleep. Earlier = morning lark, later = night owl. (Scientific name: chronotype.)")),
          value = textOutput("vb_msfsc"),
          p(class = "vb-sub", "Middle of your sleep on free days"),
          textOutput("vb_chrono_label"),
          showcase = bs_icon("clock-history"), theme = "info"),
        value_box(
          title = tagList("Social jet lag (work vs. free-day shift) ",
                          tooltip(bs_icon("info-circle"),
                                  "How much the middle of your sleep moves between work days and free days. A big shift feels a bit like jet lag, without any travel.")),
          value = textOutput("vb_sjl"),
          showcase = bs_icon("airplane"), theme = "warning"),
        value_box(
          title = with_info("Mood \u00b7 tiredness", "Your average daily scores out of 10. Higher mood is better; higher tiredness means more tired."),
          value = textOutput("vb_mt"), showcase = bs_icon("emoji-smile"), theme = "success")
      ),
      layout_columns(
        col_widths = c(8, 4),
        card(card_header(with_info("Your sleep vs. daylight", "Blue bars are your sleep. The yellow band is the time the sun was up. Orange triangles show when work started.")),
             plotOutput("p_timeline", height = 390)),
        card(card_header("What stands out"), uiOutput("insights"))
      ),
      card(
        card_header(with_info("How you compare to the US population", "Each chart shows people from a US national health survey (NHANES). The orange line is you. 'Later than 60% of people' means your value is higher than 60 out of 100 people.")),
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
        card(card_header(with_info("Mood & tiredness across your week", "Your own daily scores. There is no survey comparison for these.")),
             plotOutput("p_moodtired", height = 300)),
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
          tags$li(strong("Social jet lag"), " (work vs. free-day shift): how far the middle of your sleep moves between work days and free days."),
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
}

ui <- function(request) build_ui()

# ---------------------------- server -----------------------------

server <- function(input, output, session) {
  
  # ---- day labels ----
  lapply(1:7, function(i) {
    output[[paste0("dlab", i)]] <- renderText(format(as.Date(input$date) - 7 + i, "%a %d %b"))
  })
  
  # ---- day type: weekends are free days by default; free days have no work start ----
  observeEvent(input$date, {
    for (i in 1:7) {
      d <- as.Date(input$date) - 7 + i
      updateSelectInput(session, paste0("work", i),
                        selected = if (format(d, "%u") %in% c("6", "7")) "free" else "work")
    }
  })
  lapply(1:7, function(i) {
    observeEvent(input[[paste0("work", i)]], {
      session$sendCustomMessage("daytype", list(i = i, free = identical(input[[paste0("work", i)]], "free")))
    })
  })
  
  # ---- "See my results" button ----
  observeEvent(input$go_results, {
    if (!isTRUE(input$manual_sun) && !isTRUE(!is.na(input$lat) && !is.na(input$lon))) {
      showNotification("Please add your location first (type a place and click 'Find location'), or enter sunrise and sunset yourself.",
                       type = "warning", duration = 8)
    } else {
      nav_select("main_nav", "results")
    }
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
    st <- suncalc::getSunlightTimes(date = as.Date(input$date) - 6:0,
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
        date  = as.Date(input$date) - 7 + day,
        work  = vapply(day, function(i) identical(input[[paste0("work", i)]], "work"), logical(1)),
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
      scale_colour_manual(NULL, values = c(Mood = pal$good, Tiredness = pal$tired)) +
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
    else span(class = "pill", style = paste0("background:", pal$warn_bg, ";color:", pal$warn_fg, ";"),
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
    ref_plot("sjl", metrics()$sjl, "Social jet lag (h)", fmt_dur, "More"))
  
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