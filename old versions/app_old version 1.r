# Save as app.R, then click Source in RStudio.
# Run from a writable project folder. Test with fictitious data first.
# Not a validated MCTQ, population benchmark, or production authentication system.

packages <- c("shiny", "DBI", "RSQLite", "digest", "uuid",
              "tidygeocoder", "suncalc", "ggplot2")
missing_packages <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages)) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}
library(shiny)

# ---------------------------- DATABASE ----------------------------
# Participants returning to this SAME database can resume with their code.
# Never commit this database or CSV exports to GitHub.
db_path <- Sys.getenv("SLEEP_DIARY_DB", unset = "data/sleep_diary.sqlite")
dir.create(dirname(db_path), recursive = TRUE, showWarnings = FALSE)

with_db <- function(fun) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "PRAGMA busy_timeout = 5000")
  fun(con)
}

with_db(function(con) {
  DBI::dbExecute(con, "PRAGMA journal_mode = WAL")
  DBI::dbExecute(con, paste(
    "CREATE TABLE IF NOT EXISTS diary (",
    "token_hash TEXT NOT NULL, date TEXT NOT NULL, place TEXT NOT NULL,",
    "latitude REAL NOT NULL, longitude REAL NOT NULL, timezone TEXT NOT NULL,",
    "season TEXT NOT NULL, day_type TEXT NOT NULL, work_start TEXT,",
    "sleep_onset TEXT NOT NULL, wake_time TEXT NOT NULL, alarm INTEGER NOT NULL,",
    "tiredness INTEGER NOT NULL, mood INTEGER NOT NULL,",
    "sunrise TEXT, sunset TEXT, manual_sun INTEGER NOT NULL, daylight_hours REAL,",
    "sleep_hours REAL NOT NULL, midpoint_hour REAL NOT NULL, updated_at TEXT NOT NULL,",
    "PRIMARY KEY (token_hash, date))"
  ))
})

valid_code <- function(code) {
  length(code) == 1L && !is.na(code) &&
    grepl("^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$", code)
}
hash_code <- function(code) {
  if (!valid_code(code)) stop("Enter a valid access code.")
  digest::digest(code, algo = "sha256", serialize = FALSE)
}
get_week <- function(code, start) {
  with_db(function(con) DBI::dbGetQuery(con,
    "SELECT * FROM diary WHERE token_hash = ? AND date >= ? AND date <= ? ORDER BY date",
    params = list(hash_code(code), as.character(as.Date(start)),
                  as.character(as.Date(start) + 6))))
}
put_day <- function(code, record) {
  row <- data.frame(token_hash = hash_code(code), record, check.names = FALSE)
  columns <- names(row)
  updates <- setdiff(columns, c("token_hash", "date"))
  sql <- paste0(
    "INSERT INTO diary (", paste(columns, collapse = ","), ") VALUES (",
    paste(rep("?", length(columns)), collapse = ","), ") ",
    "ON CONFLICT(token_hash,date) DO UPDATE SET ",
    paste(paste0(updates, "=excluded.", updates), collapse = ",")
  )
  with_db(function(con) DBI::dbExecute(con, sql, params = unname(as.list(row[1, ]))))
}

# -------------------------- CALCULATIONS --------------------------
valid_time <- function(x) {
  length(x) == 1L && !is.na(x) && grepl("^([01][0-9]|2[0-3]):[0-5][0-9]$", x)
}
time_hour <- function(x) {
  if (!valid_time(x)) stop("Enter times as HH:MM, for example 23:30 or 07:00.")
  as.numeric(substr(x, 1, 2)) + as.numeric(substr(x, 4, 5)) / 60
}
clock_label <- function(x) {
  if (!is.finite(x)) return("Unavailable")
  m <- as.integer(round((x %% 24) * 60)) %% 1440L
  sprintf("%02d:%02d", m %/% 60L, m %% 60L)
}
circular_mean <- function(hours) {
  hours <- hours[is.finite(hours)]
  if (!length(hours)) return(NA_real_)
  angle <- hours * 2 * pi / 24
  a <- mean(sin(angle)); b <- mean(cos(angle))
  if (sqrt(a^2 + b^2) < 1e-8) return(NA_real_)
  (atan2(a, b) * 24 / (2 * pi)) %% 24
}
sleep_values <- function(date, onset, wake, tz) {
  bedtime <- time_hour(onset); waking <- time_hour(wake)
  if (bedtime == waking) stop("Sleep onset and waking cannot be identical.")
  if (!tz %in% OlsonNames()) stop("Choose a valid time zone.")
  date <- as.Date(date)
  if (is.na(date)) stop("Choose a valid date.")
  onset_date <- date - as.integer(bedtime > waking)
  onset_text <- paste(onset_date, onset)
  wake_text <- paste(date, wake)
  start <- as.POSIXct(onset_text, format = "%Y-%m-%d %H:%M", tz = tz)
  end <- as.POSIXct(wake_text, format = "%Y-%m-%d %H:%M", tz = tz)
  if (is.na(start) || is.na(end) ||
      format(start, "%Y-%m-%d %H:%M", tz = tz) != onset_text ||
      format(end, "%Y-%m-%d %H:%M", tz = tz) != wake_text) {
    stop("Invalid local time, possibly during a daylight-saving clock change.")
  }
  hours <- as.numeric(difftime(end, start, units = "hours"))
  if (!is.finite(hours) || hours <= 0 || hours > 24) stop("Check your sleep times: duration must be between 0 and 24 hours.")
  midpoint <- start + hours * 1800
  mid_hour <- as.numeric(format(midpoint, "%H", tz = tz)) +
    as.numeric(format(midpoint, "%M", tz = tz)) / 60 +
    as.numeric(format(midpoint, "%S", tz = tz)) / 3600
  list(hours = hours, midpoint = mid_hour)
}
season_for <- function(date, latitude) {
  month <- as.integer(format(as.Date(date), "%m"))
  season <- if (month %in% c(12, 1, 2)) "Winter" else if (month %in% 3:5) {
    "Spring"
  } else if (month %in% 6:8) "Summer" else "Autumn"
  if (abs(latitude) < 1) return("Near equator: no seasonal label")
  if (latitude < 0) {
    opposite <- c(Winter = "Summer", Spring = "Autumn", Summer = "Winter", Autumn = "Spring")
    season <- unname(opposite[season])
  }
  paste(season, if (latitude >= 0) "(Northern convention)" else "(Southern convention)")
}
sun_values <- function(date, lat, lon, tz, manual, sunrise, sunset) {
  if (manual) {
    rise <- time_hour(sunrise); set <- time_hour(sunset)
    if (rise == set) stop("Identical sunrise/sunset times are ambiguous.")
    return(list(sunrise = sunrise, sunset = sunset, hours = (set - rise) %% 24))
  }
  sun <- suncalc::getSunlightTimes(date = as.Date(date), lat = lat, lon = lon,
    keep = c("sunrise", "sunset"), tz = tz)
  rise <- sun$sunrise[1]; set <- sun$sunset[1]
  fmt <- function(x) if (is.na(x)) NA_character_ else format(x, "%H:%M", tz = tz)
  hours <- if (is.na(rise) || is.na(set)) NA_real_ else {
    as.numeric(difftime(set, rise, units = "hours"))
  }
  if (is.finite(hours) && (hours < 0 || hours > 24)) hours <- NA_real_
  list(sunrise = fmt(rise), sunset = fmt(set), hours = hours)
}

# City lookup only on button click. Shared cache and spacing in this R process.
# Public deployment needs policy review and coordinated service-wide limits.
geo_cache <- new.env(parent = emptyenv())
last_lookup <- as.POSIXct("1970-01-01", tz = "UTC")
lookup_city <- function(place) {
  key <- tolower(trimws(place))
  if (!nzchar(key) || nchar(key) > 120) stop("Enter a city and country, not your address.")
  if (exists(key, envir = geo_cache, inherits = FALSE)) return(get(key, envir = geo_cache))
  elapsed <- as.numeric(difftime(Sys.time(), last_lookup, units = "secs"))
  if (elapsed < 1.1) Sys.sleep(1.1 - elapsed)
  last_lookup <<- Sys.time()
  location <- tidygeocoder::geo(address = place, method = "osm",
    lat = "latitude", long = "longitude", limit = 1, quiet = TRUE)
  if (!nrow(location) || !is.finite(location$latitude[1]) || !is.finite(location$longitude[1])) {
    stop("City not found. Try city, country, or enter coordinates manually.")
  }
  result <- list(lat = location$latitude[1], lon = location$longitude[1])
  assign(key, result, envir = geo_cache)
  result
}

# ------------------------------- UI -------------------------------
ui <- fluidPage(
  tags$head(tags$style(HTML(
    "body {background:#fafbfd;} .well {background:#edf3f7;} .help-block {color:#475569;}"
  ))),
  titlePanel("One-week sleep and daylight diary"),
  p("Record sleep, work/study obligations, daylight, tiredness and mood. Test with fictitious data first."),
  sidebarLayout(
    sidebarPanel(
      h4("1. Your private diary"),
      actionButton("new", "Create a new access code"),
      textInput("code", "Your private access code"),
      actionButton("resume", "Open / resume diary"),
      textOutput("status"),
      helpText("Copy and privately save your code. Anyone with it can access your diary. There is no code recovery."),
      checkboxInput("consent", "I agree to save these entries in this prototype database.", FALSE),
      h4("2. Your week"),
      dateInput("week", "First waking date", value = Sys.Date() - 6),
      selectInput("day", "Day to enter / edit", choices = character(0)),
      h4("3. Place and daylight"),
      textInput("place", "City, country", "Amsterdam, Netherlands"),
      helpText("City lookup sends the city text to public OpenStreetMap Nominatim. Do not enter a street address."),
      actionButton("find", "Find coordinates"),
      numericInput("lat", "Latitude: verify result", 52.3676, min = -90, max = 90, step = 0.0001),
      numericInput("lon", "Longitude: verify result", 4.9041, min = -180, max = 180, step = 0.0001),
      selectInput("tz", "Confirm local time zone", choices = OlsonNames(), selected = "Europe/Amsterdam"),
      textOutput("sun_info"),
      checkboxInput("manual", "Enter sunrise/sunset manually", FALSE),
      conditionalPanel("input.manual",
        textInput("sunrise", "Sunrise, HH:MM", "07:00"),
        textInput("sunset", "Sunset, HH:MM", "18:00")),
      h4("4. Sleep and wellbeing"),
      radioButtons("type", "Was the waking date a work/study day?",
        choices = c("Work/study" = "work", "Free" = "free")),
      conditionalPanel("input.type == 'work'",
        textInput("work_start", "Required work/study start, HH:MM", "09:00")),
      textInput("onset", "Estimated time you fell asleep, HH:MM", "23:00"),
      textInput("wake", "Final waking time, HH:MM", "07:00"),
      helpText("Use the waking date. If sleep onset is later on the clock than waking, onset belongs to the previous date. One main sleep episode only."),
      checkboxInput("alarm", "Alarm or another person determined waking", FALSE),
      sliderInput("tired", "Tiredness today: 0 = none, 10 = extreme", 0, 10, 5, step = 1),
      sliderInput("mood", "Mood today: 0 = very negative, 10 = very positive", 0, 10, 5, step = 1),
      actionButton("save", "Save / update this day", class = "btn-primary"),
      helpText("Replace all suggested defaults with your answers. Save before changing dates. Saving the same date replaces its entry.")
    ),
    mainPanel(tabsetPanel(
      tabPanel("Your week",
        h3("Weekly summary"), verbatimTextOutput("summary"),
        plotOutput("sleep_plot", height = "300px"),
        plotOutput("rating_plot", height = "350px"), tableOutput("table")),
      tabPanel("Export / delete",
        p("Downloads contain location, dates and personal ratings. Store them privately."),
        downloadButton("csv", "Download this week"), hr(),
        checkboxInput("delete_ok", "Delete the selected week's entries from the active database", FALSE),
        actionButton("delete", "Delete selected week")),
      tabPanel("Population comparison",
        h3("Coming later"),
        p("No reference dataset has been added. This app does not invent population averages or percentiles."),
        p("Future comparisons need compatible measurements and a documented sample. Not every public dataset represents the general population.")),
      tabPanel("Notes and privacy",
        h4("What these measurements mean"),
        p("Sleep duration is elapsed time from estimated actual sleep onset to final waking. Night awakenings and naps are not subtracted or added."),
        p("Mood and tiredness are custom single-item ratings, not validated scales. Ideally complete them at a consistent time near the end of the waking date."),
        p("The work/free midpoint difference uses circular clock-time means. It is a diary-derived description, not a validated MCTQ score or diagnosis. Alarm-driven free days are included."),
        p("Time zone must be confirmed: longitude alone cannot determine it. Autumn DST repeated times are ambiguous and require manual review before analysis."),
        p("Sunrise/sunset describe astronomical daylight for the waking date, not light exposure. Polar events can be missing. Season labels are meteorological hemisphere conventions, not local climate."),
        h4("Private prototype only"),
        p("Anyone with an access code can read, change or delete its diary. Codes are hashed, but the database is not encrypted. Locations and dates can still identify people."),
        p("Only sessions using the same database share saved diaries. Online use needs HTTPS, restricted access and persistent storage; wider use needs suitable authentication and a research data-management plan."),
        p("Deletion removes database rows but does not securely erase backups, SQLite pages or exported files. Never upload the database or exports to GitHub."),
        h4("Project status"),
        p("This first version has descriptive plots only. The final assignment still needs an appropriate inferential analysis. Repeated daily rows from the same person must not be treated as independent participants."),
        h4("References"),
        tags$ul(
          tags$li(tags$a(href = "https://shiny.posit.co/", "Shiny")),
          tags$li(tags$a(href = "https://jessecambon.github.io/tidygeocoder/", "tidygeocoder")),
          tags$li(tags$a(href = "https://www.openstreetmap.org/copyright", "OpenStreetMap contributors")),
          tags$li(tags$a(href = "https://operations.osmfoundation.org/policies/nominatim/", "Nominatim usage policy")),
          tags$li(tags$a(href = "https://cran.r-project.org/package=suncalc", "suncalc")),
          tags$li(tags$a(href = "https://docs.ropensci.org/mctq/", "mctq: planned standardized module"))
        ))
    ))
  )
)

# ----------------------------- SERVER -----------------------------
server <- function(input, output, session) {
  active <- reactiveVal(NULL)
  revision <- reactiveVal(0L)
  refresh <- function() revision(isolate(revision()) + 1L)
  error_notice <- function(e) showNotification(conditionMessage(e), type = "error", duration = 8)
  blank <- function(x) if (!length(x) || is.na(x)) "" else as.character(x)

  observeEvent(input$new, {
    code <- tolower(uuid::UUIDgenerate(use.time = FALSE))
    active(code)
    updateTextInput(session, "code", value = code)
    updateCheckboxInput(session, "consent", value = FALSE)
    refresh()
    showNotification("New diary opened. Save your access code privately.", duration = 10)
  })
  observeEvent(input$resume, {
    code <- tolower(trimws(input$code))
    if (!valid_code(code)) {
      showNotification("Enter a complete access code generated by this app.", type = "error")
      return()
    }
    active(code)
    updateCheckboxInput(session, "consent", value = FALSE)
    refresh()
  })
  output$status <- renderText({
    if (is.null(active())) "No diary open." else "Diary open: only this code's entries are displayed."
  })
  observeEvent(input$week, {
    req(input$week)
    days <- as.character(as.Date(input$week) + 0:6)
    previous <- isolate(input$day)
    chosen <- if (length(previous) == 1L && previous %in% days) previous else days[1]
    updateSelectInput(session, "day", choices = days, selected = chosen)
  }, ignoreInit = FALSE)

  records <- reactive({
    revision()
    if (is.null(active()) || is.null(input$week)) return(data.frame())
    get_week(active(), input$week)
  })

  observeEvent(list(input$day, active(), revision()), {
    req(input$day)
    d <- records()
    r <- if (nrow(d)) d[d$date == input$day, , drop = FALSE] else d
    if (nrow(r)) {
      updateTextInput(session, "place", value = r$place[1])
      updateNumericInput(session, "lat", value = r$latitude[1])
      updateNumericInput(session, "lon", value = r$longitude[1])
      updateSelectInput(session, "tz", selected = r$timezone[1])
      updateRadioButtons(session, "type", selected = r$day_type[1])
      updateTextInput(session, "work_start", value = blank(r$work_start[1]))
      updateTextInput(session, "onset", value = r$sleep_onset[1])
      updateTextInput(session, "wake", value = r$wake_time[1])
      updateCheckboxInput(session, "alarm", value = as.logical(r$alarm[1]))
      updateSliderInput(session, "tired", value = r$tiredness[1])
      updateSliderInput(session, "mood", value = r$mood[1])
      updateCheckboxInput(session, "manual", value = as.logical(r$manual_sun[1]))
      updateTextInput(session, "sunrise", value = blank(r$sunrise[1]))
      updateTextInput(session, "sunset", value = blank(r$sunset[1]))
    } else {
      # Retain location/time zone; reset the daily form for an empty date.
      updateRadioButtons(session, "type", selected = "work")
      updateTextInput(session, "work_start", value = "09:00")
      updateTextInput(session, "onset", value = "23:00")
      updateTextInput(session, "wake", value = "07:00")
      updateCheckboxInput(session, "alarm", value = FALSE)
      updateSliderInput(session, "tired", value = 5)
      updateSliderInput(session, "mood", value = 5)
      updateCheckboxInput(session, "manual", value = FALSE)
      updateTextInput(session, "sunrise", value = "07:00")
      updateTextInput(session, "sunset", value = "18:00")
    }
  }, ignoreInit = FALSE)

  observeEvent(input$find, {
    tryCatch({
      location <- lookup_city(input$place)
      updateNumericInput(session, "lat", value = location$lat)
      updateNumericInput(session, "lon", value = location$lon)
      showNotification("Verify the coordinates and confirm the time zone.")
    }, error = error_notice)
  })
  check_location <- function() {
    if (length(input$lat) != 1L || !is.finite(input$lat) || abs(input$lat) > 90 ||
        length(input$lon) != 1L || !is.finite(input$lon) || abs(input$lon) > 180) {
      stop("Enter valid latitude and longitude.")
    }
    if (!input$tz %in% OlsonNames()) stop("Choose a valid time zone.")
  }
  current_sun <- reactive({
    req(input$day)
    check_location()
    sun_values(input$day, input$lat, input$lon, input$tz,
               isTRUE(input$manual), input$sunrise, input$sunset)
  })
  output$sun_info <- renderText({
    req(input$day)
    tryCatch({
      sun <- current_sun()
      rise <- if (is.na(sun$sunrise)) "unavailable" else sun$sunrise
      set <- if (is.na(sun$sunset)) "unavailable" else sun$sunset
      light <- if (is.na(sun$hours)) "unavailable" else sprintf("%.2f hours", sun$hours)
      paste0("Sunrise: ", rise, "; sunset: ", set, "; daylight: ", light,
             ". ", season_for(input$day, input$lat))
    }, error = function(e) conditionMessage(e))
  })

  observeEvent(input$save, {
    tryCatch({
      if (is.null(active())) stop("Create or open a diary first.")
      if (!isTRUE(input$consent)) stop("Agree to prototype storage before saving.")
      req(input$day, input$week)
      if (!input$day %in% as.character(as.Date(input$week) + 0:6)) stop("Select a day within your week.")
      check_location()
      if (!nzchar(trimws(input$place)) || nchar(input$place) > 120) stop("Enter a short city/country label.")
      if (!input$type %in% c("work", "free")) stop("Choose a day type.")
      if (input$type == "work" && !valid_time(input$work_start)) stop("Required work start must be HH:MM.")
      ratings <- c(input$tired, input$mood)
      if (length(ratings) != 2L || any(!is.finite(ratings)) ||
          any(ratings < 0 | ratings > 10) || any(ratings != round(ratings))) {
        stop("Ratings must be whole numbers from 0 to 10.")
      }
      sleep <- sleep_values(input$day, input$onset, input$wake, input$tz)
      sun <- current_sun()
      record <- data.frame(
        date = input$day, place = trimws(input$place), latitude = input$lat,
        longitude = input$lon, timezone = input$tz, season = season_for(input$day, input$lat),
        day_type = input$type,
        work_start = if (input$type == "work") input$work_start else NA_character_,
        sleep_onset = input$onset, wake_time = input$wake,
        alarm = as.integer(isTRUE(input$alarm)), tiredness = as.integer(input$tired),
        mood = as.integer(input$mood), sunrise = sun$sunrise, sunset = sun$sunset,
        manual_sun = as.integer(isTRUE(input$manual)), daylight_hours = sun$hours,
        sleep_hours = sleep$hours, midpoint_hour = sleep$midpoint,
        updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        stringsAsFactors = FALSE)
      put_day(active(), record)
      refresh()
      showNotification("Day saved. Saving this date again updates it.")
      if (sleep$hours < 3 || sleep$hours > 14) {
        showNotification("Unusual duration: check your sleep onset, date and time zone.", type = "warning", duration = 10)
      }
    }, error = error_notice)
  })

  output$summary <- renderText({
    d <- records()
    if (!nrow(d)) return("Open a diary and save your first day.")
    work <- d$midpoint_hour[d$day_type == "work"]
    free <- d$midpoint_hour[d$day_type == "free"]
    mw <- circular_mean(work); mf <- circular_mean(free)
    difference <- if (!is.finite(mw) || !is.finite(mf)) NA_real_ else abs(((mf - mw + 12) %% 24) - 12)
    comparison <- if (is.na(difference)) "Unavailable: need both day types and usable midpoints." else sprintf("%.2f hours", difference)
    paste0("Days entered: ", nrow(d), " / 7\n",
      "Work/study days: ", length(work), "; free days: ", length(free), "\n",
      "Average reported sleep: ", sprintf("%.2f hours", mean(d$sleep_hours)), "\n",
      "Work/study circular mean sleep midpoint: ", clock_label(mw), "\n",
      "Free-day circular mean sleep midpoint: ", clock_label(mf), "\n",
      "Absolute diary-derived work/free midpoint difference: ", comparison, "\n",
      "Average tiredness: ", sprintf("%.1f / 10", mean(d$tiredness)), "\n",
      "Average mood: ", sprintf("%.1f / 10", mean(d$mood)), "\n",
      "Descriptive only. Incomplete weeks and few free days need caution.")
  })
  output$table <- renderTable({
    d <- records()
    if (!nrow(d)) return(NULL)
    data.frame(Date = d$date, Type = d$day_type, Onset = d$sleep_onset,
      Wake = d$wake_time, `Sleep hours` = round(d$sleep_hours, 2),
      Midpoint = vapply(d$midpoint_hour, clock_label, character(1)),
      Sunrise = d$sunrise, Sunset = d$sunset,
      Tiredness = d$tiredness, Mood = d$mood, check.names = FALSE)
  })
  output$sleep_plot <- renderPlot({
    d <- records()
    validate(need(nrow(d) > 0, "Save a day to see your sleep plot."))
    d$plot_date <- as.Date(d$date)
    d$label <- factor(d$day_type, c("work", "free"), c("Work/study", "Free"))
    print(ggplot2::ggplot(d, ggplot2::aes(plot_date, sleep_hours, fill = label)) +
      ggplot2::geom_col(width = 0.65) +
      ggplot2::scale_fill_manual(values = c("Work/study" = "#326A96", "Free" = "#D08138")) +
      ggplot2::scale_x_date(date_breaks = "1 day", date_labels = "%a\n%d %b") +
      ggplot2::coord_cartesian(xlim = c(as.Date(input$week) - 0.5, as.Date(input$week) + 6.5)) +
      ggplot2::labs(title = "Reported sleep duration across your week", x = NULL, y = "Elapsed hours", fill = "Day type") +
      ggplot2::theme_minimal(base_size = 13) + ggplot2::theme(legend.position = "top"))
  })
  output$rating_plot <- renderPlot({
    d <- records()
    validate(need(nrow(d) > 0, "Save a day to see your ratings."))
    long <- rbind(
      data.frame(date = as.Date(d$date), value = d$mood, measure = "Mood: higher = more positive"),
      data.frame(date = as.Date(d$date), value = d$tiredness, measure = "Tiredness: higher = more tired"))
    print(ggplot2::ggplot(long, ggplot2::aes(date, value, colour = measure)) +
      ggplot2::geom_point(size = 3) + ggplot2::facet_wrap(~measure, ncol = 1) +
      ggplot2::scale_colour_manual(values = c("#38856D", "#9D5267")) +
      ggplot2::scale_y_continuous(limits = c(0, 10), breaks = c(0, 5, 10)) +
      ggplot2::scale_x_date(date_breaks = "1 day", date_labels = "%a\n%d %b") +
      ggplot2::coord_cartesian(xlim = c(as.Date(input$week) - 0.5, as.Date(input$week) + 6.5)) +
      ggplot2::labs(title = "Daily wellbeing ratings", x = NULL, y = "Rating (0–10)") +
      ggplot2::theme_minimal(base_size = 12) + ggplot2::theme(legend.position = "none"))
  })
  output$csv <- downloadHandler(
    filename = function() paste0("sleep-diary-", input$week, ".csv"),
    content = function(file) {
      req(active())
      d <- records()
      d$token_hash <- NULL
      if (nrow(d)) d$place <- ifelse(grepl("^[[:space:]]*[=+@-]", d$place), paste0("'", d$place), d$place)
      utils::write.csv(d, file, row.names = FALSE, na = "")
    })
  observeEvent(input$delete, {
    tryCatch({
      if (is.null(active())) stop("Open a diary first.")
      if (!isTRUE(input$delete_ok)) stop("Tick deletion confirmation first.")
      req(input$week)
      with_db(function(con) DBI::dbExecute(con,
        "DELETE FROM diary WHERE token_hash = ? AND date >= ? AND date <= ?",
        params = list(hash_code(active()), as.character(as.Date(input$week)),
                      as.character(as.Date(input$week) + 6))))
      refresh()
      updateCheckboxInput(session, "delete_ok", value = FALSE)
      showNotification("Selected week's entries deleted from the active database.")
    }, error = error_notice)
  })
}

# ------------------------------ RUN -------------------------------
shiny::runApp(shiny::shinyApp(ui = ui, server = server), launch.browser = TRUE)
