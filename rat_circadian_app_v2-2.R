# Rat body clock lab: animated 30-day double-plotted actogram
# Run with: shiny::runApp("rat_circadian_app_v2.R")   (needs only the 'shiny' package)
library(shiny)

dt <- 0.1; nb <- 240; ND <- 30
tc <- (0:(nb - 1)) * dt

# Light phase-response curve for a night-active animal. phi = 12 is activity onset.
# Light early in the subjective night delays the clock, late in the night advances it,
# during the subjective day it does (almost) nothing.
prc <- function(ph) ifelse(ph >= 12, -sin(2 * pi * (ph - 12) / 12), 0)

simulate_rat <- function(segs, photo, inten, tau, K, asch, alpha, mask, noise, t0, maxrate, seed) {
  set.seed(seed)
  Lv <- unlist(lapply(seq_len(nrow(segs)), function(i) {
    day <- switch(segs$reg[i],
      LD = as.numeric(((tc - segs$lon[i]) %% 24) < photo) * inten,
      DD = rep(0, nb),
      LL = rep(inten, nb))
    rep(day, segs$days[i])
  }))
  n <- length(Lv); Phi <- numeric(n + 1); ph <- numeric(n)
  Phi[1] <- 12 - t0
  if (segs$reg[1] == "LD") {
    # Warm-up: the rat has already lived in this light cycle for 60 days before day 1,
    # so its clock is already settled (entrained) and shows no initial shift.
    Lw <- rep(as.numeric(((tc - segs$lon[1]) %% 24) < photo) * inten, 60)
    Pw <- Phi[1]
    for (L in Lw) Pw <- Pw + dt * (24 / (tau + asch * L) + K * L * prc(Pw %% 24))
    Phi[1] <- Pw %% 24
  }
  for (i in seq_len(n)) {
    ph[i] <- Phi[i] %% 24
    rate <- 24 / (tau + asch * Lv[i]) + K * Lv[i] * prc(ph[i]) + rnorm(1, 0, noise / sqrt(dt))
    Phi[i + 1] <- Phi[i] + dt * rate
  }
  d <- (ph - 12) %% 24
  drive <- plogis(d / 0.4) * plogis((alpha - d) / 0.4)
  cnt <- rpois(n, maxrate * drive * (1 - mask * Lv) * rgamma(n, 8, 8))
  onsets <- which(diff(floor((Phi - 12) / 24)) > 0) * dt
  list(cnt = cnt, L = Lv, onsets = onsets, segs = segs, photo = photo)
}

reg_label <- function(r) switch(r, LD = "\u2600\uFE0F\U0001F319 Light/Dark cycle", DD = "\U0001F311 All dark", LL = "\u2600\uFE0F All light")
reg_choices <- c("\u2600\uFE0F\U0001F319 Light/Dark cycle (LD)" = "LD", "\U0001F311 All dark (DD)" = "DD", "\u2600\uFE0F All light (LL)" = "LL")

block_ui <- function(i, reg, lon) {
  div(class = "blk",
    strong(textOutput(paste0("blkdays", i), inline = TRUE)),
    selectInput(paste0("reg", i), NULL, reg_choices, reg),
    conditionalPanel(sprintf("input.reg%d == 'LD'", i),
      numericInput(paste0("lon", i), "Lights come on at (hour, 0\u201323)", lon, 0, 23.5, 0.5)))
}

ui <- fluidPage(
  tags$head(tags$style(HTML("
    .blk{background:#f4f6fa;border-radius:8px;padding:8px 10px;margin-bottom:8px;}
    .blk .form-group{margin-bottom:4px;}
    .card{background:#fff;border-left:5px solid #4a6fa5;border-radius:6px;padding:8px 12px;margin-bottom:8px;box-shadow:0 1px 3px #0002;}
    .tip{color:#666;font-size:12px;margin-top:-6px;margin-bottom:10px;}
    details{background:#fffbe6;border-radius:6px;padding:8px 12px;margin-bottom:10px;}
    .presets .btn{margin:2px;}
  "))),
  titlePanel("\U0001F400 Rat Body Clock Lab"),
  tags$details(open = NA,
    tags$summary(strong("New here? What am I looking at?")),
    p("Rats have an internal 'body clock' that makes them run at night. This lab simulates a rat with a running wheel for 30 days.
       Each ", strong("row"), " of the chart is one day; each ", strong("black bar"), " is how much the rat ran at that time.
       Grey background = lights off, white = lights on. The chart shows two days side-by-side so you can follow the pattern across midnight."),
    p("Pick a lighting schedule on the left (or press a preset) and watch the chart ", strong("draw itself day by day"), ".
       A staircase of bars that slants means the clock is running free; a straight vertical edge means the clock is locked to the lights.")),
  sidebarLayout(
    sidebarPanel(width = 4,
      h4("1. Quick experiments"),
      div(class = "presets",
        actionButton("p_norm", "\u2600\uFE0F\U0001F319 Normal 12:12"),
        actionButton("p_dark", "\U0001F311 All dark"),
        actionButton("p_light", "\u2600\uFE0F All light"),
        actionButton("p_lddd", "Light/Dark \u2192 dark"),
        actionButton("p_ldll", "Light/Dark \u2192 light"),
        actionButton("p_jet", "\u2708\uFE0F Jet lag")),
      h4("2. Or build your own 30 days"),
      sliderInput("split", "Switch the lighting after day\u2026", 1, 30, c(10, 20), step = 1),
      div(class = "tip", "Drag the two handles to split the 30 days into up to three blocks."),
      block_ui(1, "LD", 6), block_ui(2, "DD", 6), block_ui(3, "LL", 6),
      sliderInput("photo", "Hours of light per day in a Light/Dark cycle", 1, 23, 12, 1),
      checkboxInput("adv", "Show advanced settings (rat settings)", FALSE),
      conditionalPanel("input.adv",
        h4("3. Rat settings"),
        sliderInput("tau", "Rat's natural day length (h)", 23, 25.5, 24.2, 0.05),
        div(class = "tip", "With no light cues the rat's clock runs on its own schedule, a bit longer than 24 h."),
        sliderInput("K", "How strongly light resets the clock", 0, 0.6, 0.3, 0.01),
        div(class = "tip", "0 = clock ignores light (can't sync). Higher = easier to sync."),
        sliderInput("alpha", "Hours the rat is active each day", 4, 16, 11, 0.5),
        sliderInput("inten", "Light brightness (0.1\u20131)", 0.1, 1, 1, 0.05),
        sliderInput("asch", "Slowing of clock in constant light (h)", 0, 2, 0.6, 0.05),
        sliderInput("t0", "First running start on day 1 (hour; only used if day 1 is dark/light)", 0, 24, 18, 0.5),
        sliderInput("maxrate", "Max wheel turns per 6 min", 5, 100, 40, 5),
        numericInput("seed", "Random seed (change for a different rat)", 1))
    ),
    mainPanel(width = 8,
      tabsetPanel(
        tabPanel("\U0001F9EA Experiment",
          br(),
          fluidRow(
            column(3, actionButton("play", "\u23F8 Pause", width = "100%")),
            column(3, actionButton("restart", "\u21BB Replay", width = "100%")),
            column(3, sliderInput("speed", "Speed (days/sec)", 1, 10, 3, 1, ticks = FALSE)),
            column(3, checkboxInput("anim", "Animate when I change settings", TRUE))),
          plotOutput("acto", height = "640px"),
          fluidRow(
            column(6, h4("\U0001F4AC What's happening?"), uiOutput("story")),
            column(6, h4("When does running start each day?"), plotOutput("onset", height = "300px"),
                   div(class = "tip", "Dots = start of running. Blue line = lights on, orange = lights off. Dots wrap around at midnight.")))),
        tabPanel("\U0001F4D6 Learn",
          br(),
          h4("The body clock"),
          p("Almost every animal has an internal clock that takes about a day to complete a cycle ('circadian' = 'about a day'). In rats it controls when they sleep and run. It lives in a tiny brain region called the SCN."),
          h4("Free-running (all dark)"),
          p("With no light to look at, the clock runs at its own pace, about 24.2 hours in rats. That's slightly longer than a real day, so every day the rat starts running a bit later. On the chart this looks like a slanted line of bars drifting to the right."),
          h4("Entrainment (light/dark cycle)"),
          p("Light acts like a daily 'reset button'. Morning light nudges the clock earlier, just enough to cancel the extra 0.2 h. The result: the rat starts running at the same time every day, right when the lights go out. Scientists call this 'entrainment' or 'being entrained'."),
          h4("Why light only works at certain times"),
          p("The clock responds differently depending on when light arrives. Light early in the rat's night pushes the clock later; light late in its night pushes it earlier; light in its day does almost nothing. This timing rule is called a ", em("phase response curve"), "."),
          h4("All light (constant light)"),
          p("In constant light there's no daily rhythm of 'light then dark', so nothing resets the clock. In night-active animals light also makes the clock run slower (", em("Aschoff's rule"), "), so the pattern drifts later each day with a longer cycle than in the dark."),
          h4("Jet lag"),
          p("Shift the lights later by 6 hours. The clock can't jump; it shifts a little each day, so for several days the rat's running is out of step with the lights until it catches up."),
          h4("Things to try"),
          tags$ul(
            tags$li("Press 'All dark' and note the slant: that's the clock's own period."),
            tags$li("Set 'How strongly light resets the clock' to 0 in a normal 12:12 cycle. The rat can't sync."),
            tags$li("Set natural day length to 25.4 h. Light isn't strong enough to keep up, so entrainment breaks."),
            tags$li("Press 'Jet lag' and count how many days it takes to re-sync."),
            tags$li("Try a 2-hour light day (short light) and see how running gets timed.")),
          p(em("Note: this is a simplified teaching model, not real data. It mimics the main behaviors of rat running-wheel records.")))
      )
    )
  )
)

server <- function(input, output, session) {
  # ---- presets
  setp <- function(regs, lons, split) {
    for (i in 1:3) {
      updateSelectInput(session, paste0("reg", i), selected = regs[i])
      updateNumericInput(session, paste0("lon", i), value = lons[i])
    }
    updateSliderInput(session, "split", value = split)
  }
  observeEvent(input$p_norm,  setp(c("LD", "LD", "LD"), c(6, 6, 6), c(30, 30)))
  observeEvent(input$p_dark,  setp(c("DD", "DD", "DD"), c(6, 6, 6), c(30, 30)))
  observeEvent(input$p_light, setp(c("LL", "LL", "LL"), c(6, 6, 6), c(30, 30)))
  observeEvent(input$p_lddd,  setp(c("LD", "DD", "DD"), c(6, 6, 6), c(15, 30)))
  observeEvent(input$p_ldll,  setp(c("LD", "LL", "LL"), c(6, 6, 6), c(15, 30)))
  observeEvent(input$p_jet,   setp(c("LD", "LD", "LD"), c(6, 12, 12), c(10, 30)))

  # ---- schedule blocks
  blocks <- reactive({
    sp <- input$split
    days <- c(sp[1], sp[2] - sp[1], ND - sp[2])
    lon <- sapply(1:3, function(i) { v <- input[[paste0("lon", i)]]; if (is.null(v) || is.na(v)) 6 else v })
    s <- data.frame(reg = sapply(1:3, function(i) input[[paste0("reg", i)]]),
                    days = days, lon = lon, stringsAsFactors = FALSE)
    s$start <- c(0, cumsum(days)[-3]); s$end <- cumsum(days)
    s
  })
  for (i in 1:3) local({
    k <- i
    output[[paste0("blkdays", k)]] <- renderText({
      b <- blocks()[k, ]
      if (b$days == 0) "Block (unused)" else sprintf("Days %d\u2013%d", b$start + 1, b$end)
    })
  })

  params <- reactive(list(segs = blocks(), photo = input$photo, inten = input$inten %||% 1,
    tau = input$tau, K = input$K, asch = input$asch %||% 0.6, alpha = input$alpha, mask = 0,
    noise = 0, t0 = input$t0 %||% 18, maxrate = input$maxrate %||% 40, seed = input$seed %||% 1))
  params_d <- debounce(params, 500)

  sim <- reactive({
    p <- params_d(); p$segs <- p$segs[p$segs$days > 0, ]
    req(nrow(p$segs) > 0)
    do.call(simulate_rat, p)
  })

  # ---- animation state
  rv <- reactiveValues(shown = ND, playing = FALSE)
  observeEvent(sim(), {
    if (isTRUE(input$anim)) { rv$shown <- 1; rv$playing <- TRUE } else { rv$shown <- ND; rv$playing <- FALSE }
  })
  observe({
    req(rv$playing)
    invalidateLater(round(1000 / input$speed))
    isolate({ if (rv$shown >= ND) rv$playing <- FALSE else rv$shown <- rv$shown + 1 })
  })
  observe(updateActionButton(session, "play", label = if (rv$playing) "\u23F8 Pause" else "\u25B6 Play"))
  observeEvent(input$play, {
    if (!rv$playing && rv$shown >= ND) rv$shown <- 1
    rv$playing <- !rv$playing
  })
  observeEvent(input$restart, { rv$shown <- 1; rv$playing <- TRUE })

  # ---- actogram
  output$acto <- renderPlot({
    r <- sim(); s <- r$segs; shown <- rv$shown
    M <- matrix(r$cnt, nrow = ND, byrow = TRUE); Lm <- matrix(r$L, nrow = ND, byrow = TRUE)
    mx <- max(M, 1)
    par(mar = c(4.5, 4.5, 3.5, 8), cex = 1.1)
    plot(NA, xlim = c(0, 48), ylim = c(ND, 0), xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n",
         xlab = "Time of day (hours): two days shown side by side", ylab = "Day of experiment",
         main = sprintf("Running-wheel actogram: day %d of %d", shown, ND))
    for (row in 1:ND) for (k in 0:1) {
      d <- row + k; if (d > ND) next
      rl <- rle(Lm[d, ] == 0); en <- cumsum(rl$lengths); st <- en - rl$lengths + 1
      if (any(rl$values)) rect(k * 24 + (st[rl$values] - 1) * dt, row - 1, k * 24 + en[rl$values] * dt, row, col = "grey82", border = NA)
      if (d <= shown) {
        idx <- which(M[d, ] > 0)
        if (length(idx)) rect(k * 24 + (idx - 1) * dt, row - M[d, idx] / mx * 0.95, k * 24 + idx * dt, row, col = "black", border = NA)
      }
    }
    abline(v = 24, col = "grey50", lty = 3)
    axis(1, at = seq(0, 48, 6), labels = c(seq(0, 24, 6), seq(6, 24, 6)))
    ax <- c(1, seq(5, ND, 5)); axis(2, at = ax - 0.5, labels = ax, las = 1)
    abline(h = s$end[-nrow(s)], col = "red", lty = 2, lwd = 2)
    par(xpd = NA)
    mid <- (s$start + s$end) / 2
    text(48.6, mid, sapply(s$reg, function(x) switch(x, LD = "Light/\nDark", DD = "Dark", LL = "Light")), adj = 0, col = "red", cex = 0.9)
    mtext("grey = lights off   white = lights on   black bars = running", side = 3, line = 0, cex = 0.8, col = "grey30")
  })

  # ---- onset plot
  output$onset <- renderPlot({
    r <- sim(); s <- r$segs; shown <- rv$shown
    par(mar = c(4, 4, 1, 1))
    plot(NA, xlim = c(0, ND), ylim = c(0, 24), xlab = "Day", ylab = "Running starts at (hour)", yaxt = "n")
    axis(2, at = seq(0, 24, 6), las = 1)
    ld <- s[s$reg == "LD", ]
    if (nrow(ld)) for (i in seq_len(nrow(ld))) {
      segments(ld$start[i], ld$lon[i], ld$end[i], ld$lon[i], col = "steelblue", lwd = 3)
      lo <- (ld$lon[i] + r$photo) %% 24
      segments(ld$start[i], lo, ld$end[i], lo, col = "darkorange", lwd = 3)
    }
    abline(v = s$end[-nrow(s)], col = "red", lty = 2)
    o <- r$onsets; o <- o[o / 24 < shown]
    points(o / 24, o %% 24, pch = 16, cex = 1.1)
  })

  # ---- plain-language narration
  output$story <- renderUI({
    r <- sim(); s <- r$segs; on <- r$onsets; shown <- rv$shown
    cards <- lapply(seq_len(nrow(s)), function(i) {
      if (s$start[i] >= shown) return(NULL)
      lo <- s$start[i] * 24; hi <- s$end[i] * 24
      ob <- on[on >= lo + (hi - lo) / 2 & on < hi]
      if (length(ob) < 2) ob <- on[on >= lo & on < hi]
      per <- if (length(ob) >= 2) mean(diff(ob)) else NA
      drift <- round((per - 24) * 60)
      txt <- if (is.na(per)) "This block is too short to measure the clock. Try making it longer."
      else if (s$reg[i] == "DD") sprintf("No light, so the clock runs free at about %.2f h per cycle. Running starts about %d minutes %s each day.",
                                         per, abs(drift), if (drift >= 0) "later" else "earlier")
      else if (s$reg[i] == "LL") sprintf("Constant light: no daily reset signal, the clock slows down (about %.2f h per cycle), so running starts about %d minutes %s each day.",
                                          per, abs(drift), if (drift >= 0) "later" else "earlier")
      else {
        loff <- (s$lon[i] + r$photo) %% 24
        psi <- ((loff - tail(ob %% 24, 1) + 12) %% 24) - 12
        if (abs(per - 24) < 0.1)
          sprintf("\U0001F512 The clock is locked to the lights. Running starts about %.1f h %s lights-off.", abs(psi), if (psi >= 0) "before" else "after")
        else
          sprintf("\u26A0\uFE0F Not locked to the lights (still slipping %d min %s per day). Either the clock is still catching up after a change, light is too weak a signal, or the natural day length is too far from 24 h.",
                  abs(drift), if (drift >= 0) "later" else "earlier")
      }
      div(class = "card", strong(sprintf("Days %d\u2013%d: %s", s$start[i] + 1, s$end[i], reg_label(s$reg[i]))), p(txt, style = "margin:4px 0 0 0;"))
    })
    tagList(cards)
  })
}

`%||%` <- function(a, b) if (is.null(a)) b else a
shinyApp(ui, server)
