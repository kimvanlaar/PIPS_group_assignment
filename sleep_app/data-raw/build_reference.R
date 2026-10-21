# ---------------------------------------------------------------
# data-raw/build_reference.R
# Run ONCE (from the sleep-app folder) to create data/nhanes_ref.rds
#
#   setwd("sleep-app"); source("data-raw/build_reference.R")
#
# Source: NHANES 2017 - March 2020 pre-pandemic files
#   P_DEMO  demographics + survey weights
#   P_SLQ   sleep questionnaire (MCTQ-style):
#             SLQ300/SLQ310  usual sleep time / wake time on weekdays-or-workdays
#             SLQ320/SLQ330  usual sleep time / wake time on weekends-or-non-workdays
#   P_DPQ   PHQ-9 depression screener (used here for mood + tiredness items)
#
# install.packages(c("haven", "dplyr"))
# ---------------------------------------------------------------

library(haven)
library(dplyr)
source("R/helpers.R")

base <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/"

get_xpt <- function(file) {
  tmp <- tempfile(fileext = ".xpt")
  download.file(paste0(base, file), tmp, mode = "wb", quiet = TRUE)
  haven::read_xpt(tmp)
}

message("Downloading NHANES files ...")
demo <- get_xpt("P_DEMO.xpt") %>%
  transmute(seqn = SEQN, age = RIDAGEYR,
            sex = ifelse(RIAGENDR == 1, "M", "F"),
            w = WTINTPRP)                       # interview weight (sleep Qs are interview items)

slq <- get_xpt("P_SLQ.xpt") %>%
  select(seqn = SEQN, SLQ300, SLQ310, SLQ320, SLQ330)

phq <- tryCatch(
  get_xpt("P_DPQ.xpt") %>%
    transmute(seqn = SEQN,
              phq_down  = ifelse(DPQ020 %in% 0:3, DPQ020, NA),   # feeling down, depressed, hopeless
              phq_tired = ifelse(DPQ040 %in% 0:3, DPQ040, NA)),  # tired / little energy
  error = function(e) { message("PHQ-9 file not available: ", conditionMessage(e)); NULL }
)

ref <- demo %>%
  inner_join(slq, by = "seqn") %>%
  { if (!is.null(phq)) left_join(., phq, by = "seqn") else mutate(., phq_down = NA_real_, phq_tired = NA_real_) } %>%
  mutate(
    # NHANES stores clock times as "HH:MM" text (24h)
    so_w = parse_hm(SLQ300), se_w = parse_hm(SLQ310),
    so_f = parse_hm(SLQ320), se_f = parse_hm(SLQ330),
    sd_w = (se_w - so_w) %% 24,
    sd_f = (se_f - so_f) %% 24,
    ms_w = (so_w + sd_w / 2) %% 24,
    ms_f = (so_f + sd_f / 2) %% 24
  ) %>%
  filter(age >= 16, !is.na(w), w > 0,
         !is.na(sd_w), !is.na(sd_f),
         between(sd_w, 2, 16), between(sd_f, 2, 16))

core <- mctq_core(ref$sd_w, ref$sd_f, ref$ms_w, ref$ms_f, n_w = 5)
ref <- ref %>%
  mutate(sd_week = core$sd_week, msf_sc = core$msf_sc, sjl = core$sjl) %>%
  select(seqn, age, sex, w, sd_w, sd_f, sd_week, ms_w, ms_f, msf_sc, sjl, phq_down, phq_tired)

dir.create("data", showWarnings = FALSE)
saveRDS(ref, "data/nhanes_ref.rds")

# ---- quick sanity check (weighted) ----
wm <- function(x) weighted.mean(x, ref$w, na.rm = TRUE)
message(sprintf("\nSaved %d participants to data/nhanes_ref.rds", nrow(ref)))
message(sprintf("Weighted mean sleep duration  workdays: %.2f h | free days: %.2f h", wm(ref$sd_w), wm(ref$sd_f)))
message(sprintf("Weighted mean social jet lag: %.2f h", wm(ref$sjl)))
message(sprintf("Weighted mean MSFsc: %s", fmt_hm(wm(wrap_ms(ref$msf_sc)))))
