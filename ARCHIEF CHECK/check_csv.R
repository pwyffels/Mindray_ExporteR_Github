library(readr)
library(janitor)
library(dplyr)
library(tidyr)
library(stringr)

path <- '/Volumes/PHILIPS UFD/_810804 046A39_8261161810_C400ADD6-D515-3809-1215-16301400A35A/ParameterData/ParameterData-1Min-20260919131000~20260920044100.csv'
tab <- readr::read_csv(path, na = c("--")) |> janitor::clean_names()

cat("Total rows:", nrow(tab), "\n")
cat("Rows with non-NA hr_bpm:", sum(!is.na(tab$hr_bpm)), "\n")
cat("Rows with non-NA nibp_s_mm_hg:", sum(!is.na(tab$nibp_s_mm_hg)), "\n")
cat("Rows with non-NA sp_o2_percent:", sum(!is.na(tab$sp_o2_percent)), "\n")

cols_of_interest <- c("time",
  "hr_bpm", "sp_o2_percent",
  "nibp_s_mm_hg", "nibp_d_mm_hg", "nibp_m_mm_hg",
  "art_s_mm_hg",  "art_d_mm_hg",  "art_m_mm_hg",
  "cvp_mm_hg", "ppv_percent",
  "et_o2_percent", "fi_o2_percent",
  "rr_rpm", "peep", "ppeak", "pplat", "pmean", "t_vi", "mv", "m_vspn", "ftotal",
  "pv_cs_min", "pauses_min", "vp_bs_min", "couplets_min",
  "missed_beats_min", "r_on_ts_min", "qt_ms", "q_tc_ms",
  "sev_et_fi", "co2_et_fi"
)

tab2 <- tab |> dplyr::select(any_of(cols_of_interest))

cat("\nColumns found:\n")
print(names(tab2))

cat("\nColumns NOT found:\n")
print(setdiff(cols_of_interest, names(tab)))

cat("\nAll cleaned column names:\n")
print(names(tab))

# Simulate cleaner1 for SAT (SpO2)
sat <- tab2 |> select(time, any_of("sp_o2_percent"))
cat("\nSAT data head:\n")
print(head(sat))

# What does range() return on an all-NA time column?
cat("\ntime class:", class(tab2$time), "\n")
cat("time range:", format(range(tab2$time, na.rm=TRUE)), "\n")
