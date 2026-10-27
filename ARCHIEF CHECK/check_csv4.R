sink("/Users/pietwyffels/Documents/R/Mindray_ExporteR_Github/check_output4.txt")

library(readr); library(janitor); library(dplyr); library(tidyr)
library(stringr); library(plotly)

path <- '/Volumes/PHILIPS UFD/_810804 046A39_8261161810_C400ADD6-D515-3809-1215-16301400A35A/ParameterData/ParameterData-1Min-20260919131000~20260920044100.csv'

wrangle_raw <- function(path) {
  tab <- readr::read_csv(path, na = c("--"), show_col_types = FALSE) |> janitor::clean_names()
  cols_of_interest <- c("time","hr_bpm","sp_o2_percent",
    "nibp_s_mm_hg","nibp_d_mm_hg","nibp_m_mm_hg",
    "art_s_mm_hg","art_d_mm_hg","art_m_mm_hg",
    "cvp_mm_hg","ppv_percent","et_o2_percent","fi_o2_percent",
    "rr_rpm","peep","ppeak","pplat","pmean","t_vi","mv","m_vspn","ftotal",
    "pv_cs_min","pauses_min","vp_bs_min","couplets_min",
    "missed_beats_min","r_on_ts_min","qt_ms","q_tc_ms","sev_et_fi","co2_et_fi")
  tab <- tab |> dplyr::select(any_of(cols_of_interest))
  if ("sev_et_fi" %in% names(tab))
    tab <- tab |> tidyr::separate_wider_delim(sev_et_fi, delim="/",
                    names=c("et_sevo","fi_sevo"), too_few="align_start")
  if ("co2_et_fi" %in% names(tab))
    tab <- tab |> tidyr::separate_wider_delim(co2_et_fi, delim="/",
                    names=c("et_co2","fi_co2"), too_few="align_start")
  tab |> mutate(across(-time, ~ as.numeric(str_trim(str_remove_all(.x, "\\[.*?\\]")))))
}

cleaner1 <- function(tab, subclass) {
  sel <- tab |> select(time, tidyselect::any_of(subclass))
  if (ncol(sel) == 1)
    return(tab |> select(time) |> mutate(names = NA_character_, values = NA_real_))
  sel |> na.omit() |>
    pivot_longer(cols = -time, names_to = "names", values_to = "values")
}

tab <- wrangle_raw(path)

cat("Columns after wrangle:\n")
print(names(tab))

cat("\net_co2 class:", class(tab$et_co2), "\n")
cat("et_co2 non-NA:", sum(!is.na(tab$et_co2)), "\n")
cat("et_co2 sample:\n")
print(tab |> filter(!is.na(et_co2)) |> select(time, et_co2, fi_co2) |> head(5))

cat("\net_sevo non-NA:", sum(!is.na(tab$et_sevo)), "\n")

# Check if co2_et_fi parsing works
raw <- readr::read_csv(path, na=c("--"), show_col_types=FALSE) |> janitor::clean_names()
cat("\nRaw co2_et_fi sample (non-NA):\n")
print(raw |> select(co2_et_fi) |> filter(!is.na(co2_et_fi)) |> head(5))

cat("\nRaw sev_et_fi unique values:\n")
print(unique(raw$sev_et_fi)[1:10])

# The problem: sev_et_fi is "--/--" not "--"
# Does na=c("--") catch "--/--"? No!
cat("\nIs '--/--' treated as NA?\n")
test <- readr::read_csv(path, na=c("--"), show_col_types=FALSE)
cat("sev_et_fi unique values (raw col name '+Sev(Et/Fi)'):\n")
col_idx <- which(names(test) == "+Sev(Et/Fi)")
print(unique(test[[col_idx]])[1:10])

sink()
cat("Done\n")
