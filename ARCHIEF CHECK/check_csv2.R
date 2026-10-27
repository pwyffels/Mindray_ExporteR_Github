library(readr)
library(janitor)
library(dplyr)
library(tidyr)
library(stringr)
library(plotly)

path <- '/Volumes/PHILIPS UFD/_810804 046A39_8261161810_C400ADD6-D515-3809-1215-16301400A35A/ParameterData/ParameterData-1Min-20260919131000~20260920044100.csv'

# Replicate wrangle_raw exactly
wrangle_raw <- function(path) {
  tab <- readr::read_csv(path, na = c("--")) |> janitor::clean_names()

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
  tab <- tab |> dplyr::select(any_of(cols_of_interest))

  if ("sev_et_fi" %in% names(tab))
    tab <- tab |> tidyr::separate_wider_delim(sev_et_fi, delim = "/",
                                        names = c("et_sevo", "fi_sevo"),
                                        too_few = "align_start")
  if ("co2_et_fi" %in% names(tab))
    tab <- tab |> tidyr::separate_wider_delim(co2_et_fi, delim = "/",
                                        names = c("et_co2", "fi_co2"),
                                        too_few = "align_start")

  tab |> mutate(across(-time, ~ as.numeric(str_trim(str_remove_all(.x, "\\[.*?\\]")))))
}

cleaner1 <- function(tab, subclass) {
  sel <- tab |> select(time, tidyselect::any_of(subclass))
  if (ncol(sel) == 1)
    return(tab |> select(time) |> mutate(names = NA_character_, values = NA_real_))
  sel |> na.omit() |>
    pivot_longer(cols = -time, names_to = "names", values_to = "values")
}

cat("Running wrangle_raw...\n")
tab <- tryCatch(wrangle_raw(path), error = function(e) { cat("ERROR in wrangle_raw:", conditionMessage(e), "\n"); NULL })

if (!is.null(tab)) {
  cat("wrangle_raw succeeded. Columns:\n")
  print(names(tab))
  
  # Check sev_et_fi / co2_et_fi raw values
  cat("\nSample sev_et_fi raw (before wrangle):\n")
  raw <- readr::read_csv(path, na = c("--"), show_col_types = FALSE) |> janitor::clean_names()
  cat("sev_et_fi present:", "sev_et_fi" %in% names(raw), "\n")
  if ("sev_et_fi" %in% names(raw)) {
    print(raw |> select(sev_et_fi) |> filter(!is.na(sev_et_fi)) |> head(5))
  }
  
  cat("\nppeak column class:", class(tab$ppeak), "\n")
  cat("ppeak sample values:\n")
  print(tab |> filter(!is.na(ppeak)) |> select(time, ppeak) |> head(5))
  
  # Test cleaner1 for SAT
  cat("\nTesting cleaner1 for SAT...\n")
  sat <- cleaner1(tab, "sp_o2_percent")
  cat("SAT rows:", nrow(sat), "\n")
  cat("SAT time range:", format(range(sat$time, na.rm=TRUE)), "\n")
  
  # Test the separate_wider_delim on sev_et_fi
  cat("\nChecking sev_et_fi after separate:\n")
  cat("et_sevo present:", "et_sevo" %in% names(tab), "\n")
  if ("et_sevo" %in% names(tab)) {
    cat("non-NA et_sevo rows:", sum(!is.na(tab$et_sevo)), "\n")
  }
  
  # Test make_plotly1 step by step
  cat("\nTesting make_plotly1 components...\n")
  
  struct_data <- function(tab) {
    list(
      HEMO = list(
        ECG  = cleaner1(tab, c("pv_cs_min","pauses_min","vp_bs_min","couplets_min",
                                "missed_beats_min","r_on_ts_min","qt_ms","q_tc_ms")),
        HR   = cleaner1(tab, "hr_bpm"),
        NIBP = cleaner1(tab, c("nibp_s_mm_hg","nibp_d_mm_hg","nibp_m_mm_hg")),
        ART  = cleaner1(tab, c("art_s_mm_hg","art_d_mm_hg","art_m_mm_hg"))
      ),
      VENTILATION = list(
        SAT         = cleaner1(tab, "sp_o2_percent"),
        VENTILATION = cleaner1(tab, c("rr_rpm","peep","ppeak","pplat","pmean",
                                       "t_vi","mv","m_vspn","ftotal"))
      ),
      GAS = list(
        O2   = cleaner1(tab, c("fi_o2_percent","et_o2_percent")),
        CO2  = cleaner1(tab, c("fi_co2","et_co2")),
        SEVO = cleaner1(tab, c("fi_sevo","et_sevo"))
      )
    )
  }
  
  dc <- struct_data(tab)
  
  cat("HR rows:", nrow(dc$HEMO$HR), "\n")
  cat("NIBP rows:", nrow(dc$HEMO$NIBP), "\n")
  cat("ART rows:", nrow(dc$HEMO$ART), "\n")
  cat("SAT rows:", nrow(dc$VENTILATION$SAT), "\n")
  
  cat("\nNIBP head:\n")
  print(head(dc$HEMO$NIBP))
  
  cat("\nNIBP pivot_wider test:\n")
  nibp_wide <- tryCatch(
    dc$HEMO$NIBP |> pivot_wider(names_from = names, values_from = values),
    error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NULL }
  )
  if (!is.null(nibp_wide)) print(head(nibp_wide))
}
