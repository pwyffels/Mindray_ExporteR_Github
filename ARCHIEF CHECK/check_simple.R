out <- file("/Users/pietwyffels/Documents/R/Mindray_ExporteR_Github/out.txt", "w")

tryCatch({
  library(readr); library(janitor); library(dplyr); library(tidyr); library(stringr)
  
  path <- '/Volumes/PHILIPS UFD/_810804 046A39_8261161810_C400ADD6-D515-3809-1215-16301400A35A/ParameterData/ParameterData-1Min-20260919131000~20260920044100.csv'
  
  cat("Step 1: reading CSV\n", file=out)
  tab <- suppressMessages(readr::read_csv(path, na = c("--"))) |> janitor::clean_names()
  cat("Step 2: read OK, nrow=", nrow(tab), "\n", file=out)
  
  tab <- tab |> dplyr::select(any_of(c("time","hr_bpm","sp_o2_percent",
    "nibp_s_mm_hg","nibp_d_mm_hg","nibp_m_mm_hg",
    "art_s_mm_hg","art_d_mm_hg","art_m_mm_hg",
    "cvp_mm_hg","ppv_percent","et_o2_percent","fi_o2_percent",
    "rr_rpm","peep","ppeak","pplat","pmean","t_vi","mv","m_vspn","ftotal",
    "pv_cs_min","pauses_min","vp_bs_min","couplets_min",
    "missed_beats_min","r_on_ts_min","qt_ms","q_tc_ms","sev_et_fi","co2_et_fi")))
  
  cat("Step 3: select OK, ncol=", ncol(tab), "\n", file=out)
  cat("sev_et_fi class:", class(tab$sev_et_fi), "\n", file=out)
  cat("sev_et_fi unique:", paste(unique(tab$sev_et_fi)[1:5], collapse="|"), "\n", file=out)
  
  cat("Step 4: separate sev_et_fi\n", file=out)
  tab <- tab |> tidyr::separate_wider_delim(sev_et_fi, delim="/",
                  names=c("et_sevo","fi_sevo"), too_few="align_start")
  cat("Step 5: separate sev OK\n", file=out)
  
  cat("Step 6: separate co2_et_fi\n", file=out)
  tab <- tab |> tidyr::separate_wider_delim(co2_et_fi, delim="/",
                  names=c("et_co2","fi_co2"), too_few="align_start")
  cat("Step 7: separate co2 OK\n", file=out)
  
  tab <- tab |> mutate(across(-time, ~ as.numeric(str_trim(str_remove_all(.x, "\\[.*?\\]")))))
  cat("Step 8: mutate OK\n", file=out)
  
}, error = function(e) {
  cat("ERROR:", conditionMessage(e), "\n", file=out)
})

close(out)
