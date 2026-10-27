library(readr); library(janitor); library(dplyr); library(tidyr); library(stringr)

path <- '/Volumes/PHILIPS UFD/_810804 046A39_8261161810_C400ADD6-D515-3809-1215-16301400A35A/ParameterData/ParameterData-1Min-20260919131000~20260920044100.csv'

tab <- readr::read_csv(path, na = c("--"), show_col_types = FALSE) |> janitor::clean_names()

cols_of_interest <- c("time","hr_bpm","sp_o2_percent",
  "nibp_s_mm_hg","nibp_d_mm_hg","nibp_m_mm_hg",
  "art_s_mm_hg","art_d_mm_hg","art_m_mm_hg",
  "cvp_mm_hg","ppv_percent","et_o2_percent","fi_o2_percent",
  "rr_rpm","peep","ppeak","pplat","pmean","t_vi","mv","m_vspn","ftotal",
  "pv_cs_min","pauses_min","vp_bs_min","couplets_min",
  "missed_beats_min","r_on_ts_min","qt_ms","q_tc_ms","sev_et_fi","co2_et_fi")

tab <- tab |> dplyr::select(any_of(cols_of_interest))

writeLines(capture.output({
  cat("sev_et_fi class:", class(tab$sev_et_fi), "\n")
  cat("sev_et_fi unique values:\n")
  print(unique(tab$sev_et_fi))
  cat("\nco2_et_fi class:", class(tab$co2_et_fi), "\n")
  cat("co2_et_fi unique values:\n")
  print(unique(tab$co2_et_fi))
}), "/Users/pietwyffels/Documents/R/Mindray_ExporteR_Github/check_output5.txt")

# Now test separate step
result <- tryCatch({
  tab |> tidyr::separate_wider_delim(sev_et_fi, delim="/",
                  names=c("et_sevo","fi_sevo"), too_few="align_start")
}, error = function(e) {
  writeLines(paste("ERROR separate sev:", conditionMessage(e)),
    "/Users/pietwyffels/Documents/R/Mindray_ExporteR_Github/check_output5.txt",
    append=TRUE)
  NULL
}, warning = function(w) {
  writeLines(paste("WARNING separate sev:", conditionMessage(w)),
    "/Users/pietwyffels/Documents/R/Mindray_ExporteR_Github/check_output5.txt",
    append=TRUE)
  withCallingHandlers(
    tab |> tidyr::separate_wider_delim(sev_et_fi, delim="/",
                    names=c("et_sevo","fi_sevo"), too_few="align_start"),
    warning = function(w) invokeRestart("muffleWarning")
  )
})

writeLines(capture.output({
  cat("separate sev result class:", class(result), "\n")
  if (!is.null(result)) {
    cat("columns:", paste(names(result), collapse=", "), "\n")
    cat("et_sevo sample:", head(result$et_sevo, 5), "\n")
  }
}), "/Users/pietwyffels/Documents/R/Mindray_ExporteR_Github/check_output5.txt", append=TRUE)
