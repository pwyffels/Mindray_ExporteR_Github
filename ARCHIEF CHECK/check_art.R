out <- file("/Users/pietwyffels/Documents/R/Mindray_ExporteR_Github/out_art.txt", "w")

tryCatch({
  library(readr); library(janitor); library(dplyr); library(tidyr); library(stringr)

  path <- '/Volumes/PHILIPS UFD/_810804 046A39_8261161810_C400ADD6-D515-3809-1215-16301400A35A/ParameterData/ParameterData-1Min-20260919131000~20260920044100.csv'

  tab_raw <- suppressMessages(readr::read_csv(path, na = c("--"))) |> janitor::clean_names()

  # Look at ART columns
  cat("ART columns:\n", file=out)
  art_cols <- names(tab_raw)[grepl("art_", names(tab_raw))]
  cat(paste(art_cols, collapse=", "), "\n", file=out)

  cat("\nAll cleaned names with 'art':\n", file=out)
  print(art_cols, file=out)

  # How many ART columns are there?
  cat("\nnon-NA counts:\n", file=out)
  for (col in art_cols) {
    cat(col, ":", sum(!is.na(tab_raw[[col]])), "\n", file=out)
  }

  # The issue: after select(any_of(cols_of_interest)), art_s_mm_hg etc. should be there
  # But are there multiple art_s/art_d/art_m columns?
  art_s_cols <- names(tab_raw)[grepl("art_s_mm_hg", names(tab_raw))]
  cat("\nart_s_mm_hg variants:", paste(art_s_cols, collapse=", "), "\n", file=out)

  # pivot_longer on all 3 ART cols
  art_data <- tab_raw |> 
    select(time, art_s_mm_hg, art_d_mm_hg, art_m_mm_hg) |>
    tidyr::drop_na() |>
    pivot_longer(cols = -time, names_to = "names", values_to = "values")

  cat("ART long rows:", nrow(art_data), "\n", file=out)
  cat("Unique times in ART:", length(unique(art_data$time)), "\n", file=out)
  cat("Expected rows (times * 3):", length(unique(art_data$time)) * 3, "\n", file=out)

  # Check for duplicate time+name combos
  dups <- art_data |> group_by(time, names) |> filter(n() > 1)
  cat("Duplicate time+name rows:", nrow(dups), "\n", file=out)
  if (nrow(dups) > 0) {
    cat("Sample duplicates:\n", file=out)
    capture.output(print(head(dups, 10))) |> writeLines(out)
  }

  # Test pivot_wider
  art_wide <- tryCatch(
    art_data |> pivot_wider(names_from = names, values_from = values),
    warning = function(w) {
      cat("WARNING in pivot_wider:", conditionMessage(w), "\n", file=out)
      art_data |> pivot_wider(names_from = names, values_from = values)
    }
  )
  cat("art_wide rows:", nrow(art_wide), "\n", file=out)
  cat("art_wide list-cols:", paste(names(art_wide)[sapply(art_wide, is.list)], collapse=", "), "\n", file=out)
  if (any(sapply(art_wide, is.list))) {
    cat("List-column problem! pivot_wider created list columns due to duplicate keys\n", file=out)
  }

  # Now check raw ART data for multiple readings per minute
  art_raw_check <- tab_raw |> select(time, art_s_mm_hg) |> filter(!is.na(art_s_mm_hg))
  dup_times <- art_raw_check |> group_by(time) |> filter(n() > 1)
  cat("\nDuplicate times in raw ART data:", nrow(dup_times), "\n", file=out)

  # Look at ART non-NA count
  cat("\nTotal art_s_mm_hg non-NA rows:", sum(!is.na(tab_raw$art_s_mm_hg)), "\n", file=out)
  
  # Perhaps ART has 882 rows because it was on continuously (like a real arterial line)
  cat("If 882 rows * 3 cols =", 882*3, "vs our 2646 =", 2646, "\n", file=out)

}, error = function(e) {
  cat("ERROR:", conditionMessage(e), "\n", file=out)
})

close(out)
