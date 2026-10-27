sink("/Users/pietwyffels/Documents/R/Mindray_ExporteR_Github/check_output3.txt")

library(readr)
library(janitor)
library(dplyr)
library(tidyr)
library(stringr)

path <- '/Volumes/PHILIPS UFD/_810804 046A39_8261161810_C400ADD6-D515-3809-1215-16301400A35A/ParameterData/ParameterData-1Min-20260919131000~20260920044100.csv'

tab_raw <- suppressMessages(readr::read_csv(path, na = c("--")))
tab <- tab_raw |> janitor::clean_names()

# Check the +Ppeak column type - it was read as character
cat("+Ppeak col (ppeak) class:", class(tab$ppeak), "\n")
cat("Sample ppeak values (raw):\n")
print(head(tab$ppeak, 10))

# Check what the separate_wider_delim does to sev_et_fi
cat("\nsev_et_fi sample:\n")
print(head(tab$sev_et_fi, 5))

# Now test the separate step
tab2 <- tab |> tidyr::separate_wider_delim(sev_et_fi, delim = "/",
                              names = c("et_sevo", "fi_sevo"),
                              too_few = "align_start")
cat("\nAfter separate, et_sevo sample:\n")
print(head(tab2$et_sevo, 5))

# Now check NIBP pivot_wider - do we get duplicate time rows?
nibp_long <- tab |> select(time, nibp_s_mm_hg, nibp_d_mm_hg, nibp_m_mm_hg) |>
  tidyr::drop_na() |>
  pivot_longer(cols = -time, names_to = "names", values_to = "values")

cat("\nNIBP long rows:", nrow(nibp_long), "\n")
cat("Duplicate times in NIBP?\n")
dups <- nibp_long |> group_by(time, names) |> filter(n() > 1)
cat("Duplicate rows:", nrow(dups), "\n")

cat("\nNIBP pivot_wider test:\n")
nibp_wide <- tryCatch(
  nibp_long |> pivot_wider(names_from = names, values_from = values),
  error   = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NULL },
  warning = function(w) { cat("WARNING:", conditionMessage(w), "\n"); 
    nibp_long |> pivot_wider(names_from = names, values_from = values, values_fn = first) }
)
if (!is.null(nibp_wide)) {
  cat("nibp_wide rows:", nrow(nibp_wide), "\n")
  print(head(nibp_wide))
}

sink()
cat("Done\n")
