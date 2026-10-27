out <- file("/Users/pietwyffels/Documents/R/Mindray_ExporteR_Github/out_plot.txt", "w")

tryCatch({
  library(readr); library(janitor); library(dplyr); library(tidyr)
  library(stringr); library(plotly)

  path <- '/Volumes/PHILIPS UFD/_810804 046A39_8261161810_C400ADD6-D515-3809-1215-16301400A35A/ParameterData/ParameterData-1Min-20260919131000~20260920044100.csv'

  wrangle_raw <- function(path) {
    tab <- suppressMessages(readr::read_csv(path, na = c("--"))) |> janitor::clean_names()
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
  cat("wrangle OK\n", file=out)

  hr   <- cleaner1(tab, "hr_bpm")
  nibp <- cleaner1(tab, c("nibp_s_mm_hg","nibp_d_mm_hg","nibp_m_mm_hg"))
  art  <- cleaner1(tab, c("art_s_mm_hg","art_d_mm_hg","art_m_mm_hg"))
  sat  <- cleaner1(tab, "sp_o2_percent")

  cat("HR rows:", nrow(hr), "\n", file=out)
  cat("NIBP rows:", nrow(nibp), "\n", file=out)
  cat("ART rows:", nrow(art), "\n", file=out)
  cat("SAT rows:", nrow(sat), "\n", file=out)

  # Test the add_bp_traces logic for NIBP
  cat("\nTesting NIBP pivot_wider...\n", file=out)
  nibp_wide <- nibp |> pivot_wider(names_from = names, values_from = values)
  cat("nibp_wide cols:", paste(names(nibp_wide), collapse=", "), "\n", file=out)
  cat("nibp_wide rows:", nrow(nibp_wide), "\n", file=out)
  cat("nibp_wide list-cols:", paste(names(nibp_wide)[sapply(nibp_wide, is.list)], collapse=", "), "\n", file=out)

  sys_col  <- names(nibp_wide)[grepl("_s_mm_hg$", names(nibp_wide))]
  dia_col  <- names(nibp_wide)[grepl("_d_mm_hg$", names(nibp_wide))]
  mean_col <- names(nibp_wide)[grepl("_m_mm_hg$", names(nibp_wide))]
  cat("sys_col:", sys_col, "dia_col:", dia_col, "mean_col:", mean_col, "\n", file=out)

  # Test add_segments with NIBP
  cat("\nBuilding p_hemo...\n", file=out)
  p_hemo <- tryCatch({
    p <- plotly::plot_ly()
    # add_bp_traces for nibp
    bp_wide <- nibp_wide
    p <- p |> plotly::add_segments(data = bp_wide,
                   x = ~time, xend = ~time,
                   y = as.formula(paste0("~", dia_col)),
                   yend = as.formula(paste0("~", sys_col)),
                   showlegend = FALSE,
                   line = list(color = "red", width = 1.5))
    cat("NIBP segments OK\n", file=out)
    p
  }, error = function(e) {
    cat("ERROR in NIBP segments:", conditionMessage(e), "\n", file=out)
    NULL
  })

  # Test the SpO2 time_range
  cat("\nSpO2 time_range test...\n", file=out)
  time_range <- range(sat$time, na.rm = TRUE)
  cat("time_range:", format(time_range), "\n", file=out)
  cat("Is finite:", all(is.finite(as.numeric(time_range))), "\n", file=out)

  # Test add_segments with time_range
  cat("\nBuilding p_sat...\n", file=out)
  p_sat <- tryCatch({
    plotly::plot_ly() |>
      plotly::add_segments(x = time_range[1], xend = time_range[2],
                   y = 100, yend = 100, showlegend = FALSE,
                   line = list(color = "grey", dash = "solid"))
  }, error = function(e) {
    cat("ERROR in p_sat segments:", conditionMessage(e), "\n", file=out)
    NULL
  })
  cat("p_sat built OK\n", file=out)

  # Test full subplot
  cat("\nBuilding subplot...\n", file=out)
  
  p_hemo2 <- plotly::plot_ly() |>
    plotly::add_trace(data = hr, x = ~time, y = ~as.numeric(values),
              name = "HR (bpm)", type = "scatter", mode = "lines",
              line = list(color = "darkgreen"), legendgroup = "hemo") |>
    layout(yaxis = list(title = "Pressure (mmHg) / HR (bpm)", range = c(0, 200)))
  
  p_sat2 <- plotly::plot_ly() |>
    plotly::add_trace(data = sat, x = ~time, y = ~as.numeric(values),
              name = "SpO2 (%)", type = "scatter", mode = "lines",
              line = list(color = "darkblue"), legendgroup = "sat") |>
    layout(yaxis = list(title = "SpO2 (%)", range = c(80, 101)))

  result <- tryCatch({
    plotly::subplot(p_hemo2, p_sat2, nrows = 2, shareX = TRUE, titleY = TRUE) |>
      layout(hovermode = "x unified",
             xaxis = list(title = ""),
             xaxis2 = list(title = ""),
             legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.08))
  }, error = function(e) {
    cat("ERROR in subplot:", conditionMessage(e), "\n", file=out)
    NULL
  })
  if (!is.null(result)) cat("subplot OK\n", file=out)

}, error = function(e) {
  cat("TOP-LEVEL ERROR:", conditionMessage(e), "\n", file=out)
})

close(out)
