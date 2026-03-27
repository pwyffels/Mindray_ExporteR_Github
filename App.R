## Mindray ExporteR – Shiny App
## Workflow: upload CSV → wrangle → classify → visualise (plotly) → download

library(shiny)
library(readr)
library(tidyverse)
library(janitor)
library(plotly)
library(openxlsx)

# ── data helpers ──────────────────────────────────────────────────────────────

wrangle_raw <- function(path) {
  tab <- readr::read_csv(path, na = c("--")) |> janitor::clean_names()

  # step 1: select only columns relevant for visualisation (missing ones silently skipped)
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

  # step 2: split sevo and co2 compound columns if present
  if ("sev_et_fi" %in% names(tab))
    tab <- tab |> tidyr::separate_wider_delim(sev_et_fi, delim = "/",
                                        names = c("et_sevo", "fi_sevo"),
                                        too_few = "align_start")
  if ("co2_et_fi" %in% names(tab))
    tab <- tab |> tidyr::separate_wider_delim(co2_et_fi, delim = "/",
                                        names = c("et_co2", "fi_co2"),
                                        too_few = "align_start")

  # step 3: strip unit suffixes (anything in brackets) and convert all to numeric
  tab |> mutate(across(-time, ~ as.numeric(str_trim(str_remove_all(.x, "\\[.*?\\]")))))
}

cleaner1 <- function(tab, subclass) {
  sel <- tab |> select(time, tidyselect::any_of(subclass))
  if (ncol(sel) == 1)
    return(tab |> select(time) |> mutate(names = NA_character_, values = NA_real_))
  sel |> na.omit() |>
    pivot_longer(cols = -time, names_to = "names", values_to = "values")
}

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

# ── plotly builders ───────────────────────────────────────────────────────────

# Tab 1: Hemodynamics (NIBP + ART + HR) and SpO2 – linked x-axis
make_plotly1 <- function(dc) {
  hr   <- dc$HEMO$HR
  nibp <- dc$HEMO$NIBP
  art  <- dc$HEMO$ART
  sat  <- dc$VENTILATION$SAT

  # helper: build vertical-segment + dot traces for a BP source (NIBP or ART)
  add_bp_traces <- function(p, bp_long, label_prefix) {
    # only proceed if there is actual data
    if (nrow(bp_long) == 0 || all(is.na(bp_long$values))) return(p)

    bp_wide <- bp_long |>
      pivot_wider(names_from = names, values_from = values)

    # detect column names flexibly
    sys_col  <- names(bp_wide)[grepl("_s_mm_hg$", names(bp_wide))]
    dia_col  <- names(bp_wide)[grepl("_d_mm_hg$", names(bp_wide))]
    mean_col <- names(bp_wide)[grepl("_m_mm_hg$", names(bp_wide))]

    if (length(sys_col) == 0 || length(dia_col) == 0) return(p)

    p <- p |>
      plotly::add_segments(data = bp_wide,
                   x = ~time, xend = ~time,
                   y = as.formula(paste0("~", dia_col)),
                   yend = as.formula(paste0("~", sys_col)),
                   showlegend = FALSE,
                   line = list(color = "red", width = 1.5))

    p <- p |>
      plotly::add_trace(data = bp_wide, x = ~time,
                y = as.formula(paste0("~", sys_col)),
                name = paste(label_prefix, "systolic"),
                type = "scatter", mode = "markers",
                marker = list(color = "red", size = 7),
                legendgroup = label_prefix)

    if (length(mean_col) > 0)
      p <- p |>
        plotly::add_trace(data = bp_wide, x = ~time,
                  y = as.formula(paste0("~", mean_col)),
                  name = paste(label_prefix, "mean"),
                  type = "scatter", mode = "markers",
                  marker = list(color = "red", size = 7, symbol = "diamond"),
                  legendgroup = label_prefix)

    p <- p |>
      plotly::add_trace(data = bp_wide, x = ~time,
                y = as.formula(paste0("~", dia_col)),
                name = paste(label_prefix, "diastolic"),
                type = "scatter", mode = "markers",
                marker = list(color = "red", size = 7),
                legendgroup = label_prefix)
    p
  }

  # subplot 1: BP (NIBP + ART) + HR
  p_hemo <- plotly::plot_ly() |>
    add_bp_traces(nibp, "NIBP") |>
    add_bp_traces(art,  "ART") |>
    plotly::add_trace(data = hr, x = ~time, y = ~as.numeric(values),
              name = "HR (bpm)", type = "scatter", mode = "lines",
              line = list(color = "darkgreen"), legendgroup = "hemo") |>
    layout(yaxis = list(title = "Pressure (mmHg) / HR (bpm)", range = c(0, 200)))

  # subplot 2: SpO2
  time_range <- range(sat$time, na.rm = TRUE)

  p_sat <- plotly::plot_ly() |>
    plotly::add_segments(x = time_range[1], xend = time_range[2],
                 y = 100, yend = 100, showlegend = FALSE,
                 line = list(color = "grey", dash = "solid")) |>
    plotly::add_segments(x = time_range[1], xend = time_range[2],
                 y = 90, yend = 90, showlegend = FALSE,
                 line = list(color = "grey", dash = "dash")) |>
    plotly::add_trace(data = sat, x = ~time, y = ~as.numeric(values),
              name = "SpO2 (%)", type = "scatter", mode = "lines",
              line = list(color = "darkblue"), legendgroup = "sat") |>
    layout(yaxis = list(title = "SpO2 (%)", range = c(80, 101)))

  plotly::subplot(p_hemo, p_sat, nrows = 2, shareX = TRUE, titleY = TRUE) |>
    layout(hovermode = "x unified",
           xaxis = list(title = ""),
           xaxis2 = list(title = ""),
           legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.08))
}

# Tab 2: Gas analysis – O2, CO2, Sevo – linked x-axis
make_plotly2 <- function(dc) {
  time_range <- range(dc$GAS$O2$time, na.rm = TRUE)

  p_o2 <- plotly::plot_ly() |>
    plotly::add_trace(data = dc$GAS$O2, x = ~time, y = ~as.numeric(values),
              color = ~names, type = "scatter", mode = "lines",
              colors = c(fi_o2_percent = "darkgreen", et_o2_percent = "grey"),
              legendgroup = "o2") |>
    plotly::add_segments(x = time_range[1], xend = time_range[2],
                 y = 100, yend = 100, showlegend = FALSE,
                 line = list(color = "lightgrey")) |>
    plotly::add_segments(x = time_range[1], xend = time_range[2],
                 y = 21, yend = 21, showlegend = FALSE,
                 line = list(color = "red", dash = "dash")) |>
    layout(yaxis = list(title = "O2 (%)"))

  p_co2 <- plotly::plot_ly() |>
    plotly::add_trace(data = dc$GAS$CO2, x = ~time, y = ~as.numeric(values),
              color = ~names, type = "scatter", mode = "lines",
              colors = c(et_co2 = "darkgreen", fi_co2 = "grey"),
              legendgroup = "co2") |>
    layout(yaxis = list(title = "CO2 (mmHg)"))

  p_sevo <- plotly::plot_ly() |>
    plotly::add_trace(data = dc$GAS$SEVO, x = ~time, y = ~as.numeric(values),
              color = ~names, type = "scatter", mode = "lines",
              colors = c(et_sevo = "goldenrod", fi_sevo = "grey"),
              legendgroup = "sevo") |>
    layout(yaxis = list(title = "Sevoflurane (mmHg)"))

  plotly::subplot(p_o2, p_co2, p_sevo, nrows = 3, shareX = TRUE, titleY = TRUE) |>
    layout(hovermode = "x unified",
           xaxis = list(title = ""),
           xaxis2 = list(title = ""),
           xaxis3 = list(title = ""),
           legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.05))
}

# ── Excel export ──────────────────────────────────────────────────────────────

to_wide <- function(df) {
  if (all(is.na(df$names))) return(df |> select(time))
  df |> pivot_wider(names_from = names, values_from = values)
}

build_excel <- function(dc, path) {
  wb <- openxlsx::createWorkbook()
  sheets <- list(
    "HEMO_HR"          = dc$HEMO$HR,
    "HEMO_NIBP"        = dc$HEMO$NIBP,
    "HEMO_ART"         = dc$HEMO$ART,
    "HEMO_ECG"         = dc$HEMO$ECG,
    "VENT_SAT"         = dc$VENTILATION$SAT,
    "VENT_VENTILATION" = dc$VENTILATION$VENTILATION,
    "GAS_O2"           = dc$GAS$O2,
    "GAS_CO2"          = dc$GAS$CO2,
    "GAS_SEVO"         = dc$GAS$SEVO
  )
  for (nm in names(sheets)) {
    openxlsx::addWorksheet(wb, nm)
    openxlsx::writeData(wb, nm, to_wide(sheets[[nm]]))
  }
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
}

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- shiny::fluidPage(
  titlePanel("Mindray ExporteR"),

  sidebarLayout(
    sidebarPanel(
      fileInput("csv_file", "Upload Mindray CSV", accept = c("text/csv", ".csv")),
      hr(),
      h5("Downloads"),
      downloadButton("dl_plot1", "Plot 1 (HTML)"),
      br(), br(),
      downloadButton("dl_plot2", "Plot 2 (HTML)"),
      br(), br(),
      downloadButton("dl_excel", "Data (Excel)"),
      hr(),
      p(style = "font-size:11px; color:#555; text-align:center;",
        "Built by Piet Wyffels",
        br(),
        "Department of Anesthesia, Perioperative Medicine and Pain Clinic",
        br(),
        "University Hospital Ghent, Belgium"
      ),
      img(src = "logo.png", width = "100%", style = "margin-top:10px;")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Hemodynamics / SpO2", plotlyOutput("plot1", height = "600px")),
        tabPanel("Gas analysis",        plotlyOutput("plot2", height = "700px"))
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  data_clean <- shiny::reactive({
    req(input$csv_file)
    struct_data(wrangle_raw(input$csv_file$datapath))
  })

  output$plot1 <- plotly::renderPlotly({ req(data_clean()); make_plotly1(data_clean()) })
  output$plot2 <- plotly::renderPlotly({ req(data_clean()); make_plotly2(data_clean()) })

  # save interactive plots as self-contained HTML
  output$dl_plot1 <- shiny::downloadHandler(
    filename = "plot1_hemodynamics.html",
    content  = function(f) htmlwidgets::saveWidget(make_plotly1(data_clean()), f)
  )
  output$dl_plot2 <- shiny::downloadHandler(
    filename = "plot2_gas.html",
    content  = function(f) htmlwidgets::saveWidget(make_plotly2(data_clean()), f)
  )

  output$dl_excel <- shiny::downloadHandler(
    filename = function() paste0("mindray_data_", Sys.Date(), ".xlsx"),
    content  = function(f) build_excel(data_clean(), f)
  )
}

shiny::shinyApp(ui, server)
