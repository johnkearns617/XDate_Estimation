library(shiny)
library(plotly)
library(zoo)
library(tidyverse)
library(funspotr)
library(data.table)
library(bslib)
library(shinycssloaders)
library(shinyWidgets)
library(htmltools)
library(reactable)

conflicted::conflicts_prefer(dplyr::filter)
conflicted::conflicts_prefer(lubridate::year)
conflicted::conflicts_prefer(lubridate::month)
conflicted::conflicts_prefer(dplyr::first)
conflicted::conflicts_prefer(plotly::layout)

options(scipen = 999)

# ── Data loading ──────────────────────────────────────────────────────────────
load(url("https://github.com/johnkearns617/XDate_Estimation/raw/refs/heads/main/Data/Processing/image_saves/chart_data.RData"))

charts         <- list()
monthly_charts <- list()
for (dat in tail(
  list_files_github_repo(
    "johnkearns617/XDate_Estimation",
    branch  = NULL,
    pattern = stringr::regex("(rdata)$", ignore_case = TRUE)
  ) %>%
  select(absolute_paths) %>%
  filter(grepl("image_saves", absolute_paths) & grepl("data_asof_", absolute_paths)) %>%
  pull(absolute_paths),
  30
)) {
  if (substr(dat, 109, 118) < "2026-01-01") next
  load(url(dat))
  res <- str_match(
    dat,
    "https://raw.githubusercontent.com/johnkearns617/XDate_Estimation/main/Data/Processing/image_saves/data_asof_\\s*(.*?)\\s*.RData"
  )[, 2]
  charts[[dat]] <- my_chart %>% mutate(date_run = res)
  if (exists("monthly_chart_df"))
    monthly_charts[[dat]] <- monthly_chart_df %>% mutate(date_run = res)
}
charts                <- data.table::rbindlist(charts)
monthly_charts_all    <- if (length(monthly_charts) > 0)
  data.table::rbindlist(monthly_charts, fill = TRUE) else NULL

# ── Derived KPIs ──────────────────────────────────────────────────────────────
est_xdate     <- my_chart %>% filter(record_date >= dat_value & running_bal <= 0)     %>% slice(1) %>% pull(record_date)
early_xdate   <- my_chart %>% filter(record_date >= dat_value & running_bal_lower <= 0) %>% slice(1) %>% pull(record_date)
current_space <- my_chart %>% filter(record_date == min(record_date[record_date >= dat_value])) %>% pull(running_bal) %>% first()
days_left     <- as.integer(as.Date(est_xdate) - Sys.Date())

# ── Fiscal year boundaries (Oct–Sep) ─────────────────────────────────────────
fy_start <- if(lubridate::month(dat_value) >= 10){
  as.Date(paste0(lubridate::year(dat_value),   "-10-01"))} else{as.Date(paste0(lubridate::year(dat_value) - 1, "-10-01"))}
fy_end <- fy_start %m+% years(1) %m-% days(1)
curr_ym <- as.yearmon(as.Date(dat_value))

# ── Pre-compute comparison chart datasets ─────────────────────────────────────

# FY month labels (Oct = 1 … Sep = 12) — used by both server functions
fy_month_labels <- c("Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep")

# Daily same-month comparison: tag every row in daily_chart_df
daily_comp_base <- daily_chart_df %>%
  mutate(
    cal_month    = lubridate::month(record_date),
    cal_year     = lubridate::year(record_date),
    dom          = lubridate::day(record_date),
    is_curr      = cal_year == lubridate::year(as.Date(dat_value)),
    is_forecast  = record_date >= as.Date(dat_value)
  )

# FY monthly comparison: tag every row in monthly_chart_df
monthly_comp_base <- monthly_chart_df %>%
  mutate(
    cal_month    = lubridate::month(record_date),
    cal_year     = lubridate::year(record_date),
    fy           = ifelse(cal_month >= 10, cal_year + 1, cal_year),
    fy_month     = ifelse(cal_month >= 10, cal_month - 9, cal_month + 3),
    curr_fy_year = lubridate::year(fy_end),
    is_curr_fy   = fy == lubridate::year(fy_end),
    is_forecast  = record_date >= as.Date(dat_value)
  )

# ── Forecast revision table prep ──────────────────────────────────────────────
revision_tbl <- NULL
if (!is.null(monthly_charts_all) && nrow(monthly_charts_all) > 0) {
  
  run_dates <- sort(unique(as.Date(monthly_charts_all$date_run)), decreasing = TRUE)
  today_run <- run_dates[1]
  yest_run  <- if (length(run_dates) >= 2) run_dates[2] else run_dates[1]
  ago30_run <- min(run_dates)
  
  get_totals <- function(rd) {
    monthly_charts_all %>%
      as_tibble() %>%
      dplyr::filter(as.Date(date_run) == rd) %>%
      mutate(
        in_curr_month = as.yearmon(record_date) == curr_ym,
        in_curr_fy    = record_date >= fy_start & record_date <= fy_end
      ) %>%
      group_by(cbo_category) %>%
      summarise(
        curr_month = sum(scaled_monthly[in_curr_month], na.rm = TRUE),
        curr_fy    = sum(scaled_monthly[in_curr_fy],    na.rm = TRUE),
        .groups    = "drop"
      )
  }
  
  t_today <- get_totals(today_run)
  t_yest  <- get_totals(yest_run)
  t_30d   <- get_totals(ago30_run)
  
  revision_tbl <- t_today %>%
    left_join(t_yest,  by = "cbo_category", suffix = c("", "_yest")) %>%
    left_join(t_30d,   by = "cbo_category", suffix = c("", "_30d")) %>%
    rename(
      month_today = curr_month,  fy_today = curr_fy,
      month_yest  = curr_month_yest, fy_yest = curr_fy_yest,
      month_30d   = curr_month_30d,  fy_30d  = curr_fy_30d
    ) %>%
    mutate(
      delta_month_1d  = round(month_today - month_yest, 2),
      delta_month_30d = round(month_today - month_30d,  2),
      delta_fy_1d     = round(fy_today    - fy_yest,    2),
      delta_fy_30d    = round(fy_today    - fy_30d,     2),
      flow_type       = ifelse(fy_today >= 0, "Receipt", "Outlay"),
      run_today       = format(today_run, "%b %d"),
      run_yest        = format(yest_run,  "%b %d"),
      run_30d         = format(ago30_run, "%b %d")
    ) %>%
    arrange(flow_type, desc(abs(delta_fy_30d)))
}

# ── Theme ─────────────────────────────────────────────────────────────────────
xdate_theme <- bs_theme(
  version       = 5,
  bg            = "#f7f4f0",
  fg            = "#1a1a2e",
  primary       = "#c0392b",
  secondary     = "#d6cfc7",
  success       = "#27ae60",
  danger        = "#c0392b",
  info          = "#2471a3",
  base_font     = font_google("IBM Plex Mono"),
  heading_font  = font_google("IBM Plex Sans", wght = "700"),
  code_font     = font_google("IBM Plex Mono"),
  font_scale    = 0.9,
  `border-radius` = "4px"
) %>%
  bs_add_rules("
    /* ── Global ── */
    body { background:#f7f4f0; }
    .bslib-card { background:#ffffff; border:1px solid #e2dbd3; border-radius:6px;
                  box-shadow:0 1px 4px rgba(0,0,0,.07); }
    .bslib-card .card-header { background:#faf8f5; border-bottom:1px solid #e2dbd3;
                                font-family:'IBM Plex Sans',sans-serif; font-weight:700;
                                letter-spacing:.04em; font-size:.78rem; text-transform:uppercase;
                                color:#7a6f66; padding:.6rem 1rem; }

    /* ── Title bar ── */
    .app-title-bar { background:#ffffff; border-bottom:3px solid #c0392b;
                     padding:1rem 1.5rem; display:flex; align-items:center; gap:1rem;
                     margin-bottom:1.25rem; box-shadow:0 1px 6px rgba(0,0,0,.08); }
    .app-title-bar h3 { margin:0; font-family:'IBM Plex Sans',sans-serif; font-weight:700;
                        font-size:1.3rem; color:#1a1a2e; letter-spacing:.04em; }
    .app-title-bar .subtitle { color:#7a6f66; font-size:.78rem; margin:0;
                                font-family:'IBM Plex Mono',monospace; }
    .data-badge { background:#faf8f5; border:1px solid #e2dbd3; border-radius:4px;
                  padding:.2rem .65rem; font-size:.7rem; color:#2471a3;
                  font-family:'IBM Plex Mono',monospace; white-space:nowrap; }

    /* ── KPI cards ── */
    .kpi-row { display:flex; gap:1rem; margin-bottom:1.25rem; flex-wrap:wrap; }
    .kpi-card { flex:1; min-width:160px; background:#ffffff; border:1px solid #e2dbd3;
                border-radius:6px; padding:.85rem 1.1rem; position:relative;
                overflow:hidden; transition:border-color .2s, box-shadow .2s;
                box-shadow:0 1px 4px rgba(0,0,0,.06); }
    .kpi-card:hover { border-color:#c0392b; box-shadow:0 3px 10px rgba(0,0,0,.1); }
    .kpi-card::before { content:''; position:absolute; left:0; top:0; bottom:0;
                        width:3px; border-radius:3px 0 0 3px; }
    .kpi-card.amber::before { background:#e67e22; }
    .kpi-card.red::before   { background:#c0392b; }
    .kpi-card.green::before { background:#27ae60; }
    .kpi-card.blue::before  { background:#2471a3; }
    .kpi-label  { font-size:.68rem; color:#7a6f66; letter-spacing:.08em;
                  text-transform:uppercase; margin-bottom:.35rem;
                  font-family:'IBM Plex Mono',monospace; }
    .kpi-value  { font-size:1.55rem; font-weight:700; line-height:1;
                  font-family:'IBM Plex Mono',monospace; }
    .kpi-card.amber .kpi-value { color:#e67e22; }
    .kpi-card.red   .kpi-value { color:#c0392b; }
    .kpi-card.green .kpi-value { color:#27ae60; }
    .kpi-card.blue  .kpi-value { color:#2471a3; }
    .kpi-sub { font-size:.68rem; color:#7a6f66; margin-top:.3rem;
               font-family:'IBM Plex Mono',monospace; }

    /* ── Sidebar ── */
    .sidebar-panel { background:#ffffff !important; border:1px solid #e2dbd3 !important;
                     border-radius:6px; padding:1rem;
                     box-shadow:0 1px 4px rgba(0,0,0,.06); }
    .sidebar-section { margin-bottom:1.2rem; }
    .sidebar-section h6 { font-size:.7rem; color:#7a6f66; text-transform:uppercase;
                          letter-spacing:.08em; margin-bottom:.6rem;
                          font-family:'IBM Plex Sans',sans-serif; font-weight:700; }
    .shiny-input-container label { color:#3d3530; font-size:.78rem; }

    /* ── Tabs ── */
    .nav-tabs { border-bottom:2px solid #e2dbd3; }
    .nav-tabs .nav-link { color:#7a6f66; border:none; border-bottom:2px solid transparent;
                          font-size:.8rem; letter-spacing:.04em;
                          font-family:'IBM Plex Sans',sans-serif; transition:.15s; }
    .nav-tabs .nav-link:hover { color:#1a1a2e; border-bottom-color:#d6cfc7; }
    .nav-tabs .nav-link.active { color:#c0392b; background:transparent;
                                 border-bottom:2px solid #c0392b; font-weight:700; }

    /* ── Annotation box ── */
    .annotation-box { background:#fdf6ec; border-left:3px solid #e67e22;
                      border-radius:0 4px 4px 0; padding:.65rem 1rem;
                      font-size:.78rem; color:#3d3530; margin-bottom:1rem;
                      font-family:'IBM Plex Mono',monospace; }
    .annotation-box strong { color:#c0392b; }

    /* ── Download btn ── */
    .dl-btn { font-size:.72rem; padding:.25rem .65rem; border-radius:4px;
              border:1px solid #e2dbd3; background:#faf8f5; color:#7a6f66;
              cursor:pointer; transition:.15s; }
    .dl-btn:hover { border-color:#c0392b; color:#c0392b; background:#fff5f5; }

    /* ── Footer ── */
    .app-footer { border-top:1px solid #e2dbd3; padding:.75rem 1.5rem;
                  font-size:.68rem; color:#a89e96; font-family:'IBM Plex Mono',monospace;
                  display:flex; justify-content:space-between; margin-top:1.5rem;
                  background:#ffffff; }
                  
    .plotly.html-widget, .js-plotly-plot, .plot-container { width:100% !important; }
    .shiny-plot-output, .plotly-graph-div { width:100% !important; }

    /* ── Revision table ── */
    .rev-table-wrap { overflow-x:auto; }
    .rev-tbl-header { font-family:'IBM Plex Sans',sans-serif; font-weight:700;
                      font-size:.68rem; text-transform:uppercase; letter-spacing:.06em;
                      color:#7a6f66; padding:.4rem .6rem; border-bottom:2px solid #e2dbd3; }
    .rev-tbl-group-header { font-family:'IBM Plex Sans',sans-serif; font-weight:700;
                             font-size:.65rem; text-transform:uppercase; letter-spacing:.06em;
                             color:#a89e96; text-align:center; padding:.25rem;
                             background:#faf8f5; border-bottom:1px solid #e2dbd3; }
    .reactable { font-family:'IBM Plex Mono',monospace; font-size:.78rem; }
    .reactable .rt-thead { background:#faf8f5; }
    .reactable .rt-th { border-right:1px solid #f0ebe5; }
    .reactable .rt-td { border-right:1px solid #f7f4f0; padding:.4rem .65rem;
                        vertical-align:middle; }
    .reactable .rt-tr:hover .rt-td { background:#fdf8f5 !important; }
    .reactable .rt-tr-group { border-bottom:1px solid #f0ebe5; }
    .delta-pos { color:#1a7f37; font-weight:600; }
    .delta-neg { color:#c0392b; font-weight:600; }
    .delta-zero{ color:#a89e96; }
    .rt-section-divider td { background:#faf8f5 !important; font-weight:700;
                              font-size:.7rem; text-transform:uppercase;
                              letter-spacing:.06em; color:#7a6f66;
                              padding:.3rem .65rem; border-top:2px solid #e2dbd3; }

    /* ── Historical comparison section ── */
    .comp-section-header {
      display: flex;
      justify-content: space-between;
      align-items: center;
      padding: .55rem 1rem;
      background: #faf8f5;
      border: 1px solid #e2dbd3;
      border-radius: 6px 6px 0 0;
      border-bottom: 2px solid #c0392b;
      margin-bottom: 0;
    }
    .comp-section-title {
      font-family: 'IBM Plex Sans', sans-serif;
      font-weight: 700;
      font-size: .78rem;
      text-transform: uppercase;
      letter-spacing: .06em;
      color: #1a1a2e;
    }
    .comp-section-subtitle {
      font-family: 'IBM Plex Mono', monospace;
      font-size: .68rem;
      color: #7a6f66;
      margin-top: .15rem;
    }
    /* Toggle pill styling */
    .btn-group-sm > .btn,
    .btn-sm {
      font-family: 'IBM Plex Mono', monospace !important;
      font-size: .7rem !important;
      border-radius: 3px !important;
    }
    /* Legend annotation for comparison charts */
    .comp-legend {
      display: flex;
      gap: 1.2rem;
      padding: .35rem 1rem .3rem;
      background: #fdfcfb;
      border-bottom: 1px solid #f0ebe5;
      font-family: 'IBM Plex Mono', monospace;
      font-size: .67rem;
      color: #7a6f66;
    }
    .comp-legend-item { display: flex; align-items: center; gap: .35rem; }
    .comp-legend-line {
      width: 22px; height: 2px;
      display: inline-block; border-radius: 2px;
    }
    .comp-legend-line.solid   { background: #c0392b; }
    .comp-legend-line.dashed  { background: transparent;
                                 border-top: 2px dashed #c0392b; }
    .comp-legend-line.prior   { background: #b8aaa0; }
  ")

# ── Helpers ───────────────────────────────────────────────────────────────────
kpi_card <- function(label, value, sub = NULL, color = "amber") {
  div(
    class = paste("kpi-card", color),
    div(class = "kpi-label",  label),
    div(class = "kpi-value",  value),
    if (!is.null(sub)) div(class = "kpi-sub", sub)
  )
}

chart_card <- function(title, ..., dl_id = NULL) {
  header <- div(
    style = "display:flex;justify-content:space-between;align-items:center;
             background:#faf8f5;border-bottom:1px solid #e2dbd3;
             padding:.6rem 1rem;font-family:'IBM Plex Sans',sans-serif;
             font-weight:700;letter-spacing:.04em;font-size:.78rem;
             text-transform:uppercase;color:#7a6f66;",
    span(title),
    if (!is.null(dl_id))
      downloadButton(dl_id, label = icon("download"), class = "dl-btn")
  )
  div(
    style = "background:#ffffff;border:1px solid #e2dbd3;border-radius:6px;
             box-shadow:0 1px 4px rgba(0,0,0,.07);overflow:hidden;
             margin-bottom:0;",
    header,
    div(style = "padding:0;margin:0;line-height:0;", ...)
  )
}

# ── Helper: shared ggplot theme for comparison charts ─────────────────────────
comp_theme <- function() {
  theme_void() +
    theme(
      plot.background  = element_blank(),
      panel.background = element_blank(),
      panel.grid.major = element_line(color = "#ede8e3", linewidth = 0.25),
      axis.text        = element_text(color = "#7a6f66", size = 7.5,
                                      family = "IBM Plex Mono"),
      axis.title       = element_text(color = "#7a6f66", size = 8,
                                      family = "IBM Plex Mono"),
      axis.title.y     = element_text(angle = 90, margin = margin(r = 6)),
      axis.title.x     = element_text(margin = margin(t = 4)),
      legend.text      = element_text(color = "#3d3530", size = 7,
                                      family = "IBM Plex Mono"),
      legend.key.size  = unit(.4, "cm"),
      plot.title       = element_text(color = "#1a1a2e", size = 8.5,
                                      family = "IBM Plex Sans", face = "bold",
                                      margin = margin(b = 4))
    )
}

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  theme = xdate_theme,
  tags$head(tags$style(HTML("
    .plotly .modebar { background:transparent!important; }
    .plotly .modebar-btn path { fill:#a89e96!important; }
    .js-plotly-plot, .plotly, .plotly > div { width:100% !important; }
    .shiny-plot-output, .plotly-graph-div { width: 100% !important;
      display: block !important;
      margin: 0 !important;
      padding: 0 !important;}
    .card-body { width:100%; 
                 padding: 0 !important;
                 margin: 0 !important;
                 }
  "))),
  
  # Title bar
  div(
    class = "app-title-bar",
    div(
      h3("X-DATE ESTIMATION DASHBOARD"),
      p(class = "subtitle", "U.S. Debt Ceiling — Fiscal Space & Extraordinary Measures Monitor")
    ),
    div(style = "margin-left:auto;",
        span(class = "data-badge", paste0("DATA AS OF  ", dat_value)))
  ),
  
  # KPI row
  div(
    class = "kpi-row",
    kpi_card("Estimated X-Date",   format(as.Date(est_xdate), "%b %d, %Y"),
             paste0(days_left, " days from today"), "amber"),
    kpi_card("Early-Bound X-Date", format(as.Date(early_xdate), "%b %d, %Y"),
             "Lower confidence bound", "red"),
    kpi_card("Fiscal Space",
             paste0("$", formatC(round(current_space, 0), format = "d", big.mark = ","), "B"),
             "Current running balance", "green"),
    kpi_card("Extr. Measures Date",
             format(as.Date(exmeasures_date), "%b %d, %Y"),
             "Debt ceiling hit estimate", "blue")
  ),
  
  # Main layout
  fluidRow(
    # Sidebar
    column(
      width = 2,
      div(
        class = "sidebar-panel",
        
        div(class = "sidebar-section",
            h6("Display Options"),
            materialSwitch("show_ribbon", "Show Confidence Band",
                           value = TRUE, status = "warning")
        ),
        
        div(class = "sidebar-section",
            h6("Forecast Horizon"),
            sliderInput("horizon_yrs", label = NULL,
                        min = 1, max = 10, value = 5, step = 1,
                        post = " yr")
        ),
        
        hr(style = "border-color:#e2dbd3;margin:.8rem 0;"),
        
        div(class = "sidebar-section",
            h6("About"),
            p(style = "font-size:.72rem;color:#8b949e;line-height:1.5;",
              "Model ingests daily Treasury statements to project fiscal space
               under extraordinary measures. Bounds reflect ±1σ forecast uncertainty."),
            tags$a(
              href   = "https://github.com/johnkearns617/XDate_Estimation",
              target = "_blank",
              style  = "font-size:.72rem;color:#58a6ff;",
              icon("github"), " Source code"
            )
        )
      )
    ),
    
    # Main panel
    column(
      width = 10,
      navset_tab(
        # ── Tab 1: X-Date ──────────────────────────────────────────────────
        nav_panel(
          "X-Date Outlook",
          br(),
          div(
            class = "annotation-box",
            uiOutput("xdate_text")
          ),
          chart_card(
            "Fiscal Space Remaining — Forecast",
            dl_id = "dl_xdate",
            plotlyOutput("xdate_chart",width="100%"),
            type = 8, color = "#c0392b", size = 0.6
          ),
          br(),
          chart_card(
            "Historical Model Runs — Revision History",
            dl_id = "dl_hist",
            withSpinner(
              plotlyOutput("historical_chart", height = "300px"),
              type = 8, color = "#c0392b", size = 0.6
            )
          )
        ),
        
        # ── Tab 2: Deficits ────────────────────────────────────────────────
        nav_panel(
          "Government Deficits",
          br(),
          p(style = "font-size:.75rem;color:#8b949e;margin-bottom:.75rem;",
            icon("hand-pointer"), "  Click a fiscal year bar to drill into monthly detail,
             then click a month bar to see daily breakdown."),
          fluidRow(
            column(12,
                   chart_card(
                     "Annual Outlays & Receipts by CBO Category",
                     dl_id = "dl_yearly",
                     withSpinner(
                       plotlyOutput("yearly_chart", height = "300px"),
                       type = 8, color = "black", size = 0.6
                     )
                   )
            )
          ),
          br(),
          fluidRow(
            column(6,
                   chart_card(
                     "Monthly Detail — Click a Year Above",
                     withSpinner(
                       plotlyOutput("monthly_chart", height = "280px"),
                       type = 8, color = "black", size = 0.6
                     )
                   )
            ),
            column(6,
                   chart_card(
                     "Daily Detail — Click a Month Above",
                     withSpinner(
                       plotlyOutput("daily_chart", height = "280px"),
                       type = 8, color = "black", size = 0.6
                     )
                   )
            )
          ),
          
          # ── Historical comparison section ─────────────────────────────────
          br(),
          # Section header with shared toggle
          div(
            class = "comp-section-header",
            div(
              div(class = "comp-section-title",
                  icon("clock-rotate-left"), "  Historical Year-over-Year Comparisons"),
              div(class = "comp-section-subtitle",
                  paste0("Current period vs prior years · Solid = Actual · Dashed = Forecast · Data as of ", dat_value))
            ),
            div(
              style = "display:flex; align-items:center; gap:.5rem;",
              span(style = "font-family:'IBM Plex Mono',monospace; font-size:.7rem; color:#7a6f66;",
                   "View:"),
              radioGroupButtons(
                inputId  = "comp_view",
                label    = NULL,
                choices  = c("Total Deficit" = "total",
                             "Medicare" = "Medicare",
                             "Medicaid"="Medicaid",
                             "Social Security"="Social Security",
                             "Other Spending"="Other Spending",
                             "National Defense"="National Defense",
                             "Net Interest"="Net Interest",
                             "Miscellaneous Receipts"="Miscellaneous Receipts",
                             "Corporate Income Taxes"="Corporate Income Taxes",
                             "Payroll Taxes"="Payroll Taxes",
                             "Individual Income Taxes"="Individual Income Taxes",
                             "Excise Taxes"="Excise Taxes",
                             "Estate and Gift Taxes",
                             "Customs Duties"="Customs Duties"),
                selected = "total",
                status   = "outline-secondary",
                size     = "sm",
                justified = FALSE
              )
            )
          ),
          # Shared legend strip
          div(
            class = "comp-legend",
            div(class = "comp-legend-item",
                span(class = "comp-legend-line solid"),
                "Current period — Actual"),
            div(class = "comp-legend-item",
                span(class = "comp-legend-line dashed"),
                "Current period — Forecast"),
            div(class = "comp-legend-item",
                span(class = "comp-legend-line prior"),
                "Prior years (net flow)")
          ),
          # Two side-by-side comparison charts
          fluidRow(
            style = "margin-top:0;",
            column(
              6,
              div(
                style = "background:#ffffff; border:1px solid #e2dbd3;
                         border-top:none; border-radius:0 0 0 6px;
                         box-shadow:0 1px 4px rgba(0,0,0,.07); overflow:hidden;",
                div(
                  style = "background:#faf8f5; border-bottom:1px solid #e2dbd3;
                           padding:.45rem 1rem; font-family:'IBM Plex Sans',sans-serif;
                           font-weight:700; font-size:.73rem; text-transform:uppercase;
                           letter-spacing:.05em; color:#7a6f66;
                           display:flex; justify-content:space-between; align-items:center;",
                  span(paste0("Daily Flows — ",
                              format(as.Date(dat_value), "%B"),
                              " vs Same Month, Prior Years")),
                  span(style = "font-size:.65rem; color:#a89e96; font-weight:400;",
                       "x-axis: day of month")
                ),
                withSpinner(
                  plotlyOutput("daily_comp_chart", height = "300px"),
                  type = 8, color = "#c0392b", size = 0.5
                )
              )
            ),
            column(
              6,
              div(
                style = "background:#ffffff; border:1px solid #e2dbd3;
                         border-top:none; border-radius:0 0 6px 0;
                         box-shadow:0 1px 4px rgba(0,0,0,.07); overflow:hidden;",
                div(
                  style = "background:#faf8f5; border-bottom:1px solid #e2dbd3;
                           padding:.45rem 1rem; font-family:'IBM Plex Sans',sans-serif;
                           font-weight:700; font-size:.73rem; text-transform:uppercase;
                           letter-spacing:.05em; color:#7a6f66;
                           display:flex; justify-content:space-between; align-items:center;",
                  span(paste0("Monthly Flows — FY",
                              lubridate::year(fy_end),
                              " vs Prior Fiscal Years")),
                  span(style = "font-size:.65rem; color:#a89e96; font-weight:400;",
                       "x-axis: fiscal year month (Oct → Sep)")
                ),
                withSpinner(
                  plotlyOutput("fy_comp_chart", height = "300px"),
                  type = 8, color = "#c0392b", size = 0.5
                )
              )
            )
          ),
          br(),
          # ── Revision table ──────────────────────────────────────────────
          card(
            card_header(
              div(
                style = "display:flex;justify-content:space-between;align-items:center;",
                span("Forecast Revision Attribution — What's Moving the X-Date?"),
                div(
                  style = "display:flex;gap:.5rem;align-items:center;",
                  uiOutput("revision_run_labels"),
                  downloadButton("dl_revision", label = icon("download"), class = "dl-btn")
                )
              )
            ),
            card_body(
              padding = 0,
              div(
                style = "padding:.6rem 1rem .4rem; font-size:.74rem; color:#7a6f66;
                         font-family:'IBM Plex Mono',monospace; border-bottom:1px solid #e2dbd3;
                         background:#fdf8f5;",
                icon("circle-info"), " ",
                "Positive values = net inflow (receipts) or reduced outlay. Negative = net outflow (outlays) or reduced receipt. ",
                tags$strong("Δ vs Yest"), " compares today's run to yesterday's. ",
                tags$strong("Δ vs 30d"), " compares today to the run ~30 days ago."
              ),
              withSpinner(
                uiOutput("revision_table_ui"),
                type = 8, color = "#c0392b", size = 0.5
              )
            )
          )
        )
      )
    )
  ),
  
  # Footer
  div(
    class = "app-footer",
    span("Source: U.S. Treasury Daily Statements · github.com/johnkearns617/XDate_Estimation"),
    span(paste0("Updated ", dat_value, " · Model estimates only, not financial advice"))
  )
)

# ── Server ────────────────────────────────────────────────────────────────────

light_layout <- function(p) {
  p %>% layout(
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor  = "rgba(0,0,0,0)",
    font  = list(family = "IBM Plex Mono", color = "#3d3530", size = 11),
    xaxis = list(gridcolor = "#ede8e3", zerolinecolor = "#d6cfc7",
                 tickfont  = list(color = "#7a6f66")),
    yaxis = list(gridcolor = "#ede8e3", zerolinecolor = "#d6cfc7",
                 tickfont  = list(color = "#7a6f66")),
    legend = list(bgcolor = "rgba(0,0,0,0)", font = list(color = "#3d3530")),
    hoverlabel = list(
      bgcolor     = "#ffffff",
      bordercolor = "#c0392b",
      font = list(family = "IBM Plex Mono", color = "#1a1a2e", size = 11)
    ),
    margin = list(l = 0, r = 10, t = 10, b = 0, pad = 0),
    autosize = TRUE
  ) %>%
    config(displayModeBar = TRUE,
           responsive=TRUE,
           modeBarButtonsToRemove = c("lasso2d","select2d","autoScale2d"),
           displaylogo = FALSE,
           toImageButtonOptions = list(format = "svg", filename = "xdate_chart"))
}

server <- function(input, output, session) {
  
  # Reactive: forecast data clipped to horizon
  forecast_data <- reactive({
    my_chart %>%
      filter(record_date <= (record_date[1] %m+% years(input$horizon_yrs)))
  })
  
  # ── X-Date annotation ────────────────────────────────────────────────────
  output$xdate_text <- renderUI({
    tagList(
      tags$strong("Estimated X-Date: "),
      format(as.Date(est_xdate), "%B %d, %Y"),
      tags$span(style = "color:#8b949e;margin:0 .5rem;", "|"),
      tags$strong("Early Bound: "),
      format(as.Date(early_xdate), "%B %d, %Y"),
      tags$span(style = "color:#8b949e;margin:0 .5rem;", "|"),
      tags$span(style = "color:#f85149;",
                paste0(days_left, " calendar days until estimated X-Date"))
    )
  })
  
  # ── Main forecast chart ──────────────────────────────────────────────────
  xdate_plot <- reactive({
    df <- forecast_data() %>%
      mutate(label = paste0("$", round(running_bal, 2), "B"))
    
    p <- ggplot(df,
                aes(x = record_date, group = 1,
                    text = paste0(
                      "<b>Date:</b> ", record_date,
                      "<br><b>Fiscal Space:</b> $", round(running_bal, 2), "B",
                      "<br><b>Upper Bound:</b> $", round(running_bal_upper, 2), "B",
                      "<br><b>Lower Bound:</b> $", round(running_bal_lower, 2), "B",
                      "<br><b>Ext. Measures Date:</b> ", exmeasures_date,
                      "<br><b>Est. X-Date:</b> ",
                      my_chart %>% filter(record_date >= dat_value & running_bal <= 0) %>%
                        slice(1) %>% pull(record_date)
                    ))) +
      theme_void() +
      theme(
        plot.background  = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "#ede8e3", linewidth = 0.3),
        axis.text  = element_text(color = "#7a6f66", size = 8, family = "mono"),
        axis.title = element_text(color = "#7a6f66", size = 8, family = "mono")
      ) +
      labs(x = "", y = "Fiscal Space Remaining ($B)")
    
    if (input$show_ribbon)
      p <- p + geom_ribbon(aes(ymin = running_bal_lower, ymax = running_bal_upper),
                           fill = "#e67e22", alpha = .12)
    
    p <- p +
      geom_hline(yintercept = 0, color = "#c0392b", linetype = "dashed", linewidth = .6) +
      geom_line(aes(y = running_bal), color = "#c0392b", linewidth = 1)
    
    ggplotly(p, tooltip = "text") %>% light_layout()
  })
  
  output$xdate_chart <- renderPlotly({ xdate_plot() })
  
  # ── Historical runs chart ────────────────────────────────────────────────
  hist_plot <- reactive({
    df <- charts %>%
      filter(record_date <= (record_date[1] %m+% years(input$horizon_yrs)))
    latest <- df %>% mutate(date_run = as.Date(date_run)) %>% filter(date_run == max(date_run))
    
    p <- ggplot(df, aes(x = record_date, color = as.Date(date_run), group = date_run)) +
      geom_line(aes(y = running_bal, alpha = as.Date(date_run),label=date_run), linewidth = .6) +
      geom_line(data = latest, aes(x = record_date, y = running_bal,label=date_run),
                color = "#c0392b", linewidth = 1.2, inherit.aes = FALSE) +
      geom_hline(yintercept = 0, color = "#c0392b", linetype = "dashed", linewidth = .5) +
      scale_color_gradient(low = "#d6cfc7", high = "#2471a3") +
      scale_alpha_continuous(range = c(.2, .8)) +
      theme_void() +
      theme(
        plot.background  = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "#ede8e3", linewidth = 0.3),
        axis.text  = element_text(color = "#7a6f66", size = 8, family = "mono"),
        axis.title = element_text(color = "#7a6f66", size = 8, family = "mono"),
        legend.position = "none"
      ) +
      labs(x = "", y = "Fiscal Space Remaining ($B)")
    
    ggplotly(p, tooltip = c("x", "y","label")) %>% light_layout()
  })
  
  output$historical_chart <- renderPlotly({ hist_plot() })
  
  # ── Yearly chart ─────────────────────────────────────────────────────────
  output$yearly_chart <- renderPlotly({
    p <- ggplot(yearly_chart_df %>% filter(year >= 2015),
                aes(x = year, y = scaled_yearly, fill = cbo_category)) +
      geom_bar(stat = "identity") +
      geom_line(inherit.aes  = FALSE, aes(x = year, y = yearly_deficit),
                color = "black", linewidth = 1) +
      geom_point(inherit.aes = FALSE, aes(x = year, y = yearly_deficit),
                 color = "black", size = 2) +
      theme_void() +
      theme(
        plot.background  = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "#ede8e3", linewidth = 0.3),
        axis.text  = element_text(color = "#7a6f66", size = 8, family = "mono"),
        axis.title = element_text(color = "#7a6f66", size = 8, family = "mono"),
        legend.text      = element_text(color = "#3d3530", size = 7),
        legend.key.size  = unit(.4, "cm")
      ) +
      labs(x = "", y = "Outlays / Receipts ($B)", fill = "") +
      scale_fill_manual(values = colors_df$cols, breaks = colors_df$group)
    
    ggplotly(p, source = "yearly_chart") %>% light_layout()
  })
  
  # ── Monthly chart (drill-down) ────────────────────────────────────────────
  monthly_chart_val <- eventReactive(event_data("plotly_click", source = "yearly_chart"), {
    d <- event_data("plotly_click", source = "yearly_chart")
    req(!is.null(d$x))
    
    p <- ggplot(
      monthly_chart_df %>%
        filter((year(record_date) == d$x & month(record_date) <= 9) |
                 (year(record_date) == (d$x - 1) & month(record_date) > 9)),
      aes(x = as.yearmon(record_date), y = scaled_monthly, fill = cbo_category)
    ) +
      geom_bar(stat = "identity") +
      geom_line(inherit.aes  = FALSE,
                aes(x = as.yearmon(record_date), y = monthly_deficit),
                color = "black", linewidth = 1) +
      geom_point(inherit.aes = FALSE,
                 aes(x = as.yearmon(record_date), y = monthly_deficit),
                 color = "black", size = 2) +
      theme_void() +
      theme(
        plot.background  = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "#ede8e3", linewidth = 0.3),
        axis.text  = element_text(color = "#7a6f66", size = 7, family = "mono"),
        axis.title = element_text(color = "#7a6f66", size = 8, family = "mono"),
        legend.text     = element_text(color = "#3d3530", size = 7),
        legend.key.size = unit(.4, "cm"),
        plot.title      = element_text(color = "#c0392b", size = 9, family = "mono")
      ) +
      labs(x = "", y = "Outlays / Receipts ($B)",
           fill  = "", title = paste0("Fiscal Year ", d$x)) +
      scale_fill_manual(values = colors_df$cols, breaks = colors_df$group)
    
    ggplotly(p, source = "monthly_chart") %>% light_layout()
  })
  output$monthly_chart <- renderPlotly({ monthly_chart_val() })
  
  # ── Daily chart (drill-down) ──────────────────────────────────────────────
  daily_chart_val <- eventReactive(event_data("plotly_click", source = "monthly_chart"), {
    d <- event_data("plotly_click", source = "monthly_chart")
    req(!is.null(d$x))
    
    p <- ggplot(
      daily_chart_df %>% filter(as.yearmon(record_date) == d$x),
      aes(x = record_date, y = final_pred_day, fill = cbo_category)
    ) +
      geom_bar(stat = "identity") +
      geom_line(inherit.aes  = FALSE,
                aes(x = record_date, y = daily_deficit),
                color = "black", linewidth = .9) +
      geom_point(inherit.aes = FALSE,
                 aes(x = record_date, y = daily_deficit),
                 color = "black", size = 1.8) +
      theme_void() +
      theme(
        plot.background  = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "#ede8e3", linewidth = 0.3),
        axis.text  = element_text(color = "#7a6f66", size = 7, family = "mono"),
        axis.title = element_text(color = "#7a6f66", size = 8, family = "mono"),
        legend.text     = element_text(color = "#3d3530", size = 7),
        legend.key.size = unit(.4, "cm")
      ) +
      labs(x = "", y = "Outlays / Receipts ($B)", fill = "") +
      scale_fill_manual(values = colors_df$cols, breaks = colors_df$group)
    
    ggplotly(p) %>% light_layout()
  })
  output$daily_chart <- renderPlotly({ daily_chart_val() })
  
  # ═══════════════════════════════════════════════════════════════════════════
  # ── NEW: Daily Same-Month Historical Comparison ──────────────────────────
  # ═══════════════════════════════════════════════════════════════════════════
  
  daily_comp_base1 = reactive({
    # Use req() to ensure an input is selected before proceeding
    req(input$comp_view) 
    
    if(input$comp_view!="total"){  
      daily_comp_base %>%  filter(cbo_category==input$comp_view) 
    } else{
        
      daily_comp_base
      
  }
    
  })
  
  daily_comp_chart <- reactive({
    
    curr_month_num <- lubridate::month(as.Date(dat_value))
    curr_year_num  <- lubridate::year(as.Date(dat_value))
    month_label    <- format(as.Date(dat_value), "%B")
    
    # Filter to the same calendar month across all available years
    df <- daily_comp_base1() %>%
      dplyr::filter(cal_month == curr_month_num&cal_year<=curr_year_num&cal_year>=2015)
    
    # Prior-year net deficit (one row per year × day)
    prior_lines <- df %>%
      dplyr::filter(!is_curr) %>%
      dplyr::select(cal_year, dom, daily_deficit=final_pred_day) %>%
      group_by(cal_year,dom) %>%
      summarize(daily_deficit=sum(daily_deficit,na.rm=TRUE)) %>% 
      dplyr::arrange(cal_year, dom) %>% 
      group_by(cal_year) %>% 
      mutate(daily_deficit=cumsum(daily_deficit)) %>% 
      ungroup()
    
    # Colour ramp for prior years: oldest = light, most recent = mid-blue
    prior_years  <- sort(unique(prior_lines$cal_year))
    n_prior      <- length(prior_years)
    prior_colors <- colorRampPalette(c("#ddd6d0", "#8fa8c0"))(max(n_prior, 1))
    names(prior_colors) <- as.character(prior_years)
    
      # ── Total deficit view: one line per year ────────────────────────────
      curr_net <- df %>%
        dplyr::filter(is_curr) %>%
        dplyr::select(dom, daily_deficit=final_pred_day, is_forecast) %>%
        group_by(dom) %>%
        summarize(daily_deficit=sum(daily_deficit,na.rm=TRUE),
                  is_forecast=is_forecast[1]) %>% 
        mutate(daily_deficit=cumsum(daily_deficit)) %>% 
        ungroup()
      
      curr_actual <- curr_net %>% dplyr::filter(!is_forecast)
      curr_fcst   <- curr_net %>% dplyr::filter(is_forecast|(any(is_forecast)&any(!is_forecast)&dom==max(dom[!is_forecast])))

      p <- ggplot() +
        # Prior-year lines, colour-graded oldest→newest
        geom_line(
          data = prior_lines,
          aes(x = dom, y = daily_deficit,
              group = factor(cal_year),
              color = factor(cal_year),
              text  = paste0(
                "<b>", cal_year, " (", month_label, ")</b>",
                "<br>Day ", dom, ": $", round(daily_deficit, 2), "B"
              )),
          linewidth = 0.55
        ) +
        scale_color_manual(
          values = prior_colors,
          name   = "Prior Year"
        ) 
        # Current-year actual (solid red)
        if(nrow(curr_actual)>0){
          p = p + geom_line(
            data = curr_actual,
            inherit.aes = FALSE,
            aes(x   = dom,
                y   = daily_deficit,
                group=1,
                text = paste0(
                  "<b>", curr_year_num, " (", month_label, ") — Actual</b>",
                  "<br>Day ", dom, ": $", round(daily_deficit, 2), "B"
                )),
            color     = "#c0392b",
            linewidth = 1.4,
            lineend   = "round"
          )
        } 
        # Current-year forecast (dashed red)
        if(nrow(curr_fcst)>0){
          p = p + geom_line(
            data = curr_fcst,
            aes(x   = dom,
                y   = daily_deficit,
                group=1,
                text = paste0(
                  "<b>", curr_year_num, " (", month_label, ") — Forecast</b>",
                  "<br>Day ", dom, ": $", round(daily_deficit, 2), "B"
                )),
            color     = "#c0392b",
            linewidth = 1.4,
            linetype  = "dashed",
            lineend   = "round"
          )
        } 
      
      p = p + geom_hline(yintercept = 0, color = "#9e8f86",
                   linetype = "dotted", linewidth = 0.35) +
        scale_x_continuous(
          breaks = c(1, 5, 10, 15, 20, 25, 31),
          expand = expansion(mult = c(0.01, 0.02))
        ) +
        comp_theme() +
        labs(
          x     = "Day of Month",
          y     = "Net Daily Flow ($B)",
          color = "Year"
        )
      
      ggplotly(p, tooltip = "text") %>%
        light_layout() %>%
        layout(
          legend = list(
            orientation = "v",
            x = 1.02, y = 0.98,
            font        = list(size = 9),
            bgcolor     = "rgba(255,255,255,0.85)",
            bordercolor = "#e2dbd3",
            borderwidth = 1
          )
        )
      
  })
  
  output$daily_comp_chart = renderPlotly({daily_comp_chart()})
  
  # ═══════════════════════════════════════════════════════════════════════════
  # ── NEW: Fiscal-Year Monthly Historical Comparison ───────────────────────
  # ═══════════════════════════════════════════════════════════════════════════
  
  monthly_comp_base1 = reactive({
    # Use req() to ensure an input is selected before proceeding
    req(input$comp_view) 
    
    if(input$comp_view!="total"){  
      monthly_comp_base %>%  filter(cbo_category==input$comp_view) 
    } else{
      
      monthly_comp_base
      
    }
    
  })
  
  fy_comp_chart <- reactive({
    
    curr_fy_year_num <- lubridate::year(fy_end)
    
    df <- monthly_comp_base1() %>% 
      filter(fy<=curr_fy_year_num&fy>=2015)
    
    # Prior-FY net deficit, one row per FY × FY-month
    prior_fy_lines <- df %>%
      dplyr::filter(!is_curr_fy) %>%
      dplyr::select(fy, fy_month, monthly_deficit=scaled_monthly) %>%
      group_by(fy,fy_month) %>%
      summarize(monthly_deficit=sum(monthly_deficit,na.rm=TRUE)) %>% 
      dplyr::arrange(fy, fy_month) %>% 
      group_by(fy) %>% 
      mutate(monthly_deficit=cumsum(monthly_deficit)) %>% 
      ungroup()
    
    prior_fys    <- sort(unique(prior_fy_lines$fy))
    n_prior_fy   <- length(prior_fys)
    prior_fy_colors <- colorRampPalette(c("#ddd6d0", "#8fa8c0"))(max(n_prior_fy, 1))
    names(prior_fy_colors) <- as.character(prior_fys)
    
      # ── Total deficit: one line per FY ───────────────────────────────────
      curr_fy_net <- df %>%
        dplyr::filter(is_curr_fy) %>%
        dplyr::select(fy_month, monthly_deficit=scaled_monthly, is_forecast) %>%
        group_by(fy_month) %>% 
        summarize(monthly_deficit=sum(monthly_deficit,na.rm=TRUE),
                  is_forecast=is_forecast[1]) %>% 
        ungroup() %>% 
        dplyr::arrange(fy_month) %>% 
        mutate(monthly_deficit=cumsum(monthly_deficit)) %>% 
        ungroup()
      
      curr_fy_actual <- curr_fy_net %>% dplyr::filter(!is_forecast)
      curr_fy_fcst   <- curr_fy_net %>% dplyr::filter(is_forecast|(any(is_forecast)&any(!is_forecast)&fy_month==max(fy_month[!is_forecast])))
      
      p <- ggplot() +
        # Prior-FY lines
        geom_line(
          data = prior_fy_lines,
          aes(x    = fy_month,
              y    = monthly_deficit,
              group = factor(fy),
              color = factor(fy),
              text  = paste0(
                "<b>FY", fy, "</b>",
                "<br>", fy_month_labels[fy_month],
                ": $", round(monthly_deficit, 2), "B"
              )),
          linewidth = 0.55
        ) +
        scale_color_manual(values = prior_fy_colors, name = "Prior FY") 
        # Current FY actual (solid red)
        if(nrow(curr_fy_actual)>0){ 
          p = p + geom_line(
          data = curr_fy_actual,
          aes(x    = fy_month,
              y    = monthly_deficit,
              group=1,
              text  = paste0(
                "<b>FY", curr_fy_year_num, " — Actual</b>",
                "<br>", fy_month_labels[fy_month],
                ": $", round(monthly_deficit, 2), "B"
              )),
          color     = "#c0392b",
          linewidth = 1.4,
          lineend   = "round"
        ) 
        }
        # Current FY forecast (dashed red)
        if(nrow(curr_fy_fcst)>0){
          p = p + geom_line(
            data = curr_fy_fcst,
            aes(x    = fy_month,
                y    = monthly_deficit,
                group=1,
                text  = paste0(
                  "<b>FY", curr_fy_year_num, " — Forecast</b>",
                  "<br>", fy_month_labels[fy_month],
                  ": $", round(monthly_deficit, 2), "B"
                )),
            color     = "#c0392b",
            linewidth = 1.4,
            linetype  = "dashed",
            lineend   = "round"
          ) 
        }
        
        p = p + geom_hline(yintercept = 0, color = "#9e8f86",
                   linetype = "dotted", linewidth = 0.35) +
        scale_x_continuous(
          breaks = 1:12,
          labels = fy_month_labels,
          expand = expansion(mult = c(0.02, 0.02))
        ) +
        comp_theme() +
        labs(
          x     = "",
          y     = "Net Monthly Flow ($B)",
          color = "FY"
        )
      
      ggplotly(p, tooltip = "text") %>%
        light_layout() %>%
        layout(
          legend = list(
            orientation = "v",
            x = 1.02, y = 0.98,
            font        = list(size = 9),
            bgcolor     = "rgba(255,255,255,0.85)",
            bordercolor = "#e2dbd3",
            borderwidth = 1
          )
        )
      
  })
  
  output$fy_comp_chart = renderPlotly({fy_comp_chart()})
  
  # ── Forecast Revision Table ───────────────────────────────────────────────
  
  output$revision_run_labels <- renderUI({
    if (is.null(revision_tbl)) return(NULL)
    rd <- revision_tbl %>% slice(1)
    tagList(
      span(class = "data-badge", paste0("Today: ", rd$run_today)),
      span(class = "data-badge", paste0("Yest:  ", rd$run_yest)),
      span(class = "data-badge", paste0("30d:   ", rd$run_30d))
    )
  })
  
  output$revision_table_ui <- renderUI({
    if (is.null(revision_tbl) || nrow(revision_tbl) == 0) {
      return(div(class = "no-data-msg",
                 icon("triangle-exclamation"),
                 " Revision data unavailable — historical monthly_chart_df not found in loaded .RData files."))
    }
    
    fmt_delta <- function(x) {
      if (is.na(x) || x == 0) return(span(class="delta-zero","—"))
      arrow <- if (x > 0) "▲" else "▼"
      cls   <- if (x > 0) "delta-pos" else "delta-neg"
      span(class = cls, paste0(arrow, " $", formatC(abs(x), format="f", digits=1), "B"))
    }
    
    th <- function(..., style="") tags$th(class="rev-tbl-header", style=style, ...)
    td <- function(..., style="") tags$td(style=paste0("padding:.4rem .65rem;vertical-align:middle;", style), ...)
    
    build_section <- function(flow_label) {
      rows <- revision_tbl %>% dplyr::filter(flow_type == flow_label)
      section_row <- tags$tr(
        class = "rt-section-divider",
        tags$td(colspan="9",
                style="background:#faf8f5;font-weight:700;font-size:.7rem;
                       text-transform:uppercase;letter-spacing:.06em;color:#7a6f66;
                       padding:.35rem .65rem;border-top:2px solid #e2dbd3;",
                if (flow_label == "Receipt") "📥  Receipts" else "📤  Outlays")
      )
      data_rows <- lapply(seq_len(nrow(rows)), function(i) {
        r <- rows[i, ]
        tags$tr(
          style = if (i %% 2 == 0) "background:#fafaf9;" else "background:#ffffff;",
          td(r$cbo_category, style="font-weight:600;color:#1a1a2e;min-width:180px;"),
          td(paste0("$", formatC(r$month_today, format="f", digits=1), "B"),
             style="text-align:right;color:#3d3530;"),
          td(fmt_delta(r$delta_month_1d),  style="text-align:right;"),
          td(fmt_delta(r$delta_month_30d), style="text-align:right;
                                                   border-right:2px solid #e2dbd3;"),
          td(paste0("$", formatC(r$fy_today, format="f", digits=1), "B"),
             style="text-align:right;color:#3d3530;"),
          td(fmt_delta(r$delta_fy_1d),  style="text-align:right;"),
          td(fmt_delta(r$delta_fy_30d), style="text-align:right;")
        )
      })
      c(list(section_row), data_rows)
    }
    
    rd <- revision_tbl %>% slice(1)
    table_html <- tags$div(
      class = "rev-table-wrap",
      tags$table(
        style = "width:100%;border-collapse:collapse;font-family:'IBM Plex Mono',monospace;
                 font-size:.78rem;",
        tags$thead(
          tags$tr(
            th("", style="min-width:180px;"),
            tags$th(colspan="3",
                    class="rev-tbl-group-header",
                    style="text-align:center;border-left:1px solid #e2dbd3;
                           border-right:2px solid #e2dbd3;",
                    paste0("Current Month (", format(as.Date(dat_value), "%b %Y"), ")")),
            tags$th(colspan="3",
                    class="rev-tbl-group-header",
                    style="text-align:center;",
                    paste0("Current Fiscal Year (FY", lubridate::year(fy_end), ")"))
          ),
          tags$tr(
            th("CBO Category"),
            th("Today ($B)",        style="text-align:right;border-left:1px solid #e2dbd3;"),
            th(paste0("Δ vs ", rd$run_yest, " ($B)"), style="text-align:right;"),
            th(paste0("Δ vs ", rd$run_30d, " ($B)"),
               style="text-align:right;border-right:2px solid #e2dbd3;"),
            th("Today ($B)",        style="text-align:right;"),
            th(paste0("Δ vs ", rd$run_yest, " ($B)"), style="text-align:right;"),
            th(paste0("Δ vs ", rd$run_30d, " ($B)"),  style="text-align:right;")
          )
        ),
        tags$tbody(
          c(build_section("Receipt"), build_section("Outlay"))
        )
      )
    )
    
    table_html
  })
  
  output$dl_revision <- downloadHandler(
    filename = function() paste0("forecast_revisions_", Sys.Date(), ".csv"),
    content  = function(file) {
      if (!is.null(revision_tbl))
        write.csv(revision_tbl %>%
                    select(flow_type, cbo_category, label,
                           month_today, delta_month_1d, delta_month_30d,
                           fy_today,    delta_fy_1d,    delta_fy_30d),
                  file, row.names = FALSE)
    }
  )
  
  # ── Download handlers ────────────────────────────────────────────────────
  output$dl_xdate  <- downloadHandler(
    filename = function() paste0("xdate_forecast_", Sys.Date(), ".html"),
    content  = function(file) htmlwidgets::saveWidget(xdate_plot(), file)
  )
  output$dl_hist <- downloadHandler(
    filename = function() paste0("xdate_history_", Sys.Date(), ".html"),
    content  = function(file) htmlwidgets::saveWidget(hist_plot(), file)
  )
  output$dl_yearly <- downloadHandler(
    filename = function() paste0("deficits_yearly_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(yearly_chart_df, file, row.names = FALSE)
  )
}

shinyApp(ui = ui, server = server)