ui <- bslib::page_navbar(
  title = "shinyRv02",

  theme = bslib::bs_theme(
    version = 5,
    bootswatch = "minty",
    primary = "#1B998B",
    secondary = "#5C7AEA",
    success = "#2E7D32",
    base_font = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Source Sans 3")
  ),

  header = shiny::tags$head(
    shiny::tags$style(shiny::HTML("
      body { background: #f3f7f5; }

      .bslib-page-main { padding: 1rem 1rem; }

      .card {
        border: 1px solid rgba(0,0,0,.06);
        box-shadow: 0 10px 24px rgba(0,0,0,.06);
        border-radius: 16px;
      }
      .card-header {
        background: rgba(27,153,139,.06);
        font-weight: 650;
        border-bottom: 1px solid rgba(0,0,0,.06);
      }

      h4, h5 { margin-top: .25rem; }

      .tooltip-inner{
        max-width: 560px;
        white-space: normal;
        text-align: left;
      }

      table.dataTable { font-size: 0.95rem; }
    "))
  ),

  # -------------------------------------------------------
  # 1️⃣ INGEST
  # -------------------------------------------------------
  bslib::nav_panel(
    title = "Ingest",
    mod_ingest_ui("ingest")
  ),

  # -------------------------------------------------------
  # 2️⃣ MAPPING
  # -------------------------------------------------------
  bslib::nav_panel(
    title = "Mapping",
    mod_dwc_mapping_ui("dwc_map")
  ),

  # -------------------------------------------------------
  # 3️⃣ BUILD DWC-A
  # -------------------------------------------------------
  bslib::nav_panel(
    title = "Build DwC-A",
    mod_build_dwca_ui("dwca")
  ),

  # -------------------------------------------------------
  # 4 QC & DIAGNOSTICS
  # -------------------------------------------------------
  mod_qc_ui("qc")
)
