ui <- bslib::page_navbar(
  title = "shinyRv02",

  theme = bslib::bs_theme(version = 5),

  header = shiny::tags$head(
    shiny::tags$style(shiny::HTML("
      /* Tooltips Bootstrap 5 (bslib) */
      .tooltip-inner{
        max-width: 520px;     /* mais largo */
        white-space: normal;  /* deixa quebrar linha */
        text-align: left;
      }

      /* opcional: se ainda ficar muito alto, ativa scroll */
      .tooltip-inner{
        max-height: 260px;
        overflow-y: auto;
      }
    "))
  ),

  bslib::nav_panel(
    title = "Ingest",
    mod_ingest_ui("ingest")
  ),

  bslib::nav_panel(
    title = "Mapping",
    mod_dwc_mapping_ui("dwc_map")
  )
)
