ui <- bslib::page_navbar(
  title = "shinyRv02",

  theme = bslib::bs_theme(
    version = 5
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
