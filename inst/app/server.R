server <- function(input, output, session) {

  example_map <- list(
    "Example (event.csv)" = system.file("extdata", "event.csv", package = "shinyRv02"),
    "Example (occurrence.csv)" = system.file("extdata", "occurrence.csv", package = "shinyRv02"),
    "Example (aurora.xlsx)" = system.file(
      "extdata",
      "aurora_good_quality_data_densitydata_id_corrected.xlsx",
      package = "shinyRv02"
    )
  )

  ingest <- mod_ingest_server("ingest", example_map = example_map)

  dwc_map <- mod_dwc_mapping_server(
    "dwc_map",
    df_in = ingest$tidy
  )

  # --- Build DwC-A -----------------------------------------------------------
  dwc_terms_path <- system.file("extdata", "dwc_terms.csv", package = "shinyRv02")
  dwc_terms <- read.csv(dwc_terms_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")

  dwca <- mod_build_dwca_server(
    "dwca",
    df_in = dwc_map$cleaned,
    dwc_terms = dwc_terms
  )

  invisible(list(ingest = ingest, dwc_map = dwc_map, dwca = dwca))
}
