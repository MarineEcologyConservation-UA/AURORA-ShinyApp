server <- function(input, output, session) {

  example_map <- list(
    "Example (event.csv)" = system.file(
      "extdata", "event.csv", package = "shinyRv02"
    ),
    "Example (occurrence.csv)" = system.file(
      "extdata", "occurrence.csv", package = "shinyRv02"
    ),
    "Example (aurora.xlsx)" = system.file(
      "extdata", "aurora_good_quality_data_densitydata_id_corrected.xlsx", package = "shinyRv02"
    )
  )

  ingest <- mod_ingest_server("ingest", example_map = example_map)

  dwc_map <- mod_dwc_mapping_server(
    "dwc_map",
    df_in = ingest$tidy
  )

  invisible(list(ingest = ingest, dwc_map = dwc_map))
}
