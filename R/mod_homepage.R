# modules/mod_homepage.R

#' Homepage module UI
#'
#' @param id Module id.
#' @return A Shiny UI definition.
#' @export
mod_homepage_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::div(
      id = ns("page"),
      class = "home-wrap",

      bslib::card(
        class = "hero-card mb-4",
        bslib::card_body(
          shiny::div(
            class = "hero-grid",

            shiny::div(
              shiny::tags$img(
                src = "aurora_logo.png",
                alt = "AURORA logo",
                class = "hero-logo"
              )
            ),

            shiny::div(
              shiny::div(
                class = "hero-title",
                "AURORA Shiny App - streamlining biodiversity data sharing"
              ),
              shiny::p(
                class = "hero-subtitle",
                "An integrated Shiny application designed to streamline the preparation, validation, and export of both marine and terrestrial biodiversity datasets into Darwin Core Archives for sharing on public repositories such as GBIF — the Global Biodiversity Information Facility."
              ),
              shiny::div(
                class = "badge-row",
                shiny::span(class = "soft-badge", "Biodiversity data"),
                shiny::span(class = "soft-badge", "Darwin Core Standards"),
                shiny::span(class = "soft-badge", "Taxonomy matching"),
                shiny::span(class = "soft-badge", "Quality control"),
                shiny::span(class = "soft-badge", "Metadata"),
                shiny::span(class = "soft-badge", "Darwin Core Archive (DwC-A)")
              )
            )
          )
        )
      ),

      bslib::card(
        class = "mb-4",
        bslib::card_header("tool workflow"),
        bslib::card_body(
          shiny::div(
            class = "step-grid",

            shiny::div(
              class = "step-box",
              shiny::div(class = "step-n", "1"),
              shiny::div(class = "step-title", "Ingest"),
              shiny::div(
                class = "step-text",
                "Import the source dataset and inspect its original structure before processing."
              )
            ),

            shiny::div(
              class = "step-box",
              shiny::div(class = "step-n", "2"),
              shiny::div(class = "step-title", "Field Mapping"),
              shiny::div(
                class = "step-text",
                "Map original columns of the ingested dataset to Darwin Core terms with guidance and validation."
              )
            ),

            shiny::div(
              class = "step-box",
              shiny::div(class = "step-n", "3"),
              shiny::div(class = "step-title", "Taxonomy"),
              shiny::div(
                class = "step-text",
                "Automatic matching of scientific names against taxonomic backbones of WoRMS or GBIF."
              )
            ),

            shiny::div(
              class = "step-box",
              shiny::div(class = "step-n", "4"),
              shiny::div(class = "step-title", "Darwin Tables"),
              shiny::div(
                class = "step-text",
                "Build structured Darwin Core tables for Event, Occurrence, and eMoF."
              )
            ),

            shiny::div(
              class = "step-box",
              shiny::div(class = "step-n", "5"),
              shiny::div(class = "step-title", "Metadata"),
              shiny::div(
                class = "step-text",
                "Streamline metadata creation and revision using an interface inspired by the GBIF IPT."
              )
            ),

            shiny::div(
              class = "step-box",
              shiny::div(class = "step-n", "6"),
              shiny::div(class = "step-title", "QC"),
              shiny::div(
                class = "step-text",
                "Inspect issues, diagnostics, summary outputs, and validation results before exporting."
              )
            )
          )
        )
      )
    )
  )
}