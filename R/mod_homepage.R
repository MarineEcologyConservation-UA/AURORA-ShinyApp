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

      # =========================
      # HERO
      # =========================
      bslib::card(
        class = "hero-card mb-4",
        bslib::card_body(
          shiny::div(
            class = "hero-grid",

            shiny::div(
              shiny::tags$img(
                src = "aurora_logo_home.png",
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

      # =========================
      # WORKFLOW
      # =========================
      bslib::card(
        class = "mb-4",
        bslib::card_header("Tool workflow"),
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
              shiny::div(class = "step-title", "Identification Data Cleaning"),
              shiny::div(
                class = "step-text",
                "Review and refine identification-related fields, especially scientificName and related Darwin Core terms, before taxonomic matching."
              )
            ),

            shiny::div(
              class = "step-box",
              shiny::div(class = "step-n", "4"),
              shiny::div(class = "step-title", "Taxonomy"),
              shiny::div(
                class = "step-text",
                "Automatic matching of scientific names against authoritative taxonomic backbones such as WoRMS or GBIF."
              )
            ),

            shiny::div(
              class = "step-box",
              shiny::div(class = "step-n", "5"),
              shiny::div(class = "step-title", "Darwin Core Tables"),
              shiny::div(
                class = "step-text",
                "Build structured Darwin Core tables for Event, Occurrence, and eMoF."
              )
            ),

            shiny::div(
              class = "step-box",
              shiny::div(class = "step-n", "6"),
              shiny::div(class = "step-title", "QC & Diagnostics"),
              shiny::div(
                class = "step-text",
                "Inspect issues, diagnostics, summary outputs, and validation results before final metadata preparation and export."
              )
            ),

            shiny::div(
              class = "step-box",
              shiny::div(class = "step-n", "7"),
              shiny::div(class = "step-title", "Metadata"),
              shiny::div(
                class = "step-text",
                "Streamline metadata creation and revision using an interface inspired by the GBIF IPT."
              )
            )
          ),

          # =========================
          # MANUAL (FULL WIDTH, BELOW STEP 7)
          # =========================
          bslib::card(
            class = "mt-4 w-100",
            bslib::card_body(
              shiny::div(
                style = "display:flex; justify-content:space-between; align-items:center; flex-wrap:wrap; gap:1rem; width:100%;",

                shiny::div(
                  style = "flex:1;",
                  shiny::strong("User manual"),
                  shiny::p(
                    class = "home-note",
                    style = "margin:0;",
                    "Step-by-step guidance covering ingestion, mapping, taxonomy, quality control, metadata, and export."
                  )
                ),

                shiny::tags$a(
                  href = "",
                  target = "_blank",
                  rel = "noopener noreferrer",
                  class = "btn btn-primary",
                  "Open manual"
                )
              )
            )
          )
        )
      ),

      # =========================
      # FEATURES
      # =========================
      bslib::card(
        class = "mb-4",
        bslib::card_header("Examples of supported tasks"),
        bslib::card_body(
          shiny::div(
            class = "feature-grid",

            bslib::card(
              class = "feature-card",
              bslib::card_body(
                shiny::div(class = "feature-icon", "📝"),
                shiny::div(class = "feature-title", "Map to Darwin Core"),
                shiny::div(
                  class = "feature-text",
                  "Align the original dataset fields with standardized Darwin Core (DwC) terms to ensure global interoperability."
                )
              )
            ),

            bslib::card(
              class = "feature-card",
              bslib::card_body(
                shiny::div(class = "feature-icon", "📅"),
                shiny::div(class = "feature-title", "Standardize Temporal Data"),
                shiny::div(
                  class = "feature-text",
                  "Convert all event dates into ISO 8601 format for temporal consistency."
                )
              )
            ),

            bslib::card(
              class = "feature-card",
              bslib::card_body(
                shiny::div(class = "feature-icon", "📍"),
                shiny::div(class = "feature-title", "Normalize Coordinates"),
                shiny::div(
                  class = "feature-text",
                  "Transform various coordinate formats into Decimal Degrees (WGS84)."
                )
              )
            ),

            bslib::card(
              class = "feature-card",
              bslib::card_body(
                shiny::div(class = "feature-icon", "🍃"),
                shiny::div(class = "feature-title", "Taxonomic Matching"),
                shiny::div(
                  class = "feature-text",
                  "Cross-reference scientific names with authoritative taxonomic databases such as WoRMS and GBIF to validate names and retrieve accepted classifications."
                )
              )
            ),

            bslib::card(
              class = "feature-card",
              bslib::card_body(
                shiny::div(class = "feature-icon", "🗂️"),
                shiny::div(class = "feature-title", "Build Darwin Core Tables"),
                shiny::div(
                  class = "feature-text",
                  "Structure the data into relational Event, Occurrence, and Extended Measurement or Fact (eMoF) tables to comply with the DwC-A star schema."
                )
              )
            ),

            bslib::card(
              class = "feature-card",
              bslib::card_body(
                shiny::div(class = "feature-icon", "🔍"),
                shiny::div(class = "feature-title", "Data Quality Inspection"),
                shiny::div(
                  class = "feature-text",
                  "Review processed tables to identify inconsistencies, missing values, and other issues in the dataset."
                )
              )
            ),

            bslib::card(
              class = "feature-card",
              bslib::card_body(
                shiny::div(class = "feature-icon", "📝"),
                shiny::div(class = "feature-title", "Metadata Preparation"),
                shiny::div(
                  class = "feature-text",
                  "Document dataset-level metadata to support revision, publication, and FAIR data sharing."
                )
              )
            )
          )
        )
      )
    )
  )
}