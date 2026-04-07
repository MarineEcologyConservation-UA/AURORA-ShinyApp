# modules/mod_about.R

#' About module UI
#'
#' @param id Module id.
#' @return A Shiny UI definition.
#' @export
mod_about_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::div(
      id = ns("page"),
      class = "home-wrap",

      bslib::card(
        class = "mb-4",
        bslib::card_header("The AURORA project"),
        bslib::card_body(
          shiny::div(
            class = "about-project-grid",

            shiny::div(
              class = "about-project-logo-wrap",
              shiny::tags$img(
                src = "aurora_logo.png",
                alt = "AURORA logo",
                class = "about-project-logo"
              )
            ),

            shiny::div(
              shiny::p(
                class = "home-note",
                shiny::strong("The AURORA Shiny App"),
                " was developed in the context of the research project AURORA – bringing deep-sea biodiversity data to light."
              ),
              shiny::p(
                class = "home-note",
                "The deep sea covers approximately 65% of the planet’s surface. It is the largest biome on Earth and one of the least explored and understood. Given its vastness and the growing recognition of its ecological and economic importance (e.g., climate regulation, mineral resources, biotechnological potential), sharing deep-sea biodiversity data is essential for advancing knowledge about this biome and supporting innovative initiatives such as the development of the European Digital Twin of the Ocean. However, deep-sea biodiversity data (>200 m) in European marine waters account for only 11% of records in databases like the Ocean Biogeographic Information System (OBIS). For depths greater than 3500 meters, this number drops to about 1% of total records."
              ),
              shiny::p(
                class = "home-note",
                "The Digital Marine Biodiversity Lab at the University of Aveiro holds a vast collection of biological data from numerous deep-sea campaigns. This project aims to expand deep-sea biodiversity datasets on the EMODnet Biology portal, contributing to European policies such as the Marine Strategy Framework Directive (MSFD) and the EU Biodiversity Strategy for 2030. The data flow we intend to create will begin with the release of data from the Aurora seamount and adjacent areas, located on the Gakkel Ridge in the Central Arctic Ocean."
              ),
              shiny::p(
                class = "home-note",
                "This project will also develop a user-friendly tool to facilitate biodiversity data processing, from formatting to submission to online repositories. The tool will ensure compliance with the FAIR Principles – Findable, Accessible, Interoperable, and Reusable – and international standards for scientific data management and curation."
              )
            )
          )
        )
      ),

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
                  "Convert all event dates into the ISO 8601 format (e.g., YYYY-MM-DD) for temporal consistency."
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
                shiny::div(class = "feature-title", "Taxonomic matching"),
                shiny::div(
                  class = "feature-text",
                  "Cross-reference scientific names with authoritative taxonomic databases (e.g., WoRMS, GBIF Backbone) to validate and retrieve accepted names and current classifications."
                )
              )
            ),

            bslib::card(
              class = "feature-card",
              bslib::card_body(
                shiny::div(class = "feature-icon", "🗂️"),
                shiny::div(class = "feature-title", "Build Darwin Core Archives (DwC-A)"),
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
                  "Revise processed tables to identify inconsistencies or issues within the dataset."
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
                  "Document the dataset’s metadata to facilitate revision and comply with FAIR principles."
                )
              )
            )
          )
        )
      ),

      bslib::layout_columns(
        col_widths = c(6, 6),

        bslib::card(
          class = "mb-4",
          bslib::card_header("How to cite"),
          bslib::card_body(
            shiny::p(
              class = "home-note",
              "Somensi, A., Araújo, S. M., … Matos, F. L. (2026). Aurora Shiny App: Streamlining biodiversity data sharing (Version 1.0)."
            ),
            shiny::p(
              class = "home-note",
              style = "margin-bottom:0; word-break: break-word;",
              shiny::tags$a(
                href = "http://bio-shiny.ua.pt:3838/aurora",
                target = "_blank",
                rel = "noopener noreferrer",
                "http://bio-shiny.ua.pt:3838/aurora"
              )
            )
          )
        ),

        bslib::card(
          class = "mb-4",
          bslib::card_header("Manual, support, and bug reports"),
          bslib::card_body(
            shiny::p(
              class = "home-note",
              shiny::strong("Manual: "),
              shiny::tags$a(
                href = "https://uapt33090-my.sharepoint.com/:w:/g/personal/fmatos_ua_pt/IQBwrPuUhfJ8Rog0dUVteCP2AUx-PDMQVswSuUe3Hp4PiiA?rtime=tk_1xbyU3kg",
                target = "_blank",
                rel = "noopener noreferrer",
                "Open the AURORA manual"
              )
            ),
            shiny::p(
              class = "home-note",
              shiny::strong("Technical support: "),
              "For technical support or to suggest new feature enhancements for the AURORA Shiny App, please reach out to us directly via email at ",
              shiny::tags$a(
                href = "mailto:fmatos@ua.pt",
                "fmatos@ua.pt"
              ),
              "."
            ),
            shiny::p(
              class = "home-note",
              shiny::strong("Bug reports: "),
              "If you encounter any bugs or functional glitches, we kindly ask that you report them by opening a new issue on the AURORA GitHub repository: ",
              shiny::tags$a(
                href = "https://github.com/MarineEcologyConservation-UA/AURORA-ShinyApp",
                target = "_blank",
                rel = "noopener noreferrer",
                "AURORA GitHub repository"
              ),
              "."
            )
          )
        )
      ),

      bslib::card(
        class = "mb-4",
        bslib::card_header("Funding"),
        bslib::card_body(
          shiny::p(
            class = "home-note",
            "AURORA project is funded by the Flanders Marine Institute (VLIZ) through the DTO-BioFlow project funded by the European Union."
          )
        )
      )
    )
  )
}