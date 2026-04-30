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

      # =========================
      # PROJECT
      # =========================
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
                shiny::strong("The AURORA R Shiny application"),
                " was developed as a core output of the ",
                shiny::tags$em("project AURORA - bringing deep-sea biodiversity data to light"),
                ", an initiative dedicated to streamline the mobilization of deep-sea biodiversity data."
              ),
              shiny::p(
                class = "home-note",
                "Although the deep sea covers 65% of the planet, it remains Earth’s least understood biome. Within European marine waters, deep-sea data (depths >200 m) account for only 11% of records in the Ocean Biogeographic Information System (OBIS), dropping to a mere 1% for depths below 3500 meters."
              ),
              shiny::p(
                class = "home-note",
                "To address this knowledge gap, the Digital Marine Biodiversity Lab at the University of Aveiro is mobilizing its collections of deep-sea data, starting with the Aurora seamount on the Gakkel Ridge in the Central Arctic Ocean."
              ),
              shiny::p(
                class = "home-note",
                "This data mobilisation to open repositories, such as EMODnet Biology, will directly contribute to the development of the European Digital Twin of the Ocean."
              )
            )
          )
        )
      ),

      # =========================
      # DISCLAIMER
      # =========================
      bslib::card(
        class = "mb-4",
        bslib::card_header("Disclaimer"),
        bslib::card_body(
          shiny::p(
            class = "home-note",
            "This application is provided as a tool to assist in the standardization of biodiversity datasets into Darwin Core (DwC) archives. While every effort is made to ensure the accuracy of the underlying mapping logic and transformation scripts, the outputs are provided \"as is\" without any guarantees of completeness, accuracy, or fitness for a specific purpose."
          )
        )
      ),

      # =========================
      # CITE + SUPPORT
      # =========================
      bslib::layout_columns(
        col_widths = c(6, 6),

        bslib::card(
          class = "mb-4",
          bslib::card_header("How to cite"),
          bslib::card_body(
            shiny::p(
              class = "home-note",
              "Magnenti, Á. S., Araújo, S. M., … Matos, F. L. (2026). Aurora Shiny App: Streamlining biodiversity data sharing (Version 1.0)."
            ),
            shiny::p(
              class = "home-note",
              style = "margin-bottom:0.5rem; word-break: break-word;",
              shiny::tags$a(
                href = "http://bio-shiny.ua.pt:3838/aurora",
                target = "_blank",
                rel = "noopener noreferrer",
                "http://bio-shiny.ua.pt:3838/aurora"
              )
            ),
            shiny::p(
              class = "home-note",
              style = "margin-bottom:0;",
              shiny::strong("License: "),
              shiny::tags$a(
                href = "https://creativecommons.org/licenses/by/4.0/",
                target = "_blank",
                rel = "noopener noreferrer",
                "CC-BY 4.0"
              ),
              shiny::span(" — This work may be shared and adapted, including for commercial purposes, provided appropriate credit is given.")
            )
          )
        ),

        bslib::card(
          class = "mb-4",
          bslib::card_header("Support and bug reports"),
          bslib::card_body(
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
              "If you encounter any bugs or functional glitches, please report them by opening a new issue on the AURORA GitHub repository: ",
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

      # =========================
      # INSTITUTIONAL SUPPORT
      # =========================
      bslib::card(
        class = "mb-4",
        bslib::card_header("Institutional support"),
        bslib::card_body(
          shiny::div(
            style = "display:flex; justify-content:center; align-items:center; width:100%;",
            shiny::tags$img(
              src = "partners_logo.png",
              alt = "Participating institutions logos",
              style = "max-width:100%; height:auto; display:block;"
            )
          )
        )
      ),

      # =========================
      # FUNDING
      # =========================
      bslib::card(
        class = "mb-4",
        bslib::card_header("Funding"),
        bslib::card_body(

          shiny::p(
            class = "home-note",
            "AURORA project is funded by the Flanders Marine Institute (VLIZ) through the DTO-BioFlow project funded by the European Union."
          ),

          shiny::div(
            style = "display:flex; gap:2rem; align-items:center; flex-wrap:wrap; margin-top:1rem;",

            shiny::tags$img(
              src = "https://dto-bioflow.eu/sites/default/files/2023-09/Logo_BIO-Flow2023_Positive.png",
              height = "60px"
            ),

            shiny::tags$img(
              src = "https://ec.europa.eu/regional_policy/images/information-sources/logo-download-center/eu_funded_en.jpg",
              height = "60px"
            )
          )
        )
      )
    )
  )
}