# =========================================================
# Metadata module
# File: R/mod_metadata.R
# =========================================================

# pequeno helper local
`%||%` <- function(x, y) if (is.null(x)) y else x

# ---------------------------------------------------------
# UI helpers
# ---------------------------------------------------------
.metadata_text <- function(inputId, label, placeholder = NULL, help = NULL, value = "") {
  shiny::div(
    class = "mb-3",
    shiny::textInput(inputId, label, value = value, placeholder = placeholder, width = "100%"),
    if (!is.null(help)) shiny::div(class = "metadata-help", help)
  )
}

.metadata_textarea <- function(inputId, label, placeholder = NULL, help = NULL, value = "", rows = 4) {
  shiny::div(
    class = "mb-3",
    shiny::textAreaInput(
      inputId, label, value = value, placeholder = placeholder,
      width = "100%", rows = rows, resize = "vertical"
    ),
    if (!is.null(help)) shiny::div(class = "metadata-help", help)
  )
}

.metadata_select <- function(inputId, label, choices, selected = NULL, help = NULL) {
  shiny::div(
    class = "mb-3",
    shiny::selectInput(inputId, label, choices = choices, selected = selected, width = "100%"),
    if (!is.null(help)) shiny::div(class = "metadata-help", help)
  )
}

.metadata_date <- function(inputId, label, value = NULL, help = NULL) {
  shiny::div(
    class = "mb-3",
    shiny::dateInput(inputId, label, value = value, width = "100%", format = "yyyy-mm-dd"),
    if (!is.null(help)) shiny::div(class = "metadata-help", help)
  )
}

.metadata_section <- function(title, ..., icon = NULL) {
  bslib::card(
    class = "metadata-card mb-3",
    full_screen = FALSE,
    bslib::card_header(
      shiny::div(
        class = "metadata-card-title",
        if (!is.null(icon)) bsicons::bs_icon(icon),
        shiny::tags$span(title)
      )
    ),
    bslib::card_body(...)
  )
}

.metadata_nav_buttons <- function(ns, current, prev_tab = NULL, next_tab = NULL) {
  shiny::div(
    class = "metadata-nav-buttons d-flex justify-content-between align-items-center mt-4",
    shiny::div(
      if (!is.null(prev_tab)) {
        shiny::actionButton(
          ns(paste0("prev_", current)),
          "Previous",
          class = "btn btn-outline-secondary"
        )
      }
    ),
    shiny::div(
      if (!is.null(next_tab)) {
        shiny::actionButton(
          ns(paste0("next_", current)),
          "Next",
          class = "btn btn-success"
        )
      }
    )
  )
}

# ---------------------------------------------------------
# Module UI
# ---------------------------------------------------------

#' Metadata module UI
#'
#' @param id Module id
#' @return Shiny UI
#' @export
mod_metadata_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::nav_panel(
    title = "Metadata",
    value = "metadata",

    shiny::tags$style(shiny::HTML("
      .metadata-page-title {
        font-size: 1.35rem;
        font-weight: 700;
        margin-bottom: .35rem;
      }
      .metadata-page-subtitle {
        color: #6b7280;
        margin-bottom: 1rem;
      }
      .metadata-help {
        font-size: .88rem;
        color: #6b7280;
        margin-top: -.35rem;
      }
      .metadata-note {
        background: rgba(27,153,139,.06);
        border: 1px solid rgba(27,153,139,.15);
        border-radius: 12px;
        padding: .85rem 1rem;
        margin-bottom: 1rem;
      }
      .metadata-card .card-header {
        background: rgba(27,153,139,.07);
      }
      .metadata-card-title {
        display: flex;
        align-items: center;
        gap: .5rem;
        font-weight: 650;
      }
      .metadata-pilllist .nav {
        gap: .35rem;
      }
      .metadata-pilllist .nav-link {
        border-radius: 12px;
        padding: .7rem .9rem;
        font-weight: 600;
      }
      .metadata-pilllist .nav-link.active {
        background: #1B998B !important;
        color: white !important;
      }
      .metadata-block {
        background: #fff;
        border: 1px solid rgba(0,0,0,.06);
        border-radius: 14px;
        padding: 1rem;
      }
      .metadata-repeat-title {
        font-weight: 650;
        margin-bottom: .75rem;
      }
      .metadata-divider {
        height: 1px;
        background: rgba(0,0,0,.08);
        margin: 1rem 0;
      }
      .metadata-nav-buttons {
        padding-top: .5rem;
      }
    ")),

    shiny::tags$script(shiny::HTML(sprintf("
      Shiny.addCustomMessageHandler('metadata-click-tab-%s', function(message) {
        console.log('DEBUG JS metadata-click-tab message:', message);

        var container = document.getElementById('%s');
        if (!container) {
          console.log('DEBUG JS: nav container not found');
          return;
        }

        var target = message.target;
        if (!target) {
          console.log('DEBUG JS: target missing');
          return;
        }

        var selectors = [
          '[data-value=\"' + target + '\"]',
          'a[data-value=\"' + target + '\"]',
          'button[data-value=\"' + target + '\"]'
        ];

        var el = null;
        for (var i = 0; i < selectors.length; i++) {
          el = container.querySelector(selectors[i]);
          if (el) break;
        }

        if (!el) {
          var allTabs = container.querySelectorAll('[data-value]');
          console.log(
            'DEBUG JS: target tab not found. Available data-values:',
            Array.from(allTabs).map(function(x) { return x.getAttribute('data-value'); })
          );
          return;
        }

        console.log('DEBUG JS: clicking tab ->', target, el);
        el.click();

        setTimeout(function() {
          var moduleRoot = container.closest('.tab-pane') || container;
          var topPos = moduleRoot.getBoundingClientRect().top + window.pageYOffset - 20;

          window.scrollTo({
            top: topPos,
            behavior: 'smooth'
          });
        }, 50);
      });
    ", id, ns("metadata_tabs")))),

    shiny::div(
      class = "metadata-page-title",
      "Dataset Metadata"
    ),
    shiny::div(
      class = "metadata-note",
      "The sections follow the structure of the GBIF IPT metadata form. Fields can be completed to facilitate metadata review among dataset authors, and then copied and pasted into the metadata form of the selected IPT for dataset submission."
    ),

    shiny::div(
      class = "metadata-pilllist",
      bslib::navset_pill_list(
        id = ns("metadata_tabs"),
        selected = "basic",
        well = TRUE,
        widths = c(3, 9),

        # ---------------------------------------------------
        # 1. Basic metadata
        # ---------------------------------------------------
        bslib::nav_panel(
          title = "Basic Metadata",
          value = "basic",
          shiny::div(
            class = "metadata-block",

            .metadata_section(
              "Resource identity",
              shiny::fluidRow(
                shiny::column(
                  12,
                  .metadata_text(
                    ns("basic_title"),
                    "Title *",
                    placeholder = "Ex.: Megafauna composition in Aurora seamount...",
                    help = "Título descritivo do recurso."
                  )
                )
              ),
              shiny::fluidRow(
                shiny::column(
                  6,
                  .metadata_text(
                    ns("basic_short_name"),
                    "Short Name",
                    placeholder = "Ex.: aurora_megafauna_2019",
                    help = "Nome curto do recurso."
                  )
                ),
                shiny::column(
                  6,
                  .metadata_select(
                    ns("basic_publishing_organization"),
                    "Publishing Organization *",
                    choices = c("", "No organization"),
                    help = "Organização responsável pela publicação do recurso."
                  )
                )
              )
            ),

            .metadata_section(
              "Resource type",
              shiny::fluidRow(
                shiny::column(
                  4,
                  .metadata_select(
                    ns("basic_type"),
                    "Type *",
                    choices = c("", "Sampling event", "Occurrence", "Checklist", "Metadata-only")
                  )
                ),
                shiny::column(
                  4,
                  .metadata_select(
                    ns("basic_subtype"),
                    "Subtype",
                    choices = c(
                      "",
                      "Taxonomic Authority",
                      "Nomenclator Authority",
                      "Thematic",
                      "Inventory Regional",
                      "Global Species Dataset",
                      "Derived from Occurrence",
                      "Specimen",
                      "Observation"
                    ),
                    help = "Opção inspirada na folha info da planilha."
                  )
                ),
                shiny::column(
                  4,
                  .metadata_select(
                    ns("basic_update_frequency"),
                    "Update Frequency *",
                    choices = c(
                      "",
                      "Not planned",
                      "As needed",
                      "Daily",
                      "Weekly",
                      "Monthly",
                      "Quarterly",
                      "Annually",
                      "Unknown"
                    )
                  )
                )
              )
            ),

            .metadata_section(
              "Languages and licence",
              shiny::fluidRow(
                shiny::column(
                  4,
                  .metadata_select(
                    ns("basic_data_language"),
                    "Data Language *",
                    choices = c("", "English", "Portuguese", "Spanish", "French")
                  )
                ),
                shiny::column(
                  4,
                  .metadata_select(
                    ns("basic_metadata_language"),
                    "Metadata Language *",
                    choices = c("", "English", "Portuguese", "Spanish", "French")
                  )
                ),
                shiny::column(
                  4,
                  .metadata_select(
                    ns("basic_data_licence"),
                    "Data Licence *",
                    choices = c("", "CC0 1.0", "CC-BY 4.0", "CC-BY-NC 4.0", "Other")
                  )
                )
              )
            ),

            .metadata_section(
              "Narrative fields",
              .metadata_textarea(
                ns("basic_description"),
                "Description *",
                rows = 7,
                help = "Visão geral do recurso."
              ),
              .metadata_textarea(
                ns("basic_maintenance"),
                "Maintenance",
                rows = 4,
                help = "Descrição geral da manutenção do recurso."
              ),
              .metadata_textarea(
                ns("basic_maintenance_description"),
                "Maintenance Description",
                rows = 4,
                help = "Descrição da frequência de manutenção."
              )
            ),

            .metadata_nav_buttons(ns, current = "basic", next_tab = "contacts")
          )
        ),

        # ---------------------------------------------------
        # 2. Contacts
        # ---------------------------------------------------
        bslib::nav_panel(
          title = "Contacts",
          value = "contacts",
          shiny::div(
            class = "metadata-block",
            .metadata_section(
              "Resource contacts",
              shiny::numericInput(
                ns("contacts_n"),
                "Number of contacts",
                value = 2, min = 1, max = 20, width = "220px"
              ),
              shiny::uiOutput(ns("contacts_ui"))
            ),
            .metadata_nav_buttons(ns, current = "contacts", prev_tab = "basic", next_tab = "ack")
          )
        ),

        # ---------------------------------------------------
        # 3. Acknowledgements
        # ---------------------------------------------------
        bslib::nav_panel(
          title = "Acknowledgements",
          value = "ack",
          shiny::div(
            class = "metadata-block",
            .metadata_section(
              "Acknowledgements",
              .metadata_textarea(
                ns("ack_text"),
                "Metadata that acknowledges funders and contributors",
                rows = 8,
                help = "Agradecimentos a financiadores e principais colaboradores."
              )
            ),
            .metadata_nav_buttons(ns, current = "ack", prev_tab = "contacts", next_tab = "geo")
          )
        ),

        # ---------------------------------------------------
        # 4. Geographical Coverage
        # ---------------------------------------------------
        bslib::nav_panel(
          title = "Geographical Coverage",
          value = "geo",
          shiny::div(
            class = "metadata-block",
            .metadata_section(
              "Bounding box",
              shiny::fluidRow(
                shiny::column(3, shiny::numericInput(ns("geo_west"),  "West *",  value = NA, width = "100%")),
                shiny::column(3, shiny::numericInput(ns("geo_east"),  "East *",  value = NA, width = "100%")),
                shiny::column(3, shiny::numericInput(ns("geo_south"), "South *", value = NA, width = "100%")),
                shiny::column(3, shiny::numericInput(ns("geo_north"), "North *", value = NA, width = "100%"))
              ),
              .metadata_textarea(
                ns("geo_description"),
                "Description *",
                rows = 4,
                help = "Descrição geográfica da área de estudo."
              )
            ),
            .metadata_nav_buttons(ns, current = "geo", prev_tab = "ack", next_tab = "tax")
          )
        ),

        # ---------------------------------------------------
        # 5. Taxonomic Coverage
        # ---------------------------------------------------
        bslib::nav_panel(
          title = "Taxonomic Coverage",
          value = "tax",
          shiny::div(
            class = "metadata-block",
            .metadata_section(
              "Taxonomic scope",
              .metadata_textarea(
                ns("tax_description"),
                "Description",
                rows = 4,
                help = "Descrição do escopo taxonómico."
              ),
              shiny::fluidRow(
                shiny::column(
                  6,
                  .metadata_text(
                    ns("tax_kingdom"),
                    "Kingdom",
                    placeholder = "Ex.: Animalia"
                  )
                ),
                shiny::column(
                  6,
                  .metadata_text(
                    ns("tax_order"),
                    "Order",
                    placeholder = "Ex.: Decapoda, Actiniaria, Amphipoda..."
                  )
                )
              )
            ),
            .metadata_nav_buttons(ns, current = "tax", prev_tab = "geo", next_tab = "temp")
          )
        ),

        # ---------------------------------------------------
        # 6. Temporal Coverage
        # ---------------------------------------------------
        bslib::nav_panel(
          title = "Temporal Coverage",
          value = "temp",
          shiny::div(
            class = "metadata-block",
            .metadata_section(
              "Temporal extent",
              shiny::fluidRow(
                shiny::column(6, .metadata_date(ns("temp_start_date"), "Start Date")),
                shiny::column(6, .metadata_date(ns("temp_end_date"), "End Date"))
              )
            ),
            .metadata_nav_buttons(ns, current = "temp", prev_tab = "tax", next_tab = "adddesc")
          )
        ),

        # ---------------------------------------------------
        # 7. Additional Description
        # ---------------------------------------------------
        bslib::nav_panel(
          title = "Additional Description",
          value = "adddesc",
          shiny::div(
            class = "metadata-block",
            .metadata_section(
              "Extended narrative",
              .metadata_textarea(ns("adddesc_purpose"), "Purpose", rows = 4),
              .metadata_textarea(ns("adddesc_intro"), "Introduction", rows = 6),
              .metadata_textarea(ns("adddesc_getting_started"), "Getting Started", rows = 6)
            ),
            .metadata_nav_buttons(ns, current = "adddesc", prev_tab = "temp", next_tab = "keywords")
          )
        ),

        # ---------------------------------------------------
        # 8. Keywords
        # ---------------------------------------------------
        bslib::nav_panel(
          title = "Keywords",
          value = "keywords",
          shiny::div(
            class = "metadata-block",
            .metadata_section(
              "Keyword sets",
              shiny::numericInput(
                ns("keywords_n"),
                "Number of keyword groups",
                value = 2, min = 1, max = 15, width = "260px"
              ),
              shiny::uiOutput(ns("keywords_ui"))
            ),
            .metadata_nav_buttons(ns, current = "keywords", prev_tab = "adddesc", next_tab = "project")
          )
        ),

        # ---------------------------------------------------
        # 9. Project Data
        # ---------------------------------------------------
        bslib::nav_panel(
          title = "Project Data",
          value = "project",
          shiny::div(
            class = "metadata-block",

            .metadata_section(
              "Project description",
              .metadata_text(ns("project_title"), "Project Title"),
              .metadata_textarea(ns("project_funding"), "Funding", rows = 4),
              .metadata_textarea(ns("project_study_area"), "Study Area Description", rows = 5),
              .metadata_textarea(ns("project_design"), "Design Description", rows = 5)
            ),

            .metadata_section(
              "Project personnel",
              shiny::numericInput(
                ns("project_personnel_n"),
                "Number of project personnel",
                value = 2, min = 1, max = 20, width = "260px"
              ),
              shiny::uiOutput(ns("project_personnel_ui"))
            ),

            .metadata_nav_buttons(ns, current = "project", prev_tab = "keywords", next_tab = "sampling")
          )
        ),

        # ---------------------------------------------------
        # 10. Sampling Methods
        # ---------------------------------------------------
        bslib::nav_panel(
          title = "Sampling Methods",
          value = "sampling",
          shiny::div(
            class = "metadata-block",
            .metadata_section(
              "Sampling methodology",
              .metadata_textarea(ns("sampling_study_extent"), "Study Extent *", rows = 4),
              .metadata_textarea(ns("sampling_description"), "Sampling Description *", rows = 5),
              .metadata_textarea(ns("sampling_quality_control"), "Quality Control", rows = 4),
              .metadata_textarea(ns("sampling_step_description"), "Step Description *", rows = 6)
            ),
            .metadata_nav_buttons(ns, current = "sampling", prev_tab = "project", next_tab = "citations")
          )
        ),

        # ---------------------------------------------------
        # 11. Citations
        # ---------------------------------------------------
        bslib::nav_panel(
          title = "Citations",
          value = "citations",
          shiny::div(
            class = "metadata-block",

            .metadata_section(
              "Main citation",
              .metadata_textarea(
                ns("citation_resource"),
                "Resource Citation *",
                rows = 5
              ),
              .metadata_text(
                ns("citation_resource_identifier"),
                "Resource Citation Identifier"
              )
            ),

            .metadata_section(
              "Bibliographic citations",
              shiny::numericInput(
                ns("citations_n"),
                "Number of bibliographic citations",
                value = 1, min = 0, max = 20, width = "280px"
              ),
              shiny::uiOutput(ns("citations_ui"))
            ),

            .metadata_nav_buttons(ns, current = "citations", prev_tab = "sampling", next_tab = "collections")
          )
        ),

        # ---------------------------------------------------
        # 12. Collection Data
        # ---------------------------------------------------
        bslib::nav_panel(
          title = "Collection Data",
          value = "collections",
          shiny::div(
            class = "metadata-block",

            .metadata_section(
              "Collections",
              shiny::numericInput(
                ns("collections_n"),
                "Number of collections",
                value = 0, min = 0, max = 20, width = "220px"
              ),
              shiny::uiOutput(ns("collections_ui"))
            ),

            .metadata_section(
              "Specimen preservation methods",
              shiny::numericInput(
                ns("preservation_n"),
                "Number of preservation methods",
                value = 0, min = 0, max = 20, width = "260px"
              ),
              shiny::uiOutput(ns("preservation_ui"))
            ),

            .metadata_section(
              "Curatorial units",
              shiny::numericInput(
                ns("curatorial_n"),
                "Number of curatorial unit entries",
                value = 0, min = 0, max = 20, width = "280px"
              ),
              shiny::uiOutput(ns("curatorial_ui"))
            ),

            .metadata_nav_buttons(ns, current = "collections", prev_tab = "citations", next_tab = "external")
          )
        ),

        # ---------------------------------------------------
        # 13. External Links
        # ---------------------------------------------------
        bslib::nav_panel(
          title = "External Links",
          value = "external",
          shiny::div(
            class = "metadata-block",
            .metadata_section(
              "Links",
              .metadata_text(ns("external_homepage"), "Resource Homepage"),
              shiny::numericInput(
                ns("external_formats_n"),
                "Number of other data formats",
                value = 0, min = 0, max = 20, width = "260px"
              ),
              shiny::uiOutput(ns("external_formats_ui"))
            ),
            .metadata_nav_buttons(ns, current = "external", prev_tab = "collections", next_tab = "meta")
          )
        ),

        # ---------------------------------------------------
        # 14. Additional Metadata
        # ---------------------------------------------------
        bslib::nav_panel(
          title = "Additional Metadata",
          value = "meta",
          shiny::div(
            class = "metadata-block",
            .metadata_section(
              "Additional metadata",
              .metadata_date(ns("meta_date_first_published"), "Date First Published"),
              .metadata_date(ns("meta_date_last_published"), "Date Last Published"),
              .metadata_text(ns("meta_logo_url"), "Resource Logo URL"),
              .metadata_textarea(ns("meta_additional_information"), "Additional Information", rows = 5)
            ),
            .metadata_section(
              "Alternative identifiers",
              shiny::numericInput(
                ns("alt_ids_n"),
                "Number of alternative identifiers",
                value = 0, min = 0, max = 20, width = "260px"
              ),
              shiny::uiOutput(ns("alt_ids_ui"))
            ),
            .metadata_nav_buttons(ns, current = "meta", prev_tab = "external", next_tab = "export")
          )
        ),

        # ---------------------------------------------------
        # 15. Info / reference
        # ---------------------------------------------------
        bslib::nav_panel(
          title = "Export",
          value = "export",
          shiny::div(
            class = "metadata-block",
            .metadata_section(
              "Export metadata",
              shiny::downloadButton(ns("export_metadata_ods"), "Export workbook", class = "btn btn-success")
            ),
            .metadata_nav_buttons(ns, current = "export", prev_tab = "meta")
          )
        )
      )
    )
  )
}

# ---------------------------------------------------------
# Module server
# ---------------------------------------------------------

#' Metadata module server
#'
#' @param id Module id
#' @return A list of reactives with metadata content
#' @export
mod_metadata_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    role_choices <- c(
      "",
      "Author",
      "Content Provider",
      "Custodian Steward",
      "Distributor",
      "Editor",
      "Metadata Provider",
      "Originator",
      "Owner",
      "Point Of Contact",
      "Principal Investigator",
      "Processor",
      "Publisher",
      "User",
      "Programmer",
      "Curator",
      "Reviewer"
    )

    tab_order <- c(
      "basic",
      "contacts",
      "ack",
      "geo",
      "tax",
      "temp",
      "adddesc",
      "keywords",
      "project",
      "sampling",
      "citations",
      "collections",
      "external",
      "meta",
      "export"
    )

    go_to_tab <- function(target_tab) {
      cat("DEBUG go_to_tab() ->", target_tab, "\n")
      session$sendCustomMessage(
        paste0("metadata-click-tab-", id),
        list(target = target_tab)
      )
    }

    for (i in seq_along(tab_order)) {
      current <- tab_order[i]
      prev_tab <- if (i > 1) tab_order[i - 1] else NULL
      next_tab <- if (i < length(tab_order)) tab_order[i + 1] else NULL

      if (!is.null(prev_tab)) {
        local({
          btn_id <- paste0("prev_", current)
          target <- prev_tab
          shiny::observeEvent(input[[btn_id]], {
            cat("DEBUG click ->", btn_id, "| target:", target, "\n")
            go_to_tab(target)
          }, ignoreInit = TRUE)
        })
      }

      if (!is.null(next_tab)) {
        local({
          btn_id <- paste0("next_", current)
          target <- next_tab
          shiny::observeEvent(input[[btn_id]], {
            cat("DEBUG click ->", btn_id, "| target:", target, "\n")
            go_to_tab(target)
          }, ignoreInit = TRUE)
        })
      }
    }

    shiny::observe({
      cat("DEBUG current metadata tab input ->", input$metadata_tabs %||% "NULL", "\n")
    })


    export_metadata_path <- shiny::reactive({
      req <- shiny::req
      req(TRUE)
      path <- tempfile(fileext = ".xlsx")

      basic_df <- data.frame(field = names(basic()), value = unlist(basic(), use.names = FALSE), stringsAsFactors = FALSE)
      geo_df <- data.frame(field = names(geographical()), value = unlist(geographical(), use.names = FALSE), stringsAsFactors = FALSE)
      tax_df <- data.frame(field = names(taxonomic()), value = unlist(taxonomic(), use.names = FALSE), stringsAsFactors = FALSE)
      temp_df <- data.frame(field = names(temporal()), value = unlist(temporal(), use.names = FALSE), stringsAsFactors = FALSE)
      adddesc_df <- data.frame(field = names(additional_description()), value = unlist(additional_description(), use.names = FALSE), stringsAsFactors = FALSE)
      project_raw <- project()
      project_df <- data.frame(
        field = c("title", "funding", "studyAreaDescription", "designDescription"),
        value = c(project_raw$title, project_raw$funding, project_raw$studyAreaDescription, project_raw$designDescription),
        stringsAsFactors = FALSE
      )
      sampling_df <- data.frame(field = names(sampling_methods()), value = unlist(sampling_methods(), use.names = FALSE), stringsAsFactors = FALSE)
      citations_raw <- citations()
      citations_main_df <- data.frame(
        field = c("resourceCitation", "resourceCitationIdentifier"),
        value = c(citations_raw$resourceCitation, citations_raw$resourceCitationIdentifier),
        stringsAsFactors = FALSE
      )
      ext_raw <- list(resourceHomepage = input$external_homepage, otherDataFormats = external_formats())
      ext_links_df <- data.frame(field = "resourceHomepage", value = ext_raw$resourceHomepage, stringsAsFactors = FALSE)
      addmeta_df <- data.frame(field = names(additional_metadata()[1:4]), value = unlist(additional_metadata()[1:4], use.names = FALSE), stringsAsFactors = FALSE)
      ack_df <- data.frame(field = "acknowledgements", value = input$ack_text %||% "", stringsAsFactors = FALSE)

      sheets <- list(
        Basic = basic_df,
        Contacts = contacts(),
        Acknowledgements = ack_df,
        Geographical = geo_df,
        Taxonomic = tax_df,
        Temporal = temp_df,
        AdditionalDescription = adddesc_df,
        Keywords = keywords(),
        Project = project_df,
        ProjectPersonnel = project_personnel(),
        SamplingMethods = sampling_df,
        Citations = citations_main_df,
        BibliographicCitations = bibliographic_citations(),
        Collections = collections(),
        PreservationMethods = data.frame(method = preservation_methods(), stringsAsFactors = FALSE),
        CuratorialUnits = curatorial_units(),
        ExternalLinks = ext_links_df,
        OtherDataFormats = external_formats(),
        AdditionalMetadata = addmeta_df,
        AlternativeIdentifiers = data.frame(identifier = alternative_identifiers(), stringsAsFactors = FALSE)
      )

      sheets <- lapply(sheets, function(df) {
        if (is.null(df) || nrow(df) == 0) data.frame(note = "No data", stringsAsFactors = FALSE) else df
      })

      readODS::write_ods(x = sheets, path = path, append = FALSE)
      path
    })


    output$export_metadata_ods <- shiny::downloadHandler(
      filename = function() {
        paste0("metadata_export_", Sys.Date(), ".ods")
      },
      content = function(file) {
        # Get the path from the reactive
        src <- export_metadata_path()
        
        # Copy the generated ODS file to the download destination
        file.copy(src, file, overwrite = TRUE)
      }
    )

    # ---------------------------
    # Dynamic UI: Contacts
    # ---------------------------
    output$contacts_ui <- shiny::renderUI({
      n <- input$contacts_n %||% 1

      shiny::tagList(
        lapply(seq_len(n), function(i) {
          shiny::tagList(
            shiny::div(class = "metadata-divider"),
            shiny::div(class = "metadata-repeat-title", paste("Contact", i)),
            shiny::fluidRow(
              shiny::column(4, shiny::textInput(ns(paste0("contact_first_name_", i)), "First Name")),
              shiny::column(4, shiny::textInput(ns(paste0("contact_last_name_", i)), "Last Name")),
              shiny::column(4, shiny::textInput(ns(paste0("contact_salutation_", i)), "Salutation"))
            ),
            shiny::fluidRow(
              shiny::column(6, shiny::textInput(ns(paste0("contact_organization_", i)), "Organization")),
              shiny::column(6, shiny::textInput(ns(paste0("contact_position_", i)), "Position"))
            ),
            shiny::textInput(ns(paste0("contact_address_", i)), "Address"),
            shiny::fluidRow(
              shiny::column(4, shiny::textInput(ns(paste0("contact_city_", i)), "City")),
              shiny::column(4, shiny::textInput(ns(paste0("contact_state_", i)), "State/Province")),
              shiny::column(4, shiny::textInput(ns(paste0("contact_country_", i)), "Country"))
            ),
            shiny::fluidRow(
              shiny::column(4, shiny::textInput(ns(paste0("contact_postal_code_", i)), "Postal Code")),
              shiny::column(4, shiny::textInput(ns(paste0("contact_phone_", i)), "Phone")),
              shiny::column(4, shiny::textInput(ns(paste0("contact_email_", i)), "Email"))
            ),
            shiny::fluidRow(
              shiny::column(6, shiny::textInput(ns(paste0("contact_homepage_", i)), "Home Page")),
              shiny::column(6, shiny::textInput(ns(paste0("contact_personnel_directory_", i)), "Personnel Directory"))
            ),
            shiny::textInput(ns(paste0("contact_personnel_identifier_", i)), "Personnel Identifier")
          )
        })
      )
    })

    # ---------------------------
    # Dynamic UI: Keywords
    # ---------------------------
    output$keywords_ui <- shiny::renderUI({
      n <- input$keywords_n %||% 1

      shiny::tagList(
        lapply(seq_len(n), function(i) {
          shiny::tagList(
            shiny::div(class = "metadata-divider"),
            shiny::div(class = "metadata-repeat-title", paste("Keyword group", i)),
            shiny::textInput(ns(paste0("keyword_keywords_", i)), "Keywords", placeholder = "keyword1, keyword2, keyword3"),
            shiny::textInput(ns(paste0("keyword_thesaurus_", i)), "Thesaurus / Vocabulary"),
            shiny::textInput(ns(paste0("keyword_uri_", i)), "Vocabulary URI")
          )
        })
      )
    })

    # ---------------------------
    # Dynamic UI: Project personnel
    # ---------------------------
    output$project_personnel_ui <- shiny::renderUI({
      n <- input$project_personnel_n %||% 1

      shiny::tagList(
        lapply(seq_len(n), function(i) {
          shiny::tagList(
            shiny::div(class = "metadata-divider"),
            shiny::div(class = "metadata-repeat-title", paste("Project person", i)),
            shiny::fluidRow(
              shiny::column(4, shiny::textInput(ns(paste0("proj_first_name_", i)), "First Name")),
              shiny::column(4, shiny::textInput(ns(paste0("proj_last_name_", i)), "Last Name")),
              shiny::column(4, shiny::selectInput(ns(paste0("proj_role_", i)), "Role", choices = role_choices))
            ),
            shiny::fluidRow(
              shiny::column(6, shiny::textInput(ns(paste0("proj_organization_", i)), "Organization")),
              shiny::column(6, shiny::textInput(ns(paste0("proj_position_", i)), "Position"))
            ),
            shiny::textInput(ns(paste0("proj_address_", i)), "Address"),
            shiny::fluidRow(
              shiny::column(4, shiny::textInput(ns(paste0("proj_city_", i)), "City")),
              shiny::column(4, shiny::textInput(ns(paste0("proj_state_", i)), "State/Province")),
              shiny::column(4, shiny::textInput(ns(paste0("proj_country_", i)), "Country"))
            ),
            shiny::fluidRow(
              shiny::column(4, shiny::textInput(ns(paste0("proj_postal_code_", i)), "Postal Code")),
              shiny::column(4, shiny::textInput(ns(paste0("proj_phone_", i)), "Phone")),
              shiny::column(4, shiny::textInput(ns(paste0("proj_email_", i)), "Email"))
            ),
            shiny::fluidRow(
              shiny::column(6, shiny::textInput(ns(paste0("proj_homepage_", i)), "Home Page")),
              shiny::column(6, shiny::textInput(ns(paste0("proj_personnel_directory_", i)), "Personnel Directory"))
            ),
            shiny::textInput(ns(paste0("proj_personnel_identifier_", i)), "Personnel Identifier")
          )
        })
      )
    })

    # ---------------------------
    # Dynamic UI: Citations
    # ---------------------------
    output$citations_ui <- shiny::renderUI({
      n <- input$citations_n %||% 0

      if (n == 0) return(NULL)

      shiny::tagList(
        lapply(seq_len(n), function(i) {
          shiny::tagList(
            shiny::div(class = "metadata-divider"),
            shiny::div(class = "metadata-repeat-title", paste("Bibliographic citation", i)),
            shiny::textAreaInput(ns(paste0("bib_citation_", i)), "Bibliographic Citation", rows = 4, width = "100%"),
            shiny::textInput(ns(paste0("bib_identifier_", i)), "Bibliographic Citation Identifier")
          )
        })
      )
    })

    # ---------------------------
    # Dynamic UI: Collections
    # ---------------------------
    output$collections_ui <- shiny::renderUI({
      n <- input$collections_n %||% 0
      if (n == 0) return(NULL)

      shiny::tagList(
        lapply(seq_len(n), function(i) {
          shiny::tagList(
            shiny::div(class = "metadata-divider"),
            shiny::div(class = "metadata-repeat-title", paste("Collection", i)),
            shiny::textInput(ns(paste0("collection_name_", i)), "Collection Name"),
            shiny::textInput(ns(paste0("collection_identifier_", i)), "Collection Identifier"),
            shiny::textInput(ns(paste0("collection_parent_identifier_", i)), "Parent Collection Identifier")
          )
        })
      )
    })

    output$preservation_ui <- shiny::renderUI({
      n <- input$preservation_n %||% 0
      if (n == 0) return(NULL)

      shiny::tagList(
        lapply(seq_len(n), function(i) {
          shiny::tagList(
            shiny::div(class = "metadata-divider"),
            shiny::textInput(ns(paste0("preservation_method_", i)), paste("Specimen preservation method", i))
          )
        })
      )
    })

    output$curatorial_ui <- shiny::renderUI({
      n <- input$curatorial_n %||% 0
      if (n == 0) return(NULL)

      shiny::tagList(
        lapply(seq_len(n), function(i) {
          shiny::tagList(
            shiny::div(class = "metadata-divider"),
            shiny::div(class = "metadata-repeat-title", paste("Curatorial unit", i)),
            shiny::fluidRow(
              shiny::column(4, shiny::textInput(ns(paste0("curatorial_method_type_", i)), "Method Type")),
              shiny::column(4, shiny::textInput(ns(paste0("curatorial_between_", i)), "Between")),
              shiny::column(4, shiny::textInput(ns(paste0("curatorial_and_", i)), "and"))
            ),
            shiny::textInput(ns(paste0("curatorial_unit_type_", i)), "Unit Type")
          )
        })
      )
    })

    # ---------------------------
    # Dynamic UI: External formats
    # ---------------------------
    output$external_formats_ui <- shiny::renderUI({
      n <- input$external_formats_n %||% 0
      if (n == 0) return(NULL)

      shiny::tagList(
        lapply(seq_len(n), function(i) {
          shiny::tagList(
            shiny::div(class = "metadata-divider"),
            shiny::div(class = "metadata-repeat-title", paste("Other data format", i)),
            shiny::textInput(ns(paste0("ext_name_", i)), "Name"),
            shiny::fluidRow(
              shiny::column(6, shiny::textInput(ns(paste0("ext_charset_", i)), "Character Set")),
              shiny::column(6, shiny::textInput(ns(paste0("ext_download_url_", i)), "Download URL"))
            ),
            shiny::fluidRow(
              shiny::column(6, shiny::textInput(ns(paste0("ext_data_format_", i)), "Data Format")),
              shiny::column(6, shiny::textInput(ns(paste0("ext_data_format_version_", i)), "Data Format Version"))
            )
          )
        })
      )
    })

    # ---------------------------
    # Dynamic UI: Alternative IDs
    # ---------------------------
    output$alt_ids_ui <- shiny::renderUI({
      n <- input$alt_ids_n %||% 0
      if (n == 0) return(NULL)

      shiny::tagList(
        lapply(seq_len(n), function(i) {
          shiny::tagList(
            shiny::div(class = "metadata-divider"),
            shiny::textInput(ns(paste0("alt_identifier_", i)), paste("Alternative Identifier", i))
          )
        })
      )
    })

    # ---------------------------
    # Reactive collectors
    # ---------------------------
    contacts <- shiny::reactive({
      n <- input$contacts_n %||% 0
      if (n == 0) return(data.frame())

      do.call(
        rbind,
        lapply(seq_len(n), function(i) {
          data.frame(
            firstName = input[[paste0("contact_first_name_", i)]] %||% "",
            lastName = input[[paste0("contact_last_name_", i)]] %||% "",
            salutation = input[[paste0("contact_salutation_", i)]] %||% "",
            organization = input[[paste0("contact_organization_", i)]] %||% "",
            position = input[[paste0("contact_position_", i)]] %||% "",
            address = input[[paste0("contact_address_", i)]] %||% "",
            city = input[[paste0("contact_city_", i)]] %||% "",
            stateProvince = input[[paste0("contact_state_", i)]] %||% "",
            country = input[[paste0("contact_country_", i)]] %||% "",
            postalCode = input[[paste0("contact_postal_code_", i)]] %||% "",
            phone = input[[paste0("contact_phone_", i)]] %||% "",
            email = input[[paste0("contact_email_", i)]] %||% "",
            homePage = input[[paste0("contact_homepage_", i)]] %||% "",
            personnelDirectory = input[[paste0("contact_personnel_directory_", i)]] %||% "",
            personnelIdentifier = input[[paste0("contact_personnel_identifier_", i)]] %||% "",
            stringsAsFactors = FALSE
          )
        })
      )
    })

    keywords <- shiny::reactive({
      n <- input$keywords_n %||% 0
      if (n == 0) return(data.frame())

      do.call(
        rbind,
        lapply(seq_len(n), function(i) {
          data.frame(
            keywords = input[[paste0("keyword_keywords_", i)]] %||% "",
            thesaurus = input[[paste0("keyword_thesaurus_", i)]] %||% "",
            vocabularyUri = input[[paste0("keyword_uri_", i)]] %||% "",
            stringsAsFactors = FALSE
          )
        })
      )
    })

    project_personnel <- shiny::reactive({
      n <- input$project_personnel_n %||% 0
      if (n == 0) return(data.frame())

      do.call(
        rbind,
        lapply(seq_len(n), function(i) {
          data.frame(
            firstName = input[[paste0("proj_first_name_", i)]] %||% "",
            lastName = input[[paste0("proj_last_name_", i)]] %||% "",
            role = input[[paste0("proj_role_", i)]] %||% "",
            organization = input[[paste0("proj_organization_", i)]] %||% "",
            position = input[[paste0("proj_position_", i)]] %||% "",
            address = input[[paste0("proj_address_", i)]] %||% "",
            city = input[[paste0("proj_city_", i)]] %||% "",
            stateProvince = input[[paste0("proj_state_", i)]] %||% "",
            country = input[[paste0("proj_country_", i)]] %||% "",
            postalCode = input[[paste0("proj_postal_code_", i)]] %||% "",
            phone = input[[paste0("proj_phone_", i)]] %||% "",
            email = input[[paste0("proj_email_", i)]] %||% "",
            homePage = input[[paste0("proj_homepage_", i)]] %||% "",
            personnelDirectory = input[[paste0("proj_personnel_directory_", i)]] %||% "",
            personnelIdentifier = input[[paste0("proj_personnel_identifier_", i)]] %||% "",
            stringsAsFactors = FALSE
          )
        })
      )
    })

    bibliographic_citations <- shiny::reactive({
      n <- input$citations_n %||% 0
      if (n == 0) return(data.frame())

      do.call(
        rbind,
        lapply(seq_len(n), function(i) {
          data.frame(
            citation = input[[paste0("bib_citation_", i)]] %||% "",
            identifier = input[[paste0("bib_identifier_", i)]] %||% "",
            stringsAsFactors = FALSE
          )
        })
      )
    })

    collections <- shiny::reactive({
      n <- input$collections_n %||% 0
      if (n == 0) return(data.frame())

      do.call(
        rbind,
        lapply(seq_len(n), function(i) {
          data.frame(
            collectionName = input[[paste0("collection_name_", i)]] %||% "",
            collectionIdentifier = input[[paste0("collection_identifier_", i)]] %||% "",
            parentCollectionIdentifier = input[[paste0("collection_parent_identifier_", i)]] %||% "",
            stringsAsFactors = FALSE
          )
        })
      )
    })

    preservation_methods <- shiny::reactive({
      n <- input$preservation_n %||% 0
      if (n == 0) return(character(0))

      vapply(seq_len(n), function(i) {
        input[[paste0("preservation_method_", i)]] %||% ""
      }, character(1))
    })

    curatorial_units <- shiny::reactive({
      n <- input$curatorial_n %||% 0
      if (n == 0) return(data.frame())

      do.call(
        rbind,
        lapply(seq_len(n), function(i) {
          data.frame(
            methodType = input[[paste0("curatorial_method_type_", i)]] %||% "",
            between = input[[paste0("curatorial_between_", i)]] %||% "",
            and = input[[paste0("curatorial_and_", i)]] %||% "",
            unitType = input[[paste0("curatorial_unit_type_", i)]] %||% "",
            stringsAsFactors = FALSE
          )
        })
      )
    })

    external_formats <- shiny::reactive({
      n <- input$external_formats_n %||% 0
      if (n == 0) return(data.frame())

      do.call(
        rbind,
        lapply(seq_len(n), function(i) {
          data.frame(
            name = input[[paste0("ext_name_", i)]] %||% "",
            characterSet = input[[paste0("ext_charset_", i)]] %||% "",
            downloadUrl = input[[paste0("ext_download_url_", i)]] %||% "",
            dataFormat = input[[paste0("ext_data_format_", i)]] %||% "",
            dataFormatVersion = input[[paste0("ext_data_format_version_", i)]] %||% "",
            stringsAsFactors = FALSE
          )
        })
      )
    })

    alternative_identifiers <- shiny::reactive({
      n <- input$alt_ids_n %||% 0
      if (n == 0) return(character(0))

      vapply(seq_len(n), function(i) {
        input[[paste0("alt_identifier_", i)]] %||% ""
      }, character(1))
    })

    basic <- shiny::reactive({
      list(
        title = input$basic_title,
        shortName = input$basic_short_name,
        publishingOrganization = input$basic_publishing_organization,
        type = input$basic_type,
        subtype = input$basic_subtype,
        dataLanguage = input$basic_data_language,
        metadataLanguage = input$basic_metadata_language,
        dataLicence = input$basic_data_licence,
        description = input$basic_description,
        maintenance = input$basic_maintenance,
        updateFrequency = input$basic_update_frequency,
        maintenanceDescription = input$basic_maintenance_description
      )
    })

    geographical <- shiny::reactive({
      list(
        west = input$geo_west,
        east = input$geo_east,
        south = input$geo_south,
        north = input$geo_north,
        description = input$geo_description
      )
    })

    taxonomic <- shiny::reactive({
      list(
        description = input$tax_description,
        kingdom = input$tax_kingdom,
        order = input$tax_order
      )
    })

    temporal <- shiny::reactive({
      list(
        startDate = input$temp_start_date,
        endDate = input$temp_end_date
      )
    })

    additional_description <- shiny::reactive({
      list(
        purpose = input$adddesc_purpose,
        introduction = input$adddesc_intro,
        gettingStarted = input$adddesc_getting_started
      )
    })

    project <- shiny::reactive({
      list(
        title = input$project_title,
        funding = input$project_funding,
        studyAreaDescription = input$project_study_area,
        designDescription = input$project_design,
        personnel = project_personnel()
      )
    })

    sampling_methods <- shiny::reactive({
      list(
        studyExtent = input$sampling_study_extent,
        samplingDescription = input$sampling_description,
        qualityControl = input$sampling_quality_control,
        stepDescription = input$sampling_step_description
      )
    })

    citations <- shiny::reactive({
      list(
        resourceCitation = input$citation_resource,
        resourceCitationIdentifier = input$citation_resource_identifier,
        bibliographicCitations = bibliographic_citations()
      )
    })

    additional_metadata <- shiny::reactive({
      list(
        dateFirstPublished = input$meta_date_first_published,
        dateLastPublished = input$meta_date_last_published,
        resourceLogoUrl = input$meta_logo_url,
        additionalInformation = input$meta_additional_information,
        alternativeIdentifiers = alternative_identifiers()
      )
    })

    all_metadata <- shiny::reactive({
      list(
        basic = basic(),
        contacts = contacts(),
        acknowledgements = input$ack_text,
        geographicalCoverage = geographical(),
        taxonomicCoverage = taxonomic(),
        temporalCoverage = temporal(),
        additionalDescription = additional_description(),
        keywords = keywords(),
        project = project(),
        samplingMethods = sampling_methods(),
        citations = citations(),
        collectionData = list(
          collections = collections(),
          specimenPreservationMethods = preservation_methods(),
          curatorialUnits = curatorial_units()
        ),
        externalLinks = list(
          resourceHomepage = input$external_homepage,
          otherDataFormats = external_formats()
        ),
        additionalMetadata = additional_metadata()
      )
    })

    list(
      basic = basic,
      contacts = contacts,
      geographical = geographical,
      taxonomic = taxonomic,
      temporal = temporal,
      keywords = keywords,
      project = project,
      sampling_methods = sampling_methods,
      citations = citations,
      collection_data = shiny::reactive({
        list(
          collections = collections(),
          specimenPreservationMethods = preservation_methods(),
          curatorialUnits = curatorial_units()
        )
      }),
      external_links = shiny::reactive({
        list(
          resourceHomepage = input$external_homepage,
          otherDataFormats = external_formats()
        )
      }),
      additional_metadata = additional_metadata,
      all = all_metadata
    )
  })
}