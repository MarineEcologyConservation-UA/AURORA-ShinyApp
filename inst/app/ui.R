ui <- bslib::page_navbar(
  id = "main_nav",
  title = shiny::div(
    style = "display:flex; align-items:center; gap:.65rem;",
    shiny::tags$img(
      src = "aurora_logo.png",
      alt = "AURORA logo",
      style = "height:34px; width:auto;"
    ),
    shiny::tags$span("AURORA Biodiversity Data App")
  ),

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

      .nav-link.disabled-tab,
      .dropdown-item.disabled-tab {
        pointer-events: none !important;
        opacity: .5 !important;
        cursor: not-allowed !important;
      }

      .home-wrap {
        max-width: 1280px;
        margin: 0 auto;
        padding: .35rem 0 1.5rem 0;
      }

      .hero-card {
        overflow: hidden;
        background:
          linear-gradient(135deg, rgba(27,153,139,.12), rgba(92,122,234,.08));
      }

      .hero-grid {
        display: grid;
        grid-template-columns: minmax(280px, 390px) minmax(0, 1fr);
        gap: 1.5rem;
        align-items: center;
      }

      .hero-logo {
        width: 100%;
        max-width: 360px;
        height: auto;
        display: block;
        margin: 0 auto;
        filter: drop-shadow(0 12px 24px rgba(0,0,0,.10));
      }

      .hero-title {
        font-size: 2rem;
        line-height: 1.1;
        font-weight: 800;
        margin-bottom: .65rem;
        color: #0f3d3a;
      }

      .hero-subtitle {
        font-size: 1.05rem;
        color: #355a57;
        margin-bottom: 1rem;
      }

      .badge-row {
        display: flex;
        flex-wrap: wrap;
        gap: .55rem;
        margin-top: .85rem;
      }

      .soft-badge {
        display: inline-flex;
        align-items: center;
        gap: .4rem;
        padding: .45rem .7rem;
        border-radius: 999px;
        background: rgba(255,255,255,.75);
        border: 1px solid rgba(0,0,0,.06);
        font-size: .92rem;
        font-weight: 600;
        color: #184946;
      }

      .feature-grid {
        display: grid;
        grid-template-columns: repeat(3, minmax(0, 1fr));
        gap: 1rem;
      }

      .feature-card {
        height: 100%;
      }

      .feature-icon {
        font-size: 1.3rem;
        margin-bottom: .45rem;
      }

      .feature-title {
        font-weight: 750;
        margin-bottom: .4rem;
        color: #143b38;
      }

      .feature-text {
        color: #4b5f5d;
        line-height: 1.5;
      }

      .step-grid {
        display: grid;
        grid-template-columns: repeat(6, minmax(0, 1fr));
        gap: .8rem;
      }

      .step-box {
        background: #fff;
        border: 1px solid rgba(0,0,0,.06);
        border-radius: 16px;
        padding: .9rem .85rem;
        min-height: 128px;
      }

      .step-n {
        width: 34px;
        height: 34px;
        border-radius: 999px;
        display: flex;
        align-items: center;
        justify-content: center;
        background: rgba(27,153,139,.12);
        color: #156b62;
        font-weight: 800;
        margin-bottom: .55rem;
      }

      .step-title {
        font-weight: 750;
        margin-bottom: .3rem;
        color: #173f3b;
      }

      .step-text {
        font-size: .94rem;
        color: #536765;
        line-height: 1.45;
      }

      .home-note {
        color: #506260;
        line-height: 1.6;
      }

      .home-list {
        margin-bottom: 0;
        padding-left: 1.1rem;
      }

      .home-list li {
        margin-bottom: .42rem;
        color: #4d5f5d;
      }

      .cta-box {
        background: linear-gradient(135deg, rgba(27,153,139,.08), rgba(27,153,139,.02));
        border: 1px solid rgba(27,153,139,.14);
        border-radius: 16px;
        padding: 1rem 1rem;
      }

      @media (max-width: 1100px) {
        .feature-grid { grid-template-columns: 1fr 1fr; }
        .step-grid { grid-template-columns: repeat(3, minmax(0, 1fr)); }
      }

      @media (max-width: 768px) {
        .hero-grid { grid-template-columns: 1fr; }
        .feature-grid { grid-template-columns: 1fr; }
        .step-grid { grid-template-columns: 1fr 1fr; }
        .hero-title { font-size: 1.65rem; }
      }

      @media (max-width: 520px) {
        .step-grid { grid-template-columns: 1fr; }
      }
    ")),
    shiny::tags$script(shiny::HTML("
      Shiny.addCustomMessageHandler('toggleMainTabs', function(message) {
        var allowed = message.allowed || [];
        var allTabs = ['home', 'ingest', 'field_mapping', 'taxonomy', 'darwin_tables', 'metadata', 'qc'];

        allTabs.forEach(function(value) {
          var navLink = document.querySelector('[data-value=\"' + value + '\"]');
          if (!navLink) return;

          var isAllowed = allowed.includes(value);

          if (isAllowed) {
            navLink.classList.remove('disabled-tab');
            navLink.removeAttribute('aria-disabled');
            navLink.removeAttribute('tabindex');
          } else {
            navLink.classList.add('disabled-tab');
            navLink.setAttribute('aria-disabled', 'true');
            navLink.setAttribute('tabindex', '-1');
          }
        });
      });
    "))
  ),

  # -------------------------------------------------------
  # 0 HOME
  # -------------------------------------------------------
  bslib::nav_panel(
    title = "Home",
    value = "home",
    shiny::div(
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
              shiny::div(class = "hero-title", "AURORA Ocean Biodiversity Data App"),
              shiny::p(
                class = "hero-subtitle",
                "An integrated Shiny workflow for preparing, standardising, validating, documenting, and exporting biodiversity datasets using Darwin Core-oriented practices."
              ),
              shiny::p(
                class = "home-note",
                "This application was designed to support biodiversity data processing from raw tables to structured Darwin Core outputs, with a strong focus on data quality, taxonomic standardisation, metadata, reproducibility, and FAIR-aligned publication workflows."
              ),
              shiny::div(
                class = "badge-row",
                shiny::span(class = "soft-badge", "Darwin Core"),
                shiny::span(class = "soft-badge", "Taxonomy matching"),
                shiny::span(class = "soft-badge", "Quality control"),
                shiny::span(class = "soft-badge", "Metadata"),
                shiny::span(class = "soft-badge", "DwC-A export"),
                shiny::span(class = "soft-badge", "Marine biodiversity")
              )
            )
          )
        )
      ),

      bslib::layout_columns(
        col_widths = c(8, 4),

        bslib::card(
          full_screen = FALSE,
          bslib::card_header("About the app"),
          bslib::card_body(
            shiny::p(
              class = "home-note",
              "The app brings together the main stages needed to process biodiversity occurrence data in a practical and user-friendly way. It supports data ingestion, field mapping to Darwin Core terms, taxonomic matching, data cleaning, generation of Darwin Core tables, metadata preparation, and quality-control diagnostics."
            ),
            shiny::p(
              class = "home-note",
              "Its overall objective is to help users transform heterogeneous biological datasets into cleaner, more consistent, and more interoperable data products, especially for downstream sharing with infrastructures and repositories that rely on biodiversity standards."
            ),
            shiny::p(
              class = "home-note",
              "The project concept includes standardisation pipelines, validation routines, metadata support, and integration-oriented workflows for biodiversity data management."
            )
          )
        ),

        bslib::card(
          full_screen = FALSE,
          bslib::card_header("How to start"),
          bslib::card_body(
            shiny::div(
              class = "cta-box",
              shiny::p(
                style = "margin-bottom:.55rem; font-weight:700; color:#12403b;",
                "Recommended first step"
              ),
              shiny::p(
                class = "home-note",
                style = "margin-bottom:.8rem;",
                "Open the Ingest tab and upload your file or choose one of the example datasets."
              ),
              shiny::p(
                class = "home-note",
                style = "margin-bottom:0;",
                "The remaining tabs are enabled after the ingest step is completed."
              )
            )
          )
        )
      ),

      shiny::br(),

      bslib::card(
        class = "mb-4",
        bslib::card_header("Main workflow"),
        bslib::card_body(
          shiny::div(
            class = "step-grid",

            shiny::div(
              class = "step-box",
              shiny::div(class = "step-n", "1"),
              shiny::div(class = "step-title", "Ingest"),
              shiny::div(class = "step-text", "Import CSV, TSV, TXT, XLS, or XLSX data and inspect the raw structure.")
            ),

            shiny::div(
              class = "step-box",
              shiny::div(class = "step-n", "2"),
              shiny::div(class = "step-title", "Field Mapping"),
              shiny::div(class = "step-text", "Map user columns to Darwin Core terms with guidance and validation.")
            ),

            shiny::div(
              class = "step-box",
              shiny::div(class = "step-n", "3"),
              shiny::div(class = "step-title", "Taxonomy"),
              shiny::div(class = "step-text", "Review scientific names and match them against taxonomic backbones such as WoRMS or GBIF.")
            ),

            shiny::div(
              class = "step-box",
              shiny::div(class = "step-n", "4"),
              shiny::div(class = "step-title", "Darwin Tables"),
              shiny::div(class = "step-text", "Build structured Darwin Core tables for Event, Occurrence, and eMoF.")
            ),

            shiny::div(
              class = "step-box",
              shiny::div(class = "step-n", "5"),
              shiny::div(class = "step-title", "Metadata"),
              shiny::div(class = "step-text", "Describe the dataset with project, provenance, coverage, and publication-oriented metadata.")
            ),

            shiny::div(
              class = "step-box",
              shiny::div(class = "step-n", "6"),
              shiny::div(class = "step-title", "QC"),
              shiny::div(class = "step-text", "Inspect issues, diagnostics, summary outputs, and validation results before export.")
            )
          )
        )
      ),

      bslib::card(
        class = "mb-4",
        bslib::card_header("What this project is designed to support"),
        bslib::card_body(
          shiny::div(
            class = "feature-grid",

            bslib::card(
              class = "feature-card",
              bslib::card_body(
                shiny::div(class = "feature-icon", "📥"),
                shiny::div(class = "feature-title", "Data ingestion and harmonisation"),
                shiny::div(
                  class = "feature-text",
                  "Import heterogeneous tabular biodiversity data, preview it, and prepare it for a tidy and standard-oriented workflow."
                )
              )
            ),

            bslib::card(
              class = "feature-card",
              bslib::card_body(
                shiny::div(class = "feature-icon", "🧭"),
                shiny::div(class = "feature-title", "Standardisation and validation"),
                shiny::div(
                  class = "feature-text",
                  "Apply community standards and conventions, improve consistency, and detect potential problems in coordinates, dates, required fields, and other key variables."
                )
              )
            ),

            bslib::card(
              class = "feature-card",
              bslib::card_body(
                shiny::div(class = "feature-icon", "🧬"),
                shiny::div(class = "feature-title", "Taxonomic quality"),
                shiny::div(
                  class = "feature-text",
                  "Support taxonomic name cleaning and matching to accepted scientific names using biodiversity taxonomic resources."
                )
              )
            ),

            bslib::card(
              class = "feature-card",
              bslib::card_body(
                shiny::div(class = "feature-icon", "🗂️"),
                shiny::div(class = "feature-title", "Darwin Core structuring"),
                shiny::div(
                  class = "feature-text",
                  "Transform flat source tables into Darwin Core-compliant structures suitable for downstream biodiversity data exchange."
                )
              )
            ),

            bslib::card(
              class = "feature-card",
              bslib::card_body(
                shiny::div(class = "feature-icon", "📝"),
                shiny::div(class = "feature-title", "Metadata and FAIR support"),
                shiny::div(
                  class = "feature-text",
                  "Document provenance, contacts, coverage, and other metadata elements needed for transparent and reusable data publication."
                )
              )
            ),

            bslib::card(
              class = "feature-card",
              bslib::card_body(
                shiny::div(class = "feature-icon", "🔎"),
                shiny::div(class = "feature-title", "Diagnostics and reproducibility"),
                shiny::div(
                  class = "feature-text",
                  "Generate a clearer overview of data quality issues and provide a more traceable processing workflow."
                )
              )
            )
          )
        )
      ),

      bslib::layout_columns(
        col_widths = c(6, 6),

        bslib::card(
          bslib::card_header("Project context"),
          bslib::card_body(
            shiny::p(
              class = "home-note",
              "AURORA is framed around ocean biodiversity data processing and aims to support richer, more standardised, and more interoperable biodiversity datasets. The proposed workflow combines data cleaning, standardisation, validation, metadata support, integration logic, and export-oriented preparation."
            ),
            shiny::p(
              class = "home-note",
              "The app concept also aligns with the use of biodiversity informatics tools and standards for Darwin Core compliance, taxonomic matching, geospatial and temporal quality control, and metadata production."
            )
          )
        ),

        bslib::card(
          bslib::card_header("Examples of supported tasks"),
          bslib::card_body(
            shiny::tags$ul(
              class = "home-list",
              shiny::tags$li("Map source columns to Darwin Core terms."),
              shiny::tags$li("Standardise dates to ISO-like formats."),
              shiny::tags$li("Validate latitude and longitude values."),
              shiny::tags$li("Review scientific names and accepted names."),
              shiny::tags$li("Build Event, Occurrence, and eMoF tables."),
              shiny::tags$li("Prepare metadata and inspect data-quality issues.")
            )
          )
        )
      )
    )
  ),

  # -------------------------------------------------------
  # 1 INGEST
  # -------------------------------------------------------
  bslib::nav_panel(
    title = "Ingest",
    value = "ingest",
    mod_ingest_ui("ingest")
  ),

  # -------------------------------------------------------
  # 2 MAPPING
  # -------------------------------------------------------
  bslib::nav_panel(
    title = "Field Mapping",
    value = "field_mapping",
    mod_dwc_mapping_ui("dwc_map")
  ),

  # -------------------------------------------------------
  # 3 TAXONOMY
  # -------------------------------------------------------
  bslib::nav_panel(
    title = "Taxonomy",
    value = "taxonomy",
    mod_taxonomy_match_ui("tax_match")
  ),

  # -------------------------------------------------------
  # 4 BUILD DWC-A
  # -------------------------------------------------------
  bslib::nav_panel(
    title = "Darwin Tables",
    value = "darwin_tables",
    mod_build_dwca_ui("dwca")
  ),

  # -------------------------------------------------------
  # 5 METADATA
  # -------------------------------------------------------
  mod_metadata_ui("metadata"),

  # -------------------------------------------------------
  # 6 QC & DIAGNOSTICS
  # -------------------------------------------------------
  mod_qc_ui("qc")
)