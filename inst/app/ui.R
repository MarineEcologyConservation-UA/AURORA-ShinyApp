ui <- bslib::page_navbar(
  id = "main_nav",
  title = "shinyRv02",

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
    ")),
    shiny::tags$script(shiny::HTML("
      Shiny.addCustomMessageHandler('toggleMainTabs', function(message) {
        var locked = !!message.locked;
        var protectedTabs = ['field_mapping', 'taxonomy', 'darwin_tables', 'metadata', 'qc'];

        protectedTabs.forEach(function(value) {
          var navLink = document.querySelector('[data-value=\"' + value + '\"]');
          if (navLink) {
            if (locked) {
              navLink.classList.add('disabled-tab');
              navLink.setAttribute('aria-disabled', 'true');
              navLink.setAttribute('tabindex', '-1');
            } else {
              navLink.classList.remove('disabled-tab');
              navLink.removeAttribute('aria-disabled');
              navLink.removeAttribute('tabindex');
            }
          }
        });
      });
    "))
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