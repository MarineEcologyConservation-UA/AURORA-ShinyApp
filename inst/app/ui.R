ui <- bslib::page_navbar(
  id = "main_nav",
  title = shiny::div(
    style = "display:flex; align-items:center; gap:.65rem;",
    shiny::tags$img(
      src = "aurora_logo_home.png",
      alt = "AURORA logo",
      style = "height:34px; width:auto;"
    ),
    shiny::tags$span("AURORA Shiny App")
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

      .navbar {
        min-height: 56px !important;
        padding-top: 4px !important;
        padding-bottom: 4px !important;
      }

      .navbar > .container-fluid,
      .navbar > .container,
      .navbar .container-fluid {
        min-height: 56px !important;
        padding-top: 0 !important;
        padding-bottom: 0 !important;
        align-items: center !important;
      }

      .navbar-brand {
        padding-top: 2px !important;
        padding-bottom: 2px !important;
        font-size: 1.25rem !important;
        line-height: 1.2 !important;
        font-weight: 500 !important;
      }

      .navbar-nav .nav-link {
        font-size: 1rem !important;
        padding-top: 8px !important;
        padding-bottom: 8px !important;
        line-height: 1.2 !important;
      }

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

      .navbar .navbar-collapse {
        flex-grow: 0;
        margin-left: auto;
        margin-right: 2rem;
      }

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

      .about-project-grid {
        display: grid;
        grid-template-columns: minmax(220px, 320px) minmax(0, 1fr);
        gap: 1.4rem;
        align-items: start;
      }

      .about-project-logo-wrap {
        display: flex;
        justify-content: center;
        align-items: flex-start;
      }

      .about-project-logo {
        width: 100%;
        max-width: 280px;
        height: auto;
        display: block;
        filter: drop-shadow(0 12px 24px rgba(0,0,0,.10));
      }

      @media (max-width: 1100px) {
        .feature-grid { grid-template-columns: 1fr 1fr; }
        .step-grid { grid-template-columns: repeat(3, minmax(0, 1fr)); }
      }

      @media (max-width: 768px) {
        .hero-grid { grid-template-columns: 1fr; }
        .feature-grid { grid-template-columns: 1fr; }
        .step-grid { grid-template-columns: 1fr 1fr; }
        .about-project-grid { grid-template-columns: 1fr; }
        .hero-title { font-size: 1.65rem; }
      }

      @media (max-width: 520px) {
        .step-grid { grid-template-columns: 1fr; }
      }
    ")),
    shiny::tags$script(shiny::HTML("
      Shiny.addCustomMessageHandler('toggleMainTabs', function(message) {
        var allowed = message.allowed || [];
        var allTabs = ['home', 'ingest', 'field_mapping', 'id_cleaning', 'taxonomy', 'darwin_tables', 'emof_editor', 'qc', 'metadata', 'about'];

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

  bslib::nav_panel(
    title = "Home",
    value = "home",
    mod_homepage_ui("home")
  ),

  bslib::nav_panel(
    title = "Ingest",
    value = "ingest",
    mod_ingest_ui("ingest")
  ),

  bslib::nav_panel(
    title = "Field Mapping",
    value = "field_mapping",
    mod_dwc_mapping_ui("dwc_map")
  ),

  bslib::nav_panel(
    title = "Identification Data Cleaning",
    value = "id_cleaning",
    mod_identification_cleaning_ui("id_cleaning")
  ),

  bslib::nav_panel(
    title = "Taxonomy",
    value = "taxonomy",
    mod_taxonomy_match_ui("tax_match")
  ),

  bslib::nav_panel(
    title = "Darwin Core Tables",
    value = "darwin_tables",
    mod_build_dwca_ui("dwca")
  ),

  bslib::nav_panel(
    title = "eMoF Editor",
    value = "emof_editor",
    mod_emof_data_editor_ui("emof_editor")
  ),

  mod_qc_ui("qc"),

  mod_metadata_ui("metadata"),

  bslib::nav_panel(
    title = "About",
    value = "about",
    mod_about_ui("about")
  )
)