# modules/mod_homepage.R

#' Homepage module UI
#'
#' @param id Module id.
#' @return A Shiny UI definition.
#' @export
mod_homepage_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    # JavaScript snippet to capture client IP on load and send it to R
    shiny::tags$head(
      shiny::tags$script(shiny::HTML(sprintf('
        $(document).on("shiny:connected", function(event) {
          $.getJSON("https://api.ipify.org?format=json", function(data) {
            Shiny.setInputValue("%s", data.ip);
          }).fail(function() {
            Shiny.setInputValue("%s", "127.0.0.1");
          });
        });
      ', ns("client_ip"), ns("client_ip"))))
    ),
    
    shiny::div(
      id = ns("page"),
      class = "home-wrap",

      # =========================
      # HERO + VISITOR STATISTICS (SIDE BY SIDE)
      # =========================
      shiny::div(
        style = "display: grid; grid-template-columns: 4fr 1.2fr; gap: 1rem; margin-bottom: 1.5rem;",

        # Hero section (Forced to take full height)
        bslib::card(
          class = "hero-card",
          style = "height: 100%; margin-bottom: 0;",
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
                  "An integrated Shiny application designed to streamline the preparation, validation, and export of both marine and terrestrial biodiversity datasets into Darwin Core tables for sharing on public repositories such as GBIF — the Global Biodiversity Information Facility."
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

        # Visitor Statistics (Forced to stretch seamlessly to match Hero height)
        bslib::card(
          class = "mb-0",
          style = "height: 100%; display: flex; flex-direction: column;",
          bslib::card_header("Visitor Stats", style = "padding: 0.75rem;"),
          bslib::card_body(
            style = "padding: 1rem; flex: 1; display: flex; flex-direction: column; justify-content: space-between;",
            shiny::div(
              style = "text-align: center; width: 100%;",
              shiny::div(
                style = "font-size: 0.85rem; color: #666; margin-bottom: 0.2rem;",
                "Total Visitors"
              ),
              shiny::div(
                style = "font-size: 0.75rem; color: #999; margin-bottom: 0.5rem; font-style: italic;",
                "since 2026-06-11"
              ),
              shiny::div(
                style = "font-size: 2.2rem; font-weight: bold; color: #007bff; margin-bottom: 0.5rem;",
                shiny::textOutput(ns("visitor_count"))
              ),
              shiny::h6("Visitor by Country", style = "margin: 0.5rem 0; font-size: 0.8rem;"),
              shiny::div(
                style = "max-height: 200px; overflow-y: auto; font-size: 0.85rem; width: 100%;",
                DT::DTOutput(ns("visitor_table"))
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
                "Review and edit identification-related fields, especially scientificName and related Darwin Core terms, before taxonomic matching."
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
                "Build structured Darwin Core tables for Event, Occurrence, and Extended Measurement or Fact (eMoF)."
              )
            ),

            shiny::div(
              class = "step-box",
              shiny::div(class = "step-n", "6"),
              shiny::div(class = "step-title", "eMoF Editor"),
              shiny::div(
                class = "step-text",
                "Review and edit eMoF-related fields and controlled vocabulary entries."
              )
            ),

            shiny::div(
              class = "step-box",
              shiny::div(class = "step-n", "7"),
              shiny::div(class = "step-title", "DwC Tables Overview & Quality Control (QC)"),
              shiny::div(
                class = "step-text",
                "Inspect issues, diagnostics, summary outputs, and validation results for export."
              )
            ),

            shiny::div(
              class = "step-box",
              shiny::div(class = "step-n", "8"),
              shiny::div(class = "step-title", "Metadata"),
              shiny::div(
                class = "step-text",
                "Streamline metadata creation and revision using an interface inspired by the GBIF IPT."
              )
            )
          ),

          # =========================
          # MANUAL (FULL WIDTH)
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
                  href = "https://doi.org/10.5281/zenodo.19495004",
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
                  "Structure the data into relational Event, Occurrence, and eMoF tables to comply with the DwC-A star schema."
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

#' Homepage module server
#'
#' @param id Module id.
#' @param session Shiny session object.
#' @return Module server function.
#' @export
mod_homepage_server <- function(id, session = shiny::getDefaultReactiveDomain()) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Absolute dynamic database path resolution
    base_dir <- shiny::getShinyOption("appDir", default = getwd())
    db_path <- "/var/lib/shiny-server/aurora-data/visitors.db"

    # Create directory if it doesn't exist
    if (!dir.exists(file.path(base_dir, "data"))) {
      dir.create(file.path(base_dir, "data"), showWarnings = FALSE, recursive = TRUE)
    }
    
    # Robust connection helper with WAL mode enabled to avoid locking out processes
    # Hard-coded absolute path - NO VARIABLES
    db_path <- "/var/lib/shiny-server/aurora-data/visitors.db"
    
    get_db_connection <- function() {
      db_path <- "/var/lib/shiny-server/aurora-data/visitors.db"
      
      # Ensure the directory and file are accessible
      if (!file.exists(db_path)) {
        stop("Database file not found at: ", db_path)
      }
      
      conn <- RSQLite::dbConnect(RSQLite::SQLite(), db_path)
      RSQLite::dbExecute(conn, "PRAGMA busy_timeout = 5000")
      RSQLite::dbExecute(conn, "PRAGMA journal_mode = WAL")
      return(conn)
    }

    # Unicode flag helper
    country_code_to_flag <- function(code) {
      if (is.null(code) || is.na(code) || nchar(code) != 2) return("🌍")
      code <- toupper(code)
      paste0(
        intToUtf8(127397 + utf8ToInt(substr(code, 1, 1))),
        intToUtf8(127397 + utf8ToInt(substr(code, 2, 2)))
      )
    }

    get_flag_emoji <- function(country, country_code = NULL) {
      if (is.null(country) || country == "" || country == "Unknown" || country == "Local/Development") {
        if (!is.null(country) && country == "Local/Development") return("💻 Local/Proxy")
        return("❓ Unknown")
      }
      if (!is.null(country_code) && !is.na(country_code) && nchar(country_code) == 2) {
        return(paste0(country_code_to_flag(country_code), " ", country))
      }
      return(paste0("🌍 ", country))
    }

    get_country_from_ip <- function(ip) {
      if (is.null(ip) || ip == "" || grepl("^127\\.", ip) || ip == "localhost" || ip == "::1" || ip == "0.0.0.0") {
        return(list(country = "Local/Development", code = NA))
      }
      tryCatch({
        response <- httr::GET(paste0("https://ipapi.co/", ip, "/json/"), httr::timeout(3), httr::user_agent("Aurora-Shiny-App"))
        if (httr::status_code(response) == 200) {
          data <- jsonlite::fromJSON(httr::content(response, "text"))
          return(list(
            country = if (!is.null(data$country_name)) data$country_name else "Unknown",
            code = if (!is.null(data$country_code)) data$country_code else NA
          ))
        }
        list(country = "Unknown", code = NA)
      }, error = function(e) {
        list(country = "Unknown", code = NA)
      })
    }
    
    # Initialize DB with correct schema
    init_visitor_db <- function() {
      tryCatch({
        conn <- get_db_connection()
        RSQLite::dbExecute(conn,
          "CREATE TABLE IF NOT EXISTS visitors (
            id INTEGER PRIMARY KEY AUTOINCREMENT,
            ip TEXT,
            country TEXT,
            country_code TEXT,
            user_agent TEXT,
            timestamp DATETIME DEFAULT CURRENT_TIMESTAMP
          )"
        )
        table_info <- RSQLite::dbGetQuery(conn, "PRAGMA table_info(visitors)")
        if (!"country_code" %in% table_info$name) {
          RSQLite::dbExecute(conn, "ALTER TABLE visitors ADD COLUMN country_code TEXT")
        }
        RSQLite::dbDisconnect(conn)
      }, error = function(e) {
        cat("Error initializing database:", e$message, "\n")
      })
    }
   
    
    # Global reactive update trigger for views
    db_trigger <- shiny::reactiveVal(0)
    
    # Listen exclusively to the client-side JavaScript input handle
    shiny::observeEvent(input$client_ip, {
      client_ip <- input$client_ip
      
      # Handle fallback if JS fails or returns empty
      if (is.null(client_ip) || client_ip == "") {
        client_ip <- "127.0.0.1"
      }
      
      req_ua <- session$clientData$user_agent
      user_agent <- if (!is.null(req_ua) && req_ua != "") req_ua else "Unknown/Browser"
      
      ip_data <- get_country_from_ip(client_ip)
      
      tryCatch({
        conn <- get_db_connection()
        
        RSQLite::dbExecute(
          conn,
          "INSERT INTO visitors (ip, country, country_code, user_agent) VALUES (?, ?, ?, ?)",
          params = list(client_ip, ip_data$country, ip_data$code, user_agent)
        )
        
        RSQLite::dbDisconnect(conn)
        db_trigger(db_trigger() + 1)
        
      }, error = function(e) {
        if (exists("conn")) try(RSQLite::dbDisconnect(conn), silent = TRUE)
        cat("[DATABASE WRITE ERROR]:", e$message, "\n")
      })
    }, ignoreInit = FALSE)
    
    # Render TOTAL visitor count
    output$visitor_count <- shiny::renderText({
      db_trigger() 
      tryCatch({
        conn <- get_db_connection()
        on.exit(RSQLite::dbDisconnect(conn), add = TRUE)
        result <- RSQLite::dbGetQuery(conn, "SELECT COUNT(*) as count FROM visitors")
        return(as.character(as.integer(result$count[1])))
      }, error = function(e) {
        # CHANGE THIS to help you debug:
        message("DEBUG: Visitor count query failed: ", e$message)
        return("Error loading count") 
      })
    })
    
    # Render visitor table
   output$visitor_table <- DT::renderDT({
  db_trigger() 
  
  # 1. Define clean, empty structure with known names
  empty_df <- data.frame(Country = character(0), Visits = integer(0))
  
  tryCatch({
    conn <- get_db_connection()
    on.exit(RSQLite::dbDisconnect(conn), add = TRUE)
    
    # 2. Query data
    visitors_df <- RSQLite::dbGetQuery(conn, 
      "SELECT country, country_code, COUNT(*) as visits 
       FROM visitors 
       GROUP BY country, country_code 
       ORDER BY visits DESC 
       LIMIT 10"
    )
    
    # 3. Guard against empty results
    if (is.null(visitors_df) || nrow(visitors_df) == 0) {
      return(DT::datatable(empty_df, options = list(dom = "t")))
    }
    
    # 4. Process data
    formatted_countries <- mapply(get_flag_emoji, visitors_df$country, visitors_df$country_code, USE.NAMES = FALSE)
    
    # 5. Create dataframe with explicit names, NO spaces
    visitors_ui_df <- data.frame(
      Country = as.character(formatted_countries), 
      Visits = as.integer(visitors_df$visits),
      stringsAsFactors = FALSE
    )
    
    # CRITICAL: Strip any accidental attribute names that might confuse DT
    colnames(visitors_ui_df) <- c("Country", "Visits")
    rownames(visitors_ui_df) <- NULL
    
    # 6. Render
    DT::datatable(
      visitors_ui_df,
      options = list(
        dom = "t",
        ordering = FALSE,
        searching = FALSE
      ),
      rownames = FALSE
    )
  }, error = function(e) {
    return(DT::datatable(empty_df, options = list(dom = "t")))
  })
})
  })
}