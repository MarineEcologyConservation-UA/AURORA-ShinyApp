ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "shinyRv02"),
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      shinydashboard::menuItem(
        "Ingest",
        tabName = "ingest",
        icon = shiny::icon("upload")
      ),
      shinydashboard::menuItem(
        "Mapping",
        tabName = "mapping",
        icon = shiny::icon("exchange-alt")
      )
    )
  ),
  shinydashboard::dashboardBody(
    shinydashboard::tabItems(
      shinydashboard::tabItem(
        tabName = "ingest",
        mod_ingest_ui("ingest")
      ),
      shinydashboard::tabItem(
        tabName = "mapping",
        mod_dwc_mapping_ui("dwc_map")
      )
    )
  )
)
