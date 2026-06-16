# run_dev.R
# Script para desenvolvimento local do Shiny app (modo package)

# --- localizar raiz do projeto de forma robusta ---
if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
}
proj <- here::here()

# --- ativar/usar renv ---
if (requireNamespace("renv", quietly = TRUE)) {
  suppressMessages(renv::load(project = proj))
}

# --- carregar o pacote em modo desenvolvimento ---
if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(path = proj)
} else {
  if (!requireNamespace("devtools", quietly = TRUE)) {
    stop("Package 'devtools' or 'pkgload' is required for development mode.")
  }
  devtools::load_all(path = proj)
}

# --- rodar o app ---
shiny::runApp(appDir = file.path(proj, "inst", "app"), launch.browser = TRUE)
