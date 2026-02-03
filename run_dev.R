# # run_dev.R
# # Script para desenvolvimento local do Shiny app

# # garantir que estamos na raiz do projeto
# proj <- normalizePath(dirname(sys.frame(1)$ofile), winslash = "/")
# setwd(proj)

# # ativar renv se ainda não estiver ativo
# if (!"renv" %in% loadedNamespaces()) {
#   suppressMessages(renv::activate())
# }

# # carregar o pacote em modo desenvolvimento
# if (!requireNamespace("devtools", quietly = TRUE)) {
#   stop("Package 'devtools' is required for development mode.")
# }
# devtools::load_all()

# # rodar o app
# shiny::runApp("inst/app", launch.browser = TRUE)


# run_dev.R
# Script para desenvolvimento local do Shiny app (modo package)

# --- localizar raiz do projeto de forma robusta ---
if (!requireNamespace("here", quietly = TRUE)) {
  install.packages("here")
}
proj <- here::here()
setwd(proj)

# --- ativar/usar renv ---
if (requireNamespace("renv", quietly = TRUE)) {
  suppressMessages(renv::load())
}

# --- carregar o pacote em modo desenvolvimento ---
if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("Package 'devtools' is required for development mode.")
}
devtools::load_all()

# --- rodar o app ---
shiny::runApp("inst/app", launch.browser = TRUE)

# alterar documentação (roxygen2) se necessário
# executar devtools::document()
