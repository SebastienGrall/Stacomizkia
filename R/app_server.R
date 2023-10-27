#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  
  DD <- reactiveValues()
  mod_main_page_server("main_page_1",DD)
  DD$login_button <- mod_header_server("header_1") # login_button reactive variable récupérée depuis le module
  DD$tabs <- mod_left_side_bar_server("left_side_bar_1",DD)
  DD$button_ref_schema <- mod_ref_schema_server("ref_schema_1",DD)
  DD$select_ref_schema <- mod_ref_schema_server("ref_schema_1",DD)
}
