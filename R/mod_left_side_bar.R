#' left_side_bar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_left_side_bar_ui <- function(id){
  ns <- NS(id)
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(id = ns("tabs"),
    mod_ref_schema_ui("ref_schema_1")


    )

  )

}

#' left_side_bar Server Functions
#'
#' @noRd
mod_left_side_bar_server <- function(id,DD){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$tabs,{
      # cat(input$tabs)
    })
    return(reactive(input$tabs))

  })
}

## To be copied in the UI
# mod_left_side_bar_ui("left_side_bar_1")

## To be copied in the server
# mod_left_side_bar_server("left_side_bar_1")
