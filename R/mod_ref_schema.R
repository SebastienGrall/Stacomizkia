#' ref_schema UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ref_schema_ui <- function(id){
  ns <- NS(id)
  tagList(
    spsDepend("toastr"),
    selectInput(inputId = ns("select_ref_schema"),
                label = "Choix du schema",
                choices = NULL,
                selected = NULL,
                multiple = FALSE
    ) %>%
      spsComps::bsPopover(
        title = "Choix du du schema (chaque structure correspond \u00e0 un schema)",
        content = "pour retirer un \u00e9l\u00e9ment utiliser <- ou suppr",
        placement = "right",
        bgcolor = "#0275d8",
        titlecolor = "white",
        contentcolor = "black",
        titlesize = "14px",
        contentsize = "12px",
        titleweight = "600",
        contentweight = "400",
        opacity = 1,
        html = FALSE,
        trigger = "hover focus"),
    # textOutput(ns("test_text")),
    #textOutput(ns("test_schema")),
    actionButton(inputId = ns("button_ref_schema"),
                 label = "OK") %>%
      spsComps::bsTip(
        "2. Ici vous pouvez explorer les donn\u00e9es de stations contenues dans un schema diff\u00e9rent",
        status = "primary")

  )
}

#' ref_schema Server Functions
#'
#' @noRd
mod_ref_schema_server <- function(id,DD){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
     observeEvent(DD$login_button(),
                  {
                    # cat("TEST\n")
                    shinyCatch(
                     {

                        validate(need(exists("Stacomizkia_env"), "Le programme stacomi doit \u00eatre lanc\u00e9"))
                        shinybusy::show_modal_spinner(text = "loading from db") # show the modal window

                      db_connection<-Stacomizkia_env$db_connection
                       schema<-dbGetQuery(db_connection,"SELECT * FROM ref.ts_organisme_org too WHERE NOT  org_code IN ('nat','invite') ORDER BY org_code")
                       schema<-schema[,1]
                        shinybusy::remove_modal_spinner() # remove it when done
                        updateSelectInput(session, "select_ref_schema", choices = schema, selected = '')


                      },
                      blocking_level = "error")
                  },
                  ignoreInit = TRUE,
                  ignoreNULL = TRUE
     )
    observeEvent(input$button_ref_schema,
                 {

                   shinyCatch(
                     {
                       clean_up_envir(ignore = c("ref_dc", "ref_taxa", "ref_stage", "ref_parqual", "ref_parquan", "ref_par", "ref_env")) # this will remove ref_taxa and ref_stages
                       validate(need(!is.null(input$select_ref_schema), "Schema not set"))

                       #stacomi(database_expected = TRUE, sch = input$select_ref_schema)
                     },
                     blocking_level = "error")
                 },
                 ignoreInit = TRUE,
                 ignoreNULL = TRUE
    )
    return(reactive(input$select_ref_schema))
    return(reactive(input$button_ref_schema))
  })
}


## To be copied in the UI
# mod_ref_schema_ui("ref_schema_1")

## To be copied in the server
# mod_ref_schema_server("ref_schema_1")
