#' header UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList textInput passwordInput
#' @importFrom shinydashboardPlus dashboardHeader notificationItem dropdownBlock userOutput
#' @importFrom shinydashboard dropdownMenu
#' @importFrom shinyWidgets actionBttn
#' @importFrom spsComps spsDepend
#' @importFrom stringr str_c
#' @importFrom RPostgres Postgres

mod_header_ui <- function(id){
  ns <- NS(id)

  shinydashboardPlus::dashboardHeader(title=  tags$a(href="https://github.com/SebastienGrall/Stacomizkia", target="_blank",
                                                     tags$img(height = "40px", alt="Stacomizkia",  src="www/favicon.ico", width = "40px") ),
                                      leftUi = tagList(

                                        shinydashboardPlus::dropdownBlock(
                                          id = ns("base"),
                                          title = "Options de connexion",
                                          icon = icon("database"),
                                          badgeStatus = NULL,

                                          # to have popup message on error
                                          spsComps::spsDepend("toastr"),

                                          textInput(
                                            inputId = ns("host_login"),
                                            label= "host",
                                            value = "localhost", placeholder = "localhost"
                                          ),
                                          textInput(
                                            inputId = ns("port_login"),
                                            label= "port",
                                            value = "5432", placeholder = "5432"
                                          ),
                                          textInput(
                                            inputId = ns("dbname_login"),
                                            label= "base de  donn\u00e9es",
                                            value = "bd_contmig_nat",
                                            placeholder = "bd_contmig_nat"
                                          ),
                                          textInput(
                                            inputId = ns("name_login"),
                                            label= "utilisateur",
                                            value = "user",
                                            placeholder = "Nom utilisateur utilise pour l'acc\u00e8s a la base de donn\u00e9es."
                                          ),
                                          passwordInput(
                                            inputId = ns("pass_login"),
                                            label= "mot de passe",
                                            value = "password",
                                            placeholder = "Mot de passe utilise pour l'acc\u00e8s a la base de donn\u00e9es."
                                          ),
                                          shinyWidgets::actionBttn(
                                            inputId = ns("login_button"),
                                            label = "OK",
                                            style = "pill",
                                            color = "success"
                                          )) %>%
                                          spsComps::bsPopover(
                                            "1. Cliquez d'abord ici",
                                            content= "Info de connexion vers base puis OK",
                                            placement = "right",
                                            bgcolor = "#00a65a",
                                            titlecolor = "white",
                                            contentcolor = "black"),
                                        shinydashboardPlus::userOutput(ns("statut_connection"))

                                      )
  )

}

#' header Server Functions
#'
#' @noRd
#' @importFrom shiny observeEvent isolate
#' @importFrom  spsComps shinyCatch
#' @importFrom pool poolClose
#' @importFrom shinydashboardPlus dashboardUser dashboardUserItem renderUser
#' @return The input input$login_button.

mod_header_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$login_button, ignoreInit = TRUE,                                                {
      spsComps::shinyCatch(
        {

          # utilisateur et mot de passe sont isoles
          host <- isolate(input$host_login)
          port <- isolate(input$port_login)
          dbname <- isolate(input$dbname_login)
          user <- isolate(input$name_login)
          password <- isolate(input$pass_login)
          # cat("choice db")


          db_connection<-  pool::dbPool(
            drv=RPostgres::Postgres(),
            dbname = dbname,
            host = host,
            port = port,
            user = user,
            password = password
          )
          validate(need(!is.null(db_connection), "Pas de connexion, v\u00e9rifiez les param\u00e8tres de connexion"))
          #browser()

          if (db_connection$valid == "TRUE") {


            # on stocke l'utilisateur, la connexion et les feuilles dans l'environnement global
            assign("db_connection",  db_connection, envir = Stacomizkia_env)
            #assign("schema",user,envir=Stacomizkia_env)


           # poolClose(db_connection@connection)
            # on met a jour la boite du statut de la connection
            output$statut_connection <- renderUser({
              dashboardUser(
                name = dbname,
                image = "https://e7.pngegg.com/pngimages/165/79/png-clipart-database-server-computer-icons-database-connection-backup-database-miscellaneous-angle-thumbnail.png",
                # title = ,
                subtitle = paste("Utilisateur :", user, sep = " "),
                footer = p(paste("H\u00f4te :", port, sep = " "), class = "text-center")

              )
            }) # fin output$statut_connection

          } # fin if


        },
        blocking_level = "error")
    }) # fin observeEvent connection
    return(reactive(input$login_button))

  })
}

## To be copied in the UI
# mod_header_ui("header_1")

## To be copied in the server
# mod_header_server("header_1")
