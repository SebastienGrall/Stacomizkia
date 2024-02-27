#' main_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom DBI dbGetQuery
#' @importFrom dplyr select left_join row_number mutate tbl
#' @importFrom dbplyr in_schema
#' @importFrom shinybusy show_modal_spinner remove_modal_spinner
#' @importFrom stringr str_to_lower

mod_main_page_ui <- function(id){
  ns <- NS(id)

  shinydashboard::dashboardBody(

    # hide icon
    tags$script(
      HTML(
        'var e = document.querySelector("body > div.wrapper > header > nav > div:nth-child(4) > ul > li > a > i");
           e.setAttribute("style", "display: none;");'
      )
    ),


    sidebar = shinydashboardPlus::dashboardSidebar(),

    shinydashboardPlus::box(title ="Import des 3 fichiers",
                            collapsible = TRUE,
                            width = 2,
                            fileInput(ns('file_operation'), 'S\u00e9lectionner le fichier op\u00e9ration',
                                      accept=c('text/csv',
                                               'text/comma-separated-values,text/plain',
                                               '.csv'))%>%
                              spsComps::bsPopover(
                                "Cliquez ici pour importer un fichier",
                                content= "Import du fichier op\u00e9ration",
                                placement = "right",
                                bgcolor = "#00a65a",
                                titlecolor = "white",
                                contentcolor = "black"),

                            fileInput(ns('file_lot'), 'S\u00e9lectionner le fichier lot',
                                      accept=c('text/csv',
                                               'text/comma-separated-values,text/plain',
                                               '.csv'))%>%
                              spsComps::bsPopover(
                                "Cliquez ici pour importer un fichier",
                                content= "Import du fichier lot",
                                placement = "right",
                                bgcolor = "#00a65a",
                                titlecolor = "white",
                                contentcolor = "black"),

                            fileInput(ns('file_caract_lot'), 'S\u00e9lectionner le fichier caract\u00e9ristique lot',
                                      accept=c('text/csv',
                                               'text/comma-separated-values,text/plain',
                                               '.csv'))%>%
                              spsComps::bsPopover(
                                "Cliquez ici pour importer un fichier",
                                content= "Import du fichier caract\u00e9ristique lot",
                                placement = "right",
                                bgcolor = "#00a65a",
                                titlecolor = "white",
                                contentcolor = "black"),

                            shinyWidgets::actionBttn(
                              inputId = ns("import_button"),
                              label = "Importer",
                              style = "pill",
                              color = "success"
                            )

    ),
    shinydashboardPlus::box(title ="Derni\u00e8re op\u00e9ration dans la BDD",
                            collapsible = TRUE,
                            width = 8,
                            tableOutput(ns("max_ope"))),
    shinydashboardPlus::box(title ="Dernier lot dans la BDD",
                            collapsible = TRUE,
                            width = 10,
                            tableOutput(ns("max_lot"))),

    shinydashboardPlus::box(title ="Fichier op\u00e9ration \u00e0 importer",
                            collapsible = TRUE,
                            width = 10,
                            DT::DTOutput(ns("import_operation"))),
    shinydashboardPlus::box(title ="Fichier lot \u00e0 importer",
                            collapsible = TRUE,
                            width = 11,
                            DT::DTOutput(ns("import_lot"))),
    shinydashboardPlus::box(title ="Fichier caract\u00e9ristique lot \u00e0 importer",
                            collapsible = TRUE,
                            width = 10,
                            DT::DTOutput(ns("import_caract_lot")))
  )
}

#' main_page Server Functions
#'
#' @noRd
mod_main_page_server <- function(id,DD){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(DD$button_ref_schema(),
                 {
                                      shinyCatch(
                     {
                       db_connection<-Stacomizkia_env$db_connection
                       validate(need(!is.null(db_connection), "db needs connection"))
                       schema_select<-DD$select_ref_schema()
                       validate(need(!is.null(schema_select), "no schema selected"))
                       #browser()
                       query_operation<-str_c("select * from ",schema_select,".t_operation_ope where ope_identifiant = (select max(ope_identifiant) from ",schema_select,".t_operation_ope)")
                       #max_ope <- dbGetQuery(db_connection, "select * from t_operation_ope where ope_identifiant = (select max(ope_identifiant) from t_operation_ope)" )
                       #browser()
                       max_ope <- dbGetQuery(db_connection,query_operation)
                       #browser()
                        query_lot<-str_c("select * from ",schema_select,".t_lot_lot where lot_identifiant = (select max(lot_identifiant) from ",schema_select,".t_lot_lot)" )
                        max_lot<-dbGetQuery(db_connection,query_lot)
                        output$max_ope <- renderTable(max_ope)
                        output$max_lot <-renderTable(max_lot)


                     },
                     blocking_level = "error")
                 },
                 ignoreInit = TRUE,
                 ignoreNULL = TRUE
    )  ### fin dernier operation et dernier lot dans le schema

    getData_ope <- reactive({

      inFile_ope <- input$file_operation

      if (is.null(input$file_operation))
        return(NULL)
      df_ope<-read.csv2(inFile_ope$datapath,header = F,fileEncoding="WINDOWS-1252")
       df_ope[,3]<-as.POSIXct(df_ope[,3],format='%Y/%m/%d %H:%M:%S',tz="UTC")
       df_ope[,4]<-as.POSIXct(df_ope[,4],format='%Y/%m/%d %H:%M:%S',tz="UTC")

      return(df_ope)
    })

    observeEvent({input$file_operation

    },{

      spsComps::shinyCatch({
        if (is.null(input$file_operation))
          return(NULL)
        data_ope<-getData_ope()
        if(is.null(data_ope))
          return(NULL)
        else(colnames(data_ope)<-col_names_operation)
        spsComps::shinyCatch(validate(need(!is.na(data_ope$ope_dic_identifiant),"pas de code DC sur certaines lignes, vérifier le fichier d'entrée")))
        output$import_operation <- DT::renderDT(
          DT::datatable(
            data_ope      ,
            rownames=FALSE,
            #extensions = "Buttons",
            option=list(
              scroller = TRUE,
              scrollX = TRUE,
              lengthMenu=list(c(5,20,50,-1),c("5","20","50","All")),
              "pagelength"=-1,
              dom= "Blfrtip",
              scrollX = T,
              scrolly = T
            )

          ) #%>% DT::formatDate(3, "toLocaleString") %>%
            #DT::formatDate(4, "toLocaleString")
          )

      },  # fin shiny catch
      blocking_level = "error")},
    ignoreInit=T,
    ignoreNULL = F
    ) #fin observevent

  ###fin import fichier operation

    getData_lot <- reactive({

      inFile_lot <- input$file_lot

      if (is.null(input$file_lot))
        return(NULL)
      df_lot<-read.csv2(inFile_lot$datapath,header = F,fileEncoding="WINDOWS-1252")
      return(df_lot)
    })

    observeEvent({input$file_lot

    },{

      spsComps::shinyCatch({
        if (is.null(input$file_lot))
          return(NULL)
        data_lot<-getData_lot()
        if(is.null(data_lot))
          return(NULL)
        else(colnames(data_lot)<-col_names_lot)



        output$import_lot <- DT::renderDT(
          DT::datatable(
            data_lot      ,
            rownames=FALSE,
            #extensions = "Buttons",
            option=list(
              scroller = TRUE,
              scrollX = TRUE,
              lengthMenu=list(c(5,20,50,-1),c("5","20","50","All")),
              "pagelength"=-1,
              dom= "Blfrtip",
              scrollX = T,
              scrolly = T
            )

          ))

      },  # fin shiny catch
      blocking_level = "error")},
    ignoreInit=T,
    ignoreNULL = F
    ) #fin observevent

  ###fin import fichier lot

    getData_caract_lot <- reactive({

      inFile_caract_lot <- input$file_caract_lot

      if (is.null(input$file_caract_lot))
        return(NULL)
      df_caract_lot<-read.csv2(inFile_caract_lot$datapath,header = F,fileEncoding="WINDOWS-1252")
      return(df_caract_lot)
    })

    observeEvent({input$file_caract_lot

    },{

      spsComps::shinyCatch({
        if (is.null(input$file_caract_lot))
          return(NULL)
        data_caract_lot<-getData_caract_lot()
        if(is.null(data_caract_lot))
          return(NULL)
        else(colnames(data_caract_lot)<-col_names_caract_lot)
       
        output$import_caract_lot <- DT::renderDT(
          DT::datatable(
            data_caract_lot      ,
            rownames=FALSE,
            #extensions = "Buttons",
            option=list(
              scroller = TRUE,
              scrollX = TRUE,
              lengthMenu=list(c(5,20,50,-1),c("5","20","50","All")),
              "pagelength"=-1,
              dom= "Blfrtip",
              scrollX = T,
              scrolly = T
            )

          ))

      },  # fin shiny catch
      blocking_level = "error")},
    ignoreInit=T,
    ignoreNULL = F
    ) #fin observevent

  ###fin import fichier caracteristique lot


    ###### transformation des fichiers pour MAJ les bons numeros d'operation et de lots, formatage de certaines colonnes

    observeEvent(input$import_button,
                 {
                   shinyCatch(
                    {
                      db_connection<-Stacomizkia_env$db_connection
                      validate(need(!is.null(db_connection), "db needs connection"))
                      schema_select<-DD$select_ref_schema()
                      validate(need(!is.null(schema_select), "no schema selected"))
                      validate(need(!is.null(getData_ope()), "pas de fichier operation"))
                      validate(need(!is.null(getData_lot()), "pas de fichier lot"))
                      #validate(need(!is.null(getData_caract_lot()), "pas de fichier caract lot"))
                      import_operation<-getData_ope()
                      colnames(import_operation)<-col_names_operation
                      import_lot<-getData_lot()
                      colnames(import_lot)<-col_names_lot
                      import_lot$lot_quantite<-NA
                      import_lot$lot_qte_code<-NA
                      import_lot$lot_lot_identifiant<-NA
                      import_caract_lot<-getData_caract_lot()
                      if (is.null(import_caract_lot)){}else{colnames(import_caract_lot)<-col_names_caract_lot}

                      #colnames(import_caract_lot)<-col_names_caract_lot
                      if (is.null(import_caract_lot)){}else{import_caract_lot$car_val_identifiant<-NA}
                      #import_caract_lot$car_val_identifiant<-NA

                      validate(need(identical(unique(import_lot$lot_ope_identifiant),unique(import_operation$ope_identifiant)),
                               "les num\u00e9ros d'operation dans les fichiers imports op\u00e9ration et lot ne correspondent pas"))
                      if(is.null(import_caract_lot)){}else{validate(need(import_caract_lot$car_lot_identifiant %in% import_lot$lot_identifiant,
                                    "les num\u00e9ros de lots dans le fichiers imports caract_lot ne sont pas dans le fichier import lot"))}

                      shinybusy::show_modal_spinner()

                      query_max_ope <- str_c("select max(ope_identifiant) from ",schema_select,".t_operation_ope")
                      last_ope_num<-dbGetQuery(db_connection,query_max_ope)
                      last_ope_num<-last_ope_num[1,]
                      
                      query_max_lot <-str_c("select max(lot_identifiant) from ",schema_select,".t_lot_lot")
                      last_lot_num<-dbGetQuery(db_connection,query_max_lot)
                      last_lot_num<-last_lot_num[1,]
                      if(is.na(last_ope_num)){last_ope_num=0}else{last_ope_num}
                      if(is.na(last_lot_num)){last_lot_num=0}else{last_lot_num}

                      import_operation<- import_operation %>%
                           mutate(right_ope_identifiant=row_number()) %>%
                           mutate(right_ope_identifiant=right_ope_identifiant+last_ope_num)

                      import_lot<-import_lot %>% left_join(import_operation,by = c("lot_ope_identifiant"="ope_identifiant")) %>%
                           select(1:12,20)

                      import_lot<-import_lot %>%
                           mutate(right_lot_identifiant=row_number()) %>%
                           mutate(right_lot_identifiant=right_lot_identifiant+last_lot_num)

                      if(is.null(import_caract_lot)){}else{import_caract_lot<-import_caract_lot %>% left_join (import_lot,by=c("car_lot_identifiant"="lot_identifiant")) %>%
                           select(1:8,21)}

                      import_operation$ope_identifiant<-import_operation$right_ope_identifiant
                      import_operation<- import_operation %>% select(1:8)

                      import_lot$lot_identifiant<-import_lot$right_lot_identifiant
                      import_lot$lot_ope_identifiant<-import_lot$right_ope_identifiant
                      import_lot<-import_lot %>% select(1:12)

                      if(is.null(import_caract_lot)){}else{import_caract_lot$car_lot_identifiant<-import_caract_lot$right_lot_identifiant}
                      if(is.null(import_caract_lot)){}else{import_caract_lot<-import_caract_lot %>% select(1:8)}

                      schema_select<-str_to_lower(schema_select)

                      table_ope_import<-DBI::Id(
                        schema=schema_select,
                        table="t_operation_ope"
                      )

                     table_lot_import<-DBI::Id(
                        schema=schema_select,
                        table="t_lot_lot"
                      )

                      table_caract_lot_import<-DBI::Id(
                        schema=schema_select,
                        table="tj_caracteristiquelot_car"
                      )

                      DBI::dbWriteTable(con = db_connection,name = table_ope_import,value = import_operation,append = TRUE)

                      DBI::dbWriteTable(con = db_connection,name = table_lot_import,value = import_lot,append = TRUE)
                      if(is.null(import_caract_lot)){}else{DBI::dbWriteTable(con = db_connection,name = table_caract_lot_import,value = import_caract_lot,append = TRUE)}

                      shinybusy::remove_modal_spinner()
                      shiny::showModal( shiny::modalDialog( title=paste0("Import effectu\u00e9"),
                                                            br(),
                                                            div(tags$b(paste0("Les donn\u00e9es ont bien \u00e9t\u00e9
                                                                \u00e9crites dans la base de donn\u00e9es"), style = "color: green;"))
                      ))


                     },  # fin shiny catch
                     blocking_level = "error")},
                 ignoreInit=T,
                 ignoreNULL = F
    ) #fin observevent## fin observeEvent






    #########  fin partie MAJ fichiers

  })
}

## To be copied in the UI
# mod_main_page_ui("main_page_1")

## To be copied in the server
# mod_main_page_server("main_page_1")
