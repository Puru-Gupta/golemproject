#' filter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @import dplyr shiny shinydashboard shinydashboardPlus
#' @importFrom magrittr %>%
#' @importFrom highcharter highchartOutput
#' @importFrom shiny NS tagList
mod_filter_ui <- function(id, data) {
  ns <- NS(id)
  tagList(

    fluidRow(

      column(width = 3, offset = 0,

             pickerInput(
               ns("city"),label = 'Select District(s):',
               choices = sort(unique(data$DISTRICT_NAME_ID)),
               selected = sort(unique(data$DISTRICT_NAME_ID)),
               multiple = TRUE,
               options = list(
                 `actions-box` = TRUE,
                 size = 8,
                 `selected-text-format` = "count > 2",
                 `live-search`=TRUE
               ))

      ),

      column(width = 3, offset = 0,

             pickerInput(
               ns("station"),label = 'Select Block(s):',
               choices = "",
               selected = "",
               multiple = TRUE,
               options = list(
                 `actions-box` = TRUE,
                 size = 8,
                 `selected-text-format` = "count > 2",
                 `live-search`=TRUE
               ))

      ),

      column(width = 3, offset = 0,

             pickerInput(
               ns("year"),label = 'Select Year(s):',
               choices = "",
               selected = "",
               multiple = TRUE,
               options = list(
                 `actions-box` = TRUE,
                 size = 8,
                 `selected-text-format` = "count > 2",
                 `live-search`=TRUE
               ))

      )


    )



  )
}

#' filter Server Functions
#'
#' @noRd
mod_filter_server <- function(id, data){
  moduleServer(id, function(input, output, session){

    ns <- session$ns

    rv_city      <- reactiveValues()
    rv_station   <- reactiveValues()
    rv_year      <- reactiveValues()



    isolate({
      rv_city$city       <- input$city
      rv_station$station <- input$station
      rv_year$year       <- input$year
    })



    observeEvent(rv_city$city, {
      updatePickerInput(session, inputId = "station",
                        label = "Select Station(s):",
                        choices = unique(data[DISTRICT_NAME_ID %in% input$city, BLOCK_NAME_ID]),
                        selected = unique(data[DISTRICT_NAME_ID %in% input$city, BLOCK_NAME_ID]))
    })



    observeEvent(rv_station$station, {
      updatePickerInput(session, inputId = "year",
                        label = "Select Year(s):",
                        choices = unique(data[BLOCK_NAME_ID %in% input$station, Year]),
                        selected = unique(data[BLOCK_NAME_ID %in% input$station, Year]))
    })



    observe({


      if(!isTRUE(input$city_open) & !isTRUE(input$station_open) & !isTRUE(input$year_open))

      {

        rv_city$city <- input$city
        rv_station$station <- input$station
        rv_year$year <- input$year

      }

    })


    data_filter <- reactive({

      get_data_df <- data


      filtered_df <-  get_data_df[ DISTRICT_NAME_ID %in% rv_city$city &
                                   BLOCK_NAME_ID %in% rv_station$station &
                                   Year %in% rv_year$year

                                    , ]

      #return( filtered_df )




    })



    return(list(
      filtered_data = data_filter
    ))
  })
}

## To be copied in the UI
# mod_filter_ui("filter_1")

## To be copied in the server
# mod_filter_server("filter_1")
