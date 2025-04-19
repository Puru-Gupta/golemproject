#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom magrittr %>%
#' @import highcharter
#' @import arrow
#' @import data.table
#' @importFrom highcharter highchartOutput
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic



  mod_filter_server("filter_1", data = data)

  filter_result <- mod_filter_server("filter_1", data = data)



  df_summary <- reactive({

    df <- filter_result$filtered_data()

    if(!is.null(df)){
    df[, .(count = .N), by = .(TYPE_SHORT_NAME, Year)][TYPE_SHORT_NAME %in% c("SHG", "VO", "CLF")] %>%
      dcast(Year ~ TYPE_SHORT_NAME, value.var = "count")
    }else {
      return(NULL)
    }

  })

  mod_line_server(
    id = "create_box_1",
    df = df_summary,  # <- pass as reactive
    x_col = "Year",
    y_cols = c("SHG", "CLF", "VO"),
    chart_title = "CBOs Type(s)"
  )


  cbo_type <- reactive({

    df <- filter_result$filtered_data()

    if(!is.null(df)){
      df[, .(count = .N), by = .(TYPE_SHORT_NAME)][TYPE_SHORT_NAME %in% c("SHG", "VO", "CLF")]
    }else{
      print("Data is not available!")
    }

  })

  mod_pie_chart_server(
    id = "pie_chart_1",
    df = cbo_type,
    x_col = 'TYPE_SHORT_NAME',
    y_cols = 'count')


  district_wise_cbo <- reactive({

    df <- filter_result$filtered_data()

    if(!is.null(df)){
      df[, .(count = .N), by = .(DISTRICT_NAME, DISTRICT_NAME_ID)][!is.na(DISTRICT_NAME)]
    }else{
      print("Data is not available!")
    }

  })




  mod_line_server(
    id = "dist_wise_cbo",
    df = district_wise_cbo,  # <- pass as reactive
    x_col = "DISTRICT_NAME",
    y_cols = "count",
    chart_title = "District Wise CBOs Data"
  )




}
