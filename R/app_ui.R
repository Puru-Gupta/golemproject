#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny shinydashboard shinydashboardPlus shinyWidgets
#' @importFrom magrittr %>%
#' @importFrom golem get_golem_options
#' @import highcharter
#' @importFrom highcharter highchartOutput
#' @noRd
app_ui <- function(request) {

  # data_fn <- golem::get_golem_options("data")
  # data <- data_fn()  # IMPORTANT: call it with () to get actual data

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic

      dashboardPage(
        title = "UPCCCE",

        header = dashboardHeader(),

        sidebar = dashboardSidebar(

          sidebarMenu(
            menuItem("Historical Data Analysis", tabName = "up_page", icon = icon('user'))
          )

        ),

        body = dashboardBody(

          mod_filter_ui("filter_1", data = data),

          fluidRow(
            mod_line_ui("create_box_1"),
            mod_pie_chart_ui("pie_chart_1")


          ),
          fluidRow(
            column(width = 12, offset = 0,
            mod_line_ui("dist_wise_cbo"))

          )



        )

      )


      )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "upccceproject"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
