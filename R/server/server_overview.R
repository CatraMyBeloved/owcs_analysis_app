#' -----------------------------------------------------------------------------
#' server_overview.R
#'
#' Description: Server function for overview analysis, backbone of overview
#'  analysis page.
#'
#' Author: CatraMyBeloved
#' Date Created: 03-04-2025
#' Last Modified: 02-04-2025
#' -----------------------------------------------------------------------------
#' Overview Server Module
#'
#' @description Handles the server-side logic for the main overview page, providing
#'   a high-level analysis of hero usage across all matches.
#'
#' @param id The module ID used for namespacing
#' @param all_data Reactive data frame containing match and hero usage data
#'
#' @details This module allows filtering by week, mode, role, and region to display
#'   overall hero pickrates and winrates. It calculates the frequency of hero
#'   selection across all maps and generates both tabular data and visualizations
#'   of the most commonly played heroes based on the applied filters.
#'
#' @return A Shiny module server function that handles overview analysis logic

overview_server <- function(id, all_data) {
  moduleServer(id, function(input, output, session) {
    # Filter logic
    filtered_data <- reactive({
      all_data |>
        filter(
          week %in% input$weekFilter,
          mode %in% input$modeFilter,
          role %in% input$roleFilter,
          region %in% input$regionFilter
        ) |>
        select(
          round_id, match_map_id, match_id, hero_name,
          role, map_name, mode, team_name, team, iswin
        )
    })
  })
}
