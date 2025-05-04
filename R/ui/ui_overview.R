#' -----------------------------------------------------------------------------
#' ui_overview.R
#'
#' Description: Contains UI elements and layout for overview analysis page
#'
#' Author: CatraMyBeloved
#' Date Created: 03-04-2025
#' Last Modified: 04-18-2025
#' -----------------------------------------------------------------------------

overview_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    "Overview",
    layout_sidebar(
      sidebar = sidebar(
        region_filter(ns("regionFilter")),
        role_filter(ns("roleFilter"))
      ),
      card(
        card_header("Hero Popularity per Week"),
        plotOutput(ns("popularityVis"))
      )
    )
  )
}
