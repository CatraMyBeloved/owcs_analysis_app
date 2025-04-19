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
        helpText("Select criteria to filter composition data."),
        # Using shared filter components
        week_filter(ns("weekFilter")),
        region_filter(ns("regionFilter")),
        mode_filter(ns("modeFilter")),
        role_filter(ns("roleFilter"))
      ),
      card(
        card_header("General Pickrates - Plot"),
        card_body(
          sliderInput(ns("topnPickrates"), "Top N heroes to show",
            min = 1, max = 20, value = 10
          ),
          plotOutput(ns("PickratesVis"))
        )
      ),
      card(
        card_header("General Pickrates"),
        card_body(
          dataTableOutput(ns("Pickrates"))
        )
      )
    )
  )
}
