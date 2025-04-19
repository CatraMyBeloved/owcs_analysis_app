#' -----------------------------------------------------------------------------
#' ui_ban.R
#'
#' Description: Contains UI elements and layout for ban analysis page
#'
#' Author: CatraMyBeloved
#' Date Created: 03-04-2025
#' Last Modified: 02-04-2025
#' -----------------------------------------------------------------------------


ban_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    "Ban analysis",
    layout_sidebar(
      sidebar = sidebar(
        region_filter(ns("regionFilter")),
        team_filter(ns("teamFilter")),
        map_filter(ns("mapFilter")),
        week_filter(ns("weekFilter"))
      ),
      card(
        card_header("Ban Rates - Plot"),
        card_body(
          sliderInput(ns("topnBanrates"), "Top N heroes to show",
            min = 1, max = 20, value = 10
          ),
          plotOutput(ns("banratesVis"))
        )
      ),
      card(
        card_title("Banrates per hero"),
        card_body(
          dataTableOutput(ns("banrates"))
        )
      ),
      card(
        card_header("Filtered Matches"),
        card_body(
          dataTableOutput(ns("filteredMatches"))
        )
      )
    )
  )
}
