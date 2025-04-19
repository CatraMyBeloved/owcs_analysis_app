#' -----------------------------------------------------------------------------
#' ui_composition.R
#'
#' Description: Contains UI elements and layout for composition analysis page
#'
#' Author: CatraMyBeloved
#' Date Created: 03-04-2025
#' Last Modified: 02-04-2025
#' -----------------------------------------------------------------------------


composition_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    "Composition Analysis",
    layout_sidebar(
      sidebar = sidebar(
        region_filter(ns("regionFilter")),
        team_filter(ns("teamFilter")),
        map_filter(ns("mapFilter")),
        week_filter(ns("weekFilter")),
        mode_filter(ns("modeFilter"))
      ),
      card(
        card_header(
          "Composition counts"
        ),
        card_body(
          checkboxGroupInput(ns("roleSelection"), "Roles",
            choices = list("Tank" = "tank", "DPS" = "dps", "Support" = "sup"),
            inline = TRUE
          ),
          dataTableOutput(ns("compositions"))
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
