#' -----------------------------------------------------------------------------
#' ui_interactions.R
#'
#' Description: Contains UI elements and layout for interactions analysis page
#'
#' Author: CatraMyBeloved
#' Date Created: 03-04-2025
#' Last Modified: 02-04-2025
#' -----------------------------------------------------------------------------


interaction_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    "Interaction Analysis",
    layout_sidebar(
      sidebar = sidebar(
        region_filter(ns("regionFilter")),
        team_filter(ns("teamFilter")),
        week_filter(ns("weekFilter")),
      ),
      headerPanel("Synergy and Counters"),
      card(
        card_header("Hero Selection"),
        card_body(
          fluidRow(
            column(
              width = 4,
              selectInput(ns("roleFilterSel"),
                "Role",
                choices = list(
                  "Tank" = "tank",
                  "DPS" = "dps",
                  "Support" = "sup"
                ),
                selected = "tank"
              )
            ),
            column(
              width = 4,
              selectInput(ns("heroFilterSel"),
                "Hero",
                choices = hero_list
              )
            ),
            column(
              width = 4,
              sliderInput(ns("nHeroesFilter"),
                "Heroes to show",
                min = 1, max = 20, value = 10
              )
            ),
            card_title("Synergy"),
            plotOutput(ns("synergyVis")),
            card_title("Counters"),
            plotOutput(ns("counterVis"))
          )
        )
      )
    )
  )
}
