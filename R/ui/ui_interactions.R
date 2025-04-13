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
        checkboxGroupInput(ns("regionFilter"), "Region",
          choices = list("NA" = "north_america", "EMEA" = "emea", "Korea" = "korea"),
          selected = list("north_america", "emea", "korea")
        ),
        selectInput(ns("teamFilter"), "Team",
          choices = c("All" = "All", team_list)
        ),
        checkboxGroupInput(ns("weekFilter"), "Week",
          choices = list(
            "Week 1" = 1, "Week 2" = 2,
            "Week 3" = 3, "Week 4" = 4,
            "Playoffs"
          ),
          selected = list(1, 2, 3, 4, "Playoffs")
        )
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
