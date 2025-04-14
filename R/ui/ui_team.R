#' -----------------------------------------------------------------------------
#' ui_team.R
#'
#' Description: Contains UI elements and layout for team analysis page
#'
#' Author: CatraMyBeloved
#' Date Created: 03-04-2025
#' Last Modified: 02-04-2025
#' -----------------------------------------------------------------------------

team_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    "Team Analysis",
    layout_sidebar(
      sidebar = sidebar(
        checkboxGroupInput(ns("regionFilter"), "Region",
          choices = list("NA" = "north_america", "EMEA" = "emea", "Korea" = "korea"),
          selected = list("north_america", "emea", "korea")
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
      titlePanel("Team Dashboard"),
      div(
        selectInput(ns("teamSelection"),
          label = "Select Team",
          choices = team_list, selected = NULL
        ),
        card(
          card_header("Favorite Heroes"),
          card_body(
            dataTableOutput(ns("favHeroes"))
          )
        ),
        card(
          card_header("Favorite Maps"),
          card_body(
            dataTableOutput(ns("favMaps"))
          )
        )
      )
    )
  )
}
