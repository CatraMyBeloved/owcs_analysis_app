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
    "Team Dashboard",
    layout_sidebar(
      sidebar = sidebar(
        region_filter(ns("regionFilter")),
        week_filter(ns("weekFilter")),
      ),
      div(
        selectInput(ns("teamSelection"),
          label = "Select Team",
          choices = team_list, selected = NULL
        ),
        card(
          card_header(
            div(
              class = "d-flex justify-content-between align-items-center",
              h4(textOutput(ns("teamName"))),
              span(textOutput(ns("regionName")))
            )
          )
        ),
        layout_column_wrap(
          width = 1 / 4,
          tooltip(
            value_box(
              title = "Map Win Rate",
              value = textOutput(ns("overallWinrate")),
              showcase = bs_icon("award")
            ),
            "Percentage of maps won"
          ),
          tooltip(
            value_box(
              title = "Maps played",
              value = textOutput(ns("mapsPlayed")),
              showcase = bs_icon("map")
            ),
            "Total Number of maps played"
          ),
          tooltip(
            value_box(
              title = "Best Map",
              value = textOutput(ns("bestMap")),
              showcase = bs_icon("geo-fill")
            ),
            "Map played atleast 4 times with the highest winrate"
          ),
          tooltip(
            value_box(
              title = "Signature Hero",
              value = textOutput(ns("signatureHero")),
              showcase = bs_icon("person-circle")
            ),
            "Hero played for atleast 40% of games, and has a large difference to general pickrates"
          )
        ),
        layout_column_wrap(
          width = 1 / 2,
          card(
            card_header("Map Performance"),
            navset_card_pill(
              nav_panel(
                title = "Visual",
                plotOutput(
                  ns("mapPerformanceVis")
                )
              ),
              nav_panel(
                title = "Table View",
                dataTableOutput(
                  ns("mapPerformance")
                )
              )
            )
          ),
          card(
            card_header("Composition Performance"),
            navset_card_pill(
              nav_panel(title = "Visual", plotOutput(
                ns("heroPreferencesVis")
              )),
              nav_panel(title = "Table View", dataTableOutput(
                ns("heroPreferences")
              ))
            )
          )
        )
      )
    )
  )
}
