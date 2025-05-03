detail_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    "Detailed Analysis",
    layout_sidebar(
      sidebar = sidebar(
        region_filter(ns("regionFilter")),
        team_filter(ns("teamFilter")),
        week_filter(ns("weekFilter"))
      ),
      card(
        navset_card_pill(
          nav_panel(
            title = "Teams across map",
            fluidRow(
              column(6, selectInput(ns("mapSpecificSelection"),
                "Select Map",
                choices = map_list
              ), ),
              column(6, checkboxInput(
                ns("mapSpecificCompToggle"),
                "Comparison vs OWCS average"
              ))
            ),
            plotOutput(ns("mapSpecificPickrates")),
            plotOutput(ns("mapSpecificQuadrants"))
          ),
          nav_panel(title = "Maps across team"),
          nav_panel(title = "Map - Team")
        )
      )
    )
  )
}
