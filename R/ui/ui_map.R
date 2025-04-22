#' -----------------------------------------------------------------------------
#' ui_map.R
#'
#' Description: Contains UI elements and layout for map analysis page
#'
#' Author: CatraMyBeloved
#' Date Created: 03-04-2025
#' Last Modified: 02-04-2025
#' -----------------------------------------------------------------------------


map_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    "Detailed Analysis",
    layout_sidebar(
      sidebar = sidebar(
        region_filter(ns("regionFilter")),
        map_filter(ns("mapFilter"), all_option = FALSE),
        team_filter(ns("teamFilter")),
        week_filter(ns("weekFilter")),
        role_filter(ns("roleFilter"))
      ),
      card(
        card_header(
          HTML("<h4>Map-Specific Hero Usage</h4>
          <p class='text-muted'>Heroes that are picked more/less on this map than across all maps</p>")
        ),
        card_body(
          sliderInput(ns("topnPickrates"), "Number of heroes to display", min = 1, max = 20, value = 10),
          plotOutput(ns("PickratesVisAllMaps"))
        )
      ),
      card(
        card_header(HTML("<h4>Team's Signature Picks</h4>
          <p class='text-muted'>Heroes this team uses differently than other teams on this map</p>")),
        plotOutput(
          ns("PickratesVisSelectedMaps")
        )
      ),
      card(
        card_header("Hero pickrates on selected map"),
        card_body(
          dataTableOutput(
            ns("Pickrates")
          )
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
