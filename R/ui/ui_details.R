detail_ui <- function(id) {
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
      )
    )
  )
}
