composition_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    "Composition Analysis",
    layout_sidebar(
      sidebar = sidebar(
        checkboxGroupInput(ns("regionFilter"), "Region",
                           choices = list("NA" = "north_america", "EMEA" = "emea", "Korea" = "korea"),
                           selected = list("north_america", "emea", "korea")),
        selectInput(ns("teamFilter"), "Team",
                    choices = c("All" = "All", team_list)),
        selectInput(ns("mapFilter"), "Map(s)",
                    choices = c("All" = "All", map_list)),
        checkboxGroupInput(ns("weekFilter"), "Week",
                           choices = list("Week 1" = 1, "Week 2" = 2,
                                          "Week 3" = 3, "Week 4" = 4,
                                          "Playoffs"),
                           selected = list(1,2,3,4,"Playoffs")),
        checkboxGroupInput(ns("modeFilter"), "Mode",
                           choices = list("Control", "Flashpoint", "Push",
                                          "Escort", "Hybrid"),
                           selected = list("Control", "Flashpoint", "Push",
                                           "Escort", "Hybrid"))
      ),
      card(
        card_header(
          "Composition counts"
        ),
        card_body(
          checkboxGroupInput(ns("roleSelection"), "Roles",
                             choices = list("Tank" = "tank", "DPS" = "dps", "Support" = "sup"),
                             inline = TRUE),
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