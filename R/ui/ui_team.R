#Team panel ----------

team_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    
    "Team Analysis",
    layout_sidebar(
      sidebar = sidebar(
        checkboxGroupInput(ns("regionFilter"), "Region",
                           choices = list("NA" = "north_america", "EMEA" = "emea", "Korea" = "korea"),
                           selected = list("north_america", "emea", "korea")),
        selectInput(ns("teamFilter"), "Team",
                    choices = team_list),
        checkboxGroupInput(ns("weekFilter"), "Week",
                           choices = list("Week 1" = 1, "Week 2" = 2,
                                          "Week 3" = 3, "Week 4" = 4,
                                          "Playoffs"),
                           selected = list(1,2,3,4,"Playoffs")),
        checkboxGroupInput(ns("modeFilter"), "Modes",
                           choices = list("Control", "Flashpoint", "Push",
                                          "Escort", "Hybrid"),
                           selected = list("Control", "Flashpoint", "Push",
                                           "Escort", "Hybrid")),
        checkboxGroupInput(ns("roleFilter"), "Roles",
                           choices = list("Tank" = "tank", "Support" = "sup", "DPS" = "dps"),
                           selected = list("tank", "sup", "dps"))
        
      ),
      card(
        card_header("Team pickrates"),
        card_body(
          dataTableOutput(ns("Pickrates"))
        )
      ),
      card(
        card_header("Team pickrates - Plot"),
        card_body(
          sliderInput(ns("topnPickrates"), "Top N heroes to show",
                      min = 1, max = 20, value = 10),
          plotOutput(ns("PickratesVis"))
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
