ban_ui <- function(id){
  ns <- NS(id)
  
  nav_panel(
    "Ban analysis",
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
                           selected = list(1,2,3,4,"Playoffs"))
      ),
      card(
        card_title("Banrates per hero"),
        card_body(
          dataTableOutput(ns("banrates"))
        )
      ),
      card(
        card_header("Ban Rates - Plot"),
        card_body(
          sliderInput(ns("topnBanrates"), "Top N heroes to show",
                      min = 1, max = 20, value = 10),
          plotOutput(ns("banratesVis"))
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