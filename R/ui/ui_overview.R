# R/ui/ui_overview.R

overview_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    "Overview",
    layout_sidebar(
      sidebar = sidebar(
        helpText("Select criteria to filter composition data."),
        checkboxGroupInput(ns("weekFilter"), "Week",
                           choices = list("Week 1" = 1, "Week 2" = 2,
                                          "Week 3" = 3, "Week 4" = 4,
                                          "Playoffs"),
                           selected = list(1,2,3,4,"Playoffs")),
        checkboxGroupInput(ns("regionFilter"), "Region",
                           choices = list("NA"= "north_america", "EMEA" = "emea", "Korea" = "korea"),
                           selected = list("north_america", "emea", "korea")),
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
        card_header("General Pickrates"),
        card_body(
          dataTableOutput(ns("Pickrates"))
        )
      ),
      card(
        card_header("General Pickrates - Plot"),
        card_body(
          sliderInput(ns("topnPickrates"), "Top N heroes to show",
                      min = 1, max = 20, value = 10),
          plotOutput(ns("PickratesVis"))
        )
      )
    )
  )
}