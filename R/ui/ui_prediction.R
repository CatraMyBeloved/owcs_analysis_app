# R/ui/ui_prediction.R
prediction_ui <- function(id) {
  ns <- NS(id)

  # Filter heroes by role
  tank_heroes <- heroes %>%
    filter(role == "tank") %>%
    pull(hero_name)
  dps_heroes <- heroes %>%
    filter(role == "dps") %>%
    pull(hero_name)
  sup_heroes <- heroes %>%
    filter(role == "sup") %>%
    pull(hero_name)

  nav_panel(
    "Match Prediction",
    layout_sidebar(
      sidebar = sidebar(
        # Model selection
        selectInput(ns("modelSelection"), "Select Model",
          choices = c("Random Forest", "Boosted Tree", "Stacked Model"),
          selected = "Stacked Model"
        ),

        # Map selection
        selectInput(ns("mapSelection"), "Select Map",
          choices = map_list
        ),

        # Team selections
        selectInput(ns("teamSelection"), "Your Team",
          choices = team_list
        ),
        selectInput(ns("opponentSelection"), "Opponent Team",
          choices = team_list
        )
      ),

      # Add this near the top of your prediction_ui function, right after the nav_panel title
      card(
        status = "warning",
        card_header("⚠️ Experimental Feature"),
        p("This prediction tool is experimental and should be used for fun rather than making strategic decisions."),
        p("The model works best with hero combinations and matchups that appear frequently in past tournaments. Unusual team compositions or rarely-seen hero picks may result in less accurate predictions."),
        p("Think of it like asking a coach who has only watched certain matches - they can make educated guesses based on what they've seen before, but can't predict entirely new situations with the same confidence.")
      ),

      # Main content
      card(
        card_header("Team Compositions"),
        layout_column_wrap(
          width = 1 / 2,
          # Your team composition
          card(
            card_header("Your Team"),
            selectInput(ns("tankSelection"), "Tank", choices = tank_heroes),
            fluidRow(
              column(6, selectInput(ns("dps1Selection"), "DPS 1", choices = dps_heroes)),
              column(6, selectInput(ns("dps2Selection"), "DPS 2", choices = dps_heroes))
            ),
            fluidRow(
              column(6, selectInput(ns("sup1Selection"), "Support 1", choices = sup_heroes)),
              column(6, selectInput(ns("sup2Selection"), "Support 2", choices = sup_heroes))
            )
          ),
          # Opponent team composition
          card(
            card_header("Opponent Team"),
            selectInput(ns("tankSelectionOpp"), "Tank", choices = tank_heroes),
            fluidRow(
              column(6, selectInput(ns("dps1SelectionOpp"), "DPS 1", choices = dps_heroes)),
              column(6, selectInput(ns("dps2SelectionOpp"), "DPS 2", choices = dps_heroes))
            ),
            fluidRow(
              column(6, selectInput(ns("sup1SelectionOpp"), "Support 1", choices = sup_heroes)),
              column(6, selectInput(ns("sup2SelectionOpp"), "Support 2", choices = sup_heroes))
            )
          )
        ),

        # Prediction results
        card(
          card_header("Prediction"),
          layout_column_wrap(
            width = 1 / 2,
            value_box(
              title = "Win Confidence",
              value = textOutput(ns("winProbability")),
              showcase = bs_icon("graph-up")
            ),
            value_box(
              title = "Predicted Outcome",
              value = textOutput(ns("predictionOutcome")),
              showcase = bs_icon("trophy")
            )
          )
        ),
        actionButton(ns("predictButton"), "Make Prediction",
          class = "btn-lg btn-primary",
          width = "100%"
        )
      )
    )
  )
}
