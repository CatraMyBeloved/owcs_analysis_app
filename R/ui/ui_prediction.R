# R/ui/ui_prediction.R
prediction_ui <- function(id) {
  ns <- NS(id)

  nav_panel(
    "Match Prediction",
    layout_sidebar(
      sidebar = sidebar(
        # Model selection
        selectInput(ns("modelSelection"), "Select Model",
          choices = c("Random Forest", "Boosted Tree"),
          selected = "Random Forest"
        ),

        # Map selection
        selectInput(ns("mapSelection"), "Select Map",
          choices = map_list
        )
      ),

      # Warning card
      card(
        status = "warning",
        card_header("⚠️ Experimental Feature"),
        p("This prediction tool is experimental and should be used for fun rather than making strategic decisions."),
        p("The model is based on team matchups, map choice, and hero bans."),
        p("Predictions are most accurate for common matchups and bans seen in previous tournaments.")
      ),

      # Main content - Teams and Bans (removed cards, using direct column layout)
      card(
        card_header("Team Matchup"),

        # Remove the nested cards and use direct columns
        fluidRow(
          column(
            width = 6,
            h4("Team 1"),
            selectInput(ns("team1Selection"), "Team Name",
              choices = team_list
            ),
            selectInput(ns("team1BanSelection"), "Banned Hero",
              choices = hero_list
            )
          ),
          column(
            width = 6,
            h4("Team 2"),
            selectInput(ns("team2Selection"), "Team Name",
              choices = team_list
            ),
            selectInput(ns("team2BanSelection"), "Banned Hero",
              choices = hero_list
            )
          )
        ),

        # Prediction results
        div(
          style = "margin-top: 20px;",
          card_header("Prediction"),
          layout_column_wrap(
            width = 1 / 2,
            value_box(
              title = "Win Confidence",
              value = textOutput(ns("winProbability")),
              showcase = bs_icon("graph-up")
            ),
            value_box(
              title = "Predicted Winner",
              value = textOutput(ns("predictionOutcome")),
              showcase = bs_icon("trophy")
            )
          )
        ),
        div(
          style = "margin-top: 20px;",
          actionButton(ns("predictButton"), "Make Prediction",
            class = "btn-lg btn-primary",
            width = "100%"
          )
        )
      )
    )
  )
}
