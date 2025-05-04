# R/server/server_prediction.R
prediction_server <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {
    # Load models once at startup
    models <- list(
      "Random Forest" = readRDS("models/random_forest_model_final.rds"),
      "Boosted Tree" = readRDS("models/c5_model_final.rds")
    )


    # Create prediction data when button is clicked
    prediction_data <- eventReactive(input$predictButton, {
      # Get the most recent patch date from matches
      recent_match <- app_data$matches %>%
        filter(!is.na(patch_date)) %>%
        arrange(desc(as.Date(date, format = "%d/%m/%Y"))) %>%
        slice(1)

      # Create data with the right structure for prediction
      data_for_prediction <- tibble(
        team_name = input$team1Selection,
        team_name_opp = input$team2Selection,
        map_name = input$mapSelection,
        patch_date = recent_match$patch_date[1],
        banned_hero = input$team1BanSelection,
        banned_hero_opp = input$team2BanSelection
      )

      return(data_for_prediction)
    })

    # Make prediction using selected model
    prediction_results <- eventReactive(input$predictButton, {
      req(prediction_data())

      # Get the selected model
      selected_model <- models[[input$modelSelection]]

      # Make prediction with probability
      predictions <- predict(selected_model, prediction_data(), type = "prob")

      # Extract probability for Team 1 win (class '1')
      team1_win_prob <- predictions$.pred_1

      # Create results - which team is predicted to win
      predicted_winner <- if_else(team1_win_prob > 0.5,
        input$team1Selection,
        input$team2Selection
      )

      # Calculate confidence (probability of predicted winner)
      confidence <- if_else(team1_win_prob > 0.5,
        team1_win_prob,
        1 - team1_win_prob
      )

      # Return results
      tibble(
        win_confidence = confidence,
        predicted_winner = predicted_winner
      )
    })

    # Outputs
    output$winProbability <- renderText({
      req(prediction_results())
      scales::percent(prediction_results()$win_confidence, accuracy = 0.1)
    })

    output$predictionOutcome <- renderText({
      req(prediction_results())
      prediction_results()$predicted_winner
    })
  })
}
