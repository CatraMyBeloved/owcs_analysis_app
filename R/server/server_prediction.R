# R/server/server_prediction.R
prediction_server <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {
    # Load models once at startup - no reactivity needed for this
    models <- list(
      "Random Forest" = readRDS("models/random_forest_model_final.rds"),
      "Boosted Tree" = readRDS("models/c5_model_final.rds"),
      "Stacked Model" = readRDS("models/model_stack_trees.rds")
    )

    recipe <- readRDS("models/preprocessing_recipe.rds")

    # Create prediction data when button is clicked
    prediction_data <- eventReactive(input$predictButton, {
      # Get map mode using app_data
      map_data <- app_data$maps %>%
        filter(map_name == input$mapSelection)

      selected_map_mode <- if (nrow(map_data) > 0) map_data$mode[1] else "Unknown"

      # Create a random round_id between 0 and 1700
      dummy_round_id <- sample(0:1700, 1)

      # Create a data frame with the right structure for prediction
      data_for_prediction <- tibble(
        round_id = dummy_round_id,
        team_name = input$teamSelection,
        team_name_opp = input$opponentSelection,
        map_name = input$mapSelection,
        mode = selected_map_mode,
        tank = input$tankSelection,
        dps_str = paste(sort(c(input$dps1Selection, input$dps2Selection)), collapse = ","),
        sup_str = paste(sort(c(input$sup1Selection, input$sup2Selection)), collapse = ","),
        tank_opp = input$tankSelectionOpp,
        dps_str_opp = paste(sort(c(input$dps1SelectionOpp, input$dps2SelectionOpp)), collapse = ","),
        sup_str_opp = paste(sort(c(input$sup1SelectionOpp, input$sup2SelectionOpp)), collapse = ",")
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

      # Create results
      tibble(
        win_probability = predictions$.pred_1,
        predicted_outcome = if_else(win_probability > 0.5, "Win", "Loss")
      )
    })

    # Outputs
    output$winProbability <- renderText({
      req(prediction_results())
      scales::percent(prediction_results()$win_probability, accuracy = 0.1)
    })

    output$predictionOutcome <- renderText({
      req(prediction_results())
      prediction_results()$predicted_outcome
    })
  })
}
