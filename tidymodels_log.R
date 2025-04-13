library(tidyverse)
library(tidymodels)

load("data/esports_data.RData")


pred_data <- match_maps |>
  left_join(matches, by = "match_id") |>
  left_join(teams, by = c("team1_id" = "team_id")) |>
  rename(team1_name = team_name) |>
  left_join(teams, by = c("team2_id" = "team_id")) |>
  rename(team2_name = team_name) |>
  left_join(maps, by = "map_id") |>
  select(map_name, team1_name, team2_name, map_win_team_id, team1_id, team2_id) |>
  mutate(iswin = case_when(
    team1_id == map_win_team_id ~ 1,
    .default = 0
  )) |>
  select(-team1_id, -team2_id, -map_win_team_id) |>
  mutate(iswin = as.factor(iswin))

data_split <- initial_split(pred_data, prop = 4 / 5)

data_train <- training(data_split)
data_test <- testing(data_split)

prediction_workflow <- workflow() |>
  add_model(logistic_reg())

prediction_recipe <- recipe(iswin ~ team1_name + team2_name + map_name,
  data = data_train
) |>
  step_dummy(all_nominal_predictors()) |>
  step_interact(~ starts_with("team"):starts_with("map"))

prediction_workflow <- prediction_workflow |>
  add_recipe(prediction_recipe)

prediction_fit <- fit(prediction_workflow, data = data_train)

predictions <- predict(prediction_fit, new_data = data_test)

complete_data <- data_test |>
  bind_cols(prediction = predictions) |>
  mutate(correct = case_when(
    .pred_class == iswin ~ 1,
    .default = 0
  )) 

mean(complete_data$correct)
