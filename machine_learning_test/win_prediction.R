data <- comps_with_opponents |>
  left_join(match_maps, by = "match_map_id") |>
  left_join(maps, by = "map_id") |>
  select(
    -match_map_id, -teamid, -teamid_opp,
    -map_win_team_id, -match_id, -map_id, -mode
  ) |>
  group_by(round_id) |>
  slice_sample(n = 1) |>
  ungroup()

labels <- data |> select(iswin)

predictors <- data |> select(-iswin)

rm(list = setdiff(ls(), c("predictors", "labels")))

predictors <- predictors |>
  mutate(rowid = row_number()) |>
  rowwise() |>
  mutate(
    dps_str = paste(sort(dps), collapse = ","),
    sup_str = paste(sort(sup), collapse = ","),
    dps_str_opp = paste(sort(dps_opp), collapse = ","),
    sup_str_opp = paste(sort(sup_opp), collapse = ",")
  )

predictors_collapsed <- predictors |> select(-dps, -sup, -dps_opp, -sup_opp)

data_prep <- bind_cols(predictors_collapsed, labels) |>
  mutate(iswin = as.factor(iswin))

recipe_pred <- recipe(iswin ~ ., data = data_prep) |>
  step_dummy_hash(all_nominal_predictors())

# --------- LOG REGRESSION-----------
model_win_log <- logistic_reg() |>
  set_engine("glm") |>
  set_mode("classification")

workflow_pred_log <- workflow() |>
  add_recipe(recipe_pred) |>
  add_model(model_win_log)

data_split <- initial_split(data_prep, prop = 0.8, strata = iswin)

fit <- workflow_pred_log |> fit(data = training(data_split))

preds <- predict(fit, testing(data_split), type = "prob") |>
  bind_cols(predict(fit, testing(data_split))) |>
  bind_cols(testing(data_split))

metrics(preds, truth = iswin, estimate = .pred_class)
roc_auc(preds, truth = iswin, .pred_0)
conf_mat(preds, truth = iswin, .pred_class)

#---------RANDOM FOREST----------

grid_vals_rf <- grid_regular(
  trees(range = c(100, 1000)),
  mtry(range = c(5, 30)),
  min_n(range = c(1, 10)),
  levels = 5
)

model_rf <-
  rand_forest(
    trees = tune(), # number of trees
    mtry = tune(), # number of features to consider
    min_n = tune() # minimum node size
  ) |>
  set_engine("ranger") |>
  set_mode("classification")

workflow_model_rf <- workflow() |>
  add_model(model_rf) |>
  add_recipe(recipe_pred)
folds <- vfold_cv(data_prep, v = 5, strata = iswin)

tune_results_rf <- tune_grid(
  workflow_model_rf,
  resamples = folds,
  grid = grid_vals_rf,
  metrics = metric_set(roc_auc, accuracy)
)


#----------BOOSTED TREE------------

model_boost_tree_spec <- boost_tree(
  mtry = tune(),
  trees = tune(),
  min_n = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune()
) |>
  set_engine("xgboost") |>
  set_mode("classification")

workflow_boost_tree <- workflow() |>
  add_recipe(recipe_pred) |>
  add_model(model_boost_tree_spec)


boost_params <- parameters(
  finalize(mtry(), predictors),
  min_n(range = c(2, 10)),
  tree_depth(range = c(2, 10)),
  learn_rate(range = c(0.01, 0.5)),
  loss_reduction(range = c(0, 10)),
  trees()
)

boost_grid <- grid_regular(
  boost_params,
  levels = 3
)

boost_tune <- tune_grid(
  workflow_boost_tree,
  resamples = folds,
  grid = boost_grid,
  metrics = metric_set(roc_auc, accuracy)
)
