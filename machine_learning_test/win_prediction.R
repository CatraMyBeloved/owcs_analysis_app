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
  mutate(iswin = as.factor(iswin)) |>
  select(-rowid)

data_split <- initial_split(data_prep, prop = 0.8, strata = iswin)

recipe_pred <- recipe(iswin ~ ., data = data_prep) |>
  step_zv() |> 
  step_dummy_hash(all_nominal_predictors())

folds <- vfold_cv(data_prep, v = 5, strata = iswin)

# --------- LOG REGRESSION-----------
log_reg_spec <- logistic_reg(
  penalty = tune(),
  mixture = tune()
) |>
  set_engine("glmnet") |>
  set_mode("classification")

grid_vals_log_reg <- grid_regular(
  penalty(range = c(-5, 1), trans = log10_trans()),
  mixture(range = c(0, 1)),
  levels = c(10, 5)
)

log_reg_wf <- workflow() |>
  add_recipe(recipe_pred) |>
  add_model(log_reg_spec)

log_reg_tune_results <- tune_grid(
  log_reg_wf,
  resamples = folds,
  grid = grid_vals_log_reg,
  metrics = metric_set(roc_auc, accuracy)
)

log_reg_metrics <- collect_metrics(log_reg_tune_results)

log_reg_best_params <- log_reg_tune_results |>
  select_best(metric = "accuracy")

final_log_wf <- log_reg_wf |>
  finalize_workflow(log_reg_best_params)

final_log_fit <- log_reg_wf |> fit(data = training(data_split))


test_log_reg <- logistic_reg() |>
  set_engine("glmnet") |>
  set_args(
    penalty = 1e-05,
    mixture = 0.25
  )

test_fit <- fit(test_wf, data = training(data_split))

test_wf <- workflow() |>
  add_recipe(recipe_pred) |>
  add_model(test_log_reg)


#---------RANDOM FOREST----------

grid_vals_rf <- grid_regular(
  trees(range = c(100, 1000)),
  finalize(mtry(), predictors),
  min_n(range = c(1, 10)),
  levels = 5
)

random_forest_spec <-
  rand_forest(
    trees = tune(),
    mtry = tune(),
    min_n = tune()
  ) |>
  set_engine("ranger") |>
  set_mode("classification")

workflow_random_forest <- workflow() |>
  add_model(random_forest_spec) |>
  add_recipe(recipe_pred)

random_forest_tune_results <- tune_grid(
  workflow_random_forest,
  resamples = folds,
  grid = grid_vals_rf,
  metrics = metric_set(roc_auc, accuracy)
)

random_forest_metrics <- collect_metrics(random_forest_tune_results)

random_forest_metrics |>
  filter(.metric == "accuracy") |>
  arrange(desc(mean))

#----------BOOSTED TREE XGBOOST------------

boosted_tree_spec <- boost_tree(
  mtry = tune(),
  trees = tune(),
  min_n = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune()
) |>
  set_engine("xgboost") |>
  set_mode("classification")

boosted_tree_wf <- workflow() |>
  add_recipe(recipe_pred) |>
  add_model(model_boost_tree_spec)


grid_vals_bt <- grid_regular(
  finalize(mtry(), predictors),
  min_n(range = c(2, 10)),
  tree_depth(range = c(2, 10)),
  learn_rate(range = c(0.01, 0.5)),
  loss_reduction(range = c(0, 10)),
  trees(range = c(100, 1000)),
  levels = 3
)

boosted_tree_tune_results <- tune_grid(
  workflow_boost_tree,
  resamples = folds,
  grid = boosted_tree_grid,
  metrics = metric_set(roc_auc, accuracy)
)

boosted_tree_metrics <- collect_metrics(boosted_tree_tune_results)

boosted_tree_metrics |>
  ggplot(aes(x = trees, y = mean, color = mtry)) +
  geom_point() +
  geom_line()

#---------BOOSTED TREE CATBOOST---------
#

c5_spec <- boost_tree(
  trees = tune(),
  min_n = tune()
) |>
  set_engine("C5.0") |>
  set_mode("classification")

c5_workflow <- workflow() |>
  add_recipe(recipe_pred) |>
  add_model(c5_spec)

c5_grid <- grid_regular(
  trees(range = c(1, 100)),
  min_n(range = c(1, 50)),
  levels = 5
)

c5_results <- tune_grid(
  c5_workflow,
  resamples = folds,
  grid = c5_grid,
  metrics = metric_set(roc_auc, accuracy)
)

c5_metrics <- collect_metrics(c5_results)
