library(future)
library(stacks)
plan(multisession, workers = 4)



data <- comps_with_opponents |>
  left_join(match_maps, by = "match_map_id") |>
  left_join(maps, by = "map_id") |>
  left_join(matches, by = "match_id") |>
  left_join(teams, by = "team_name") |>
  left_join(predictors_bans, by = c("match_map_id", "team_id" = "team_id")) |>
  select(
    -match_map_id, -teamid, -teamid_opp,
    -map_win_team_id, -match_id, -map_id,
    -bracket, -week, -date, -team1_id, -team2_id, -mode
  ) |>
  group_by(round_id) |>
  slice_sample(n = 1) |>
  ungroup() |>
  left_join(heroes, by = c("hero_id")) |>
  rename(banned_hero = hero_name) |>
  left_join(heroes, by = c("hero_id_opp" = "hero_id")) |>
  rename(banned_hero_opp = hero_name) |>
  select(
    -round_id, -team_id, -region, -hero_id, -hero_id_opp,
    -role.x, -role.y
  )



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
  select(-rowid, -tank, -tank_opp, -dps_str, -dps_str_opp, -sup_str, -sup_str_opp)

data_split <- initial_split(data_prep, prop = 0.8, strata = iswin)

recipe_pred <- recipe(iswin ~ ., data = training(data_split)) |>
  step_dummy_hash(all_nominal_predictors(), num_terms = 16) |>
  step_zv()


folds <- vfold_cv(training(data_split), v = 5, strata = iswin)

test_rf <- rand_forest(
  trees = 1500,
  mtry = 30,
  min_n = 1
) |>
  set_engine("ranger") |>
  set_mode("classification")


test_wf <- workflow() |>
  add_model(test_rf) |>
  add_recipe(recipe_pred)

cv_results <- test_wf |>
  fit_resamples(
    resamples = folds,
    metrics = metric_set(accuracy, roc_auc)
  )

cv_metrics <- collect_metrics(cv_results)


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
  levels = 10
)

log_reg_wf <- workflow() |>
  add_recipe(recipe_pred) |>
  add_model(log_reg_spec)

log_reg_tune_results <- tune_grid(
  log_reg_wf,
  resamples = folds,
  grid = grid_vals_log_reg,
  metrics = metric_set(roc_auc, accuracy),
  control = control_stack_grid()
)

log_reg_metrics <- collect_metrics(log_reg_tune_results)


#---------RANDOM FOREST----------

grid_vals_rf <- grid_regular(
  trees(range = c(100, 2000)),
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
  metrics = metric_set(roc_auc, accuracy),
  control = control_stack_grid()
)

random_forest_metrics <- collect_metrics(random_forest_tune_results)

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
  add_model(boosted_tree_spec)


grid_vals_bt <- grid_random(
  finalize(mtry(), predictors),
  min_n(range = c(1, 10)),
  tree_depth(range = c(2, 20)),
  learn_rate(range = c(0.01, 0.5)),
  loss_reduction(range = c(0, 10)),
  trees(range = c(100, 2000)),
  size = 150
)

boosted_tree_tune_results <- tune_grid(
  boosted_tree_wf,
  resamples = folds,
  grid = grid_vals_bt,
  metrics = metric_set(roc_auc, accuracy),
  control = control_stack_grid()
)

boosted_tree_metrics <- collect_metrics(boosted_tree_tune_results)

#---------BOOSTED TREE C5---------


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
  metrics = metric_set(roc_auc, accuracy),
  control = control_stack_grid()
)

c5_metrics <- collect_metrics(c5_results)

#-----------MLP----------------

nn_spec <- mlp(
  hidden_units = tune(),
  penalty = tune(),
  epochs = tune(),
) |>
  set_engine("nnet", MaxNWts = 50000) %>%
  set_mode("classification")

nn_workflow <- workflow() |>
  add_recipe(recipe_pred) |>
  add_model(nn_spec)

nn_grid <- grid_regular(
  hidden_units(range = c(5, 30)),
  penalty(range = c(-5, -1), trans = log10_trans()),
  epochs(range = c(50, 300)),
  levels = 5
)

nn_tune_results <- tune_grid(
  nn_workflow,
  resamples = folds,
  grid = nn_grid,
  metrics = metric_set(roc_auc, accuracy),
  control = control_stack_grid()
)

nn_metrics <- collect_metrics(nn_tune_results)

#-----------Model Stack--------------


model_stack <- stacks() |>
  add_candidates(random_forest_tune_results) |>
  add_candidates(c5_results)

model_stack_2 <- model_stack |>
  blend_predictions()

model_stack_2 <- model_stack_2 |>
  fit_members()


test_data <- testing(data_split)

test_preds <- predict(model_stack_2, test_data)
test_probs <- predict(model_stack_2, test_data, type = "prob")

test_results <- test_data |>
  select(iswin) |>
  bind_cols(
    predicted = test_preds,
    test_probs
  )

test_results <- test_results %>%
  mutate(
    iswin = factor(iswin, levels = c("0", "1")),
    .pred_class = factor(.pred_class, levels = c("0", "1"))
  )

conf_mat <- conf_mat(test_results, truth = iswin, estimate = .pred_class)
print(conf_mat)

accuracy_table <- test_results |>
  accuracy(truth = iswin, .pred_class) |>
  summarise(
    avg_accuracy = mean(.estimate)
  )

#----------------SVM-------------------

# Load required libraries
library(tidymodels)
library(kernlab)

# Create SVM specification
svm_spec <- svm_rbf(
  cost = tune(),
  rbf_sigma = tune()
) %>%
  set_engine("kernlab") %>%
  set_mode("classification")

# Create recipe
svm_recipe <- recipe(iswin ~ ., data = training(data_split)) |>
  step_string2factor(all_string_predictors()) |>
  # Step 2: Create dummy variables for categorical data
  step_dummy_hash(all_nominal_predictors())

# Create workflow
svm_workflow <- workflow() %>%
  add_recipe(svm_recipe) %>%
  add_model(svm_spec)

svm_grid <- grid_regular(
  cost(range = c(-1, 2)), # 10^-1 to 10^2 (0.1 to 100)
  rbf_sigma(range = c(-3, 0)), # 10^-3 to 10^0 (0.001 to 1)
  levels = 5 # 5 values for each parameter
)

svm_results <- tune_grid(
  svm_workflow, # The workflow you created earlier
  resamples = folds, # Cross-validation folds
  grid = svm_grid, # Our custom grid
  metrics = metric_set(
    accuracy,
    roc_auc
  ),
  control = control_grid(save_pred = TRUE) # Save predictions for analysis
)

svm_metrics <- collect_metrics(svm_results)
