library(tidymodels)
library(stacks)
library(tidyverse)
library(yardstick)
library(future)
library(tictoc)

data <- data_prep
stacked_model <- model_stack_2 # Your existing stacked model
# Enable parallel processing
plan(multisession, workers = 4)
set.seed(123) # For reproducibility

# Start timing
tic("Total Evaluation Time")

# Create comprehensive cross-validation folds
n_folds <- 10
cv_folds <- vfold_cv(data, v = n_folds, strata = iswin)

# Create a recipe for preprocessing
recipe_pred <- recipe(iswin ~ ., data = data) |>
  step_zv() |>
  step_dummy_hash(all_nominal_predictors())

# Define metrics we want to calculate
eval_metrics <- metric_set(accuracy, roc_auc, precision, recall, f_meas)

# Define models with the best parameters from your previous tuning
# Logistic Regression
log_reg_spec <- logistic_reg(
  penalty = 4.641589e-03,
  mixture = 1.00
) |>
  set_engine("glmnet") |>
  set_mode("classification")

log_reg_wf <- workflow() |>
  add_recipe(recipe_pred) |>
  add_model(log_reg_spec)

# Random Forest
rf_spec <- rand_forest(
  trees = 325,
  mtry = 15,
  min_n = 1
) |>
  set_engine("ranger", importance = "permutation") |>
  set_mode("classification")

rf_wf <- workflow() |>
  add_recipe(recipe_pred) |>
  add_model(rf_spec)

# C5.0 Boosted Tree
c5_spec <- boost_tree(
  trees = 100,
  min_n = 1
) |>
  set_engine("C5.0") |>
  set_mode("classification")

c5_wf <- workflow() |>
  add_recipe(recipe_pred) |>
  add_model(c5_spec)

# Fit models on cross-validation folds and collect results
log_reg_results <- fit_resamples(
  log_reg_wf,
  resamples = cv_folds,
  metrics = eval_metrics,
  control = control_stack_resamples()
)

rf_results <- fit_resamples(
  rf_wf,
  resamples = cv_folds,
  metrics = eval_metrics,
  control = control_stack_resamples()
)

c5_results <- fit_resamples(
  c5_wf,
  resamples = cv_folds,
  metrics = eval_metrics,
  control = control_stack_resamples()
)

# Collect and summarize metrics for each model
log_reg_metrics <- collect_metrics(log_reg_results)
rf_metrics <- collect_metrics(rf_results)
c5_metrics <- collect_metrics(c5_results)

# Create a workflow for your existing stacked model

stacked_model <- stacks() |>
  add_candidates(log_reg_results) |>
  add_candidates(rf_results) |>
  add_candidates(c5_results) |>
  blend_predictions()

print(stacked_model)

fitted_stack <- fit_members(stacked_model)

# Combine all metrics for comparison
all_metrics <- bind_rows(
  log_reg_metrics |> mutate(model = "Logistic Regression"),
  rf_metrics |> mutate(model = "Random Forest"),
  c5_metrics |> mutate(model = "C5.0"),
  stacked_metrics |> mutate(model = "Stacked Model")
)

# Print summary statistics
cat("Model Performance Summary:\n")
print(all_metrics |>
  select(model, .metric, mean, std_err) |>
  arrange(model, .metric))

# Create a visual comparison of models
ggplot(
  all_metrics |> filter(.metric %in% c("accuracy", "roc_auc")),
  aes(x = model, y = mean, fill = .metric)
) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = mean - std_err, ymax = mean + std_err),
    position = position_dodge(width = 0.9),
    width = 0.25
  ) +
  theme_minimal() +
  labs(
    title = "Model Performance Comparison",
    y = "Score",
    x = NULL
  ) +
  coord_flip()

# Stop timing
toc()
