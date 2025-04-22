hero_features_split <- initial_split(hero_features_labeled)

training_set <- training(hero_features_split)
test_set <- testing(hero_features_split)

rand_forest_spec <- rand_forest() |>
  set_engine("ranger") |>
  set_mode("classification")

rand_forest_fit <- rand_forest_spec |>
  fit(composition ~ ., data = training_set)

unlabeled_sample$prediction <- predict(rand_forest_fit, unlabeled_sample)

boost_tree_spec <- boost_tree() |>
  set_mode("classification") |>
  set_engine("xgboost") |>
  set_args(trees = 20, learn_rate = 0.15, min_n = 4)

boost_tree_spec

boost_tree_fit <- boost_tree_spec |>
  fit(composition ~ ., data = training_set)

unlabeled_sample$boost_prediction <- predict(boost_tree_fit, unlabeled_sample |>
  select(-prediction))

test_set$prediction <- predict(rand_forest_fit, test_set)
test_set$boost_prediction <- predict(boost_tree_fit, test_set |> select(-prediction))

evaluation <- test_set |>
  select(composition, prediction, boost_prediction)
