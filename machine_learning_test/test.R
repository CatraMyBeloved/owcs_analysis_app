hero_features_labeled <- hero_features_labeled |>
  select(-heroes_played)

hero_features_split <- initial_split(hero_features_labeled)

rand_forest_spec <- rand_forest() |>
  set_engine("ranger") |>
  set_mode("classification")

boost_tree_spec <- boost_tree() |>
  set_mode("classification") |>
  set_engine("xgboost") |>
  set_args(trees = 20, learn_rate = 0.15, min_n = 4)

rand_forest_wkflow <- workflow() |>
  add_model(rand_forest_spec) |>
  add_formula(composition ~ .)

boost_tree_wkflow <- workflow() |>
  add_model(boost_tree_spec) |>
  add_formula(composition ~ .)

final_boost_tree <- last_fit(boost_tree_wkflow, hero_features_split) |>
  extract_workflow()

final_boost_tree
collect_metrics(final_boost_tree)

hero_features_unlabeled <- hero_features_unlabeled |> select(-heroes_played)

prediction <- predict(final_boost_tree, new_data = hero_features_unlabeled)
