data <- comps |> select(tank, dps, sup)

data <- data %>%
  rowwise() %>%
  mutate(
    tank_text = tank,
    dps_text = paste(sort(unlist(dps)), collapse = " "),
    sup_text = paste(sort(unlist(sup)), collapse = " "),
    full_comp = paste(
      tank, paste(sort(unlist(dps)), collapse = " "),
      paste(sort(unlist(sup)), collapse = " ")
    )
  ) %>%
  ungroup() |>
  select(tank_text, dps_text, sup_text)

hash_recipe <- recipe(~., data = data) |>
  step_tokenize(all_nominal_predictors()) |>
  step_texthash(all_predictors(), num_terms = 32)

hash_prep <- prep(hash_recipe)
hashed_features <- bake(hash_prep, new_data = NULL)

wss <- tibble(k = 1:10) %>%
  mutate(
    kclust = map(k, ~ kmeans(hashed_features, centers = .x, nstart = 25)),
    wss = map_dbl(kclust, ~ .x$tot.withinss)
  )

# Plot elbow curve
ggplot(wss, aes(x = k, y = wss)) +
  geom_line() +
  geom_point() +
  labs(title = "Elbow method for optimal k")

k <- 5

kmeans_spec <- k_means(num_clusters = k) %>%
  set_engine("stats")

kmeans_fit <- kmeans_spec %>%
  fit(~., data = hashed_features)

# Extract cluster assignments
clusters <- kmeans_fit %>%
  extract_cluster_assignment()

comps_with_clusters <- data %>%
  bind_cols(cluster = clusters$.cluster)

comps_with_clusters |>
  filter(cluster == "Cluster_2") |>
  count(tank_text, dps_text, sup_text) |>
  arrange(desc(n))

# 6. Visualize with dimensionality reduction
# Use PCA to visualize clusters
pca_recipe <- recipe(~., data = hashed_features) %>%
  step_pca(all_predictors(), num_comp = 2)

pca_prep <- prep(pca_recipe)
pca_result <- bake(pca_prep, new_data = NULL)

# Visualization
pca_result %>%
  bind_cols(cluster = clusters$.cluster) %>%
  ggplot(aes(x = PC1, y = PC2, color = factor(cluster))) +
  geom_point(alpha = 0.7) +
  labs(title = "Composition Clusters", color = "Cluster")
