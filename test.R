library(dplyr)
library(stringr)
library(tidyr)

all_data <- 
  hero_composition |> 
    left_join(heroes, by = "hero_id") |> 
    left_join(rounds, by = "round_id") |> 
    left_join(match_maps, by = "match_map_id") |> 
    left_join(matches, by = "match_id") |> 
    left_join(teams, by = c("team" = "team_id")) |> 
    left_join(maps, by = "map_id") |> 
    mutate(iswin = case_when(team == map_win_team_id ~ 1,
                             .default = 0))

create_hero_matrix <- function(compositions, all_heroes){
  all_heroes <- c(unlist(all_heroes))
  n_rows <- nrow(compositions)
  n_columns <- length(all_heroes)
  
  team_matrix <- matrix(FALSE, nrow = n_rows, ncol = n_columns)
  opp_matrix <- matrix(FALSE, nrow = n_rows, ncol = n_columns)
  
  colnames(team_matrix) <- all_heroes
  colnames(opp_matrix) <- all_heroes
  
  for(i in 1:n_rows){
    team_heroes <- c(compositions$tank[i], 
                     unlist(compositions$dps[i]), 
                     unlist(compositions$sup[i]))
    
    opp_heroes <- c(compositions$tank_opp[i],
                    unlist(compositions$dps_opp[i]),
                    unlist(compositions$sup_opp[i]))
    
    team_matrix[i, match(team_heroes, all_heroes)] <- TRUE
    opp_matrix[i, match(opp_heroes, all_heroes)] <- TRUE
  }
  team_df <- as.data.frame(team_matrix)
  opp_df <- as.data.frame(opp_matrix)
  
  names(team_df) <- paste0("has_", names(team_df))
  names(opp_df) <- paste0("has_", names(opp_df), "_opp")
  
  result <- bind_cols(compositions, team_df, opp_df)
}

comps <- all_data |> 
  group_by(match_map_id, round_id, team_name) |> 
  reframe(
    tank = hero_name[role == "tank"],
    dps = list(head(hero_name[role == "dps"], 2)),
    sup = list(head(hero_name[role == "sup"], 2)),
    iswin = mean(iswin)  
  )

base_comps <- comps

compositions_with_teamid <- 
  base_comps |> 
  group_by(round_id) |> 
  mutate(teamid = row_number()) |> 
  ungroup()

comps_with_opponents <- 
  compositions_with_teamid |> 
  inner_join(
    compositions_with_teamid |> 
      select(round_id, teamid, team_name, 
             tank, dps, sup),
    by = "round_id",
    suffix = c("", "_opp"),
    relationship = "many-to-many"
  ) |> 
  filter(teamid != teamid_opp)

comps <- comps_with_opponents

test <- create_hero_matrix(comps, hero_list)    

clustering_columns <- test |> 
  select(starts_with("has_") & !contains("_opp"))

k <- 6
cluster_result <- kmeans(clustering_columns, centers = k)

test$cluster <- cluster_result$cluster

centers <- as.data.frame(cluster_result$centers)

centers_long <- centers |> 
  mutate(cluster = row_number()) |> 
  pivot_longer(cols = -cluster,
               names_to = "hero",
               values_to = "frequency")
centers_long |> 
  ggplot(aes(y = hero, x = factor(cluster), fill = frequency)) +
  geom_tile() 

dist_matrix <- dist(clustering_columns, method = "binary")

# Step 4: Perform hierarchical clustering
# Try different linkage methods
hclust_complete <- hclust(dist_matrix, method = "complete")
hclust_average <- hclust(dist_matrix, method = "average")
hclust_ward <- hclust(dist_matrix, method = "ward.D2")

# Step 5: Visualize dendrograms
# Plot the dendrogram - we'll use ward's method which often gives nice clusters
par(mar = c(3, 1, 1, 12))  # Adjust margins
plot(hclust_ward, hang = -1, labels = FALSE, main = "Hierarchical Clustering of Team Compositions", cex = 0.6)

k <- 5
clusters <- cutree(hclust_ward, k = k)

test$hclust <- clusters

# Step 7: Compare cluster sizes
hclust_sizes <- table(test$hclust)
kmeans_sizes <- table(test$cluster)
print("Hierarchical Clustering Sizes:")
print(hclust_sizes)
print("Kmeans clustering sizes:")
print(kmeans_sizes)

dend <- as.dendrogram(hclust_ward)
colors_to_use <- rainbow(k)

# Plot the colored dendrogram
par(mar = c(3, 1, 1, 12))
plot(dend, main = "Hierarchical Clustering with Colored Clusters")
