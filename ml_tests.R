library(tidyverse)
library(tidymodels)

#----- prepare data -------


load("data/esports_data.RData")

all_data <-
  hero_composition |>
  left_join(heroes, by = "hero_id") |>
  left_join(rounds, by = "round_id") |>
  left_join(match_maps, by = "match_map_id") |>
  left_join(matches, by = "match_id") |>
  left_join(teams, by = c("team" = "team_id")) |>
  left_join(maps, by = "map_id") |>
  mutate(iswin = case_when(team == map_win_team_id ~ 1,
    .default = 0
  ))

hero_list <- heroes |>
  pull(hero_name) |>
  as.list()
create_hero_matrix <- function(compositions, all_heroes) {
  all_heroes <- c(unlist(all_heroes))
  n_rows <- nrow(compositions)
  n_columns <- length(all_heroes)

  team_matrix <- matrix(FALSE, nrow = n_rows, ncol = n_columns)
  opp_matrix <- matrix(FALSE, nrow = n_rows, ncol = n_columns)

  colnames(team_matrix) <- all_heroes
  colnames(opp_matrix) <- all_heroes

  for (i in 1:n_rows) {
    team_heroes <- c(
      compositions$tank[i],
      unlist(compositions$dps[i]),
      unlist(compositions$sup[i])
    )

    opp_heroes <- c(
      compositions$tank_opp[i],
      unlist(compositions$dps_opp[i]),
      unlist(compositions$sup_opp[i])
    )

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
      select(
        round_id, teamid, team_name,
        tank, dps, sup
      ),
    by = "round_id",
    suffix = c("", "_opp"),
    relationship = "many-to-many"
  ) |>
  filter(teamid != teamid_opp)

comps <- comps_with_opponents

comp_matrix <- create_hero_matrix(comps, hero_list)

hero_features <- comp_matrix %>%
  select(starts_with("has_") & !contains("_opp"))
