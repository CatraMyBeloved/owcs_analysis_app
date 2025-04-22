#' -----------------------------------------------------------------------------
#' update.R
#'
#' Description: Script to download current state of the spreadsheet and turn it
#' into tables
#'
#' Author: CatraMyBeloved
#' Date Created: 03-04-2025
#' Last Modified: 02-04-2025
#' -----------------------------------------------------------------------------


library(rvest)
library(tidyverse)
url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vRTkWc3-NK0HtsOjipS8cqYMJRTddCSgecM-KD4wbNSpdjDJcpaYt-E-N8jU6j13CefwHGZ3I22SV-x/pubhtml"

data <- url %>%
  read_html() %>%
  html_table()

message("Downloading newest data...")

column_names <- c(
  "vod_link", "patch_date", "bracket",
  "week", "date", "map", "t1_round",
  "t1_team", "t1_result", "vs", "t2_result",
  "t2_team", "t2_round", "first_ban_team",
  "first_ban_hero", "second_ban_team",
  "second_ban_hero", "t1_tank", "t1_dps1",
  "t1_dps2", "t1_sup1", "t1_sup2", "t2_tank",
  "t2_dps1", "t2_dps2", "t2_sup1", "t2_sup2", "notes"
)

clean_table <- function(table, col_names) {
  table <- table[3:nrow(table), c(2, 4:29)]
  names(table) <- col_names
  table <- table[!is.na(table$vod_link) & table$vod_link != "", ]
  table
}

cleaned_data <- lapply(data, clean_table, col_names = column_names)

cleaned_data[[1]] <- cleaned_data[[1]] |> mutate(
  region = "china",
  week = "hangzhou_lan"
)
cleaned_data[[2]] <- cleaned_data[[2]] |> mutate(region = "korea")
cleaned_data[[3]] <- cleaned_data[[3]] |> mutate(region = "emea")
cleaned_data[[4]] <- cleaned_data[[4]] |> mutate(region = "north_america")

combined_data <- bind_rows(cleaned_data)

message("Processing into tables...")

# Modification to team creation logic in update.R
new_teams <- combined_data |>
  select(t1_team, t2_team, region, week) |>
  pivot_longer(cols = c(t1_team, t2_team), values_to = "team_name") |>
  mutate(is_lan = (region == "china" & week == "hangzhou_lan")) |>
  group_by(team_name) |>
  arrange(is_lan) |>
  slice(1) |>
  ungroup() |>
  select(-is_lan, -week, -name) |>
  mutate(team_id = row_number())

new_maps <- combined_data |>
  select(map) |>
  distinct(map) |>
  mutate(map_id = row_number()) |>
  mutate(
    map_name = map, map = NULL,
    mode = case_when(
      map_name %in% c("Lijiang Tower", "Antarctic Peninsula", "Oasis", "Nepal", "Samoa", "Ilios") ~ "Control",
      map_name %in% c("New Junk City", "Suravasa") ~ "Flashpoint",
      map_name %in% c("King's Row", "Numbani", "Paraiso", "Eichenwalde", "Blizzard World", "Dorado") ~ "Hybrid",
      map_name %in% c("Esperanca", "New Queen Street", "Colosseo", "Runasapi") ~ "Push",
      map_name %in% c("Gibraltar", "Havana", "Rialto", "Route 66", "Circuit Royal") ~ "Escort"
    )
  )

new_heroes <- combined_data |>
  select(
    t1_tank, t1_dps1, t1_dps2, t1_sup1,
    t1_sup2, t2_tank, t2_dps1, t2_dps2,
    t2_sup1, t2_sup2
  ) |>
  pivot_longer(everything(), values_to = "hero_name", names_to = "role") |>
  mutate(role = str_remove(role, "t[12]_")) |>
  mutate(role = str_remove(role, "[12]")) |>
  distinct(hero_name, role) |>
  mutate(hero_id = row_number())

new_matches <- combined_data |>
  select(date, patch_date, bracket, week, t1_team, t2_team) |>
  distinct() |>
  mutate(match_id = row_number()) |>
  left_join(new_teams, by = c("t1_team" = "team_name")) |>
  rename(team1_id = team_id) |>
  left_join(new_teams, by = c("t2_team" = "team_name")) |>
  rename(team2_id = team_id) |>
  mutate(week = case_when(
    week == "hangzhou_lan" ~ "hangzhou_lan", # Preserve Hangzhou LAN explicitly
    is.na(suppressWarnings(as.numeric(week))) ~ "Playoffs", # Other non-numeric weeks become Playoffs
    .default = week # Keep numeric weeks as is
  )) |>
  select(match_id, team1_id, team2_id, everything(), -t1_team, -t2_team, -region.x, -region.y)

new_match_maps <- combined_data |>
  select(date, t1_team, t2_team, map, t1_result, t2_result) |>
  group_by(date, t1_team, t2_team, map) |>
  mutate(map_winner = case_when(
    any(grepl("W", t1_result)) ~ t1_team,
    any(grepl("W", t2_result)) ~ t2_team
  )) |>
  ungroup() |>
  select(-t1_result, -t2_result) |>
  distinct() |>
  left_join(new_teams, c("t1_team" = "team_name")) |>
  rename(t1_id = team_id) |>
  left_join(new_teams, c("t2_team" = "team_name")) |>
  rename(t2_id = team_id) |>
  left_join(new_teams, c("map_winner" = "team_name")) |>
  rename(map_win_team_id = team_id) |>
  select(-t1_team, -t2_team, -map_winner, -region.x, -region.y) |>
  mutate(match_map_id = row_number()) |>
  left_join(new_matches, c("date", "t1_id" = "team1_id", "t2_id" = "team2_id")) |>
  select(-patch_date, -week, -bracket, -region) |>
  left_join(new_maps, c("map" = "map_name")) |>
  select(-map, -mode, -date, -t1_id, -t2_id)

new_rounds <- combined_data |>
  select(date, t1_round, t1_team, t2_team, map, t1_result, t2_result) |>
  mutate(round_unique_id = paste(date, map, t1_team, t2_team, t1_round,
    t1_result, t2_result,
    sep = "_"
  )) |>
  left_join(new_maps, c("map" = "map_name")) |>
  left_join(new_teams, c("t1_team" = "team_name")) |>
  rename(t1_id = team_id) |>
  left_join(new_teams, c("t2_team" = "team_name")) |>
  rename(t2_id = team_id) |>
  left_join(new_matches, c("date", "t1_id" = "team1_id", "t2_id" = "team2_id")) |>
  left_join(new_match_maps, c("match_id", "map_id")) |>
  select(t1_result, t2_result, match_map_id, mode, t1_round, map_win_team_id, round_unique_id) |>
  rename(name = t1_round) |>
  mutate(round_id = row_number()) |>
  mutate(t1_result = case_when(
    grepl("\\(W\\)", t1_result) ~ str_remove(t1_result, "\\(W\\)"),
    .default = t1_result
  )) |>
  mutate(t2_result = case_when(
    grepl("\\(W\\)", t2_result) ~ str_remove(t2_result, "\\(W\\)"),
    .default = t2_result
  )) |>
  mutate(t1_result = case_when(
    t1_result == "W" ~ 1,
    t1_result == "" ~ 0,
    is.na(t1_result) ~ NA_integer_,
    TRUE ~ as.integer(as.numeric(t1_result))
  ), t2_result = case_when(
    t2_result == "W" ~ 1,
    t2_result == "" ~ 0,
    is.na(t2_result) ~ NA_integer_,
    TRUE ~ as.integer(as.numeric(t2_result))
  )) |>
  group_by(match_map_id) |>
  mutate(
    t1_win = t1_result - lag(t1_result, default = 0),
    t2_win = t2_result - lag(t2_result, default = 0)
  ) |>
  ungroup() |>
  select(-map_win_team_id, -mode)

new_hero_composition <- combined_data |>
  # Create the same unique round identifier
  mutate(round_unique_id = paste(date, map, t1_team, t2_team, t1_round,
    t1_result, t2_result,
    sep = "_"
  )) |>
  select(
    -vod_link, -patch_date, -bracket, -week, -vs, -t2_round,
    -first_ban_team, -first_ban_hero, -second_ban_team, -second_ban_hero, -region
  ) |>
  left_join(new_teams, c("t1_team" = "team_name")) |>
  rename(t1_id = team_id) |>
  left_join(new_teams, c("t2_team" = "team_name")) |>
  rename(t2_id = team_id) |>
  left_join(new_maps, c("map" = "map_name")) |>
  pivot_longer(
    cols = c(
      "t1_tank", "t1_dps1", "t1_dps2",
      "t1_sup1", "t1_sup2", "t2_tank",
      "t2_dps1", "t2_dps2", "t2_sup1", "t2_sup2"
    ),
    names_to = "role", values_to = "hero_name"
  ) |>
  mutate(team = str_sub(role, start = 1, end = 2)) |>
  mutate(team = case_when(
    team == "t1" ~ t1_id,
    team == "t2" ~ t2_id
  )) |>
  mutate(role = str_remove(role, "t[12]_")) |>
  left_join(new_matches, c("date", "t1_id" = "team1_id", "t2_id" = "team2_id")) |>
  left_join(new_match_maps, c("match_id", "map_id")) |>
  # Join with the rounds table using our unique identifier
  left_join(new_rounds |> select(round_id, round_unique_id), by = "round_unique_id") |>
  left_join(new_heroes, c("hero_name")) |>
  mutate(hero_comp_id = row_number()) |>
  select(hero_comp_id, round_id, hero_id, team) |>
  # Remove any potential duplicates
  distinct()

new_bans <- combined_data |>
  select(date, t1_team, t2_team, map, first_ban_team, first_ban_hero, second_ban_team, second_ban_hero) |>
  left_join(new_teams, by = c("t1_team" = "team_name")) |>
  rename(team1_id = team_id) |>
  left_join(new_teams, by = c("t2_team" = "team_name")) |>
  rename(team2_id = team_id) |>
  left_join(new_matches, by = c("team1_id", "team2_id", "date")) |>
  left_join(new_maps, by = c("map" = "map_name")) |>
  left_join(new_match_maps, by = c("match_id", "map_id")) |>
  distinct(
    match_map_id, first_ban_team, first_ban_hero,
    second_ban_team, second_ban_team, second_ban_hero
  ) |>
  pivot_longer(
    cols = c(first_ban_hero, second_ban_hero),
    names_to = "first_second",
    values_to = "hero"
  ) |>
  mutate(first_bool = (first_second == "first_ban_hero")) |>
  mutate(team = case_when(
    first_bool ~ first_ban_team,
    .default = second_ban_team
  )) |>
  left_join(new_teams, by = c("team" = "team_name")) |>
  left_join(new_heroes, by = c("hero" = "hero_name")) |>
  select(match_map_id, team_id, first_bool, hero_id)

message("Tables created.")
