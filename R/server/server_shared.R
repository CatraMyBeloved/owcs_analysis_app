#' Shared Server Components Module
#'
#' @description Contains shared server-side functions, helpers, and utilities
#'   used across multiple server modules.
#'
#' @details This file is intended to hold commonly used server-side functions
#'   that are needed in multiple modules to prevent code duplication. This includes
#'   data transformation functions, filtering utilities, and shared calculations
#'   that maintain consistency across the application.
#'
#' @note This file is currently in development and may be populated with shared
#'   components as the application grows.
#'
#'
calculate_weighted_pickrates <- function(data) {
  maps_played_per_team <- data |>
    group_by(team_name) |>
    distinct(match_map_id) |>
    summarise(
      maps_played = n()
    )

  maps_played_total <- maps_played_per_team |>
    summarise(
      total_maps = sum(maps_played)
    ) |>
    pull(total_maps)

  heroes_pickrate_overall <- data |>
    group_by(team_name, hero_name, role) |>
    distinct(match_map_id, .keep_all = TRUE) |>
    summarise(
      maps_played_with_hero = n(),
      maps_won_with_hero = sum(iswin),
      .groups = "drop"
    ) |>
    left_join(maps_played_per_team, by = "team_name") |>
    group_by(hero_name, role) |>
    summarise(
      maps_with_hero_total = sum(maps_played_with_hero),
      weighted_pickrate = maps_with_hero_total / maps_played_total,
      .groups = "drop"
    )

  heroes_winrate <- data |>
    group_by(hero_name, role) |>
    summarise(
      avg_winrate = mean(iswin),
      .groups = "drop"
    )

  result <- heroes_pickrate_overall |>
    left_join(heroes_winrate, by = "hero_name", suffix = c("", "_2"))

  return(result)
}



calculate_pickrates <- function(filtered_data) {
  total_maps_val <- n_distinct(filtered_data$match_map_id)



  # Use the direct value instead of calling the reactive function
  pickrates <- filtered_data |>
    distinct(hero_name, match_map_id, role, .keep_all = TRUE) |>
    group_by(hero_name, role) |>
    summarise(
      appearances = n(),
      pickrate = appearances / max(1, total_maps_val), # Using max(1, val) prevents division by zero
      winrate = mean(iswin)
    ) |>
    filter(appearances > 0) |>
    arrange(desc(pickrate))

  return(pickrates)
}

comps_func <- function(filtered_data) {
  result <- filtered_data |>
    group_by(match_map_id, round_id, team_name) |>
    reframe(
      tank = hero_name[role == "tank"],
      dps = list(head(hero_name[role == "dps"], 2)),
      sup = list(head(hero_name[role == "sup"], 2)),
      iswin = mean(iswin)
    )
  return(result)
}

comps_with_opponents_func <- function(comps) {
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

  return(comps_with_opponents)
}

create_hero_matrix <- function(compositions, all_heroes) {
  all_heroes <- c(unlist(all_heroes))
  n_rows <- nrow(compositions)
  n_columns <- length(all_heroes)

  # Pre-allocate matrices
  team_matrix <- matrix(FALSE, nrow = n_rows, ncol = n_columns)
  opp_matrix <- matrix(FALSE, nrow = n_rows, ncol = n_columns)

  colnames(team_matrix) <- all_heroes
  colnames(opp_matrix) <- all_heroes

  # Create hero index lookup once - this is much faster than repeated match() calls
  hero_indices <- setNames(seq_along(all_heroes), all_heroes)

  # Process each row with optimized lookup
  for (i in seq_len(n_rows)) {
    # Extract heroes for current composition
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

    # Direct lookup via named vector (much faster than match())
    if (length(team_heroes) > 0) {
      idx <- hero_indices[team_heroes]
      idx <- idx[!is.na(idx)] # Handle any missing heroes gracefully
      if (length(idx) > 0) team_matrix[i, idx] <- TRUE
    }

    if (length(opp_heroes) > 0) {
      idx <- hero_indices[opp_heroes]
      idx <- idx[!is.na(idx)]
      if (length(idx) > 0) opp_matrix[i, idx] <- TRUE
    }
  }

  # Convert to data frames
  team_df <- as.data.frame(team_matrix)
  opp_df <- as.data.frame(opp_matrix)

  names(team_df) <- paste0("has_", names(team_df))
  names(opp_df) <- paste0("has_", names(opp_df), "_opp")

  # Combine results
  result <- bind_cols(compositions, team_df, opp_df)

  return(result)
}
