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

calculate_pickrates <- function(filtered_data){
  
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

comps_func <- function(filtered_data){
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

comps_with_opponents_func <- function(comps){
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
  
  return(comps_with_opponents)
}

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
  
  return(result)
}
