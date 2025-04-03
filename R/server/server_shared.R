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
