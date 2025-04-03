#' -----------------------------------------------------------------------------
#' prepare.R
#' 
#' Description: Preparation file thats run once on startup. Downloads data from
#' AWS storage and creates necessary variables.
#' 
#' Author: CatraMyBeloved
#' Date Created: 03-04-2025
#' Last Modified: 02-04-2025
#' -----------------------------------------------------------------------------

download_success <- download_from_s3("owcs-analysis", "esports_data.RData", "./data/esports_data.RData")

if (!download_success) {
  
  message("Failed to download data. Falling back to local.")
  
} else {
  load("./data/esports_data.RData", envir = .GlobalEnv)
  
  message("Data received from AWS S3.")
}

# Create team, hero and map lists for later use in filters
team_list <- as.list(unique(teams$team_name))
hero_list <- as.list(unique(heroes$hero_name))
map_list <- as.list(unique(maps$map_name))

# Make sure they are available in the global environment
assign("team_list", team_list, envir = .GlobalEnv)
assign("hero_list", hero_list, envir = .GlobalEnv)
assign("map_list", map_list, envir = .GlobalEnv)