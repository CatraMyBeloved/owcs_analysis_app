#' -----------------------------------------------------------------------------
#' ui_shared_filters.R
#'
#' Description: Contains shared UI filter components used across multiple pages
#'
#' Author: CatraMyBeloved
#' Date Created: 04-18-2025
#' Last Modified: 04-18-2025
#' -----------------------------------------------------------------------------


week_filter <- function(id, selected = NULL) {
  # Default to all weeks if nothing specified
  if (is.null(selected)) {
    selected <- c(1, 2, 3, 4, "Playoffs", "hangzhou_lan")
  }

  checkboxGroupInput(id, "Week",
    choices = list(
      "Week 1" = 1, "Week 2" = 2,
      "Week 3" = 3, "Week 4" = 4,
      "Playoffs", "Hangzhou LAN" = "hangzhou_lan"
    ),
    selected = selected
  )
}


region_filter <- function(id, selected = NULL) {
  # Default to all regions if nothing specified
  if (is.null(selected)) {
    selected <- c("north_america", "emea", "korea", "china")
  }

  checkboxGroupInput(id, "Region",
    choices = list(
      "NA" = "north_america",
      "EMEA" = "emea",
      "Korea" = "korea",
      "China" = "china"
    ),
    selected = selected
  )
}


mode_filter <- function(id, selected = NULL) {
  # Default to all modes if nothing specified
  if (is.null(selected)) {
    selected <- c("Control", "Flashpoint", "Push", "Escort", "Hybrid")
  }

  checkboxGroupInput(id, "Mode",
    choices = list(
      "Control", "Flashpoint", "Push",
      "Escort", "Hybrid"
    ),
    selected = selected
  )
}


role_filter <- function(id, selected = NULL) {
  # Default to all roles if nothing specified
  if (is.null(selected)) {
    selected <- c("tank", "dps", "sup")
  }

  checkboxGroupInput(id, "Roles",
    choices = list(
      "Tank" = "tank",
      "DPS" = "dps",
      "Support" = "sup"
    ),
    selected = selected
  )
}


team_filter <- function(id) {
  selectInput(id, "Team",
    choices = c("All" = "All", team_list)
  )
}


map_filter <- function(id, all_option = TRUE) {
  if (all_option) {
    choices <- c("All" = "All", map_list)
  } else {
    choices <- map_list
  }

  selectInput(id, "Map",
    choices = choices
  )
}
