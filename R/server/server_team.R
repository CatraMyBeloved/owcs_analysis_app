#' -----------------------------------------------------------------------------
#' server_team.R
#'
#' Description: Server function for team analysis, backbone of team
#'  analysis page.
#'
#' Author: CatraMyBeloved
#' Date Created: 03-04-2025
#' Last Modified: 02-04-2025
#' -----------------------------------------------------------------------------
#' Team Analysis Server Module
#'
#' @description Handles the server-side logic for analyzing team-specific hero
#'   preferences and performance compared to the league average.
#'
#' @param id The module ID used for namespacing
#' @param all_data Reactive data frame containing match and hero usage data
#'
#' @details This module focuses on team-specific hero usage patterns, calculating
#'   how a team's hero picks differ from league averages. It processes data filtered
#'   by week, mode, role, and region to identify a team's signature heroes and
#'   heroes they avoid. Compares team-specific pickrates and winrates against the
#'   weighted league average and visualizes the differences.
#'
#' @return A Shiny module server function that handles team-specific analysis

team_server <- function(id, all_data) {
  moduleServer(id, function(input, output, session) {
    filtered_data <- reactive({
      all_data |>
        filter(
          week %in% input$weekFilter,
          region %in% input$regionFilter
        )
    })

    filtered_matches_all_teams <- reactive({
      matches |>
        left_join(teams, by = c("team1_id" = "team_id")) |>
        rename(team_1 = team_name) |>
        left_join(teams, by = c("team2_id" = "team_id")) |>
        rename(team_2 = team_name) |>
        rename(region = region.x) |>
        filter(
          week %in% input$weekFilter,
          region %in% input$regionFilter
        )
    })

    filtered_matches <- reactive({
      filtered_matches_all_teams() |>
        filter(
          ((team_1 == input$teamFilter) | (team_2 == input$teamFilter))
        )
    })

    favorite_heroes <- reactive({
      filtered_data() |>
        filter(team_name == input$teamSelection) |>
        distinct(hero_name, role, match_map_id) |>
        count(hero_name, role) |>
        group_by(role) |>
        slice_max(order_by = n, n = input$nHeroes, with_ties = FALSE) |>
        mutate(rank = row_number()) |>
        pivot_wider(
          id_cols = rank,
          names_from = role,
          values_from = hero_name
        )
    })

    output$favHeroes <- renderDT(
      favorite_heroes()
    )
  })
}
