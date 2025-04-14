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
          region %in% input$regionFilter,
          team_name == input$teamSelection
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
      total_maps_val <- n_distinct(filtered_data()$match_map_id)

      filtered_data() |>
        distinct(hero_name, match_map_id, role, .keep_all = TRUE) |>
        group_by(hero_name, role) |>
        summarise(
          appearances = n(),
          pickrate = appearances / max(1, total_maps_val), # Using max(1, val) prevents division by zero
          winrate = mean(iswin)
        ) |>
        filter(appearances > 0) |>
        arrange(desc(pickrate))
    })

    favorite_maps <-
      reactive({
        filtered_data() |>
          filter(team_name == input$teamSelection) |>
          distinct(match_map_id, .keep_all = TRUE) |>
          group_by(map_name) |>
          summarise(
            times_played = n(),
            winrate = mean(iswin)
          ) |>
          arrange(desc(times_played))
      })

    output$favMaps <- renderDT(
      favorite_maps() |>
        datatable(
          options = list(
            searching = TRUE,
            pageLength = 5,
            autoWidth = TRUE
          )
        ) |>
        formatPercentage(c("winrate"), digits = 1)
    )

    output$favHeroes <- renderDT(
      favorite_heroes() |>
        datatable(
          options = list(
            searching = TRUE,
            pageLength = 5,
            autoWidth = TRUE
          )
        ) |>
        formatPercentage(c("pickrate", "winrate"), digits = 1)
    )
  })
}
