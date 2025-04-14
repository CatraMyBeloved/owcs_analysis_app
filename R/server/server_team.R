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
    filtered_data_all <- reactive({
      all_data |>
        filter(
          week %in% input$weekFilter,
          region %in% input$regionFilter
        )
    })

    filtered_data <- reactive({
      filtered_data_all() |>
        filter(team_name == input$teamSelection)
    })

    output$overallWinrate <- renderText({
      win_rate <- filtered_data() %>%
        summarize(wr = mean(iswin, na.rm = TRUE)) %>%
        pull(wr)

      percent(win_rate, accuracy = 0.1)
    })

    output$mapsPlayed <- renderText({
      n_maps <- filtered_data() |>
        summarize(maps_played = n_distinct(match_map_id)) |>
        pull(maps_played)
    })

    bestMap <- reactive({
      min_games <- 3

      filtered_data() |>
        distinct(match_map_id, .keep_all = TRUE) |>
        group_by(map_name) |>
        summarize(
          times_played = n(),
          winrate = mean(iswin),
          .groups = "drop"
        ) |>
        filter(times_played >= min_games) |>
        arrange(desc(winrate)) |>
        slice_head(n = 1)
    })

    output$bestMap <- renderText({
      paste(bestMap()$map_name, " ", percent(bestMap()$winrate, 0.1))
    })
  })
}
