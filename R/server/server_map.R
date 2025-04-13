#' -----------------------------------------------------------------------------
#' server_map.R
#'
#' Description: Server function for map analysis, backbone of map analysis page.
#'
#' Author: CatraMyBeloved
#' Date Created: 03-04-2025
#' Last Modified: 02-04-2025
#' -----------------------------------------------------------------------------
#' Map Analysis Server Module
#'
#' @description Handles the server-side logic for analyzing hero performance and
#'   usage on specific maps.
#'
#' @param id The module ID used for namespacing
#' @param all_data Reactive data frame containing match and hero usage data
#'
#' @details This module filters data by map and other criteria (week, role, region,
#'   team) to show which heroes are commonly used on each map. Calculates pickrates
#'   and winrates for heroes on the selected map. Includes dynamic team filtering
#'   based on region selection and visualizes the top heroes for each map.
#'
#' @return A Shiny module server function that handles map-specific analysis

map_server <- function(id, all_data) {
  moduleServer(id, function(input, output, session) {
    filtered_data_all_teams <- reactive({
      filtered_data_all_teams <- all_data |>
        filter(
          week %in% input$weekFilter,
          role %in% input$roleFilter,
          region %in% input$regionFilter
        )

      return(filtered_data_all_teams)
    })


    filtered_data <- reactive({
      temp <- filtered_data_all_teams()
      if (input$teamFilter != "All") {
        temp <- temp |>
          filter(team_name == input$teamFilter)
      }

      return(temp)
    })


    selected_map_id <- reactive({
      maps |>
        filter(map_name == input$mapFilter) |>
        pull(map_id)
    })

    filtered_matches <- reactive({
      filtered_matches <- matches |>
        left_join(teams, by = c("team1_id" = "team_id")) |>
        rename(team_1 = team_name) |>
        left_join(teams, by = c("team2_id" = "team_id")) |>
        rename(team_2 = team_name) |>
        rename(region = region.x) |>
        filter(
          week %in% input$weekFilter,
          region %in% input$regionFilter
        )

      if (input$teamFilter != "All") {
        filtered_matches <- filtered_matches |>
          filter(
            ((team_1 == input$teamFilter) | (team_2 == input$teamFilter))
          )
      }




      filtered_match_ids <- filtered_matches |>
        right_join(match_maps, by = "match_id") |>
        filter(map_id == selected_map_id()) |>
        select(match_id) |>
        distinct()

      filtered_matches <- filtered_matches |>
        filter(match_id %in% filtered_match_ids$match_id)

      return(filtered_matches)
    })

    # Calculations -----------

    average_pickrates_all_maps <- reactive({
      pickrates <- calculate_pickrates(filtered_data_all_teams())
      return(pickrates)
    })



    average_pickrates_selected_map <- reactive({
      pickrates <- filtered_data_all_teams() |>
        filter(map_id == selected_map_id()) |>
        calculate_pickrates()
      return(pickrates)
    })

    teams_in_region <- reactive({
      # Get region filter from input
      selected_regions <- input$regionFilter

      # Filter teams based on selected regions
      filtered_teams <- teams %>%
        filter(region %in% selected_regions) %>%
        pull(team_name)

      # Return as a named list for selectInput
      setNames(as.list(filtered_teams), filtered_teams)
    })

    observeEvent(input$regionFilter, {
      # Get filtered teams
      teams_list <- teams_in_region()

      # Handle case when no teams match (provide a placeholder)
      if (length(teams_list) == 0) {
        teams_list <- list("No teams available" = "")
      }

      # Update the select input
      updateSelectInput(
        inputId = session$ns("teamFilter"),
        choices = c("All" = "All", teams_list),
        # Try to maintain current selection if it's still valid
        selected = if (input$teamFilter %in% names(teams_list)) input$teamFilter else NULL
      )
    })

    total_n_played <- reactive({
      filtered_data() |>
        group_by(map_name) |>
        summarise(n_played = n_distinct(match_map_id))
    })

    total_pickrates_for_maps <- reactive({
      res <- filtered_data() |>
        group_by(map_name, hero_name, role) |>
        summarize(
          all_appearances = n(),
          hero_played = n_distinct(match_map_id),
          hero_wins = sum(iswin),
          .groups = "drop"
        ) |>
        left_join(total_n_played(), by = "map_name") |>
        mutate(
          selected_map_team_pickrate = hero_played / n_played,
          winrate = hero_wins / all_appearances
        ) |>
        arrange(desc(selected_map_team_pickrate))
    })

    pickrate_comparison <- reactive({
      res2 <- total_pickrates_for_maps() |>
        filter(map_name == input$mapFilter) |>
        left_join(average_pickrates_all_maps(), by = c("hero_name", "role")) |>
        rename(all_maps_pickrate = pickrate) |>
        left_join(average_pickrates_selected_map(), by = c("hero_name", "role")) |>
        rename(selected_map_pickrate = pickrate) |>
        mutate(
          pickrate_diff_all_maps = selected_map_team_pickrate - all_maps_pickrate,
          pickrate_diff_selected_map = selected_map_team_pickrate - selected_map_pickrate
        )
    })


    # Outputs -----------

    output$Pickrates <- renderDT({
      pickrate_comparison() |>
        select(hero_name, hero_played, selected_map_team_pickrate, pickrate_diff_all_maps, pickrate_diff_selected_map, winrate.x) |>
        datatable(
          colnames = c(
            "Hero", "Appearances on map",
            "Pickrate by team", "Pickrate diff to all maps", "Pickrate diff to", "Winrate"
          ),
          filter = "top",
          options = list(
            searching = TRUE,
            pageLength = 10,
            autoWidth = TRUE
          )
        ) |>
        formatPercentage("selected_map_team_pickrate", digits = 1) |>
        formatPercentage("winrate.x", digits = 1) |>
        formatPercentage("pickrate_diff_all_maps", digits = 1) |>
        formatPercentage("pickrate_diff_selected_map", digits = 1)
    })

    output$PickratesVisAllMaps <- renderPlot({
      comparison_column <- "pickrate_diff_all_maps"


      title <- "Pickrate Difference vs All Maps"

      pickrate_comparison() |>
        select(hero_name, selected_map_team_pickrate, all_of(comparison_column)) |>
        mutate(abs_diff = abs(.data[[comparison_column]])) |>
        arrange(desc(abs_diff)) |>
        head(input$topnPickrates) |>
        ggplot(aes(
          x = reorder(hero_name, abs_diff),
          y = .data[[comparison_column]],
          fill = .data[[comparison_column]] < 0
        )) +
        geom_col() +
        scale_fill_manual(
          values = c("FALSE" = "#ed946b", "TRUE" = "#6bebed"),
          labels = c("TRUE" = "Below Average", "FALSE" = "Above Average")
        ) +
        coord_flip() +
        labs(
          title = title,
          x = "Hero",
          y = "Difference"
        ) +
        guides(fill = guide_legend(title = NULL))
    })

    output$PickratesVisSelectedTeam <- renderPlot({
      comparison_column <- "pickrate_diff_selected_map"


      title <- "Pickrate Difference vs all Teams on selected map"

      pickrate_comparison() |>
        select(hero_name, selected_map_team_pickrate, all_of(comparison_column)) |>
        mutate(abs_diff = abs(.data[[comparison_column]])) |>
        arrange(desc(abs_diff)) |>
        head(input$topnPickrates) |>
        ggplot(aes(
          x = reorder(hero_name, abs_diff),
          y = .data[[comparison_column]],
          fill = .data[[comparison_column]] < 0
        )) +
        geom_col() +
        scale_fill_manual(
          values = c("FALSE" = "#ed946b", "TRUE" = "#6bebed"),
          labels = c("TRUE" = "Below Average", "FALSE" = "Above Average")
        ) +
        coord_flip() +
        labs(
          title = title,
          x = "Hero",
          y = "Difference"
        ) +
        guides(fill = guide_legend(title = NULL))
    })

    output$filteredMatches <- renderDT({
      filtered_matches() |>
        select(team_1, team_2, date, bracket) |>
        datatable(
          colnames = c("Team 1", "Team 2", "Date", "Bracket"),
        )
    })
  })
}
