detail_server <- function(id, all_data) {
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
  })
}
