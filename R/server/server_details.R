detail_server <- function(id, all_data) {
  moduleServer(id, function(input, output, session) {
    filtered_data_weeks <- reactive({
      filtered_data_all_teams <- all_data |>
        filter(
          week %in% input$weekFilter,
          region %in% input$regionFilter
        )

      return(filtered_data_all_teams)
    })

    filtered_data_map_specific <- reactive({
      filtered_data_weeks() |>
        filter(map_name == input$mapSpecificSelection)
    })

    filtered_data_team_specific <- reactive({
      filtered_data_weeks() |>
        filter(team_name == input$teamSpecificSelection)
    })

    filtered_data_details <- reactive({
      filtered_data_weeks() |>
        filter((team_name == input$teamSpecificSelection) & (map_name == input$mapSpecificSelection))
    })

    map_pickrates <- reactive({
      maps_played_per_team <- filtered_data_map_specific() |>
        group_by(team_name) |>
        summarise(
          maps_played = n_distinct(match_map_id)
        )

      heroes_pickrate_overall <- filtered_data_map_specific() |>
        group_by(team_name, hero_name, role) |>
        distinct(match_map_id, .keep_all = TRUE) |>
        summarise(
          maps_played_with_hero = n(),
          maps_won_with_hero = sum(iswin),
          .groups = "drop"
        ) |>
        left_join(maps_played_per_team, by = "team_name") |>
        mutate(pickrate_per_team = maps_played_with_hero / maps_played) |>
        group_by(hero_name, role) |>
        summarise(
          pickrate = mean(pickrate_per_team)
        )

      heroes_winrate <- filtered_data_map_specific() |>
        group_by(hero_name, role) |>
        summarise(
          avg_winrate = mean(iswin),
          .groups = "drop"
        )

      result <- heroes_pickrate_overall |>
        left_join(heroes_winrate, by = "hero_name", suffix = c("", "_2"))

      return(result)
    })

    output$mapSpecificPickrates <- renderPlot({
      data <- map_pickrates()

      role_colors <- c(
        "tank" = "#FFCF59", # red
        "sup" = "#4496B5", # blue
        "dps" = "#FF7659" # green
      )

      if (!input$mapSpecificCompToggle) {
        data |>
          ggplot(aes(
            x = reorder(hero_name, pickrate),
            y = pickrate,
            fill = role
          )) +
          geom_col() +
          scale_fill_manual(values = role_colors) +
          coord_flip() +
          labs(
            title = "Pickrates on Map",
            subtitle = "Average pickrates across all teams",
            x = "Hero",
            y = "Pickrate"
          )
      }
    })
  })
}
