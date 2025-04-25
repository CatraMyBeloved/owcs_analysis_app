detail_server <- function(id, all_data) {
  moduleServer(id, function(input, output, session) {
    filtered_data_weeks <- reactive({
      filtered_data_all <- all_data |>
        filter(
          week %in% input$weekFilter,
          region %in% input$regionFilter
        )

      return(filtered_data_all)
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

    general_pickrates <- reactive({
      result <- calculate_weighted_pickrates(filtered_data_weeks())
    })

    map_pickrates <- reactive({
      result <- calculate_weighted_pickrates(filtered_data_map_specific())
    })

    output$mapSpecificPickrates <- renderPlot({
      data <- map_pickrates()
      gen_data <- general_pickrates()

      role_colors <- c(
        "tank" = "#FFCF59", # red
        "sup" = "#4496B5", # blue
        "dps" = "#FF7659" # green
      )

      if (!input$mapSpecificCompToggle) {
        data |>
          ggplot(aes(
            x = reorder(hero_name, weighted_pickrate),
            y = weighted_pickrate,
            fill = role
          )) +
          geom_col() +
          scale_fill_manual(values = role_colors) +
          coord_flip() +
          labs(
            title = "Pickrates on Map",
            subtitle = "Average pickrates across all teams",
            x = "Hero",
            y = "Pickrate",
            caption = "Note: DPS and Support heroes appear more frequently as they occupy 2 slots per team, while Tank heroes only have 1 slot."
          ) +
          facet_wrap(~role, scales = "free") +
          guides(fill = guide_legend(title = NULL))
      } else {
        data |>
          left_join(gen_data, by = "hero_name", suffix = c("", "_gen")) |>
          mutate(
            pickrate_diff = weighted_pickrate - weighted_pickrate_gen,
            color = if_else(pickrate_diff > 0, "pos", "neg")
          ) |>
          slice_max(order_by = abs(pickrate_diff), n = 15) |>
          ggplot(aes(
            x = reorder(hero_name, pickrate_diff),
            y = pickrate_diff,
            fill = pickrate_diff > 0
          )) +
          geom_col() +
          scale_fill_manual(
            values = c("FALSE" = "#ed946b", "TRUE" = "#6bebed"),
            labels = c("TRUE" = "Below Average", "FALSE" = "Above Average")
          ) +
          coord_flip() +
          labs(
            title = "Pickrate difference to OWCS average",
            subtitle = "Pickrate difference in percentage points",
            x = "Hero",
            y = "Pickrate difference",
            color = "Colors"
          ) +
          guides(fill = guide_legend(title = NULL))
      }
    })
  })
}
