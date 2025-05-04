overview_server <- function(id, all_data) {
  moduleServer(id, function(input, output, session) {
    # Filter logic
    filtered_data <- reactive({
      all_data |>
        filter(
          role %in% input$roleFilter,
          region %in% input$regionFilter
        )
    })

    pickrates_per_week <- reactive({
      data <- filtered_data()
      # Apply our weighted pickrate function to each week
      pickrates <- data |>
        split(f = data$week) |>
        map_dfr(calculate_weighted_pickrates, .id = "week")

      pickrates <- pickrates |>
        mutate(
          week = factor(week,
            levels = c("1", "2", "3", "4", "Playoffs", "hangzhou_lan"),
            ordered = TRUE
          )
        )
    })

    output$popularityVis <- renderPlot({
      # Get the pickrates data
      pickrate_data <- pickrates_per_week()

      # Select top N heroes
      top_heroes <- pickrate_data |>
        group_by(hero_name) |>
        summarise(avg_pickrate = mean(weighted_pickrate)) |>
        slice_max(order_by = avg_pickrate, n = 5) |>
        pull(hero_name)

      # Filter for just the top heroes
      plot_data <- pickrate_data |>
        filter(hero_name %in% top_heroes)

      # Create the combined plot
      ggplot(plot_data, aes(
        x = week, y = weighted_pickrate,
        color = hero_name, fill = hero_name, group = hero_name
      )) +
        # Area with transparency
        geom_area(alpha = 0.2, position = "identity") +
        # Line on top of the area
        geom_line(size = 1) +
        # Points to mark exact values
        geom_point(size = 3) +
        # Formatting
        scale_y_continuous(labels = scales::percent) +
        scale_color_brewer(palette = "Set2") +
        scale_fill_brewer(palette = "Set2") +
        labs(
          title = "Hero Popularity Over Time",
          x = "Week",
          y = "Weighted Pickrate",
          color = "Hero",
          fill = "Hero"
        )
    })
  })
}
