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

    hero_preferences_map_specific <- reactive({
      hero_data <- map_pickrates() |>
        left_join(
          general_pickrates(),
          by = "hero_name",
          suffix = c("_map", "_general")
        ) |>
        mutate(
          pickrate_deviation = weighted_pickrate_map - weighted_pickrate_general,
          winrate_deviation = avg_winrate_map - avg_winrate_general,
          category = case_when(
            (winrate_deviation < 0) & (pickrate_deviation < 0) ~ "Wisely Avoided",
            (winrate_deviation > 0) & (pickrate_deviation < 0) ~ "Underused Strength",
            (winrate_deviation < 0) & (pickrate_deviation > 0) ~ "Overused Weakness",
            (winrate_deviation > 0) & (pickrate_deviation > 0) ~ "Core Strength"
          )
        )
    })

    output$mapSpecificQuadrants <- renderPlot({
      hero_data <- hero_preferences_map_specific()

      x_limit <- max(abs(hero_data$winrate_deviation)) * 1.05
      y_limit <- max(abs(hero_data$pickrate_deviation)) * 1.05

      quadrant_colors <- c(
        "Wisely Avoided" = "#8491B4", # Muted blue-gray
        "Underused Strength" = "#41B6E6", # Bright blue
        "Overused Weakness" = "#E6A243", # Orange
        "Core Strength" = "#579920" # Green
      )

      if (max(hero_data$maps_with_hero_total_map) > 20) {
        hero_data <- hero_data |>
          filter(maps_with_hero_total_map > 2)
      }

      hero_data |>
        ggplot(aes(
          x = winrate_deviation,
          y = pickrate_deviation,
          color = category
        )) +
        geom_point(size = 3) +
        geom_hline(yintercept = 0, linewidth = 1.5, color = "#D6D6D6") +
        geom_vline(xintercept = 0, linewidth = 1.5, color = "#D6D6D6") +
        coord_cartesian(xlim = c(-x_limit, x_limit), ylim = c(-y_limit, y_limit)) +
        annotate("rect",
          xmin = 0, xmax = x_limit, ymin = 0, ymax = y_limit,
          fill = "#7AC14233", alpha = 0.15
        ) +
        annotate("rect",
          xmin = -x_limit, xmax = 0, ymin = 0, ymax = y_limit,
          fill = "#E6A24333", alpha = 0.15
        ) +
        annotate("rect",
          xmin = 0, xmax = x_limit, ymin = -y_limit, ymax = 0,
          fill = "#41B6E633", alpha = 0.15
        ) +
        annotate("rect",
          xmin = -x_limit, xmax = 0, ymin = -y_limit, ymax = 0,
          fill = "#8491B433", alpha = 0.15
        ) +
        # Quadrant labels
        annotate("text",
          x = x_limit * 0.7, y = y_limit * 0.9, label = "CORE STRENGTHS",
          size = 3.5, color = "#FFFFFF"
        ) +
        annotate("text",
          x = -x_limit * 0.7, y = y_limit * 0.9, label = "OVERUSED WEAKNESSES",
          size = 3.5, color = "#FFFFFF"
        ) +
        annotate("text",
          x = x_limit * 0.7, y = -y_limit * 0.9, label = "UNDERUSED STRENGTHS",
          size = 3.5, color = "#FFFFFF"
        ) +
        annotate("text",
          x = -x_limit * 0.7, y = -y_limit * 0.9, label = "WISELY AVOIDED",
          size = 3.5, color = "#FFFFFF"
        ) +
        geom_text_repel(
          aes(label = hero_name),
          size = 3,
          color = "#FFFFFF",
          bg.color = "#2D2D2D",
          bg.r = 0.15,
          max.overlaps = 15,
          seed = 42
        ) +
        labs(
          title = paste0(input$mapSpecificSelection, "Hero Performance"),
          subtitle = "Comparing pick rate and win rate relative to league averages",
          x = "Win Rate Deviation from League Average",
          y = "Pick Rate Deviation from League Average"
        ) +
        guides(color = "none") +
        scale_color_manual(values = quadrant_colors)
    })
  })
}
