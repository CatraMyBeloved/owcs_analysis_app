interaction_server <- function(id, all_data) {
  moduleServer(id, function(input, output, session) {
    filtered_data <- reactive({
      filtered_data <- all_data |>
        filter(
          week %in% input$weekFilter,
          region %in% input$regionFilter
        )


      if (input$teamFilter != "All") {
        filtered_data <- filtered_data |> filter(team_name == input$teamFilter)
      }

      return(filtered_data)
    })

    comps <- reactive({
      result <- comps_func(filtered_data())
      return(result)
    })

    comps_with_opponents <- reactive({
      comps_with_opponents <- comps_with_opponents_func(comps())
      return(comps_with_opponents)
    })

    comp_with_indicators <- reactive({
      result <- create_hero_matrix(comps_with_opponents(), hero_list)
      return(result)
    })

    popular_combinations <- reactive({
      selected_hero <- input$heroFilterSel
      column_name_selected <- paste0("has_", selected_hero)

      popular_combinations <- comp_with_indicators() |>
        filter(.data[[column_name_selected]] == 1) |>
        summarise(
          across(starts_with("has_") & !contains("_opp"), sum)
        ) |>
        pivot_longer(
          cols = starts_with("has_"),
          names_to = "hero",
          values_to = "count"
        ) |>
        mutate(
          hero = str_remove(hero, "has_")
        ) |>
        filter(hero != selected_hero) |>
        arrange(desc(count))

      return(popular_combinations)
    })

    synergies <- reactive({
      selected_hero <- input$heroFilterSel
      n_heroes <- input$nHeroesFilter
      selected_column <- paste0("has_", selected_hero)
      synergy <- data.frame()

      hero_columns <- popular_combinations() |>
        head(n_heroes) |>
        pull(hero) |>
        sapply(function(x) paste0("has_", x))

      selected_winrate <-
        comp_with_indicators() |>
        filter(.data[[selected_column]] == 1) |>
        summarize(
          winrate = mean(iswin),
          rounds = n()
        )

      for (column in hero_columns) {
        if (column == selected_column) next

        other_winrate <-
          comp_with_indicators() |>
          filter(.data[[column]] == 1) |>
          summarise(
            winrate = mean(iswin),
            rounds = n()
          )

        paired_winrate <- comp_with_indicators() |>
          filter(.data[[column]] == 1 & .data[[selected_column]] == 1) |>
          summarise(
            winrate = mean(iswin),
            rounds = n()
          )

        if (paired_winrate$rounds < 15) next

        synergy_stats <- data.frame(
          hero = gsub("has_", "", column),
          paired_rounds = paired_winrate$rounds,
          paired_winrate = round(paired_winrate$winrate * 100, 1),
          selected_winrate = round(selected_winrate$winrate * 100, 1),
          other_winrate = round(other_winrate$winrate * 100, 1),
          synergy_effect = round((paired_winrate$winrate - selected_winrate$winrate) * 100, 1)
        )

        synergy <- bind_rows(synergy, synergy_stats)
      }

      synergy <- synergy |>
        arrange(desc(abs(synergy_effect)))

      return(synergy)
    })


    popular_enemies <- reactive({
      selected_hero <- input$heroFilterSel
      column_name_selected <- paste0("has_", selected_hero)

      popular_enemies <- comp_with_indicators() |>
        filter(.data[[column_name_selected]] == 1) |>
        summarise(
          across(starts_with("has_") & contains("_opp"), sum)
        ) |>
        pivot_longer(
          cols = starts_with("has_") & contains("_opp"),
          names_to = "hero",
          values_to = "count"
        ) |>
        mutate(
          hero = gsub("has_", "", hero),
          hero = gsub("_opp", "", hero)
        ) |>
        arrange(desc(count)) |>
        filter(hero != selected_hero)

      return(popular_enemies)
    })

    counters <- reactive({
      selected_hero <- input$heroFilterSel
      n_heroes <- input$nHeroesFilter
      selected_column <- paste0("has_", selected_hero)
      counters <- data.frame()

      hero_columns <- popular_enemies() |>
        head(n_heroes) |>
        pull(hero) |>
        sapply(function(x) paste0("has_", x, "_opp"))

      selected_winrate <-
        comp_with_indicators() |>
        filter(.data[[selected_column]] == 1) |>
        summarize(
          winrate = mean(iswin),
          rounds = n()
        )

      for (column in hero_columns) {
        winrate_vs_counter <- comp_with_indicators() |>
          filter(.data[[column]] == 1 & .data[[selected_column]] == 1) |>
          summarise(
            winrate = mean(iswin),
            rounds = n()
          )

        if (winrate_vs_counter$rounds < 15) next


        counter_stats <- data.frame(
          hero = gsub("^has_|_opp$", "", column),
          counter_rounds = winrate_vs_counter$rounds,
          counter_winrate = round(winrate_vs_counter$winrate * 100, 1),
          selected_winrate = round(selected_winrate$winrate * 100, 1),
          counter_effect = round((winrate_vs_counter$winrate - selected_winrate$winrate) * 100, 1)
        )
        counters <- bind_rows(counters, counter_stats)
      }
      counters <- counters |>
        arrange(counter_effect)
    })

    output$synergyVis <- renderPlot({
      synergy_data <- synergies()
      selected_hero <- input$heroFilterSel

      baseline_winrate <- synergy_data$selected_winrate[1]

      max_value <- max(synergy_data$synergy_effect)
      min_value <- min(synergy_data$synergy_effect)

      synergy_data$normalized_effect <- ifelse(
        synergy_data$synergy_effect < 0,
        synergy_data$synergy_effect / abs(min_value),
        synergy_data$synergy_effect / max_value
      )


      synergy_data |>
        ggplot(aes(x = reorder(hero, synergy_effect), y = synergy_effect)) +
        geom_col(aes(fill = normalized_effect)) +
        scale_fill_gradient2(
          low = "#ed946b", # Color for negative values
          mid = "darkgrey", # Color at midpoint (0)
          high = "#6bebed", # Color for positive values
          midpoint = 0, # Set midpoint to 0
          breaks = c(-1, -0.5, 0, 0.5, 1),
          labels = c(
            min_value, min_value / 2, 0,
            max_value / 2, max_value
          ),
          name = "synergy_effect"
        ) +
        coord_flip() +
        labs(title = "Synergy", subtitle = "Winrate when playing with x", x = "Hero", y = "Change in Winrate")
    })


    output$counterVis <- renderPlot({
      counter_data <- counters()
      selected_hero <- input$heroFilterSel

      baseline_winrate <- counter_data$selected_winrate[1]

      max_value <- max(counter_data$counter_effect)
      min_value <- min(counter_data$counter_effect)

      counter_data$normalized_effect <- ifelse(
        counter_data$counter_effect < 0,
        counter_data$counter_effect / abs(min_value),
        counter_data$counter_effect / max_value
      )


      counter_data |>
        ggplot(aes(x = reorder(hero, desc(counter_effect)), y = counter_effect)) +
        geom_col(aes(fill = normalized_effect)) +
        scale_fill_gradient2(
          low = "#ed946b", # Color for negative values
          mid = "darkgrey", # Color at midpoint (0)
          high = "#6bebed", # Color for positive values
          midpoint = 0, # Set midpoint to 0
          breaks = c(-1, -0.5, 0, 0.5, 1),
          labels = c(
            min_value, min_value / 2, 0,
            max_value / 2, max_value
          ),
          name = "counter_effect"
        ) +
        coord_flip() +
        labs(title = "Counters", subtitle = "Winrate when playing against x", x = "Hero", y = "Change in Winrate")
    })

    observeEvent(input$roleFilterSel, {
      # Get filtered teams
      filtered_hero_list <- heroes |>
        filter(role == input$roleFilterSel) |>
        pull(hero_name)

      # Update the select input
      updateSelectInput(
        session, # Pass session as first argument
        inputId = "heroFilterSel", # Use just the input ID without session$ns()
        choices = filtered_hero_list,
        # Try to maintain current selection if it's still valid
        selected = if (input$heroFilterSel %in% filtered_hero_list) input$heroFilterSel else filtered_hero_list[1]
      )
    })
  })
}
