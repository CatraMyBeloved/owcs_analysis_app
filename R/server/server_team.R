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
#' @details
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

    general_pickrates <- reactive({
      calculate_pickrates(filtered_data_all())
    })

    team_pickrates <- reactive({
      calculate_pickrates(filtered_data())
    })

    output$teamName <- renderText({
      input$teamSelection
    })

    region_name <- reactive({
      region <- teams |>
        filter(team_name == input$teamSelection) |>
        pull(region)

      region_name <- case_when(
        region == "north_america" ~ "North America",
        region == "emea" ~ "EMEA",
        region == "korea" ~ "Asia/Korea"
      )

      return(region_name)
    })

    output$regionName <- renderText({
      region_name()
    })

    output$overallWinrate <- renderText({
      win_rate <- filtered_data() |>
        summarize(wr = mean(iswin, na.rm = TRUE)) |>
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

    signature_hero <- reactive({
      general_pickrates <- general_pickrates()

      team_pickrates <- team_pickrates()

      signature_hero <- general_pickrates |>
        inner_join(team_pickrates, by = "hero_name", suffix = c("", "_team")) |>
        mutate(pickrate_diff = pickrate_team - pickrate) |>
        filter(pickrate_team >= 0.3) |>
        arrange(desc(pickrate_diff)) |>
        pull(hero_name)

      return(signature_hero)
    })

    output$signatureHero <- renderText({
      signature_hero()[1]
    })

    map_performance <- reactive({
      map_performance <- filtered_data() |>
        group_by(map_name) |>
        distinct(match_map_id, .keep_all = TRUE) |>
        summarise(
          times_played = n(),
          times_won = sum(iswin),
          winrate = mean(iswin)
        )

      return(map_performance)
    })

    output$mapPerformance <- renderDataTable({
      datatable(
        map_performance() |> select(map_name, winrate, times_played) |>
          arrange(desc(times_played)),
        options = list(
          searching = FALSE,
          pageLength = 10,
          autoWidth = TRUE
        ),
        colnames = c("Map Name", "Winrate", "Times Played"),
        rownames = FALSE
      ) |>
        formatPercentage(c("winrate"), digits = 1)
    })

    output$mapPerformanceVis <- renderPlot({
      map_data <- map_performance() |>
        mutate(deviation = winrate * 100 - 50)

      map_data$color <- ifelse(map_data$deviation >= 0, "positive", "negative")

      map_data |>
        filter(times_played >= 2) |>
        ggplot(aes(
          x = reorder(map_name, desc(deviation)),
          y = deviation,
          color = color
        )) +
        geom_segment(
          aes(
            xend = map_name,
            y = 0,
            yend = deviation,
          ),
          linewidth = 1.3
        ) +
        scale_color_manual(values = c(
          "positive" = "#6bebed",
          "negative" = "#ed946b"
        )) +
        geom_point(size = 5) +
        labs(
          title = "Map Winrate deviation",
          subtitle = "Deviation from 50%",
          x = "Map Name",
          y = "Winrate deviation"
        ) +
        coord_flip() +
        guides(color = "none")
    })

    # TODO: gotta work on this stuff to account for pickrate per team

    hero_preferences <- reactive({
      maps_played_per_team <- filtered_data_all() |>
        group_by(team_name) |>
        summarise(
          maps_played = n_distinct(match_map_id)
        )

      heroes_pickrate <- filtered_data_all() |>
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

      heroes_winrate <- filtered_data_all() |>
        group_by(hero_name, role) |>
        summarise(
          league_winrate = mean(iswin),
          .groups = "drop"
        )

      maps_played_selected_team <- filtered_data() |>
        group_by(team_name) |>
        summarise(
          maps_played = n_distinct(match_map_id)
        ) |>
        pull(maps_played)

      hero_preferences <- filtered_data() |>
        group_by(hero_name, role) |>
        distinct(match_map_id, .keep_all = TRUE) |>
        summarise(
          times_played = n(),
          winrate = mean(iswin),
          .groups = "drop"
        ) |>
        left_join(heroes_pickrate, by = "hero_name", suffix = c("", "_general")) |>
        mutate(
          pickrate_team = times_played / maps_played_selected_team,
          pickrate_deviation = pickrate_team - pickrate
        ) |>
        left_join(heroes_winrate, by = c("hero_name", "role")) |>
        mutate(
          winrate_differential = winrate - league_winrate
        )

      return(hero_preferences)
    })




    output$heroPreferences <- renderDataTable({
      datatable(
        hero_preferences() |>
          select(hero_name, winrate, pickrate_deviation),
        colnames = c("Hero", "Winrate", "Pickrate Deviation"),
        rownames = FALSE,
        options = list(
          searching = FALSE,
          pageLength = 10,
          autoWidth = TRUE
        )
      ) |>
        formatPercentage(c("winrate", "pickrate_deviation"))
    })

    output$heroPreferencesVis <- renderPlot({
      team_win_rate <- filtered_data() |>
        summarize(wr = mean(iswin, na.rm = TRUE)) |>
        pull(wr)


      hero_data <- hero_preferences()

      hero_data <- hero_data |>
        mutate(
          winrate_deviation = winrate * 100 - team_win_rate * 100,
          pickrate_deviation = pickrate_deviation * 100,
          category = case_when(
            (winrate_deviation < 0) & (pickrate_deviation < 0) ~ "Wisely Avoided",
            (winrate_deviation > 0) & (pickrate_deviation < 0) ~ "Underused Strength",
            (winrate_deviation < 0) & (pickrate_deviation > 0) ~ "Overused Weakness",
            (winrate_deviation > 0) & (pickrate_deviation > 0) ~ "Core Strength"
          )
        )



      x_limit <- max(abs(hero_data$winrate_deviation)) * 1.05
      y_limit <- max(abs(hero_data$pickrate_deviation)) * 1.05


      quadrant_colors <- c(
        "Wisely Avoided" = "#8491B4", # Muted blue-gray
        "Underused Strength" = "#41B6E6", # Bright blue
        "Overused Weakness" = "#E6A243", # Orange
        "Core Strength" = "#579920" # Green
      )

      hero_data |>
        filter(times_played > 3) |>
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
          title = paste0(input$teamSelection, "'s Hero Performance"),
          subtitle = "Comparing pick rate and win rate relative to league averages",
          x = "Win Rate Deviation from average team winrate",
          y = "Pick Rate Deviation from League Average"
        ) +
        guides(color = "none") +
        scale_color_manual(values = quadrant_colors)
    })


    output$heroPreferencesVistest <- renderPlot({
      hero_data <- hero_preferences()

      hero_data$color <- if_else(hero_data$pickrate_deviation < 0, "negative", "positive")

      hero_data |>
        filter(times_played > 10) |>
        ggplot(
          aes(
            x = reorder(hero_name, desc(pickrate_deviation)),
            y = pickrate_deviation,
            color = color
          )
        ) +
        geom_segment(
          aes(
            xend = hero_name,
            y = 0,
            yend = pickrate_deviation,
          ),
          linewidth = 1.3
        ) +
        geom_point(size = 5) +
        scale_color_manual(values = c(
          "positive" = "#6bebed",
          "negative" = "#ed946b"
        )) +
        coord_flip() +
        guides(
          color = "none"
        )
    })
  })
}
