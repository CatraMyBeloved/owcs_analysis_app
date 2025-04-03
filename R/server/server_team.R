# Server logic for Team Analysis module
# 
# This module handles the data processing and calculations related to
# team-specific hero selection patterns and comparisons between teams.
# It powers the Team Analysis tab in the application.

team_server <- function(id, all_data){
  moduleServer(id, function(input, output, session){
    
    filtered_data <- reactive({
      all_data |>
        filter(week %in% input$weekFilter,
               mode %in% input$modeFilter,
               role %in% input$roleFilter,
               region %in% input$regionFilter)
    })
    
    filtered_matches_all_teams <- reactive({
      matches |> 
        left_join(teams, by = c("team1_id" = "team_id")) |> 
        rename(team_1 = team_name) |> 
        left_join(teams, by = c("team2_id" = "team_id")) |> 
        rename(team_2 = team_name) |> 
        rename(region = region.x) |> 
        filter(week %in% input$weekFilter, 
               region %in% input$regionFilter
        )
    })
    
    filtered_matches <- reactive({
      filtered_matches_all_teams() |> 
        filter(
          ((team_1 == input$teamFilter) | (team_2 == input$teamFilter))
        )
    })
    
    total_maps <- reactive({
      n_distinct(filtered_data()$match_map_id)
    })
    
    pickrates <- reactive({
      # Calculate total maps directly within this function
      filtered_data_val <- filtered_data()
      total_maps_val <- n_distinct(filtered_data_val$match_map_id)
      
      # Use the direct value instead of calling the reactive function
      filtered_data_val |> 
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
    
    # Calculations ----------
    maps_by_team <- reactive({
      filtered_data() |>
        group_by(team_name) |>
        summarise(total_maps = n_distinct(match_map_id))
    })
    
    
    hero_usage <- reactive({
      filtered_data() |>
        group_by(team_name, hero_name, role) |>
        distinct(match_map_id, .keep_all = TRUE) |> 
        summarise(maps_with_hero = n(),
                  wins_with_hero = sum(iswin),
                  .groups = "drop")
    })
    
    # Calculate team pickrates for all teams
    team_pickrates_all <- reactive({
      hero_usage() |>
        left_join(maps_by_team(), by = "team_name") |>
        mutate(team_pickrate = maps_with_hero / total_maps,
               team_winrate = wins_with_hero / maps_with_hero)
    })
    
    # Calculate weighted average pickrate across teams
    average_team_pickrates <- reactive({
      team_pickrates_all() |>
        group_by(hero_name, role) |>
        summarise(
          total_weighted_pickrate = sum(team_pickrate * total_maps, na.rm = TRUE),
          total_weights = sum(total_maps, na.rm = TRUE),
          avg_pickrate = total_weighted_pickrate / total_weights,
          .groups = "drop"
        )
    })
    
    # Hero pickrates for the selected team + diff to weighted avg
    pickrate_comparison <- reactive({
      # Get the selected team's pickrates
      selected_team_pickrates <- team_pickrates_all() |>
        filter(team_name == input$teamFilter)
      
      # Join with average pickrates
      selected_team_pickrates |>
        left_join(average_team_pickrates(), by = c("hero_name", "role")) |>
        left_join(pickrates(), by = c("hero_name", "role")) |> 
        mutate(pickrate_diff = team_pickrate - avg_pickrate,
               winrate_diff = team_winrate - winrate) |>
        select(hero_name, role, maps_with_hero, team_pickrate, avg_pickrate, pickrate_diff, team_winrate, winrate_diff) |>
        arrange(desc(abs(pickrate_diff)))
    })
    
    # Outputs ----------
    output$Pickrates <- renderDT({
      pickrate_comparison() |>
        select(-role) |>
        datatable(
          colnames = c("Hero", "Maps played",
                       "Team pickrate", "Avg pickrate",
                       "Pickrate difference", "Team winrate", "Winrate diff"),
          filter = "top",
          options = list(
            searching = TRUE, 
            pageLength = 10,
            autoWidth = TRUE
          )) |>
        formatPercentage("team_pickrate", digits = 1) |>
        formatPercentage("avg_pickrate", digits = 1) |>
        formatPercentage("pickrate_diff", digits = 1) |> 
        formatPercentage("team_winrate", digits = 1) |> 
        formatPercentage("winrate_diff", digits = 1)
      
      
    })
    
    output$PickratesVis <- renderPlot({
      pickrate_comparison() |>
        head(input$topnPickrates) |>
        mutate(pickrate_diff = pickrate_diff * 100) |>
        ggplot(aes(x = reorder(hero_name, abs(pickrate_diff)), y = pickrate_diff, fill = pickrate_diff > 0)) +
        geom_col() +
        scale_fill_manual(values = c("#FF9E7A", "#7AB8FF"), 
                          labels = c("Below Average", "Above Average"),
                          name = "Pickrate") +
        labs(x = "Hero", y = "Pickrate difference (pp)") +
        coord_flip()
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