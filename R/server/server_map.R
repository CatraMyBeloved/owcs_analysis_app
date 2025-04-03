map_server <- function(id, all_data){
  moduleServer(id, function(input, output, session){
    
    filtered_data_by_maps <- reactive({
      filtered_data <- all_data |> 
        filter(week %in% input$weekFilter,
               role %in% input$roleFilter,
               region %in% input$regionFilter)
      
      if(input$teamFilter != "All"){
        filtered_data <- filtered_data |> filter(team_name == input$teamFilter)
      } 
      
      
      
      return(filtered_data)
    })
    
    filtered_matches <- reactive({
      filtered_matches <- matches |> 
        left_join(teams, by = c("team1_id" = "team_id")) |> 
        rename(team_1 = team_name) |> 
        left_join(teams, by = c("team2_id" = "team_id")) |> 
        rename(team_2 = team_name) |> 
        rename(region = region.x) |> 
        filter(week %in% input$weekFilter, 
               region %in% input$regionFilter)
      
      if(input$teamFilter != "All"){
        filtered_matches <- filtered_matches |> 
          filter(
            ((team_1 == input$teamFilter) | (team_2 == input$teamFilter))
          )
      }
      
      selected_map_id <- maps |> 
        filter(map_name == input$mapFilter) |> 
        pull(map_id)
      
      
      filtered_match_ids <- filtered_matches |> 
        right_join(match_maps, by = "match_id") |> 
        filter(map_id == selected_map_id) |> 
        select(match_id) |> 
        distinct()
      
      filtered_matches <- filtered_matches |> 
        filter(match_id %in% filtered_match_ids$match_id)
      
      return(filtered_matches)
    })
    
    # Calculations -----------
    
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
      if(length(teams_list) == 0) {
        teams_list <- list("No teams available" = "")
      }
      
      # Update the select input
      updateSelectInput(
        inputId = session$ns("teamFilter"),
        choices = c("All" = "All", teams_list),
        # Try to maintain current selection if it's still valid
        selected = if(input$teamFilter %in% names(teams_list)) input$teamFilter else NULL
      )
    })
    
    total_n_played <- reactive({
      filtered_data_by_maps() |>
        group_by(map_name) |>
        summarise(n_played = n_distinct(match_map_id)) 
    })
    
    total_pickrates_for_maps <- reactive({
      filtered_data_by_maps() |> 
        group_by(map_name, hero_name, role) |> 
        summarize(all_appearances= n(),
                  hero_played = n_distinct(match_map_id),
                  hero_wins = sum(iswin),
                  .groups = "drop") |> 
        left_join(total_n_played(), by = "map_name") |>
        mutate(pickrate = hero_played / n_played,
               winrate = hero_wins / all_appearances) |> 
        arrange(desc(pickrate))
    })
    
    # Outputs -----------
    
    output$Pickrates <- renderDT({
      total_pickrates_for_maps() |> 
        filter(map_name == input$mapFilter) |> 
        select(hero_name, hero_played, pickrate, winrate) |> 
        datatable(
          colnames = c("Hero", "Appearances on map",
                       "Pickrate", "Winrate"),
          filter = "top",
          options = list(
            searching = TRUE, 
            pageLength = 10,
            autoWidth = TRUE
          )) |>
        formatPercentage("pickrate", digits = 1) |> 
        formatPercentage("winrate", digits = 1)
    })
    
    output$PickratesVis <- renderPlot({
      total_pickrates_for_maps() |> 
        filter(map_name == input$mapFilter) |> 
        head(input$topnPickrates) |> 
        ggplot(aes(x = reorder(hero_name, pickrate), y = pickrate, fill = role)) +
        geom_col() + 
        labs(x = "Hero", y = "Pickrate (%)") + 
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