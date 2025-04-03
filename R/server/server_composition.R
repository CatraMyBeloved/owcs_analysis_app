server_composition <- function(id, all_data){
  moduleServer(id, function(input, output, session){
    
    filtered_data <- reactive({
      filtered_data <- all_data |> 
        filter(week %in% input$weekFilter,
               mode %in% input$modeFilter,
               region %in% input$regionFilter) |> 
        select(round_id, match_map_id, match_id, hero_name,
               role, map_name, mode, team_name) 
      
      if(input$teamFilter != "All"){
        filtered_data <- filtered_data |> filter(team_name == input$teamFilter)
      } 
      
      if(input$mapFilter != "All"){
        filtered_data <- filtered_data |> filter(map_name == input$mapFilter)
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
      
      if(input$mapFilter != "All"){
        selected_map_id <- maps |> 
          filter(input$mapFilter == map_name) |> 
          pull(map_id)
      }
      
      filtered_match_ids <- filtered_matches |> 
        right_join(match_maps, by = "match_id") 
      
      if(input$mapFilter != "All"){
        filtered_match_ids <- filtered_match_ids |> 
          filter(map_id == selected_map_id)
      }
      
      filtered_match_ids <- filtered_match_ids |> 
        select(match_id) |> 
        distinct()
      
      
      filtered_matches <- filtered_matches |> 
        filter(match_id %in% filtered_match_ids$match_id)
      
      return(filtered_matches)
    })
    
    
    #Calculations ----------
    
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
        choices = c("All" = "All",teams_list),
        # Try to maintain current selection if it's still valid
        selected = if(input$teamFilter %in% names(teams_list)) input$teamFilter else NULL
      )
    })
    
    compositions <- reactive({
      filtered_data() |> 
        group_by(match_map_id, round_id, team_name) |> 
        reframe(
          tank = hero_name[role == "tank"],
          dps = paste(head(sort(unique(hero_name[role == "dps"])), 2), collapse = ", "),
          sup = paste(head(sort(unique(hero_name[role == "sup"])), 2), collapse = ", ")
        )
    })
    
    composition_counts <- reactive({
      compositions() |> 
        count(!!!syms(input$roleSelection), name = "n_played") 
    })
    
    output$compositions <- renderDT(
      datatable(composition_counts() |> arrange(desc(n_played)),
                colnames = c(input$roleSelection, "Rounds played"),
                options = list(
                  searching = TRUE, 
                  pageLength = 10,
                  autoWidth = TRUE
                )) 
    )
    
    output$filteredMatches <- renderDT({
      filtered_matches() |> 
        select(team_1, team_2, date, bracket) |> 
        datatable(
          colnames = c("Team 1", "Team 2", "Date", "Bracket"),
        )
    })
  })
}