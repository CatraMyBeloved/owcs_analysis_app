# Ban page server module
# This module handles data filtering and analysis related to hero bans.
# Backend for the "Ban Analysis" tab in the app.


ban_server <- function(id, all_bans){
  moduleServer(id, function(input, output, session){
    
    filtered_ban_data <- reactive({
      filtered_data <- all_bans |> filter(
        week %in% input$weekFilter,
        region %in% input$regionFilter
      )
      
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
        filtered_match_ids <- filtered_matches |> 
          filter(map_id == selected_map_id)
      }
      
      filtered_match_ids <- filtered_match_ids |> 
        select(match_id) |> 
        distinct()
      
      
      filtered_matches <- filtered_matches |> 
        filter(match_id %in% filtered_match_ids$match_id)
      
      return(filtered_matches)
      
    })
    # update available teams by region
    
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
    
    total_maps_ban <- reactive({
      n_distinct(filtered_ban_data()$match_map_id)
    })
    
    banrates <- reactive({
      filtered_ban_data() |>
        group_by(hero_name) |> 
        summarize(
          first_ban = sum(first_bool == TRUE),
          second_ban = sum(first_bool == FALSE),
          total_ban = n(),
          first_ban_rate = first_ban / total_maps_ban(),
          second_ban_rate = second_ban / total_maps_ban(),
          total_ban_rate = total_ban / total_maps_ban()
        ) |> 
        arrange(desc(total_ban))
    })
    
    #outputs ---------
    
    output$banrates <- renderDT(
      banrates() |> 
        datatable(
          colnames = c("Hero", "N first bans", "N follow-up bans", "N total bans",
                       "Rate first bans", "Rate second bans", "Total rate"),
          options = list(
            searching = TRUE, 
            pageLength = 10,
            autoWidth = TRUE
          )
        ) |> 
        formatPercentage(c("first_ban_rate", "second_ban_rate", "total_ban_rate"), digits = 1)
      
    )
    
    output$banratesVis <- renderPlot({
      # Prepare data in long format
      ban_data_long <- banrates() |>
        head(input$topnBanrates) |>
        select(hero_name, first_ban_rate, second_ban_rate) |>
        pivot_longer(
          cols = c(first_ban_rate, second_ban_rate),
          names_to = "ban_type",
          values_to = "ban_rate"
        ) |>
        mutate(
          ban_type = case_when(
            ban_type == "first_ban_rate" ~ "First Ban",
            ban_type == "second_ban_rate" ~ "Second Ban"
          ),
          hero_name = reorder(hero_name, ban_rate, sum),
          ban_type = factor(ban_type, levels = c("Second Ban", "First Ban"))
        )
      
      # Create the stacked bar chart
      ggplot(ban_data_long, aes(x = hero_name, y = ban_rate, fill = ban_type)) +
        geom_col() +
        scale_fill_manual(
          values = c("First Ban" = "#3A85C7", "Second Ban" = "#5AAAE0"),
          name = "Ban Type"
        )  +
        labs(
          x = NULL,
          y = "Ban Rate",
          title = "Hero Ban Rates"
        ) +
        coord_flip() +
        theme_minimal() +
        scale_y_continuous(labels = scales::percent) +
        guides(fill = guide_legend(reverse = TRUE))
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