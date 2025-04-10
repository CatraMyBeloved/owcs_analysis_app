interaction_server <- function(id, all_data){
  moduleServer(id, function(input, output, session){
    
    filtered_data <- reactive({
      filtered_data <- all_data |> 
        filter(week %in% input$weekFilter,
               region %in% input$regionFilter) 
      
      
      if(input$teamFilter != "All"){
        filtered_data <- filtered_data |> filter(team_name == input$teamFilter)
      } 
      
      return(filtered_data)
    })
    
    comps <- reactive({
      filtered_data() |> 
        group_by(match_map_id, round_id, team_name) |> 
        reframe(
          tank = hero_name[role == "tank"],
          dps = list(head(hero_name[role == "dps"], 2)),
          sup = list(head(hero_name[role == "sup"], 2)),
          iswin = mean(iswin)
        ) 
    })
    
    comps_with_opponents <- reactive({
      base_comps <- comps()
      
      compositions_with_teamid <- 
        base_comps |> 
        group_by(round_id) |> 
        mutate(teamid = row_number()) |> 
        ungroup()
      
      comps_with_opponents <- 
        compositions_with_teamid |> 
        inner_join(
          compositions_with_teamid |> 
            select(round_id, teamid, team_name, 
                   tank, dps, sup),
          by = "round_id",
          suffix = c("", "_opp"),
          relationship = "many-to-many"
        ) |> 
        filter(teamid != teamid_opp)
      
      return(comps_with_opponents)
    })
    
    
    create_hero_matrix <- function(compositions, all_heroes){
      all_heroes <- c(unlist(all_heroes))
      n_rows <- nrow(compositions)
      n_columns <- lenght(all_heroes)
      
      team_matrix <- matrix(FALSE, nrow = n_rows, ncol = n_columns)
      opp_matrix <- matrix(FALSE, nrow = n_rows, ncol = n_columns)
      
      colnames(team_matrix) <- all_heroes
      colnames(opp_matrix) <- all_heroes
      
      for(i in 1:n_rows){
        team_heroes <- c(compositions$tank[i], 
                         unlist(compositions$dps[i]), 
                         unlist(compositions$sup[i]))
        
        opp_heroes <- c(compositions$tank_opp[i],
                        unlist(compositions$dps_opp[i]),
                        unlist(compositions$sup_opp[i]))
        
        team_matrix[i, match(team_heroes, all_heroes)] <- TRUE
        opp_matrix[i, match(opp_heroes, all_heroes)] <- TRUE
      }
      team_df <- as.data.frame(team_matrix)
      opp_df <- as.data.frame(opp_matrix)
      
      names(team_df) <- paste0("has_", names(team_df))
      names(opp_df) <- paste0("has_", names(opp_df), "_opp")
      
      result <- bind_cols(compositions, team_df, opp_df)
      
      return(result)
    }
    
    comps_with_indicators <- reactive({
      result <- create_hero_matrix(comps_with_opponents(), hero_list)
      return(result)
    })
    
    popular_combinations <- reactive({
      
      selected_hero <- input$heroFilterSel
      column_name_selected <- paste0("has_", selected_hero)
      
      popular_combinations |> 
        filter(.data[[column_name_selected]]) |> 
        summarize(
          across(starts_with("has_") & !contain("_opp"),
                 mean)
        )
    })
  })
}
      