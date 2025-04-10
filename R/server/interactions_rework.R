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
    
    comps_with_indicators <-reactive({
      
      compositions_indicator <- comps_with_opponents()
      
      column_names_team <- paste0("has_", hero_list)
      column_names_opp <- paste0("has_", hero_list, "_opp")
      
      compositions_indicator[column_names_team]<- 0
      compositions_indicator[column_names_opp] <- 0
      
      compositions_indicator <- compositions_indicator |> 
        rowwise() |> 
        mutate(all_heroes_team = list(c(tank, unlist(dps), unlist(sup))),
               all_heroes_opp = list(c(tank_opp, unlist(dps_opp), unlist(sup_opp))))
      
     
      
      compositions_indicator <- compositions_indicator |> 
        mutate(across(starts_with("has_") & !contains("_opp"), 
                      ~str_detect(all_heroes_team, str_remove(cur_column(), "has_"))),
               across(starts_with("has_") & contains("_opp"),
                      ~str_detect(all_heroes_opp, str_remove(cur_column(), "has_") |> 
                                    str_remove("_opp"))))
      
    
    return(comps_with_indicators)
  })
    
  })
}
      