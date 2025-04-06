server_interactions <- function(id, all_data){
  moduleServer(id, function(input, output, session){
    
  filtered_data <- reactive({
    filtered_data <- all_data |> 
      filter(week %in% input$weekFilter,
             mode %in% input$modeFilter,
             region %in% input$regionFilter) |> 
      select(round_id, match_map_id, match_id, hero_name,
             role, map_name, mode, team_name) 
  
    
    if(input$mapFilter != "All"){
      filtered_data <- filtered_data |> filter(map_name == input$mapFilter)
    }
    
    return(filtered_data)
  })
  
  compositions <- reactive({
    filtered_data() |> 
      group_by(match_map_id, round_id, team_name) |> 
      reframe(
        tank = hero_name[role == "tank"],
        dps = paste(head(sort(unique(hero_name[role == "dps"])), 2), collapse = ", "),
        sup = paste(head(sort(unique(hero_name[role == "sup"])), 2), collapse = ", "),
        iswin = mean(iswin)
        )
  })
  
  
  
})
}
