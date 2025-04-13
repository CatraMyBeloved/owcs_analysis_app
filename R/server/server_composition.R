#' -----------------------------------------------------------------------------
#' server_composition.R
#' 
#' Description: Server function for composition analysis, backbone of composition
#'  analysis page.
#' 
#' Author: CatraMyBeloved
#' Date Created: 03-04-2025
#' Last Modified: 02-04-2025
#' -----------------------------------------------------------------------------
#' Composition Analysis Server Module
#' 
#' @description Handles the server-side logic for team composition analysis,
#'   allowing users to examine hero combinations used by teams.
#'
#' @param id The module ID used for namespacing
#' @param all_data Reactive data frame containing hero compositions and match data
#'
#' @details This module filters match data based on user selections (week, mode,
#'   region, team, map) and processes it to identify team compositions. It groups
#'   heroes by role (tank, DPS, support) and counts the frequency of specific
#'   compositions. Includes dynamic team filtering based on region selection and
#'   displays filtered matches.
#'
#' @return A Shiny module server function that handles composition analysis logic

composition_server <- function(id, all_data){
  moduleServer(id, function(input, output, session){
    
# ----------- Filtering ----------
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
#---------- Calculations ----------
    
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
    
    comp_with_indicators 
#---------- Outputs -----------
    
    output$filteredMatches <- renderDT({
      filtered_matches() |> 
        select(team_1, team_2, date, bracket) |> 
        datatable(
          colnames = c("Team 1", "Team 2", "Date", "Bracket"),
        )
    })
  })
}