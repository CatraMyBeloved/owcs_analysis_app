#' -----------------------------------------------------------------------------
#' server_map.R
#' 
#' Description: Server function for map analysis, backbone of map analysis page.
#' 
#' Author: CatraMyBeloved
#' Date Created: 03-04-2025
#' Last Modified: 02-04-2025
#' -----------------------------------------------------------------------------
#' Map Analysis Server Module
#' 
#' @description Handles the server-side logic for analyzing hero performance and
#'   usage on specific maps.
#'
#' @param id The module ID used for namespacing
#' @param all_data Reactive data frame containing match and hero usage data
#'
#' @details This module filters data by map and other criteria (week, role, region,
#'   team) to show which heroes are commonly used on each map. Calculates pickrates
#'   and winrates for heroes on the selected map. Includes dynamic team filtering
#'   based on region selection and visualizes the top heroes for each map.
#'
#' @return A Shiny module server function that handles map-specific analysis

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
    
    general_pickrates <- reactive({
      pickrates <- calculate_pickrates(filtered_data_by_maps())
      return(pickrates)
    })
    
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
      res <- filtered_data_by_maps() |> 
        group_by(map_name, hero_name, role) |> 
        summarize(all_appearances= n(),
                  hero_played = n_distinct(match_map_id),
                  hero_wins = sum(iswin),
                  .groups = "drop") |> 
        left_join(total_n_played(), by = "map_name") |>
        mutate(map_pickrate = hero_played / n_played,
               winrate = hero_wins / all_appearances) |> 
        arrange(desc(map_pickrate)) 
    })
    
    pickrate_comparison <- reactive({
      
      res2 <- total_pickrates_for_maps() |> 
        filter(map_name == input$mapFilter) |> 
        left_join(general_pickrates(), by = c("hero_name", "role")) |> 
        mutate(pickrate_diff = map_pickrate - pickrate)

    })

    
    # Outputs -----------
    
    output$Pickrates <- renderDT({
      pickrate_comparison() |> 
        select(hero_name, hero_played, map_pickrate,  pickrate_diff, winrate.x) |> 
        datatable(
          colnames = c("Hero", "Appearances on map",
                       "Pickrate", "Difference to avg","Winrate"),
          filter = "top",
          options = list(
            searching = TRUE, 
            pageLength = 10,
            autoWidth = TRUE
          )) |>
        formatPercentage("map_pickrate", digits = 1) |> 
        formatPercentage("winrate.x", digits = 1) |> 
        formatPercentage("pickrate_diff", digits = 1)
      
    })
    
    color_helper <- function(number){
      if(number < 0){
        return("neg")
      } else{
        return("pos")
      }
    }
    
    output$PickratesVis <- renderPlot({
      pickrate_comparison() |> 
        arrange(desc(abs(pickrate_diff))) |> 
        head(input$topnPickrates) |>
        mutate(pickrate_diff = pickrate_diff * 100) |>
        ggplot(aes(x = reorder(hero_name, abs(pickrate_diff)), y = pickrate_diff, fill = ifelse(pickrate_diff < 0, "Below Average", "Above Average"))) +
        geom_col() +
        scale_fill_manual(values = c("Below Average" =  "#FF9E7A", "Above Average" = "#7AB8FF"), 
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