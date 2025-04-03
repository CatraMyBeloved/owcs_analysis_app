#' -----------------------------------------------------------------------------
#' server_overview.R
#' 
#' Description: Server function for overview analysis, backbone of overview
#'  analysis page.
#' 
#' Author: CatraMyBeloved
#' Date Created: 03-04-2025
#' Last Modified: 02-04-2025
#' -----------------------------------------------------------------------------
#' Overview Server Module
#' 
#' @description Handles the server-side logic for the main overview page, providing
#'   a high-level analysis of hero usage across all matches.
#'
#' @param id The module ID used for namespacing
#' @param all_data Reactive data frame containing match and hero usage data
#'
#' @details This module allows filtering by week, mode, role, and region to display
#'   overall hero pickrates and winrates. It calculates the frequency of hero 
#'   selection across all maps and generates both tabular data and visualizations
#'   of the most commonly played heroes based on the applied filters.
#'
#' @return A Shiny module server function that handles overview analysis logic

overview_server <- function(id, all_data) {
  moduleServer(id, function(input, output, session) {
    # Filter logic
    filtered_data <- reactive({
      all_data |> 
        filter(week %in% input$weekFilter,
               mode %in% input$modeFilter,
               role %in% input$roleFilter,
               region %in% input$regionFilter) |> 
        select(round_id, match_map_id, match_id, hero_name,
               role, map_name, mode, team_name, team, iswin) 
    })
    
    # Calculations
    total_maps <- reactive({
      n_distinct(filtered_data()$match_map_id)
    })
    
    pickrates <- reactive({
      filtered_data() |> 
        distinct(hero_name, match_map_id, role, .keep_all = TRUE) |> 
        group_by(hero_name, role) |> 
        summarise(
          appearances = n(),
          pickrate = appearances/total_maps(),
          winrate = mean(iswin)
        ) |>
        filter(appearances > 0) |> 
        arrange(desc(pickrate))
    })
    
    # Outputs
    output$Pickrates <- renderDT({
      pickrates() |> 
        select(hero_name, appearances, pickrate, winrate) |> 
        datatable(
          colnames = c("Hero", "Maps played", "Pickrate", "Winrate"),
          filter = "top",
          options = list(
            searching = TRUE, 
            pageLength = 10,
            autoWidth = TRUE
          )) |> 
        formatPercentage(c("pickrate", "winrate"), digits = 1)
    })
    
    output$PickratesVis <- renderPlot({
      pickrates() |> 
        mutate(pickrate = pickrate * 100) |> 
        head(input$topnPickrates) |> 
        ggplot(aes(x = reorder(hero_name, pickrate), y = pickrate, fill = role)) +
        geom_col() + 
        labs(x = "Hero", y = "Pickrate (%)") + 
        coord_flip() 
    })
    

  })
}