
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