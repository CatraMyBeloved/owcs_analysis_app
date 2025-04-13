interaction_server <- function(id, all_data) {
  moduleServer(id, function(input, output, session) {
    filtered_data <- reactive({
      filtered_data <- all_data |>
        filter(
          week %in% input$weekFilter,
          region %in% input$regionFilter
        )


      if (input$teamFilter != "All") {
        filtered_data <- filtered_data |> filter(team_name == input$teamFilter)
      }

      return(filtered_data)
    })

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

    comp_counts <- reactive({
      comps() |>
        count(tank, dps, sup) |>
        arrange(desc(count)) |>
        mutate(tank = list(tank))
    })
  })
}
