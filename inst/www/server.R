library(hdsResistanceModel)
 
shinyServer(function(input, output) {
  cur_scenario <- reactive({
    mod_pars <- list()
    mod_pars[['timeStop']] <- input$timeStop
    x <- get_scenario(input$scenario, modified_parameters = mod_pars)
    print (x)
    x
  })

  pv <- reactive({
    ss <- run_system(cur_scenario(), input$seed)
    pv <- format_data(ss)
    return(pv)
  })

  output$scenarioUI <- renderUI({
    make_scenario_ui(cur_scenario())
  })

  output$pars <- renderPrint({
      print(cur_scenario())
  })

  output$Strain <- renderPlot({
    print(plot_component(pv(), component = 'Strain'))
  })
  output$tCells <- renderPlot({
    print(plot_component(pv(), component = "tCells"))
  })
  output$notMutation <- renderPlot({
    print(plot_component(pv(), component = "notMutation"))
  })
})
