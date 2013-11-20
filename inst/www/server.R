library(hdsResistanceModel)

shinyServer(function(input, output, session) {
  selected_scenario <- reactive({
    x <- get_scenario(input$scenario)
    updateNumericInput(session, "timeStop", value = x$timeStop)
    #updateNumericInput(session, "Pf", value = data.frame(as.list(x$Pf)))
    input$scenario
  })

  updated_scenario <- reactive({
    mod_pars <- list()
    mod_pars[['timeStop']] <- input$timeStop
#    if (!is.null(input$Pf)) {mod_pars[['Pf']] <- as.numeric(input$Pf[1,])}
    x <- get_scenario(selected_scenario(), modified_parameters = mod_pars)
    return(x)
  })

  pv <- reactive({
    ss <- run_system(updated_scenario(), input$seed)
    pv <- format_data(ss)
    return(pv)
  })

  output$scenarioUI <- renderUI({
    make_scenario_ui(updated_scenario())
  })

  output$pars <- renderPrint({
      print(updated_scenario())
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
