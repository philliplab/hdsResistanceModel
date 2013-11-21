library(hdsResistanceModel)

scenario_changed <- FALSE

shinyServer(function(input, output, session) {
 
  selected_scenario <- reactive({
    x <- get_scenario(input$scenario)
    scenario_changed <<- TRUE
    updateNumericInput(session, "timeStep", value = x$timeStep)
    updateNumericInput(session, "timeStop", value = x$timeStop)
    updateNumericInput(session, "Pf", value = data.frame(as.list(x$Pf)))
    print ('---------> SELECTED')
    print (scenario_changed)
    return(input$scenario)
  })

  updated_scenario <- reactive({
    print ('---------> UPDATED - start')
    print (scenario_changed)
    scenario_name <- selected_scenario()
    mod_pars <- list()
    print ('---------> UPDATED - middle')
    print (scenario_changed)
    input_timeStop <- input$timeStop
    input_timeStep <- input$timeStep
    input_Pf <- input$Pf
    if (!scenario_changed){
      mod_pars[['timeStop']] <- input_timeStop
      mod_pars[['timeStep']] <- input_timeStep
      mod_pars[['Pf']] <- as.numeric(input_Pf[1,])
      x <- get_scenario(scenario_name, modified_parameters = mod_pars)
    }  else {
      x <- get_scenario(scenario_name)
      scenario_changed <<- FALSE
    }

    print ('---------> UPDATED - end')
    print (scenario_changed)
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
