library(hdsResistanceModel)

par_value_holder <- list()
par_value_holder[['timeStop']] <- NULL

shinyServer(function(input, output, session) {
 
  selected_scenario <- reactive({
    x <- get_scenario(input$scenario)
    updateNumericInput(session, "timeStop", value = x$timeStop)
    par_value_holder[['timeStop']] <<- x$timeStop
    #updateNumericInput(session, "Pf", value = data.frame(as.list(x$Pf)))
    print ('---------> SELECTED')
    print (par_value_holder)
    return(input$scenario)
  })

  updated_scenario <- reactive({
    print ('---------> UPDATED - start')
    print (par_value_holder)
    scenario_name <- selected_scenario()
    mod_pars <- list()
    print ('---------> UPDATED - middle')
    print (par_value_holder)
    its <- input$timeStop
    if (is.null(par_value_holder[['timeStop']])){
      mod_pars[['timeStop']] <- its
    }  else {
      mod_pars[['timeStop']] <- par_value_holder[['timeStop']]
      par_value_holder[['timeStop']] <<- NULL
    }

#    if (!is.null(input$Pf)) {mod_pars[['Pf']] <- as.numeric(input$Pf[1,])}
    x <- get_scenario(scenario_name, modified_parameters = mod_pars)
    print ('---------> UPDATED - end')
    print (par_value_holder)
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
