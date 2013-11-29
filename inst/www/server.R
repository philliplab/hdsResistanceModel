library(hdsResistanceModel)

scenario_changed <- FALSE
err_msg <- NULL

shinyServer(function(input, output, session) {
 
  selected_scenario <- reactive({
    x <- get_scenario(input$scenario)
    scenario_changed <<- TRUE
    updateNumericInput(session, "timeStep", value = x$timeStep)
    updateNumericInput(session, "timeStop", value = x$timeStop)
    updateNumericInput(session, "kBase", value = data.frame(as.list(x$kBase)))
    updateNumericInput(session, "mutMat", value = data.frame(as.list(x$mutMat)))
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
    input_kBase <- input$kBase
    input_mutMat <- input$mutMat
    if (!scenario_changed){
      mod_pars[['timeStop']] <- input_timeStop
      mod_pars[['timeStep']] <- input_timeStep
      mod_pars[['kBase']] <- as.numeric(input_kBase[1,])
      mod_pars[['mutMat']] <- as.numeric(input_mutMat)
      x <- try(get_scenario(scenario_name, modified_parameters = mod_pars))
    }  else {
      x <- get_scenario(scenario_name)
      scenario_changed <<- FALSE
    }
    if (class(x) == 'try-error') {
      err_msg <<- attr(x, 'condition')[[1]]
      x <- get_scenario(scenario_name)
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

  output$status <- renderText({
    x <- updated_scenario()
    if (is.null(err_msg)) ret_val <- 'System Ran Successfully'
    else {
      ret_val <- paste('The following error(s) were encountered: "',
                   err_msg,
                   '". Resetting to the values specified in the saved scenario',
                   sep = "")

    }
    err_msg <<- NULL
    return(ret_val)
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
