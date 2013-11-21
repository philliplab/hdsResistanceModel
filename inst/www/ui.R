library(hdsResistanceModel)
 
shinyUI(pageWithSidebar(
  headerPanel('Hybrid Dynamic and Stochastic Model of HIV Resistance'),
  sidebarPanel(
    selectInput('scenario', 'Scenario', get_scenario_names()),
    numericInput('seed', 'seed', 1),
#    submitButton(),
    uiOutput('scenarioUI')
  ),
  mainPanel(
    plotOutput(outputId='Strain'),
    plotOutput(outputId='tCells'),
    plotOutput(outputId='notMutation'),
    verbatimTextOutput('pars')
  )
))
