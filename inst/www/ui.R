library(hdsResistanceModel)
 
shinyUI(pageWithSidebar(
  headerPanel('Hybrid Dynamic and Stochastic Model of HIV Resistance'),
  sidebarPanel(
    selectInput('scenario', 'Scenario', get_scenario_names()),
    numericInput('seed', 'seed', 1),
    submitButton(),
    uiOutput('scenarioUI')
  ),
  mainPanel(
    h5(textOutput('status')),
    tabsetPanel(tabPanel("Plots", 
                         plotOutput(outputId='Strain'),
                         plotOutput(outputId='tCells'),
                         plotOutput(outputId='notMutation')
                         ),
                tabPanel("Parameters",
                         h4("Scenario Parameters"),
                         verbatimTextOutput('pars'),
                         h4("Input Parameters"),
                         verbatimTextOutput('inputPars')
                         )
                )
  )
))
