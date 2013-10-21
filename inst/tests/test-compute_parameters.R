context("compute_parameters")

test_that("the compute_parameters function matches precomputed values",{
  constants <- get_de_defaults()
  scenario_parameters <- get_scenario('Systematic_Buildup')
  set.seed(1)
  params <- compute_parameters(constants, scenario_parameters)
  expected_params <- structure(
    list(er = 1e-04, mu_T = 0.02, mu_P = 0.5, S_T = 2e+08, 
         f = 0.37, deathThreshold = 0.01, offThreshold = 0.1, timeStep = 1, 
         timeStop = 10000, systemName = "Systematic_Buildup", 
         systemDescription = "A Systematic build-up starting with one strain and adding ones slowly", 
         Pf = c(0.5000003, 0.5000034), treatments = list(), mutationAcceleration = 1, 
         Td = 0.5, N_S = 2, offStrains = 2, stochasticEventThresholdSource = function () 
         {
           runif(1)
         }, Epow = structure(c(0, 1, 1, 0), .Dim = c(2L, 2L)), 
         mutateDisc = structure(c(0, 3.7e-05, 0, 0), .Dim = c(2L, 2L)), 
         mutateCont = structure(c(0.37, 0, 0, 0), .Dim = c(2L, 2L)), 
         stochasticEventThreshold = 0.2655086631421, 
         E = structure(c(0.37, 3.7e-05, 3.7e-05, 0.37), .Dim = c(2L, 2L)), 
         k = c(1.35135216216216e-10, 1.35136054054054e-10), 
         fitnessAdjustment = c(0.5000003, 0.5000034), baseRate = 2.7027027027027e-10, 
         N_d = 1), .Names = c("er", "mu_T", "mu_P", "S_T", "f", "deathThreshold", 
                              "offThreshold", "timeStep", "timeStop", "systemName", "systemDescription", 
                              "Pf", "treatments", "mutationAcceleration", "Td", "N_S", "offStrains", 
                              "stochasticEventThresholdSource", "Epow", "mutateDisc", "mutateCont", 
                              "stochasticEventThreshold", "E", "k", "fitnessAdjustment", "baseRate", 
                              "N_d"))
  expect_that(params, equals(expected_params))
})

test_that("the compute_parameters function does not fail some sanity checks on many test scenarios", {
  scenario_names <- c('Simple_1_2', 'Simple_2_1', 'Simple_2_2', 'RepRes_1_1', 'Simple_1_0', 
                      'BadStart_1_1', 'PoorTreat_1_1', 'AccuRes_1_2', 'AccuTams_1_4', 
                      'AccuTams_1_3', 'Systematic_Buildup')
  
  
  for (scenario_name in scenario_names){
    constants <- get_de_defaults()
    scenario_parameters <- get_scenario(scenario_name)
    for (seed in 1:10){
      set.seed(seed)
      params <- compute_parameters(constants, scenario_parameters)
      with(params, {
        expect_that(length(k), equals(length(Pf)))
        expect_that(length(k), equals(N_S))
        expect_that(N_d > -1, is_true())
        expect_that(nrow(E), equals(ncol(E)))
        expect_that(dim(mutateCont), equals(dim(E)))
        expect_that(dim(mutateDisc), equals(dim(E)))
      })
    }
  }
})