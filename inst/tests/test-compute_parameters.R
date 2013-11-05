context("compute_parameters")

test_that("the compute_parameters function matches precomputed values",{
  scenario_parameters <- get_scenario('AccuTams_1_2')
  set.seed(1)
  params <- compute_parameters(scenario_parameters)
  expected_params <- list(timeStep = 1, timeStop = 1500, systemName = "AccuTams_1_2", 
    systemDescription = "One wild strain is present initially. Two other strains can evolve - 2 point mutations to get\n    get one strain and from this strain another 2 point mutations to get to a third possible strain.\n    Strains have increasing fitness.", 
    Pf = c(1, 1, 1), treatments = list(), mutationAcceleration = 1, 
    Td = 0.2, N_S = 3, offStrains = c(2, 3), stochasticEventThresholdSource = function () 
    {
        return(runif(1))
    }, Epow = structure(c(0, 2, 4, 2, 0, 2, 4, 2, 0), .Dim = c(3L, 
    3L)), er = 1e-04, mu_T = 0.02, mu_P = 0.5, S_T = 2e+08, f = 0.37, 
    deathThreshold = 0.01, offThreshold = 0.1, deathModifier = 1.001, 
    newStrainLevel = 1, mutateDisc = structure(c(0, 3.7e-09, 
    3.7e-17, 0, 0, 0, 0, 0, 0), .Dim = c(3L, 3L)), mutateCont = structure(c(0.37, 
    0, 0, 0, 0, 0, 0, 0, 0), .Dim = c(3L, 3L)), stochasticEventThreshold = 0.2655086631421, 
    E = structure(c(0.37, 3.7e-09, 3.7e-17, 3.7e-09, 0.37, 3.7e-09, 
    3.7e-17, 3.7e-09, 0.37), .Dim = c(3L, 3L)), k = c(1.36392259606545e-10, 
    1.44927536231884e-10, 1.60472972972973e-10), fitnessAdjustment = c(0.807442176870748, 
    0.857971014492754, 0.95), baseRate = 1.68918918918919e-10, 
    N_d = 1)
  expect_that(params, equals(expected_params))
})

test_that("the compute_parameters function does not fail some sanity checks on many test scenarios", {
  scenario_names <- c('Simple_1_2', 'Simple_2_1', 'Simple_2_2', 'RepRes_1_1', 'Simple_1_0', 
                      'BadStart_1_1', 'PoorTreat_1_1', 'AccuRes_1_2', 'AccuTams_1_4', 
                      'AccuTams_1_3', 'Systematic_Buildup')
  
  
  for (scenario_name in scenario_names){
    scenario_parameters <- get_scenario(scenario_name)
    for (seed in 1:10){
      set.seed(seed)
      params <- compute_parameters(scenario_parameters)
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
