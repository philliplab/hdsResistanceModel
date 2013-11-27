context("Scenario")

scenario_spec <- list(
    timeStep = 1,
    timeStop = 1000,
    systemName = "tc_Simple_1_2",
    systemDescription = "One wild type virus that can stochastically mutate into 2 target strains. This allows for 2 different stochastic events and 2 different 'paths' through the system. No treatment effect but strain 2 is more fit than strain 1",
    kBase = c(0.95, 1, 0.96),
    treatments = list(list(t = 0, A = 0.0, Ts = c(0.8, 0.80, 0.75))),
    mutationAcceleration = 1.5*(10^(-1)),
    Td = 0.2,
    N_S = 3,
    offStrains = c(2,3),
    stochasticEventThresholdSource = function(){return(runif(1))},
    mutMat = c(0, 2, 2,
             2, 0, 2,
             2, 2, 0)
  )

test_that("A correct specification yields a correctly formatted list",{
  x <- do.call(scenario, scenario_spec)
  x_names <- sort(names(x))
  expected_names <- sort(c("deathModifier", "deathThreshold", "mutMat", "er", "f", "mu_P", 
                           "mu_T", "mutationAcceleration", "N_S", "offStrains", 
                           "offThreshold", "kBase", "S_T", "stochasticEventThresholdSource",
                           "systemDescription", "systemName", "Td", "timeStep", "timeStop", 
                           "treatments", "newStrainLevel"))
  expect_that(x, is_a('list'))
  expect_that(x[['timeStep']], equals(1))
  expect_that(x[['timeStop']], equals(1000))
  expect_that(x[['systemName']], equals("tc_Simple_1_2"))
  expect_that(x[['systemDescription']], equals("One wild type virus that can stochastically mutate into 2 target strains. This allows for 2 different stochastic events and 2 different 'paths' through the system. No treatment effect but strain 2 is more fit than strain 1"))
  expect_that(x[['kBase']], equals(c(0.95, 1, 0.96)))
  expect_that(x[['treatments']], equals(list(list(t = 0, A = 0.0, Ts = c(0.8, 0.80, 0.75)))))
  expect_that(x[['mutationAcceleration']], equals(1.5*(10^(-1))))
  expect_that(x[['Td']], equals(0.2))
  expect_that(x[['N_S']], equals(3))
  expect_that(x[['offStrains']], equals(c(2,3)))
  expect_that(x[['stochasticEventThresholdSource']], equals(function(){return(runif(1))}))
  expect_that(x[['mutMat']], equals(c(0, 2, 2, 2, 0, 2, 2, 2, 0)))
  expect_that(x_names, equals(expected_names))
})

test_that("An incorrect specification throws an error",{
  # Numeric variables
  numeric_variables <- c("mutMat", "mutationAcceleration", "N_S", "offStrains", 
                         "kBase", "Td", "timeStep", "timeStop", "er", "mu_T",
                         "mu_P", "S_T", "f", "deathThreshold", "offThreshold",
                         "deathModifier", "newStrainLevel")
  for (var_name in numeric_variables){
    nonnumeric_variable <- scenario_spec
    nonnumeric_variable[[var_name]] <- "Hello"
    err_msg <- str_c(var_name, ' is not numeric')
    expect_that(do.call(scenario, nonnumeric_variable), throws_error(err_msg))
  }
  
  # timeStep / timeStop
  wrong_timeStep <- scenario_spec
  wrong_timeStep[['timeStep']] <- 150
  err_msg <- "timeStep must be < timeStop/10"
  expect_that(do.call(scenario, wrong_timeStep), throws_error(err_msg))
  
  # treatments
  wrong_treatment1 <- scenario_spec
  wrong_treatment1[['treatments']] <- 1
  err_msg <- "treatments must be a list"
  expect_that(do.call(scenario, wrong_treatment1), throws_error(err_msg))

  wrong_treatment2 <- scenario_spec
  wrong_treatment2[['treatments']] <- list()
  err_msg <- "at least one treatment must be specified"
  expect_that(do.call(scenario, wrong_treatment2), throws_error(err_msg))

  wrong_treatment3 <- scenario_spec
  wrong_treatment3[['treatments']] <- list(list(t=1))
  err_msg <- "first treatment must start at t=0"
  expect_that(do.call(scenario, wrong_treatment3), throws_error(err_msg))

  wrong_treatment4 <- scenario_spec
  wrong_treatment4[['treatments']] <- list(list(t=0))
  err_msg <- "incorrect treatment - must be a list with params t, A and Ts"
  expect_that(do.call(scenario, wrong_treatment4), throws_error(err_msg))

  wrong_treatment5 <- scenario_spec
  wrong_treatment5[['treatments']] <- list(list(t=0, A = 1, Ts = c(0,0,0), bla = "boom"))
  err_msg <- "incorrect treatment - must be a list with params t, A and Ts"
  expect_that(do.call(scenario, wrong_treatment5), throws_error(err_msg))

  wrong_treatment6 <- scenario_spec
  wrong_treatment6[['treatments']] <- list(list(t=0, A = 1, Ts = c(0,0,0)),
                                           list(t=10, A = 1, Ts = c(0,0,0), bla = "boom"))
  err_msg <- "incorrect treatment - must be a list with params t, A and Ts"
  expect_that(do.call(scenario, wrong_treatment6), throws_error(err_msg))

  wrong_treatment6 <- scenario_spec
  wrong_treatment6[['treatments']] <- list(list(t=0, A = 1, Ts = c(1,0,0)))
  err_msg <- "must be greater than Td" # Only use partial match because of regexp weirdness
  expect_that(do.call(scenario, wrong_treatment6), throws_error(err_msg))

  err_msg <- "must be greater than Td" # Only use partial match because of regexp weirdness
  expect_that(get_scenario('AccuTams_1_2_3', modified_parameters = list(kBase = c(0.5,1,1))), 
              throws_error(err_msg))
})
