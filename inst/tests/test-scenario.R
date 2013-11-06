context("Scenario")

scenario_spec <- list(
    timeStep = 1,
    timeStop = 1000,
    systemName = "tc_Simple_1_2",
    systemDescription = "One wild type virus that can stochastically mutate into 2 target strains. This allows for 2 different stochastic events and 2 different 'paths' through the system. No treatment effect but strain 2 is more fit than strain 1",
    Pf = c(0.95, 1, 0.96),
    treatments = list(list(t = 0, Te = 0.0, Ts = c(0.8, 0.80, 0.75))),
    mutationAcceleration = 1.5*(10^(-1)),
    Td = 0.2,
    N_S = 3,
    offStrains = c(2,3),
    stochasticEventThresholdSource = function(){return(runif(1))},
    Epow = c(0, 2, 2,
             2, 0, 2,
             2, 2, 0)
  )

test_that("A correct specification yields a correctly formatted list",{
  x <- do.call(scenario, scenario_spec)
  x_names <- sort(names(x))
  expected_names <- sort(c("deathModifier", "deathThreshold", "Epow", "er", "f", "mu_P", 
                           "mu_T", "mutationAcceleration", "N_S", "offStrains", 
                           "offThreshold", "Pf", "S_T", "stochasticEventThresholdSource",
                           "systemDescription", "systemName", "Td", "timeStep", "timeStop", 
                           "treatments", "newStrainLevel"))
  expect_that(x, is_a('list'))
  expect_that(x[['timeStep']], equals(1))
  expect_that(x[['timeStop']], equals(1000))
  expect_that(x[['systemName']], equals("tc_Simple_1_2"))
  expect_that(x[['systemDescription']], equals("One wild type virus that can stochastically mutate into 2 target strains. This allows for 2 different stochastic events and 2 different 'paths' through the system. No treatment effect but strain 2 is more fit than strain 1"))
  expect_that(x[['Pf']], equals(c(0.95, 1, 0.96)))
  expect_that(x[['treatments']], equals(list(list(t = 0, Te = 0.0, Ts = c(0.8, 0.80, 0.75)))))
  expect_that(x[['mutationAcceleration']], equals(1.5*(10^(-1))))
  expect_that(x[['Td']], equals(0.2))
  expect_that(x[['N_S']], equals(3))
  expect_that(x[['offStrains']], equals(c(2,3)))
  expect_that(x[['stochasticEventThresholdSource']], equals(function(){return(runif(1))}))
  expect_that(x[['Epow']], equals(c(0, 2, 2, 2, 0, 2, 2, 2, 0)))
  expect_that(x_names, equals(expected_names))
})

test_that("An incorrect specification throws an error",{
  numeric_variables <- c("Epow", "mutationAcceleration", "N_S", "offStrains", 
                         "Pf", "Td", "timeStep", "timeStop", "er", "mu_T",
                         "mu_P", "S_T", "f", "deathThreshold", "offThreshold",
                         "deathModifier", "newStrainLevel")
  for (var_name in numeric_variables){
    nonnumeric_variable <- scenario_spec
    nonnumeric_variable[[var_name]] <- "Hello"
    err_msg <- str_c(var_name, ' is not numeric')
    expect_that(do.call(scenario, nonnumeric_variable), throws_error(err_msg))
  }
  wrong_timeStep <- scenario_spec
  wrong_timeStep[['timeStep']] <- 150
  err_msg <- "timeStep must be < timeStop/10"
  expect_that(do.call(scenario, wrong_timeStep), throws_error(err_msg))
})
