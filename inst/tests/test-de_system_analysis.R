context("de_system_analysis")

test_that("the correct steady states are found", {
  x <- compute_parameters(get_scenario('tc_Simple_1_2'))
  y <- with(x, {environment(findSteadyState) <- environment(); findSteadyState()})
  expected_steady_state <- c(23368421.0526316, 0.00999000999000999, 0.00999000999000999, 
                             8421052631.57895, 1)
  for (i in 1:length(y)){
    expect_that(y[i], equals(expected_steady_state[i]))    
  }
  
  x <- compute_parameters(get_scenario('tc_AccuTams_1_2'))
  y <- with(x, {environment(findSteadyState) <- environment(); findSteadyState()})
  expected_steady_state <- c(1364112.76054388, 0.00999000999000999, 
                             0.00999000999000999, 9907830218.88217, 1)
  for (i in 1:length(y)){
    expect_that(y[i], equals(expected_steady_state[i]))    
  }
})

test_that("the steady states found by the 1d versions of the analytic steady solvers are true steady states", {
  fitnesses <- c(0.8000001, 0.8000002, 0.80001, 0.8001, 0.81, 0.9, 0.95)
  for (fitness in fitnesses){
    scenario <- get_scenario('tc_Simple_1_0')
    predicted_steady_state <- kBase_steady_state_relationship(fitness, scenario)[1]
    scenario$kBase <- fitness
    ss <- run_system(scenario, 1)
    expect_that(all(abs(ss$X1 - predicted_steady_state) < 10^-7), is_true())
  }
})

test_that("the fitness predicted by the analytical steady state solver to realize a certain steady state is correct",{
  population_levels <- c(10^2, 10^3, 10^4, 10^5, 10^6, 234, 5467.6, 23129.432)
  for (population_level in population_levels){
    scenario <- get_scenario('tc_Simple_1_0')
    predicted_fitness <- steady_state_kBase_relationship(population_level, scenario)
    scenario$kBase <- predicted_fitness
    ss <- run_system(scenario, 1)
    expect_that(all(abs(ss$X1 - population_level) < (10^-7) * population_level), is_true())
  }
})

test_that("The calc_growth_times function works", {
  ss <- run_system(get_scenario('tc_Simple_1_2'), 1)
  expect_that(calc_growth_time(ss, 2, 2, 100), equals(147))
  expect_that(calc_growth_time(ss, 2, 2, 1000), equals(235))
  expect_that(is.na(calc_growth_time(ss, 3, 2, 1000)), is_true())
  expect_that(calc_growth_time(ss, 3, 2, 5), equals(150))
})

test_that("The compute_stable_populations function works", {
  ss <- run_system(get_scenario('tc_AccuTams_1_2'),1)
  stable_pops <- compute_stable_populations(ss, compari = 5)
  expect_that(names(stable_pops), equals(c("Strain 1", "Strain 2", "Strain 3")))
  expect_that(names(stable_pops[[1]]), equals(c("0.00999", "1364112.76053", "1364112.76054")))

  stable_pops <- compute_stable_populations(ss, compari = 6)
  expect_that(names(stable_pops), equals(c("Strain 1", "Strain 2", "Strain 3")))
  expect_that(names(stable_pops[[1]]), equals(c("0.00999")))
})



