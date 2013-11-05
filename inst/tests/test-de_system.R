context('de_system')

test_that('a base case system runs and produces expected stable populations',{
  scenario_spec <- get_scenario('AccuTams_1_2')
  ss <- run_system(scenario_spec, 1)
  stable_pops <- compute_stable_populations(ss, compari = 5)
  expect_that(names(stable_pops[[1]]), equals(c("0.00999", "1364112.76053", "1364112.76054")))
  extinction_points <- compute_offThreshold(ss, scenario_spec)
  expect_that(extinction_points, equals(0.104748887263143))
})

test_that('the deathThreshold can be changed',{
  scenario_spec <- get_scenario('AccuTams_1_2', modified_parameters = list(deathThreshold = 0.05))
  ss <- run_system(scenario_spec, 1)
  stable_pops <- compute_stable_populations(ss, compari = 5)
  expect_that(names(stable_pops[[1]]), equals("0.04995"))

  scenario_spec <- get_scenario('AccuTams_1_2', modified_parameters = list(deathThreshold = 0.001))
  ss <- run_system(scenario_spec, 1)
  stable_pops <- compute_stable_populations(ss, compari = 5)
  expect_that(names(stable_pops[[3]]), equals("0.001"))

  scenario_spec <- get_scenario('AccuTams_1_2', modified_parameters = list(deathThreshold = 0.0001))
  ss <- run_system(scenario_spec, 1)
  stable_pops <- compute_stable_populations(ss, compari = 5)
  expect_that(names(stable_pops[[3]]), equals("1e-04"))
})

test_that('the deathModifier can be changed', {
  scenario_spec <- get_scenario('AccuTams_1_2', modified_parameters = list(deathModifier = 2))
  ss <- run_system(scenario_spec, 1)
  stable_pops <- compute_stable_populations(ss, compari = 6)
  expect_that(names(stable_pops[[1]]), equals(c("0.005")))
  expect_that(names(stable_pops[[2]]), equals(c("0.005", "2.417192")))
  expect_that(names(stable_pops[[3]]), equals("0.005"))
})

test_that('Changing the offThreshold via get_scenario works',{
  scenario_spec <- get_scenario('AccuTams_1_2', modified_parameters = list(offThreshold = 0.2))
  ss <- run_system(scenario_spec, 1)
  extinction_points <- compute_offThreshold(ss, scenario_spec)
  expect_that(extinction_points, equals(0.201404339457047))

  scenario_spec <- get_scenario('AccuTams_1_2', modified_parameters = list(offThreshold = 0.02))
  ss <- run_system(scenario_spec, 1)
  extinction_points <- compute_offThreshold(ss, scenario_spec)
  expect_that(extinction_points, equals(0.0209667819904174))
})
