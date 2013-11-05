context('de_system')

test_that('a base case system runs and produces a solved system with the expected properties',{
  scenario_spec <- get_scenario('AccuTams_1_2')
  ss <- run_system(scenario_spec, 1)
  stable_pops <- compute_stable_populations(ss, compari = 5)
  expect_that(names(stable_pops[[1]]), equals(c("0.00999", "1364112.76053", "1364112.76054")))
  extinction_points <- compute_offThreshold(ss, scenario_spec)
  expect_that(extinction_points, equals(0.104748887263143))
  new_strain_levels <- compute_newStrainLevel(ss, scenario_spec)
  expect_that(new_strain_levels, equals(c(1.02200334156279, 1.07931700878306)))
})

test_that('the deathThreshold variable can be changed',{
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

test_that('the deathModifier variable can be changed', {
  scenario_spec <- get_scenario('AccuTams_1_2', modified_parameters = list(deathModifier = 2))
  ss <- run_system(scenario_spec, 1)
  stable_pops <- compute_stable_populations(ss, compari = 6)
  expect_that(names(stable_pops[[1]]), equals(c("0.005")))
  expect_that(names(stable_pops[[2]]), equals(c("0.005", "2.417192")))
  expect_that(names(stable_pops[[3]]), equals("0.005"))
})

test_that('the newStrainLevel variable can be changed', {
  scenario_spec <- get_scenario('AccuTams_1_2', modified_parameters = list(newStrainLevel = 1))
  ss <- run_system(scenario_spec, 1)
  new_strain_levels <- compute_newStrainLevel(ss, scenario_spec)

})

test_that('the offThreshold variable can be changed',{
  scenario_spec <- get_scenario('AccuTams_1_2', modified_parameters = list(offThreshold = 0.2))
  ss <- run_system(scenario_spec, 1)
  extinction_points <- compute_offThreshold(ss, scenario_spec)
  expect_that(extinction_points, equals(0.201404339457047))

  scenario_spec <- get_scenario('AccuTams_1_2', modified_parameters = list(offThreshold = 0.02))
  ss <- run_system(scenario_spec, 1)
  extinction_points <- compute_offThreshold(ss, scenario_spec)
  expect_that(extinction_points, equals(0.0209667819904174))
})

test_that('the newStrainLevel variable can be changed', {
  scenario_spec <- get_scenario('AccuTams_1_2', modified_parameters = list(newStrainLevel = 2))
  ss <- run_system(scenario_spec, 1)
  new_strain_levels <- compute_newStrainLevel(ss, scenario_spec)
  expect_that(new_strain_levels, equals(c(2.04006706130329, 2.1763859363346)))

  scenario_spec <- get_scenario('AccuTams_1_2', modified_parameters = list(newStrainLevel = 200))
  ss <- run_system(scenario_spec, 1)
  new_strain_levels <- compute_newStrainLevel(ss, scenario_spec)
  expect_that(new_strain_levels, equals(c(203.616717742328, 214.404925733389)))
})

test_that('the system can function when extinction / re-emergence levels are much higher than nornal', {
  scenario_spec <- get_scenario('AccuTams_1_2', modified_parameters = list(newStrainLevel = 100, offThreshold = 10))
  ss <- run_system(scenario_spec, 1)
  new_strain_levels <- compute_newStrainLevel(ss, scenario_spec)
  expect_that(new_strain_levels, equals(c(101.81032852282, 98.2401298876285, 96.3574121948022, 
                                          99.9158545095834, 97.184619769535, 98.4983789980245, 
                                          98.5939500806866, 96.9254823276668, 99.4748075741849, 
                                          96.4422318731418, 106.169398872464)))
})
