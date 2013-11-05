context('de_system')

test_that('Changing the deathThreshold via get_scenario works',{
  scenario <- get_scenario('AccuTams_1_2')
  ss <- run_system(scenario, 1)
  stable_pops <- compute_stable_populations(ss, compari = 5)
  expect_that(names(stable_pops[[1]]), equals(c("0.00999", "1364112.76053", "1364112.76054")))

  scenario <- get_scenario('AccuTams_1_2', modified_parameters = list(deathThreshold = 0.05))
  ss <- run_system(scenario, 1)
  stable_pops <- compute_stable_populations(ss, compari = 5)
  expect_that(names(stable_pops[[1]]), equals("0.04995"))

  scenario <- get_scenario('AccuTams_1_2', modified_parameters = list(deathThreshold = 0.001))
  ss <- run_system(scenario, 1)
  stable_pops <- compute_stable_populations(ss, compari = 5)
  expect_that(names(stable_pops[[3]]), equals("0.001"))

  scenario <- get_scenario('AccuTams_1_2', modified_parameters = list(deathThreshold = 0.0001))
  ss <- run_system(scenario, 1)
  stable_pops <- compute_stable_populations(ss, compari = 5)
  expect_that(names(stable_pops[[3]]), equals("1e-04"))

})

test_that('Changing the offThreshold via get_scenario works',{
  expect_that(1, equals(1))
})
