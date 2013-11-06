context("get_scenario")

test_that("the get_scenario function works", {
  expect_that(get_scenario("This scenario definitely does not exist"), throws_error("Scenario does not exist"))
  expect_that(get_scenario("tc_Simple_1_2"), is_a("list"))
  mod_scenario <- get_scenario("tc_Simple_1_2", modified_parameters = list(f=1))
  expect_that(mod_scenario[['f']], equals(1))
  mod_scenario <- get_scenario("tc_Simple_1_2", modified_parameters = list(Epow=1:4))
  expect_that(mod_scenario[['Epow']], equals(1:4))
  modify_invalid_par <- quote(get_scenario("tc_Simple_1_2", modified_parameters = list(invalid_par=1:4)))
  expect_that(eval(modify_invalid_par), throws_error())
})

test_that("The get_scenario_names function works", {
  scenario_names <- get_scenario_names()
  test_scenario_names <- c('tc_Simple_1_2', 'tc_Simple_1_0', 'tc_AccuTams_1_2')
  expect_that(all(test_scenario_names %in% scenario_names), is_true())
  for (scenario_name in scenario_names){
    expect_that(get_scenario(scenario_name), is_a('list'))
  }
})
