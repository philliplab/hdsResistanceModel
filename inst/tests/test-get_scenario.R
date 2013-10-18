context("get_scenario")

test_that("the get_scenario function works", {
  expect_that(get_scenario("This scenario definitely does not exist"), throws_error("Scenario does not exist"))
  expect_that(get_scenario("Simple_1_2"), is_a("list"))
})