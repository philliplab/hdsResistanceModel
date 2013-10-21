context("de_system_analysis")

test_that("the correct steady states are found", {
  x <- compute_parameters(get_de_defaults(), get_scenario('Simple_1_2'))
  y <- with(x, {environment(findSteadyState) <- environment(); findSteadyState()})
  expected_steady_state <- c(23368421.0526316, 0.00999000999000999, 0.00999000999000999, 
                             8421052631.57895, 1)
  for (i in 1:length(y)){
    expect_that(y[i], equals(expected_steady_state[i]))    
  }
  
  x <- compute_parameters(get_de_defaults(), get_scenario('AccuTams_1_4'))
  y <- with(x, {environment(findSteadyState) <- environment(); findSteadyState()})
  expected_steady_state <- c(350667.165481985, 0.00999000999000999, 0.00999000999000999, 
                             0.00999000999000999, 0.00999000999000999, 9976306272.60257, 1)
  for (i in 1:length(y)){
    expect_that(y[i], equals(expected_steady_state[i]))    
  }
})