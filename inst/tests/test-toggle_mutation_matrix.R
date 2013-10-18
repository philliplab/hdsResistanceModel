context("toggle_mutation_matrix")

test_that("The toggle_mutation_mstrix function checks its input correctly",{
  E <- matrix(1:4, nrow = 2)
  strains <- 2
  expect_that(toggle_mutation_matrix(E, strains, "continous", 2), throws_error("Invalid value for type"))
  expect_that(toggle_mutation_matrix(E, strains, "continuous", 3), 
              throws_error("Dimensions implied by E, strains and N_S does not match"))
  expect_that(toggle_mutation_matrix(E, c(2,3), "continuous", 2), 
              throws_error("Dimensions implied by E, strains and N_S does not match"))
  expect_that(toggle_mutation_matrix(list(1), strains, "continuous", 2), throws_error("E is not a matrix"))
})

test_that("The mutation matric is altered as expected by the toggle_mutation_matrix function",{
  E <- matrix(1:4, nrow = 2)
  strains <- 2
  expect_that(toggle_mutation_matrix(E, strains, "continuous", 2), is_a("matrix"))
  expect_that(toggle_mutation_matrix(E, strains, "discrete", 2), is_a("matrix"))
  
  expected_value <- structure(c(1, 0, 0, 0), .Dim = c(2L, 2L))
  expect_that(toggle_mutation_matrix(E, strains, "continuous", 2), equals(expected_value))
  
  expected_value <- structure(c(0, 2, 0, 0), .Dim = c(2L, 2L))
  expect_that(toggle_mutation_matrix(E, strains, "discrete", 2), equals(expected_value))
})