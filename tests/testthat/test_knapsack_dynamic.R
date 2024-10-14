suppressWarnings(RNGversion(min(as.character(getRversion()),"3.5.3")))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <- data.frame(
  w=sample(1:4000, size = n, replace = TRUE),
  v=runif(n = n, 0, 10000)
)

test_that("Correct object is returned", {
  expect_silent(gk <- knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500))
  expect_named(gk, c("value", "elements"))
})

test_that("functions rejects errounous input.", {
  expect_error(knapsack_dynamic("hej", 3500))
  expect_error(knapsack_dynamic(x = knapsack_objects[1:8,], W = -3500))
})

test_that("Function return correct results.", {
  gk <- knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)
  expect_equal(round(gk$value), 16770)
  expect_true(all(round(gk$elements) %in% c(5, 8)))
  
  gk <- knapsack_dynamic(x = knapsack_objects[1:100,], W = 3500)
  expect_equal(round(gk$value), 79608)
  expect_true(all(round(gk$elements) %in% c(8,18,25,35,37,43,55,58,71,72,77,80,89,92)))
  
  gk <- knapsack_dynamic(x = knapsack_objects[1:8,], W = 2000)
  expect_equal(round(gk$value), 15428)
  expect_true(all(round(gk$elements) %in% c(3, 8)))
  
  gk <- knapsack_dynamic(x = knapsack_objects[1:100,], W = 2000)
  expect_equal(round(gk$value), 59284)
  expect_true(all(round(gk$elements) %in% c(8,25,35,37,43,55,71,77,80,89,92)))
  
  st <- system.time(gk <- knapsack_dynamic(x = knapsack_objects[1:16,], W = 2000))
  expect_true(as.numeric(st)[2] <= 0.01)
  
  gk <- knapsack_dynamic(x = knapsack_objects[1:800,], W = 3500)
  expect_true(round(gk$value)>=192647)
  
  gk <- knapsack_dynamic(x = knapsack_objects[1:1200,], W = 3500)
  expect_true(round(gk$value)>=276034)
})