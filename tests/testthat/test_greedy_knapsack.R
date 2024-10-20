suppressWarnings(RNGversion(min(as.character(getRversion()),"3.5.3")))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <- data.frame(
  w=sample(1:4000, size = n, replace = TRUE),
  v=runif(n = n, 0, 10000)
)

test_that("Correct object is returned", {
  expect_silent(gk <- greedy_knapsack(x = knapsack_objects[1:8,], W = 3500))
  expect_named(gk, c("value", "elements"))
})

test_that("functions rejects errounous input.", {
  expect_error(greedy_knapsack("hej", 3500))
  expect_error(greedy_knapsack(x = knapsack_objects[1:8,], W = -3500))
})

test_that("Function return correct results.", {
  gk <- greedy_knapsack(x = knapsack_objects[1:8,], W = 3500)
  expect_equal(round(gk$value), 15428)
  expect_true(all(round(gk$elements) %in% c(3, 8)))
  
  gk <- greedy_knapsack(x = knapsack_objects[1:12,], W = 3500)
  expect_equal(round(gk$value), 15428)
  expect_true(all(round(gk$elements) %in% c(3, 8)))
  
  gk <- greedy_knapsack(x = knapsack_objects[1:8,], W = 2000)
  expect_equal(round(gk$value), 15428)
  expect_true(all(round(gk$elements) %in% c(3, 8)))
  
  gk <- greedy_knapsack(x = knapsack_objects[1:12,], W = 2000)
  expect_equal(round(gk$value), 15428)
  expect_true(all(round(gk$elements) %in% c(3, 8)))
  
  st <- system.time(gk <- greedy_knapsack(x = knapsack_objects[1:16,], W = 2000))
  expect_true(as.numeric(st)[2] <= 0.01)
  
  gk <- greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
  expect_equal(round(gk$value), 192647)
  
  gk <- greedy_knapsack(x = knapsack_objects[1:1200,], W = 3500)
  expect_equal(round(gk$value), 270290)
})
# Since greedy algorithm packs items based on ratio of value and weight, it may 
# be possible that there is variablitity owing to sorting itself.
# If the distribution has many smaller sized items, a greedy algorithm would pack 
# it more effeciently.
# The algorithm is still greedy beacuse:  it selects items based on the highest 
# value-to-weight ratio. This is the essence of a greedy algorithm: making the 
# locally optimal choice at each step with the hope of finding a global optimum.
# Optimal Substructure: The problem can be broken down into smaller subproblems, 
# and the solution to the problem can be constructed efficiently from the 
# solutions of the subproblems. In the context of the unbounded knapsack problem, 
# once an item is chosen, the remaining capacity can be treated as a smaller 
# instance of the same problem.

# Run time calculation: Greedy algorithm has 2 main steps: 
#   1. Sorting:  (O(n\log(n)))
#   2. Selection: (O(n))
  