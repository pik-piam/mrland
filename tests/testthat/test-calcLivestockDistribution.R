test_that("arguments are validated before any data is read", {
  expect_error(calcLivestockDistribution(output = "bogus"), "output must be one of")
  expect_error(calcLivestockDistribution(landProxy = "bogus"), "landProxy must be one of")
  expect_error(calcLivestockDistribution(category = "bogus"), "category must be either")
})

test_that("the nearest land-use year is picked", {
  landYears <- c(1995, 2000, 2005, 2010)
  expect_equal(nearestLandYear(2007, landYears), "y2005")
  expect_equal(nearestLandYear(2009, landYears), "y2010")
  expect_equal(nearestLandYear(2005, landYears), "y2005")
  expect_equal(nearestLandYear(1800, landYears), "y1995")
  expect_equal(nearestLandYear(2100, landYears), "y2010")
})

test_that("each species gets its own land proxy", {
  land <- new.magpie(c("AAA", "BBB"), c("y2000", "y2010"), c("past", "range"), fill = 0)
  land[, , "past"]  <- 1
  land[, , "range"] <- 10

  expect_equal(as.vector(getLandProxy("Ct", "y2000", land, "speciesSpecific")), c(1, 1))
  expect_equal(as.vector(getLandProxy("Bf", "y2000", land, "speciesSpecific")), c(1, 1))
  expect_equal(as.vector(getLandProxy("Sh", "y2000", land, "speciesSpecific")), c(10, 10))
  expect_equal(as.vector(getLandProxy("Gt", "y2000", land, "speciesSpecific")), c(10, 10))
  expect_equal(as.vector(getLandProxy("Ho", "y2000", land, "speciesSpecific")), c(10, 10))
})

test_that("pastRange sums both grassland classes for every species", {
  land <- new.magpie(c("AAA", "BBB"), "y2000", c("past", "range"), fill = 0)
  land[, , "past"]  <- 1
  land[, , "range"] <- 10
  expect_equal(as.vector(getLandProxy("Ct", "y2000", land, "pastRange")), c(11, 11))
  expect_equal(as.vector(getLandProxy("Ch", "y2000", land, "pastRange")), c(11, 11))
})

test_that("a species with no proxy under speciesSpecific is an error, not a silent 0", {
  land <- new.magpie("AAA", "y2000", c("past", "range"), fill = 1)
  expect_error(getLandProxy("Ch", "y2000", land, "speciesSpecific"), "No land proxy defined")
})
