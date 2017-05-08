
context("test get and set functions")

test_that("parties can be extracted", {
  
  # create pollyvote container
  parties_test <- c("csu", "linke", "spd", "grune")
  pv <- create_pollyvote(perm_parties = parties_test)
  
  # extract parties
  parties <- get_parties(pv)
  
  expect_that(parties_test, equals(parties))
  
})


test_that("regions can be extracted", {
  
  # create pollyvote container
  regions_test <- c("bayern", "nrw", "bawu")
  pv <- create_pollyvote(perm_regions = regions_test)
  
  # extract parties
  regions <- get_regions(pv)
  
  expect_that(regions_test, equals(regions))
  
})


test_that("elections can be extracted", {
  
  # create pollyvote container
  elections_test <- c("btw")
  pv <- create_pollyvote(perm_elections = elections_test)
  
  # extract parties
  elections <- get_elections(pv)
  
  expect_that(elections_test, equals(elections))
  
})


test_that("parties can be set up", {
  
  # create pollyvote container
  pv = create_pollyvote()
  
  # set parties
  parties <- c("csu", "linke", "spd", "grune")
  pv <- set_parties(parties, pv)
  
  # extract parties
  parties_test <- get_parties(pv)
  
  expect_that(parties, equals(parties_test))
  
})


test_that("regions can be set up", {
  
  # create pollyvote container
  pv = create_pollyvote()
  
  # set regions
  regions <- c("bayern", "nrw", "bawu", "hh")
  pv <- set_regions(regions, pv)
  
  # extract regions
  regions_test <- get_regions(pv)
  
  expect_that(regions, equals(regions_test))
  
})


