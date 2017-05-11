
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


test_that("countries can be extracted", {
  
  # create pollyvote container
  countries_test <- c("D")
  pv <- create_pollyvote(perm_countries = countries_test)
  
  # extract countries
  countries <- get_countries(pv)
  
  expect_that(countries_test, equals(countries))
  
})


test_that("sources can be extracted", {
  
  # create pollyvote container
  sources_test <- c("experts", "survey")
  pv <- create_pollyvote(perm_sources = sources_test)
  
  # extract sources
  sources <- get_sources(pv)
  
  expect_that(sources_test, equals(sources))
  
})


test_that("earliest date can be extracted", {
  
  # create pollyvote container
  date_earliest_test <- as.POSIXct("2015-11-23", format = "%Y-%m-%d")
  pv <- create_pollyvote(perm_date_earliest = date_earliest_test)
  
  # extract earliest date
  date_earliest <- get_date_earliest(pv)
  
  expect_that(date_earliest_test, equals(date_earliest))
  
})


test_that("latest date can be extracted", {
  
  # create pollyvote container
  date_latest_test <- as.POSIXct("2016-01-29", format = "%Y-%m-%d")
  pv <- create_pollyvote(perm_date_latest = date_latest_test)
  
  # extract latest date
  date_latest <- get_date_latest(pv)
  
  expect_that(date_latest_test, equals(date_latest))
  
})


test_that("colnames can be extracted", {
  
  # create pollyvote container
  pv <- create_pollyvote()
  
  # extract latest date
 expect_character(get_colnames(pv), any.missing = FALSE)
  
})


# test_that("parties can be set up", {
#   
#   # create pollyvote container
#   pv = create_pollyvote()
#   
#   # set parties
#   parties <- c("csu", "linke", "spd", "grune")
#   pv <- set_parties(parties, pv)
#   
#   # extract parties
#   parties_test <- get_parties(pv)
#   
#   expect_that(parties, equals(parties_test))
#   
# })
# 
# 
# test_that("regions can be set up", {
#   
#   # create pollyvote container
#   pv = create_pollyvote()
#   
#   # set regions
#   regions <- c("bayern", "nrw", "bawu", "hh")
#   pv <- set_regions(regions, pv)
#   
#   # extract regions
#   regions_test <- get_regions(pv)
#   
#   expect_that(regions, equals(regions_test))
#   
# })


