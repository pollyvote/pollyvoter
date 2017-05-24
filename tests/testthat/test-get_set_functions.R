
context("test get and set functions")

test_that("parties can be extracted", {
  
  # create pollyvote container
  parties_test <- c("csu", "linke", "spd", "grune")
  pv <- create_pollyvote(perm_parties = parties_test)
  
  # extract parties
  parties <- get_perm_parties(pv)
  
  expect_that(parties_test, equals(parties))
  
})


test_that("regions can be extracted", {
  
  # create pollyvote container
  regions_test <- c("bayern", "nrw", "bawu")
  pv <- create_pollyvote(perm_regions = regions_test)
  
  # extract regions
  regions <- get_perm_regions(pv)
  
  expect_that(regions_test, equals(regions))
  
})


test_that("elections can be extracted", {
  
  # create pollyvote container
  elections_test <- c("btw")
  pv <- create_pollyvote(perm_elections = elections_test)
  
  # extract elections
  elections <- get_perm_elections(pv)
  
  expect_that(elections_test, equals(elections))
  
})


test_that("countries can be extracted", {
  
  # create pollyvote container
  countries_test <- c("D")
  pv <- create_pollyvote(perm_countries = countries_test)
  
  # extract countries
  countries <- get_perm_countries(pv)
  
  expect_that(countries_test, equals(countries))
  
})


test_that("sources can be extracted", {
  
  # create pollyvote container
  sources_test <- c("experts", "survey")
  pv <- create_pollyvote(perm_sources = sources_test)
  
  # extract sources
  sources <- get_perm_sources(pv)
  
  expect_that(sources_test, equals(sources))
  
})


test_that("earliest date can be extracted", {
  
  # create pollyvote container
  date_earliest_test <- as.POSIXct("2015-11-23", format = "%Y-%m-%d")
  pv <- create_pollyvote(perm_date_earliest = date_earliest_test)
  
  # extract earliest date
  date_earliest <- get_perm_date_earliest(pv)
  
  expect_that(date_earliest_test, equals(date_earliest))
  
})


test_that("latest date can be extracted", {
  
  # create pollyvote container
  date_latest_test <- as.POSIXct("2016-01-29", format = "%Y-%m-%d")
  pv <- create_pollyvote(perm_date_latest = date_latest_test)
  
  # extract latest date
  date_latest <- get_perm_date_latest(pv)
  
  expect_that(date_latest_test, equals(date_latest))
  
})


test_that("colnames can be extracted", {
  
  # create pollyvote container
  pv <- create_pollyvote()
  
  # extract latest date
  expect_character(get_perm_colnames(pv), any.missing = FALSE)
})


test_that("source types can be extracted", {
  
  # create pollyvote container
  source_types_test <- "poll"
  pv <- create_pollyvote(perm_source_types = source_types_test)
  
  # extract source types
  source_types <- get_perm_source_types(pv)
  
  expect_that(source_types_test, equals(source_types))
  
})
