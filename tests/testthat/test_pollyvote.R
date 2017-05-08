context("pollyvote")

test_that("pollyvote objects can be created", {
  # create empty pollyvote container
  pv = create_pollyvote(id = "test_pv")
  assert_class(pv, "pollyvote")
})

test_that("new data can be added to pollyvote objects", {
  # create empty pollyvote container
  pv = create_pollyvote(perm_countries = "D")
  
  # get data
  individual.polls = readxl::read_excel(system.file("extdata/German_PollyVote_2013.xlsx", 
                                                    package = "pollyvoter"), 
                                        sheet = "Wahlumfrage",
                                        skip = 1)
  assert_data_frame(individual.polls, min.cols = 2, min.rows = 2)
  colnames(individual.polls)[1] = "id"
  colnames(individual.polls)[3] = "source"
  colnames(individual.polls) = convert_names(colnames(individual.polls))
  # sort out empty rows and only the needed columns
  individual.polls = individual.polls[!is.na(individual.polls$id), 1:11]
  # coerce to numeric, get NAs
  individual.polls[,4:11] = apply(individual.polls[,4:11], 2, as.numeric)
  # coerce to long format
  ind.polls = wide_to_long_format(individual.polls)
  
  # add data to pollyvote
  expect_error(add_data(pv, newdata = ind.polls, country = "USA"))
  pv = add_data(pv, newdata = ind.polls, country = "D", election = "BTW")
  
  head(get_data(pv))
})