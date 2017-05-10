context("pollyvote_object")

test_that("new data can be added to and retrieved from a pollyvote objects", {
  # create empty pollyvote container
  pv = create_pollyvote(perm_countries = "D")
  
  # get data
  data("polls_individual")
  
  # add data to pollyvote
  expect_error(add_data(pv, newdata = polls_individual, country = "USA"))
  pv = add_data(pv, newdata = polls_individual, country = "D", election = "BTW")
  
  # test get_data()
  assert_data_frame(get_data(pv), min.cols = 2, min.rows = 2)
})