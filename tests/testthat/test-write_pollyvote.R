context("pollyvote_object")

test_that("predictions and error calculations from pollyvote objects can be saved", {
  # create empty pollyvote container
  pv = create_pollyvote(perm_countries = "D")

  # get data
  data("polls_individual")

  # add data to pollyvote
  pv = add_data(pv, newdata = polls_individual, country = "D", election = "BTW")

  # # test write.pollyvote
  # write.pollyvote(pv, "delete.csv", "write.csv2", prediction = "pollyvote",
  #                 write_args = list(row.names = FALSE), agg_fun = "median")
  # assert_file_exists("delete.csv")
  # file.remove("delete.csv")
})
