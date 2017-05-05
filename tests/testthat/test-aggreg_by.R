
context("test data aggregation")

test_that("data can be aggregated", {
  
  # read and convert data
  elect_data_wide <- import_exp_data_wide("Wahlumfrage")
  elect_data_long <- wide_to_long_format(elect_data_wide)
  
  # run function
  elect_data_aggr_mean <- aggreg_by(elect_data_long, group_variab = c("date", "party"), aggr_func = mean)
  elect_data_aggr_max <- aggreg_by(elect_data_long, group_variab = c("date", "party"), aggr_func = max)
  
})