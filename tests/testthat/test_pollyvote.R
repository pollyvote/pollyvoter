context("pollyvote")

test_that("pollyvote objects can be created", {
  # create empty pollyvote container
  pv = create_pollyvote(id = "test_pv")
  assert_class(pv, "pollyvote")
})

test_that("new data can be added to and retrieved from a pollyvote objects", {
  # create empty pollyvote container
  pv = create_pollyvote(perm_countries = "D")
  
  # get data
  data("poll_data_long")
 
  # add data to pollyvote
  expect_error(add_data(pv, newdata = poll_data_long, country = "USA"))
  pv = add_data(pv, newdata = poll_data_long, country = "D", election = "BTW")
  
  # test get_data()
  assert_data_frame(get_data(pv), min.cols = 2, min.rows = 2)
})


test_that("the data from a pollyvote object can be aggregated", {
  # create empty pollyvote container
  pv = create_pollyvote(perm_countries = "D")
  
  # get data
  data("poll_data_long")
  
  # add data to pollyvote
  pv = add_data(pv, newdata = poll_data_long, country = "D", region = "national", 
                source.type = "poll", election = "BTW")
  
  # add other data (actually the same) to the pv object
  pv = add_data(pv, newdata = poll_data_long, country = "D", region = "national", 
                source.type = "expert", election = "BTW")
  pv = add_data(pv, newdata = poll_data_long, country = "D", region = "national", 
                source.type = "eco.model", election = "BTW")
  
  # get the daily aggregated prediction
  dat_aggr = pv %>% 
    get_data %>% 
    filter(source.type %in% c("expert", "poll")) %>%
    group_by(date, source.type, party) %>% 
    summarize(aggreg_percent = mean(percent, na.rm = TRUE))
  dat_aggr
  # maybe create an aggregated pv object?
  
  
  # combine the daily predictions with a weighted mean
  dat_final = dat_aggr %>%
    mutate(weight = ifelse(source.type == "expert", 1, 2)) %>%
    group_by(date, party) %>% 
    summarize(aggreg_percent = weighted.mean(aggreg_percent, weights = weight))
  dat_final
  
  
})
  