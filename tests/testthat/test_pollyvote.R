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


test_that("predictions can be added to pollyvote objects", {
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
  
  # add prediction functions to the pollyvote object
  pv = add_prediction(pv, "expert", function(pv) {
    pv %>% 
      get_data %>% 
      filter(source.type %in% c("expert")) %>%
      group_by(date, source.type, party) %>% 
      summarize(percent = mean(percent, na.rm = TRUE))
  })
  
  pv = add_prediction(pv, "poll", function(pv) {
    pv %>% 
      get_data %>% 
      filter(source.type %in% c("poll")) %>%
      group_by(date, source.type, party) %>% 
      summarize(percent = mean(percent, na.rm = TRUE))
  })
  
  # check the predictions
  assert_data_frame(predict(pv, name = "expert"))
  assert_data_frame(predict(pv, name = "poll"))
  
  
  
  # add a final prediction function
  pv = add_prediction(pv, "pollyvote", function(pv) {
    # extract predicted data
    pred_data = plyr::rbind.fill(predict(pv, "expert"), predict(pv, "poll"))
    # aggregate it with a weighted mean (random example)
    dat_final = pred_data %>%
      mutate(weight = ifelse(source.type == "expert", 1, 2)) %>%
      group_by(date, party) %>% 
      summarize(percent = weighted.mean(percent, weights = weight))
    dat_final
  })
  
  assert_data_frame(predict(pv, "pollyvote"))
  
  # pv = add_prediction(pv, "mega_pollyvote", function(pv) {
  #   # extract predicted data
  #   pred_data = plyr::rbind.fill(predict(pv, "pollyvote"), predict(pv, "poll"))
  #   # aggregate it with a weighted mean (random example)
  #   dat_final = pred_data %>%
  #     group_by(date, party) %>% 
  #     summarize(percent = mean(percent))
  #   dat_final
  # })
  # 
  # predict(pv, "mega_pollyvote")
  
  
})

test_that("predictions can be added to pollyvote objects", {
  # create empty pollyvote container
  pv = create_pollyvote(perm_countries = "D")
  
  # get data
  data("poll_data_long")
  
  # add data to pollyvote
  pv = add_data(pv, newdata = poll_data_long, country = "D", region = "national", 
                source.type = "poll", election = "BTW")
  
  # add prediction functions to the pollyvote object
  pv = add_prediction(pv, "poll", function(pv) {
    pv %>% 
      get_data %>% 
      filter(source.type %in% c("poll")) %>%
      group_by(date, source.type, party) %>% 
      summarize(percent = mean(percent, na.rm = TRUE))
  })
  # check the predictions
  assert_data_frame(predict(pv, name = "poll"))
  
  # add an election result
  data("election_result")
  pv = add_election_result(pv, "BTW", election_result)
  # check the get function
  assert_data_frame(get_election_result(pv, "BTW"))
})

test_that("the error of pollyvote object can be calculated", {
  # create empty pollyvote container
  pv = create_pollyvote(perm_countries = "D")
  
  # get data
  data("poll_data_long")
  
  # add data to pollyvote
  pv = add_data(pv, newdata = poll_data_long, country = "D", region = "national", 
                source.type = "poll", election = "BTW")
  
  # add prediction functions to the pollyvote object
  pv = add_prediction(pv, "poll", function(pv) {
    pv %>% 
      get_data %>% 
      filter(source.type %in% c("poll")) %>%
      group_by(date, source.type, party) %>% 
      summarize(percent = mean(percent, na.rm = TRUE))
  })
  
  # add an election result
  data("election_result")
  pv = add_election_result(pv, "BTW", election_result)
  
  # add an error calculation function
  pv = add_error_calc(pv, "poll_only", function(pv) {
    # extract predicted data
    pred_data = predict(pv, "poll")
    # extract election result
    result = get_election_result(pv, "BTW")
    joined = left_join(x = pred_data, y = result, by = "party") %>%
      rename(percent.est = percent.x, percent.true = percent.y)
    mutate(joined, error = abs(percent.est - percent.true))
  })
  
  assert_data_frame(error_calc(pv, "poll_only"))
  
})