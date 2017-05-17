context("pollyvote_object")

test_that("predictions can be added to pollyvote objects", {
  # create empty pollyvote container
  pv = create_pollyvote(perm_countries = "D")
  
  # get data
  data("polls_individual")
  
  # add data to pollyvote
  pv = add_data(pv, newdata = polls_individual, country = "D", region = "national", 
                source_type = "poll", election = "BTW")
  
  # add other data (actually the same) to the pv object
  pv = add_data(pv, newdata = polls_individual, country = "D", region = "national", 
                source_type = "expert", election = "BTW")
  pv = add_data(pv, newdata = polls_individual, country = "D", region = "national", 
                source_type = "eco.model", election = "BTW")
  
  # add prediction functions to the pollyvote object
  pv = add_prediction(pv, "expert", function(pv) {
    pv %>% 
      get_data %>% 
      filter(source_type %in% c("expert")) %>%
      group_by(date, source_type, party) %>% 
      # TODO NA handling here?
      summarize(percent = mean(percent, na.rm = TRUE))
  })
  
  pv = add_prediction(pv, "poll", function(pv) {
    pv %>% 
      get_data %>% 
      filter(source_type %in% c("poll")) %>%
      group_by(date, source_type, party) %>% 
      summarize(percent = mean(percent, na.rm = TRUE))
  })
  
  # check the predictions
  assert_data_frame(predict(pv, method = "expert"))
  assert_data_frame(predict(pv, method = "poll"))
  
  
  
  # add a final prediction function
  pv = add_prediction(pv, "pollyvote", function(pv) {
    # extract predicted data
    pred_data = plyr::rbind.fill(predict(pv, "expert"), predict(pv, "poll"))
    # aggregate it with a weighted mean (random example)
    dat_final = pred_data %>%
      mutate(weight = ifelse(source_type == "expert", 1, 2)) %>%
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


test_that("aggregations can be added to pollyvote objects", {
  # create empty pollyvote container
  pv = create_pollyvote(perm_countries = "D")
  
  # get data
  data("polls_individual")
  
  # add data to pollyvote
  pv = add_data(pv, newdata = polls_individual, country = "D", region = "national", 
                source_type = "poll", election = "BTW")
  
  # add aggregation
  pv = add_aggr_source_type(pv, method = "aggr_poll", which_source_type = "poll", 
                            agg_fun = "median", na.handle = "na.rm")
  assert_data_frame(predict(pv, "aggr_poll"))
  })