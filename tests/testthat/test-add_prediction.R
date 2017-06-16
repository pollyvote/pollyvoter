context("pollyvote_object")

test_that("predictions can be added to pollyvote objects", {
  # create empty pollyvote container
  pv = create_pollyvote(perm_countries = "D")
  
  # get data
  data("polls_individual", "polls_tix", "markets_prognosys")
  
  # add data to pollyvote
  pv = add_data(pv, newdata = polls_individual, country = "D", region = "national", 
                source_type = "poll", election = "BTW")
  
  # add other data to the pv object
  pv = add_data(pv, newdata = polls_tix, 
                source = "polls_tix",
                country = "D", region = "national", 
                source_type = "poll", election = "BTW")
  pv = add_data(pv, newdata = markets_prognosys, 
                source = "markets_prognosys",
                country = "D", region = "national", 
                source_type = "eco.model", election = "BTW")
  
  # add a simple prediction
  pv = add_prediction(pv, "eco.model", function(pv) {
    pv %>% 
      get_data %>% 
      filter(source_type %in% c("eco.model")) %>%
      group_by(date, source_type, party) %>% 
      # TODO NA handling here?
      summarize(percent = mean(percent, na.rm = TRUE))
  })
  # check the prediction
  assert_data_frame(predict(pv, method = "eco.model"))
  
  # add a complex prediction function to the pollyvote object
  pv = add_prediction(pv, "mean_for_source_type", function(pv, source_type = NA,
                                                           na_handle = c("last", "omit", "mean_within", "mean_across"), 
                                                           ...) {
    na_handle = match.arg(na_handle)
    assert_choice(source_type, unique(get_data(pv)[,"source_type"]))
    # avoid NSE, ugly but works
    filter.source_type = source_type
    
    raw.dat = pv %>% 
      get_data  %>%
      filter(source_type %in% filter.source_type)
    
    preprocessed.dat = fill_na(raw.dat, na_handle = na_handle, pv = pv, ...)
    
    final.dat = preprocessed.dat %>%
      group_by(date, source_type, party) %>% 
      summarize(percent = mean(percent, na.rm = TRUE))
    
    return(final.dat)
  })
  
  # check the prediction
  assert_data_frame(predict(pv, method = "mean_for_source_type", source_type = "poll"))
  assert_data_frame(predict(pv, method = "aggr_source_type", which_source_type = "poll"))
  
  
  
  # # add a final prediction function
  # pv = add_prediction(pv, "pollyvote", function(pv) {
  #   # extract predicted data
  #   pred_data = plyr::rbind.fill(predict(pv, "eco.model"), predict(pv, "poll"))
  #   # aggregate it with a weighted mean (random example)
  #   dat_final = pred_data %>%
  #     mutate(weight = ifelse(source_type == "expert", 1, 2)) %>%
  #     group_by(date, party) %>% 
  #     summarize(percent = weighted.mean(percent, weights = weight))
  #   dat_final
  # })
  # 
  # assert_data_frame(predict(pv, "pollyvote"))
  
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
                            agg_fun = "median", na_handle = "mean_within")
  assert_data_frame(predict(pv, "aggr_poll"))
})
