context("pollyvote_object")

test_that("the the prediction of a pollyvote object can be plotted", {
  # create empty pollyvote container
  pv = create_pollyvote()
  
  # get data
  data("polls_individual")
  
  # add data to pollyvote
  pv = add_data(pv, newdata = polls_individual, country = "D", region = "national", 
                source.type = "poll", election = "BTW")
  
  # add prediction functions to the pollyvote object
  pv = add_prediction(pv, "poll", function(pv) {
    pv %>% 
      get_data %>% 
      filter(source.type %in% c("poll")) %>%
      group_by(date, source.type, party) %>% 
      summarize(percent = mean(percent, na.rm = TRUE))
  })
  
  assert_class(plot(pv, "poll"), "ggplot")
  
})