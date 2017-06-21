context("pollyvote_object")

test_that("the prediction of a pollyvote object can be plotted", {
  # create empty pollyvote container
  pv = create_pollyvote()
  
  # get data
  data("polls_individual")
  
  # add data to pollyvote
  pv = add_data(pv, newdata = polls_individual, country = "D", region = "national", 
                source_type = "poll", election = "BTW")
  
  # add prediction functions to the pollyvote object
  pv = add_prediction(pv, "poll", function(pv) {
    pv %>% 
      get_data %>% 
      filter(source_type %in% c("poll")) %>%
      group_by(date, source_type, party) %>% 
      summarize(percent = mean(percent, na.rm = TRUE))
  })
  assert_class(plot(pv, prediction_method = "poll"), "ggplot")
  
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
      rename(percent = percent.x, percent.true = percent.y)
    return(mutate(joined, error = abs(percent - percent.true)))
  })
  assert_class(plot(pv, error_calc_method = "poll_only"), "ggplot")
  
  # confidence interval is just a special case of error calculation
  # add an error calculation function with a ci flag and alpha value
  pv = add_error_calc(pv, "poll_only_ci", function(pv, ci = FALSE, alpha = 0.05) {
    error_dat = error_calc(pv, "poll_only")
    if(!ci) {
      return(error_dat)
    } else {
      ec_mean_error = error_dat %>% 
        group_by(party) %>%
        summarize(mean_error = mean(error))
      ec_ci = left_join(error_dat, ec_mean_error, by = "party") %>%
        mutate(ci_lower = percent - qnorm(1-alpha/2) * mean_error,
               ci_upper = percent + qnorm(1-alpha/2) * mean_error)
      return(ec_ci)
    }
  })
  assert_class(plot(pv, error_calc_method = "poll_only_ci"), "ggplot")
  # draw a CI
  p = plot(pv, error_calc = "poll_only_ci", ci = TRUE)
  p + geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), 
                  linetype = "dashed",
                  fill = "grey70", alpha = 0.1)
})