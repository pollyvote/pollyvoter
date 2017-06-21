context("pollyvote_object")

test_that("the initial coalition function of a pollyvote object work", {
  # create empty pollyvote container
  pv = create_pollyvote(perm_countries = "D")
  
  # add data to pollyvote
  data("polls_individual")
  pv = add_data(pv, newdata = polls_individual, country = "D", region = "national", 
                source_type = "poll", election = "BTW")
  
  # add an election result
  data("election_result")
  pv = add_election_result(pv, "BTW", election_result, date = "2013-09-22")
  
  pv = add_prediction(pv, method = "coalition_prediction", fun = function(pv, prediction, coalitions, ...) {
    raw_prediction = predict(pv, prediction, ...)
    raw_predictions = list()
    for (i in 1:length(coalitions)) {
      raw_predictions[[i]] = raw_prediction %>%
        filter(party %in% coalitions[[i]]) %>%
        group_by(date) %>%
        summarize(percent = sum(percent)) %>%
        mutate(party = paste(coalitions[[i]], collapse = "-"))
    }
    
    coalition_prediction = bind_rows(raw_predictions)
    return(coalition_prediction)
  })
  
  assert_data_frame(predict(pv, "coalition_prediction", prediction = "pollyvote", 
                            coalitions = list(c("afd", "fdp"), "cdu/csu", c("cdu/csu", "grune"))))
  
  pv = add_error_calc(pv, method = "coalition_error_calc", fun = function(pv, error_calc_method, coalitions, ci = FALSE, alpha = 0.05, ...) {
    raw_error_calc = error_calc(pv, error_calc_method, ci = ci, alpha = alpha, ...)
    raw_error_calcs = list()
    for (i in 1:length(coalitions)) {
      if(!ci) {
        raw_error_calcs[[i]] = raw_error_calc %>%
          filter(party %in% coalitions[[i]]) %>%
          group_by(date.x) %>% # TODO replace with date, see issue #6
          summarize(percent = sum(percent), percent.true = sum(percent.true), 
                    error = abs(sum(percent) - sum(percent.true))) %>%
          mutate(party = paste(coalitions[[i]], collapse = "-"))
      } else {
        raw_error_calcs[[i]] = raw_error_calc %>%
          filter(party %in% coalitions[[i]]) %>%
          group_by(date.x) %>% # TODO replace with date, see issue #6
          summarize(percent = sum(percent), percent.true = sum(percent.true), 
                    error = abs(sum(percent) - sum(percent.true)),
                    ci_lower = sum(ci_lower),
                    ci_upper = sum(ci_upper)) %>%
          mutate(party = paste(coalitions[[i]], collapse = "-"))
      }
    }
    
    coalition_error_calc = bind_rows(raw_error_calcs)
    return(coalition_error_calc)
  })
  
  assert_data_frame(
    error_calc(pv, method = "coalition_error_calc",
               # arguments to coalition_error_calc
               error_calc_method = "prediction_election",
               coalitions = list(c("afd", "fdp"), "cdu/csu", c("cdu/csu", "grune")),
               # arguments to prediction_election
               prediction = "pollyvote", election = "BTW")
  )
  
  assert_data_frame(
    error_calc(pv, method = "coalition_error_calc",
               # arguments to coalition_error_calc
               error_calc_method = "prediction_election",
               coalitions = list(c("afd", "fdp"), "cdu/csu", c("cdu/csu", "grune")),
               ci = TRUE, alpha = 0.05,
               # arguments to prediction_election
               prediction = "pollyvote", election = "BTW")
  )
  
  
})