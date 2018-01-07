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
          group_by(date) %>% 
          summarize(percent = sum(percent), percent.true = sum(percent.true), 
                    error = abs(sum(percent) - sum(percent.true))) %>%
          mutate(party = paste(coalitions[[i]], collapse = "-"))
      } else {
        raw_error_calcs[[i]] = raw_error_calc %>%
          filter(party %in% coalitions[[i]]) %>%
          group_by(date) %>% 
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


test_that("valid coalitons 1", {
  
  pv = create_pollyvote(perm_countries = "D")
  coalitions = list(c("spd", "fdp"), c("grune", "fdp"))
  permitted_parties = c("spd", "fdp", "grune")
  
  val_coalitions = valid_coalitions(coalitions, pv, permitted_parties)
  
  expect_that(coalitions, equals(val_coalitions))
  
})


test_that("valid coalitons 2", {
  
  pv = create_pollyvote(perm_countries = "D")
  coalitions = list(c("spd", "fdp"), c("grune", "fdp"), c("def", "abc"),
                    c("wer", "fdp"))
  permitted_parties = c("spd", "fdp", "grune")
  perm_coalitions = list(c("spd", "fdp"), c("grune", "fdp"))
  
  val_coalitions = suppressWarnings(valid_coalitions(coalitions, pv, 
                                                   permitted_parties))
  
  expect_that(perm_coalitions, equals(val_coalitions))
  
})


# test_that("coalition prediction omit", {
#   
#   data("polls_individual")
#   data("election_result")
#   one_day = as.POSIXct("2013-4-24", tz = "UTC")
#   data_elect = polls_individual[polls_individual$date == one_day, ]
#   parties = c("grune", "spd")
#   data_elect = data_elect[data_elect$party %in% parties, ]
#   
#   pv = create_pollyvote(perm_countries = "D")
#   pv = add_data(pv, newdata = data_elect, country = "D", region = "national", 
#                 source_type = "poll", election = "BTW")
#   pv = add_election_result(pv, "BTW 2013", election_result, date = "2013-09-22")
#   coalitions = list(c("spd", "grune"), c("grune", "fdp"))
#   
#   pred_coalitions = calc_coalitions(pv, coalitions = coalitions,
#                                     threshold_handle = "omit")
#   
#   expec_results = data.frame(date = one_day, grune_fdp = NA, 
#                              spd_grune = 37)
#   expect_that(expec_results, equals(pred_coalitions))
#   
# })


# test_that("coalition prediction ignore", {
#   
#   data("polls_individual")
#   data("election_result")
#   one_day = as.POSIXct("2013-4-24", tz = "UTC")
#   data_elect = polls_individual[polls_individual$date == one_day, ]
#   parties = c("grune", "spd")
#   data_elect = data_elect[data_elect$party %in% parties, ]
#   
#   pv = create_pollyvote(perm_countries = "D")
#   pv = add_data(pv, newdata = data_elect, country = "D", region = "national", 
#                 source_type = "poll", election = "BTW")
#   pv = add_election_result(pv, "BTW 2013", election_result, date = "2013-09-22")
#   coalitions = list(c("spd", "grune"), c("grune", "fdp"))
#   
#   pred_coalitions = calc_coalitions(pv, coalitions = coalitions,
#                                     threshold_handle = "ignore")
#   
#   expec_results = data.frame(date = one_day, grune_fdp = 14, 
#                              spd_grune = 37)
#   expect_that(expec_results, equals(pred_coalitions))
#   
# })
