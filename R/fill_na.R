fill_na = function(dat, na_handle = c("last", "omit", "mean_within", "mean_across"), 
                   pv = NULL, ... ) {
  na_handle = match.arg(na_handle)
  # na handling
  if (na_handle == "last") {
    preprocessed_dat = dat %>% 
      complete(date,
               nesting(source, source_type, party)) %>%
      group_by(source, party) %>% 
      fill(percent, .direction = "down")
    
  } else if (na_handle == "omit") {
    preprocessed_dat = dat
    
  } else if (na_handle == "mean_within") {
    preprocessed_dat = dat %>%
      complete(date,
               nesting(source, source_type, party)) %>%
      group_by(date, party) %>%
      mutate(percent = ifelse(is.na(percent), mean(percent, na.rm = T), percent))
    
  } else if (na_handle == "mean_across") {
    raw_dat = dat %>%
      complete(date,
               nesting(source, source_type, party))
    
    # create a small fake pollyvote prediction by
    # calculating the mean per source type and then over all aggregated source types
    across.dat = pv %>% 
      get_data %>% 
      group_by(date, source_type, party) %>%
      summarize(percent = mean(percent, na.rm = TRUE)) %>%
      group_by(date, party) %>%
      summarize(percent = mean(percent, na.rm = TRUE))
    
    preprocessed_dat = left_join(raw_dat, across.dat, by = c("date", "party"))
    preprocessed_dat = preprocessed_dat %>% 
      mutate(percent = ifelse(is.na(percent.x), percent.y, percent.x))
  }
  return(preprocessed_dat)
}
