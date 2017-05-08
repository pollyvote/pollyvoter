# this is a helper file to make example data available

library("readxl")
# warning, this function seems to not work properly,
poll_data_wide = readxl::read_excel(system.file("extdata/German_PollyVote_2013.xlsx", 
                                                  package = "pollyvoter"), 
                                      sheet = 7,
                                      skip = 1)

colnames(poll_data_wide)[1] = "id"
colnames(poll_data_wide)[3] = "source"
colnames(poll_data_wide) = convert_names(colnames(poll_data_wide))

# sort out empty rows and only the needed columns
poll_data_wide = poll_data_wide[!is.na(poll_data_wide$id), 1:11]

# coerce to numeric, get NAs
poll_data_wide[, 4:11] = apply(poll_data_wide[, 4:11], 2, as.numeric) %>% suppressWarnings

poll_data_long = tidyr::gather(poll_data_wide, "party", "percent", 
                               dplyr::one_of("cdu/csu", "spd", "grune", "fdp", 
                                             "linke", "piraten", "afd", "sonstige")) 

devtools::use_data(poll_data_wide, poll_data_long)


# Other data type
poll_data_wide = readxl::read_excel(system.file("extdata/German_PollyVote_2013.xlsx", 
                                                package = "pollyvoter"), 
                                    sheet = 7,
                                    skip = 1)
