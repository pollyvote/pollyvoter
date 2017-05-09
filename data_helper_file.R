# this is a helper file to make example data available

library("readxl")

######################### polls ##############################

##### Individual polls
polls_individual <- readxl::read_excel(system.file("extdata/German_PollyVote_2013.xlsx", 
                                                package = "pollyvoter"), 
                                    sheet = 7, skip = 1)

colnames(polls_individual)[1] <- "id"
colnames(polls_individual)[3] <- "source"
colnames(polls_individual) <- convert_names(colnames(polls_individual))

# sort out empty rows and only the needed columns
polls_individual <- polls_individual[!is.na(polls_individual$id), 1:11]

# coerce to numeric, get NAs
polls_individual[, 4:11] <- apply(polls_individual[, 4:11], 2, as.numeric)
polls_individual <- tidyr::gather(polls_individual, "party", "percent", 
                                 one_of("cdu/csu", "spd", "grune", "fdp", 
                                        "linke", "piraten", "afd", "sonstige")) 

###### Wahlumfrage
polls_wahlumfrage <- readxl::read_excel(system.file("extdata/German_PollyVote_2013.xlsx", 
                                                   package = "pollyvoter"), 
                                       sheet = 6, skip = 1)
colnames(polls_wahlumfrage)[1] <- "date"

# sort out empty rows and only the needed columns
polls_wahlumfrage <- polls_wahlumfrage[!is.na(polls_wahlumfrage$date), 1:9]
colnames(polls_wahlumfrage) <- convert_names(colnames(polls_wahlumfrage))

# coerce to numeric, get NAs
polls_wahlumfrage[, 4:9] <- apply(polls_wahlumfrage[, 4:9], 2, as.numeric)
polls_wahlumfrage <- tidyr::gather(polls_wahlumfrage, "party", "percent", 
                                  one_of("cdu/csu", "spd", "grune", "fdp", 
                                        "linke", "piraten", "afd", "sonstige")) 

###### PollyTix
polls_tix <- readxl::read_excel(system.file("extdata/German_PollyVote_2013.xlsx", 
                                                   package = "pollyvoter"), 
                                sheet = 5, skip = 1)
colnames(polls_tix)[1] <- "date"

# sort out empty rows and only the needed columns
polls_tix <- polls_tix[!is.na(polls_tix$date), 1:9]
colnames(polls_tix) <- convert_names(colnames(polls_tix))

# coerce to numeric, get NAs
polls_tix[, 4:9] <- apply(polls_tix[, 4:9], 2, as.numeric)
polls_tix <- tidyr::gather(polls_tix, "party", "percent", 
                          one_of("cdu/csu", "spd", "grune", "fdp", 
                                 "linke", "piraten", "afd", "sonstige")) 



##################### markets ##########################

###### Eix
markets_eix <- readxl::read_excel(system.file("extdata/German_PollyVote_2013.xlsx", 
                                            package = "pollyvoter"), 
                                sheet = 9, skip = 1)
colnames(markets_eix)[1] <- "date"

# sort out empty rows and only the needed columns
markets_eix <- markets_eix[!is.na(markets_eix$date), 1:9]
colnames(markets_eix) <- convert_names(colnames(markets_eix))

# coerce to numeric, get NAs
markets_eix[, 4:9] <- apply(markets_eix[, 4:9], 2, as.numeric)
markets_eix <- tidyr::gather(markets_eix, "party", "percent", 
                           one_of("cdu/csu", "spd", "grune", "fdp", 
                                  "linke", "piraten", "afd", "sonstige")) 


###### Prognosys
markets_prognosys <- readxl::read_excel(system.file("extdata/German_PollyVote_2013.xlsx", 
                                                  package = "pollyvoter"), 
                                      sheet = 10, skip = 1)
colnames(markets_prognosys)[1] <- "date"

# sort out empty rows and only the needed columns
markets_prognosys <- markets_prognosys[!is.na(markets_prognosys$date), 1:9]
colnames(markets_prognosys) <- convert_names(colnames(markets_prognosys))

# coerce to numeric, get NAs
markets_prognosys[, 4:9] <- apply(markets_prognosys[, 4:9], 2, as.numeric)
markets_prognosys <- tidyr::gather(markets_prognosys, "party", "percent", 
                                 one_of("cdu/csu", "spd", "grune", "fdp", 
                                        "linke", "piraten", "afd", "sonstige")) 

###### Wahlfieber 1
markets_wahlfieber_1 <- readxl::read_excel(system.file("extdata/German_PollyVote_2013.xlsx", 
                                                  package = "pollyvoter"), 
                                      sheet = 11, skip = 1)
colnames(markets_wahlfieber_1)[1] <- "date"

# sort out empty rows and only the needed columns
markets_wahlfieber_1 <- markets_wahlfieber_1[!is.na(markets_wahlfieber_1$date), 1:9]
colnames(markets_wahlfieber_1) <- convert_names(colnames(markets_wahlfieber_1))

# coerce to numeric, get NAs
markets_wahlfieber_1[, 4:9] <- apply(markets_wahlfieber_1[, 4:9], 2, as.numeric)
markets_wahlfieber_1 <- tidyr::gather(markets_wahlfieber_1, "party", "percent", 
                                 one_of("cdu/csu", "spd", "grune", "fdp", 
                                        "linke", "piraten", "afd", "sonstige")) 



##################### election result ##########################

election_result <- readxl::read_excel(system.file("extdata/German_PollyVote_2013.xlsx", 
                                                       package = "pollyvoter"), 
                                      sheet = 24, skip = 0)
parties <- convert_names(names(election_result)[2:9])
election_result <- election_result[1, 2:9]
election_result <- data.frame(party = parties, percent = as.numeric(election_result))


##################### save as package data ##########################

devtools::use_data(polls_individual, polls_wahlumfrage, polls_tix, markets_eix, markets_prognosys, 
                   markets_wahlfieber_1, election_result)

