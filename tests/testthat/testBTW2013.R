context("test example data")

test_that("excel files can be read", {
  library("readxl")
  # warning, this function seems to not work properly,
  # it reads the sheet after the one that was specified, amybe because of the Figure?
  individual.polls = read_excel(system.file("extdata/German_PollyVote_2013.xlsx", 
                                            package = "pollyvoter"), 
                                sheet = "Wahlumfrage",
                                skip = 1)
  assert_data_frame(individual.polls, min.cols = 2, min.rows = 2)
  
  colnames(individual.polls)[1] = "id"
  colnames(individual.polls)[3] = "survey.institute"
  colnames(individual.polls) = tolower(colnames(individual.polls))
  
  # sort out empty rows and only the needed columns
  individual.polls = individual.polls[!is.na(individual.polls$id), 1:11]
  
  # coerce to numeric, get NAs
  individual.polls[,4:11] = apply(individual.polls[,4:11], 2, as.numeric)
  
  ind.polls = tidyr::gather(individual.polls, party, percent, one_of("cdu/csu", "spd", "grüne", 
                                                  "fdp", "linke", "piraten", "afd", "sonstige"))
  
})
