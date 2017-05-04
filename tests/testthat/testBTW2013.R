context("test example data")

test_that("excel files can be read", {
  library("readxl")
  # warning, this function seems to not work properly,
  # it reads the sheet after the one that was specified, amybe because of the Figure?
  individual.polls = read_excel("data/German_PollyVote_2013.xlsx", 
                                sheet = "Wahlumfrage",
                                skip = 1)
  colnames(individual.polls)[1] = "id"
  
  # sort out empty rows and only the needed columns
  individual.polls = individual.polls[!is.na(individual.polls$id), 1:11]
  individual.polls[,4:11] = apply(individual.polls[,4:11], 2, as.numeric)
  
  library("tidyr")
  
})