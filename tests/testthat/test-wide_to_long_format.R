
context("test conversion: wide to long format")

test_that("data frame can be convert to long format", {
  
  library("readxl")
  # warning, this function seems to not work properly,
  # it reads the sheet after the one that was specified, amybe because of the Figure?
  individual_polls_wide <- read_excel("data/German_PollyVote_2013.xlsx", 
                                sheet = "Wahlumfrage",
                                skip = 1)
  colnames(individual_polls_wide)[1] <- "id"
  colnames(individual_polls_wide)[3] <- "survey.institute"
  colnames(individual_polls_wide) <- tolower(colnames(individual_polls_wide))
  
  # sort out empty rows and only the needed columns
  individual_polls_wide <- individual_polls_wide[!is.na(individual_polls_wide$id), 1:11]
  
  # coerce to numeric, get NAs
  individual_polls_wide[, 4:11] <- apply(individual_polls_wide[, 4:11], 2, as.numeric)
  
  # convert to long format
  library("tidyr")
  individual_polls_long = gather(individual_polls_wide, party, percent, 
                                 one_of("cdu/csu", "spd", "grüne", 
                                        "fdp", "linke", "piraten", 
                                        "afd", "sonstige"))
  
  # use function wide_to_long_format
  individual_polls_long_test <- wide_to_long_format(individual_polls_wide)
  
  # compare dimensions of output
  expect_equal(dim(individual_polls_long_test), dim(individual_polls_long))

  
})