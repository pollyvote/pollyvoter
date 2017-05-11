# this file is to prevent the R check from giving a NOTE because of the use of
# non-standard evaulation of the following variables

# the names that are permissible col names in a pollyvote container
utils::globalVariables(c("id", "country", "source", "source.type", "election", "date", 
                "region", "region.type", "party", "percent"), add = TRUE)
