

setwd("/Users/samanthascott/Desktop/20945043/Question3/Data/Tennis")
fnames <- list.files()
csv <- lapply(fnames, read.csv)
result <- do.call(rbind, csv)