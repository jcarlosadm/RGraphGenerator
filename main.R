# main file

FILENAME = "out.csv"

dir.create("output", showWarnings = FALSE)

repoAnalysis = read.csv(FILENAME, header = FALSE)

source("repoAnalysis.R", echo = TRUE)
