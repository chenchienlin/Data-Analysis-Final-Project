library(stringr)
library(data.table)
foods <- read.csv("~/Test/foods.csv", comment.char="#", stringsAsFactors=FALSE)

foods <- foods[,c(-8,-9)]
A <- str_split_fixed(foods$用料, "\n", 50)
foods <- c(foods, A)
foods <- data.table(foods)

str(foods)
A <- strsplit(foods$用料,"\n")

B <- strsplit ("Hello World"," |o")
C <- B[[1]][1]
