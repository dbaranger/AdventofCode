library(data.table)
library(dplyr)
library(stringr)

setwd("~/AdventofCode2022/R/Day6")

input = read.table(file = "input",sep = ",",stringsAsFactors = F,header =  F,fill = T)

buffer = str_split(string = input$V1,pattern = "",simplify = T)

window=14 # or 4
tracker = data.frame(char = c(1:length(buffer)),
                     unique_ones = NA)

for(i in window:length(buffer)){

  temp = buffer[(i-window+1):i]

  tracker$unique_ones[i] = length(unique(temp))

}

which(tracker$unique_ones == window)[1]
