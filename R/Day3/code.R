library(data.table)
library(dplyr)
library(stringr)

setwd("R/Day3")

input = read.table(file = "input",sep = " ",stringsAsFactors = F,header =  F)

i=1

for (i in 1:dim(input)[1]){

  rucksack = input[i,1] %>% str_split(pattern = "",simplify = T)

  rucksack1 = rucksack[1:(length(rucksack)/2)]
  rucksack2 = rucksack[((length(rucksack)/2)+1):length(rucksack)]

  input$priority[i]=intersect(rucksack1,rucksack2)

}

key = data.frame(let = c(letters,LETTERS),
                 val=c(1:52))

input$val = 0

for (i in 1:dim(input)[1]){
  input$val[i] = match(input$priority[i],key$let)

}

sum(input$val )

###############################


input = read.table(file = "input",sep = " ",stringsAsFactors = F,header =  F)

input2 = input$V1 %>% matrix(ncol = 3,nrow = 100,byrow = T) %>% as.data.frame()


for (i in 1:dim(input2)[1]){

  rucksack1 = input2[i,1] %>% str_split(pattern = "",simplify = T)
  rucksack2 = input2[i,2] %>% str_split(pattern = "",simplify = T)
  rucksack3 = input2[i,3] %>% str_split(pattern = "",simplify = T)

  int1=intersect(rucksack1,rucksack2)
  int2 = intersect(int1,rucksack3)
  input2$priority[i]=int2

}

input2$val = 0

for (i in 1:dim(input2)[1]){
  input2$val[i] = match(input2$priority[i],key$let)

}

sum(input2$val )
