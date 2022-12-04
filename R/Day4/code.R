library(data.table)
library(dplyr)
library(stringr)

setwd("R/Day4")

input = read.table(file = "input",sep = ",",stringsAsFactors = F,header =  F)

colnames(input)=c("elf1","elf2")

compare_lengths = function(elf1,elf2){


    e1 = strsplit(elf1,split = "-") [[1]] %>% as.numeric()
    e2 = strsplit(elf2,split = "-") [[1]] %>% as.numeric()


    e1x=c(e1[1]:e1[2])
    e2x=c(e2[1]:e2[2])

    if(length(intersect(e1x,e2x)) == length(e1x) | length(intersect(e1x,e2x)) ==length(e2x)){out=1}else{out=0}

    return(out)
}


compare_lengths(elf1 = input$elf1[1],elf2 = input$elf2[1])

input$overlap=0

for(i in 1:dim(input)[1]){

  input$overlap[i] = compare_lengths(elf1 = input$elf1[i],elf2 = input$elf2[i])
}

sum(input$overlap)

#################


compare_lengths2 = function(elf1,elf2){


  e1 = strsplit(elf1,split = "-") [[1]] %>% as.numeric()
  e2 = strsplit(elf2,split = "-") [[1]] %>% as.numeric()


  e1x=c(e1[1]:e1[2])
  e2x=c(e2[1]:e2[2])

  if(length(intersect(e1x,e2x)) > 0 ){out=1}else{out=0}

  return(out)
}


#compare_lengths(elf1 = input$elf1[1],elf2 = input$elf2[1])

input$overlap2=0

for(i in 1:dim(input)[1]){

  input$overlap2[i] = compare_lengths2(elf1 = input$elf1[i],elf2 = input$elf2[i])
}

sum(input$overlap2)

