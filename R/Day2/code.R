library(data.table)
library(dplyr)
setwd("Day2")

input = read.table(file = "input",sep = " ",stringsAsFactors = F,header =  F)
colnames(input) = c("Them","Me")

input$score1 = 0
input$score1[input$Me == "X"] = 1
input$score1[input$Me == "Y"] = 2
input$score1[input$Me == "Z"] = 3

score.key = data.frame(Z = c("X","Y","Z"),
                       tie = c("A","B","C"),
                       win = c("C","A","B"),
                       lose = c("B","C","A"))

points = c(3,6,0)

input$score2 = NA

for(i in 1:dim(input)[1]){
  input$score2[i] =  points[match(input$Them[i],t(score.key[match(input$Me[i],score.key$Z),-1]))]
}

sum(input[,c(3,4)])


#########################################################


input = read.table(file = "input",sep = " ",stringsAsFactors = F,header =  F)
colnames(input) = c("Them","Outcome")

outcome.key= data.frame(Them = c("A","B","C"),
                             Y = c("X","Y","Z"), #tie
                             Z = c("Y","Z","X"), #win
                             X = c("Z","X","Y")) #lose
input$Me = NA

for(i in 1:dim(input)[1]){
  input$Me[i] =  outcome.key[match(input$Them[i],outcome.key$Them),match(input$Outcome[i],colnames(outcome.key)) ]
}


input$score1 = 0
input$score1[input$Me == "X"] = 1
input$score1[input$Me == "Y"] = 2
input$score1[input$Me == "Z"] = 3

score.key = data.frame(Z = c("X","Y","Z"),
                       tie = c("A","B","C"),
                       win = c("C","A","B"),
                       lose = c("B","C","A"))

points = c(3,6,0)

input$score2 = NA

for(i in 1:dim(input)[1]){
  input$score2[i] =  points[match(input$Them[i],t(score.key[match(input$Me[i],score.key$Z),-1]))]
}

sum(input[,c(4,5)])


