library(data.table)
library(dplyr)
library(stringr)
library(data.tree)

setwd("~/AdventofCode2022/R/Day10")
input = read.table(file = "input",sep = "",stringsAsFactors = F,header =  F,fill = T)


register=1
register.history=NA

for(i in 1:dim(input)[1]){

  if(input$V1[i] == "noop"){
    register.history = c(register.history,register)

    }
  if(input$V1[i] == "addx"){

    register.history = c(register.history,register,register)
    register=register+input$V2[i]
  }

    }

register.history=register.history[-1]

x.out3 = c(1:length(register.history))*register.history
sum(x.out3[c(seq(20,220,40))])




CRT=data.frame(register = register.history,col=rep(c(0:39),6))
CRT$sprite1 = CRT$register-1
CRT$sprite2 = CRT$register
CRT$sprite3 = CRT$register+1

CRT$screen = "."

for(i in 1: dim(CRT)[1]){
  matching=match(CRT$col[i],CRT[i,3:5])
 if(!is.na(matching)){CRT$screen[i] = "#"}

}


CRT.screen=matrix(data =CRT$screen,nrow = 6,byrow = T )
