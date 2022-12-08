library(data.table)
library(dplyr)
library(stringr)
library(data.tree)

setwd("~/AdventofCode2022/R/Day8")
options(scipen=999)
input = read.table(file = "input",sep = "",stringsAsFactors = F,header =  F,fill = T)

input = fread(file = "input",stringsAsFactors = F,na.strings = "",data.table = F,header = F)

out=matrix(data = NA,nrow = 99,ncol = 99) %>% as.data.frame()
for(i in 1: dim(input)[1]){
  out[i,] = str_split(string = input$V1[i],pattern = "",simplify = T) %>% as.numeric()

}


visible=matrix(data = NA,nrow = 99,ncol = 99) %>% as.data.frame()

for(i in 1:99){
  for (j in 1:99){
    tree=out[i,j]

    if(j==1){left=-1}else{    left = max(out[i, c(1:(j-1))])}
    if(j==99){right=-1}else{right = max(out[i, c((j+1):99)])}
    if(i==1){top=-1}else{top = max(out[c(1:(i-1)),j ])}
    if(i==99){bottom=-1}else{bottom = max(out[c((i+1):99),j ])}

   if(tree > left | tree > right | tree > top | tree > bottom){
     visible[i,j]=1
        }else{visible[i,j]=0}

  }
}

sum(visible)

####################


numvisible=matrix(data = NA,nrow = 99,ncol = 99) %>% as.data.frame()

for(i in 1:99){
  for (j in 1:99){
    tree=out[i,j]

    if(j==1){left=0}else{
      trees = (out[i, c(1:(j-1))])
      left=j- max(which(trees >= tree))
        if(length(left) ==0){left=j-1}
        if(is.na(left)){left=j-1}
        if(is.infinite(left)){left=j-1}
      }
    if(j==99){right=0}else{
      trees = (out[i, c((j+1):99)])
      right =  which(trees >= tree)[1]
       if(length(right) ==0){right=99-j}
       if(is.na(right)){right=99-j}
       if(is.infinite(right)){right=99-j}

      }
    if(i==1){top=0}else{
      trees = (out[c(1:(i-1)),j ])
      top = i- max(which(trees >= tree))
       if(length(top) ==0){top=i-1}
       if(is.na(top)){top=i-1}
       if(is.infinite(top)){top=i-1}

      }
    if(i==99){bottom=0}else{
      trees = (out[c((i+1):99),j ])
      bottom =  which(trees >= tree)[1]
        if(length(bottom) ==0){bottom=99-i}
        if(is.na(bottom)){bottom=99-i}
        if(is.infinite(bottom)){bottom=99-i}

      }

    numvisible[i,j]=left*right*top*bottom

  }
}

max(numvisible)
#83200
