library(data.table)
library(dplyr)
library(stringr)

setwd("~/AdventofCode2022/R/Day5")

input = read.table(file = "input",sep = " ",stringsAsFactors = F,header =  F,fill = T)

moves = input[-c(1:9),c(1:6)] %>% as.data.frame()
moves = moves[,c(2,4,6)]%>%  as.data.frame()
moves = sapply(moves, as.numeric) %>% as.data.frame()
colnames(moves) = c('move.num','from','to')

input = read.table(file = "input",sep = ",",stringsAsFactors = F,header =  F,fill = T)

crates = input[c(1:8),] %>% as.data.frame()
crates= crates %>% sapply(function(X){str_split(string = X,pattern = "",simplify = T)}) %>% matrix(nrow =8 )
crates=crates[,c(2,6,10,14,18,22,26,30,34)]

rotate <- function(x) t(apply(x, 2, rev))
crates = rotate(crates)

full.crates = matrix(data = NA,nrow = 9,ncol = 100)
full.crates[,c(1:8)] = crates
full.crates=as.data.frame(full.crates)
full.crates[full.crates == " "]=NA


where.move=function(rownum){
  row.t = full.crates[rownum,]
  simp.row = row.t[!is.na(row.t)]
  return(length(simp.row))
}


for(j in 1:dim(moves)[1]){
print(j)
    move.t = moves[j,]

    for(i in 1:move.t$move.num){
      full.crates[move.t$to, (where.move(move.t$to)+1)] =   full.crates[move.t$from, (where.move(move.t$from))]
      full.crates[move.t$from, (where.move(move.t$from))] = NA
        }

}

for(i in 1:9){
  out=full.crates[i,where.move(i)]
  if(i==1){final.out=out}else{final.out = paste(final.out,out,sep="")}
  }
final.out

#############



full.crates = matrix(data = NA,nrow = 9,ncol = 100)
full.crates[,c(1:8)] = crates
full.crates=as.data.frame(full.crates)
full.crates[full.crates == " "]=NA




for(j in 1:dim(moves)[1]){
  print(j)
  move.t = moves[j,]

  full.crates[move.t$to, ((where.move(move.t$to)+1) : (where.move(move.t$to)+(move.t$move.num) ))] =
  full.crates[move.t$from, c((where.move(move.t$from)-(move.t$move.num-1)):where.move(move.t$from))]

  full.crates[move.t$from, c((where.move(move.t$from)-(move.t$move.num-1)):where.move(move.t$from))] = NA


}

for(i in 1:9){
  out=full.crates[i,where.move(i)]
  if(i==1){final.out=out}else{final.out = paste(final.out,out,sep="")}
}
final.out
