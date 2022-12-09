library(data.table)
library(dplyr)
library(stringr)
library(data.tree)

setwd("~/AdventofCode2022/R/Day9")
input = read.table(file = "input",sep = "",stringsAsFactors = F,header =  F,fill = T)

movement_grid = matrix(0,nrow = 1000,ncol = 1000)
start.pos=data.frame(row=500,col=500)


h.pos = start.pos
t.pos = start.pos

movement_grid[t.pos$row,t.pos$col] = 1


h.move  =function(direction,h.pos){
  if(direction == "U"){h.pos$row = h.pos$row+1}
  if(direction == "D"){h.pos$row = h.pos$row-1}
  if(direction == "L"){h.pos$col = h.pos$col-1}
  if(direction == "R"){h.pos$col = h.pos$col+1}
  return(h.pos)
}

check.t = function(h.pos,t.pos){
  within.grid = expand.grid(rows = c((h.pos$row-1) : (h.pos$row+1) ),
                            cols = c((h.pos$col-1) : (h.pos$col+1)))
  matching = intersect(which(within.grid$rows == t.pos$row),which(within.grid$cols == t.pos$col))
  if(length(matching) == 1){matching = 1}else{matching=0}
  return(matching)
}

t.move=function(h.pos,t.pos,direction){
  t.pos.out=t.pos

  if(h.pos$row == t.pos$row && h.pos$col == t.pos$col){
    t.pos.out=t.pos
  }else{

  if( h.pos$row == t.pos$row && abs(h.pos$row - t.pos$row)==1 | h.pos$col == t.pos$col && abs(h.pos$row - t.pos$row)==1 ){
    if(direction == "U"){t.pos.out$row = t.pos$row+1}
    if(direction == "D"){t.pos.out$row = t.pos$row-1}
    if(direction == "L"){t.pos.out$col = t.pos$col-1}
    if(direction == "R"){t.pos.out$col = t.pos$col+1}
 #   t.pos=as.data.frame(t(t.pos))
  }else{
    possible.pos = data.frame(row = c(h.pos$row,h.pos$row,(h.pos$row-1),(h.pos$row+1) ),
                              col = c((h.pos$col-1) , (h.pos$col+1),h.pos$col,h.pos$col)
                              )

   possible.moves =  apply(possible.pos,1,function(X){check.t(h.pos = data.frame(row=X[1],col=X[2]),t.pos = t.pos)})
   t.pos.out = possible.pos[which(possible.moves == 1),]

   if(dim(t.pos.out)[1] == 0){
     possible.pos =  expand.grid(rows = c((h.pos$row-1) : (h.pos$row+1) ),
                                                cols = c((h.pos$col-1) : (h.pos$col+1)))

     possible.moves =  apply(possible.pos,1,function(X){check.t(h.pos = data.frame(row=X[1],col=X[2]),t.pos = t.pos)})
     t.pos.out = possible.pos[which(possible.moves == 1),]


   }


  }
}
  return(t.pos.out)
}

################################################################################

for(i in 1: dim(input)[1]){

  move.num = input$V2[i]
  direction = input$V1[i]
  for(j in 1:move.num){

    # move H
  h.pos = h.move(direction = direction,h.pos = h.pos)
   # check T
  if(check.t(h.pos = h.pos,t.pos = t.pos) == 0){
    t.pos = t.move(h.pos = h.pos,t.pos = t.pos,direction = direction)
    movement_grid[t.pos[1,1],t.pos[1,2]] = 1
  }
  }
}

sum(movement_grid)

####################

movement_grid = matrix(0,nrow = 1000,ncol = 1000)

pos.info = data.frame(row=rep(500,10),col=rep(500,10))


for(i in 1: dim(input)[1]){
  #print(i/ dim(input)[1]*100)
  move.num = input$V2[i]
  direction = input$V1[i]
  for(j in 1:move.num){
    #movement_grid = matrix(0,nrow = 20,ncol = 20)
    #movement_grid[pos.info[1,1],pos.info[1,2]] =0
    # move H
    pos.info[1,] = h.move(direction = direction,h.pos = pos.info[1,])
    # check T
    #movement_grid[pos.info[1,1],pos.info[1,2]] = 1
    for(k in 2:10){

    if(check.t(h.pos =  pos.info[(k-1),],t.pos =  pos.info[k,]) == 0){
     # movement_grid[pos.info[k,1],pos.info[k,2]] =0
      pos.info[k,]  = t.move(h.pos = pos.info[(k-1),],t.pos = pos.info[k,] ,direction = direction)
     # movement_grid[pos.info[k,1],pos.info[k,2]] = k
    }

      if(k==10){movement_grid[pos.info[k,1],pos.info[k,2]] = 1}

    }



    }
}

sum(movement_grid)
