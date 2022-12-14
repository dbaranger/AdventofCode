library(data.table)
library(plyr)
library(dplyr)
library(stringr)
#library(data.tree)

setwd("~/AdventofCode2022/R/Day12")
input = read.table(file = "input",sep = "",stringsAsFactors = F,header =  F,fill = T)

input2 = matrix(data = NA,nrow = dim(input)[1],ncol = nchar(input[1,])) %>% as.data.frame()

for(i in 1:dim(input)[1]){
  input2[i,] = str_split(input[i,],pattern = "",simplify = T)
}

start.search = which(input2 == "S",arr.ind = T) %>% as.data.frame()
end.search = which(input2 == "E",arr.ind = T)%>% as.data.frame()

input2[start.search$row,start.search$col] = "a"
input2[end.search$row,end.search$col] = "z"

#######################################################
######################################

possible.moves2 = function(loc){

  poss=data.frame(row = c(loc$row-1,loc$row+1,loc$row,loc$row),
                  col = c(loc$col,loc$col,loc$col-1,loc$col+1)
  )
  poss[poss == 0 ] =NA
  poss=na.omit(poss)

  loc.value = input2[loc$row,loc$col]


  poss$value= apply(poss,1,function(X){
    #return(X)
    return(input2[X[1],X[2] ])
  })

  valid = unique(c(letters[which(letters < loc.value)],letters[which(letters == loc.value)],letters[which(letters == loc.value)+1]))

  poss$valid = match(poss$value,valid)

  poss=na.omit(poss)
return(poss)
  }


queue = start.search
count = 1
locations = expand.grid(row = c(1:41),col = c(1:136))
locations$counter = NA

locations[intersect(which(locations$row == queue$row[count]),which(locations$col == queue$col[count])),3] = 0


while(is.na(dplyr::filter(locations, row == end.search$row,col==end.search$col) %>% dplyr::select(counter) %>% unname()%>% c() )){
  print(count)
  counter = locations[intersect(which(locations$row == queue$row[count]),which(locations$col == queue$col[count])),3] + 1

  poss = possible.moves2(queue[count,])

  poss =   merge(poss,locations,all.x = T)
  poss = poss %>% dplyr::filter(is.na(counter))

if(dim(poss)[1] >0){
  for(p in 1: dim(poss)[1]){
    locations[intersect(which(locations$row == poss$row[p]),which(locations$col == poss$col[p])),3] = counter
    queue = rbind(queue,poss[p,c(1:2)])
  }
}
  count = count+1
}

dplyr::filter(locations, row == end.search$row,col==end.search$col) %>% dplyr::select(counter)

#######################################

for(i in 1: 136){
out=data.frame(row=c(1:41),col=i,var=input2[,i])
if(i==1){input.long=out}else{input.long = rbind(input.long,out)}

}

input.long=input.long %>% dplyr::filter(var == "a")
input.long$path = NA


for(i in 1:dim(input.long)[1]){
print(i/1838*100)
  path.length=function(X){
  queue = input.long[X,c(1,2)]
  count = 1
  locations = expand.grid(row = c(1:41),col = c(1:136))
  locations$counter = NA

  locations[intersect(which(locations$row == queue$row[count]),which(locations$col == queue$col[count])),3] = 0

 out= tryCatch(
    {
  while(is.na(dplyr::filter(locations, row == end.search$row,col==end.search$col) %>% dplyr::select(counter) %>% unname()%>% c() )){
   # print(count)
    counter = locations[intersect(which(locations$row == queue$row[count]),which(locations$col == queue$col[count])),3] + 1

    poss = possible.moves2(queue[count,])

    poss =   merge(poss,locations,all.x = T)
    poss = poss %>% dplyr::filter(is.na(counter))

    if(dim(poss)[1] >0){
      for(p in 1: dim(poss)[1]){
        locations[intersect(which(locations$row == poss$row[p]),which(locations$col == poss$col[p])),3] = counter
        queue = rbind(queue,poss[p,c(1:2)])
      }
    }
    count = count+1
  }

 return(dplyr::filter(locations, row == end.search$row,col==end.search$col) %>% dplyr::select(counter))
    },
 error = function(X){return(NA)}
 )


  }

 pl =  path.length(X=i)

 input.long$path[i] = pl

  }
min(na.omit(input.long$path))

input.long$path %>% unlist() %>% na.omit() %>% min()

)
