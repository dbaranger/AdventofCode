library(data.table)
library(dplyr)
library(stringr)
library(data.tree)

setwd("~/AdventofCode2022/R/Day11")
input = read.table(file = "input",sep = "\t",stringsAsFactors = F,header =  F,fill = T)

Monkeys=list()

starts = seq(2,dim(input)[1],6)
stops = seq(6,dim(input)[1],6)

for(m in 1: length(starts)){

input.temp = input[starts[m]:stops[m],]

Monkey.temp=list(inspected=0,
                 items = input.temp[1] %>%
                   str_split(pattern = ":",simplify = T) %>%
                   as.data.frame() %>%
                   dplyr::select("V2") %>%
                   str_split(pattern = ",",simplify = T) %>%
                   as.numeric() %>% as.list(),
                 operation = input.temp[2] %>%
                   str_split(pattern = ":",simplify = T) %>%
                   as.data.frame() %>%
                   dplyr::select("V2") %>%
                   as.matrix() %>% as.character(),
                 test = input.temp[3] %>%
                   str_split(pattern = " ",simplify = T) %>%
                   as.data.frame() %>%
                   dplyr::select("V6") %>%
                   as.matrix() %>% as.numeric(),
                 if.true = input.temp[4] %>%
                        str_split(pattern = " ",simplify = T) %>%
                        as.data.frame() %>%
                        dplyr::select("V10") %>%
                        as.matrix() %>% as.numeric(),
                 if.false = input.temp[5] %>%
                        str_split(pattern = " ",simplify = T) %>%
                        as.data.frame() %>%
                        dplyr::select("V10") %>%
                        as.matrix() %>% as.numeric()

                 )

Monkey.temp$if.true = Monkey.temp$if.true+1
Monkey.temp$if.false = Monkey.temp$if.false+1

Monkeys[[m]]= Monkey.temp

}



for(rounds in 1:20){
  for(m in 1:8){

if(length(Monkeys[[m]]$items) > 0){
    Monkeys[[m]]$inspected = Monkeys[[m]]$inspected + length(Monkeys[[m]]$items)


    for(i in 1: length(Monkeys[[m]]$items) ){

      old =  Monkeys[[m]]$items[[i]]
      eval(parse(text=Monkeys[[m]]$operation))
      new=floor(new/3)

      if((new/ Monkeys[[m]]$test) == floor(new/ Monkeys[[m]]$test)){
        Monkeys[[Monkeys[[m]]$if.true]]$items[length( Monkeys[[Monkeys[[m]]$if.true]]$items)+1] = new
      }else{
        Monkeys[[Monkeys[[m]]$if.false]]$items[length( Monkeys[[Monkeys[[m]]$if.false]]$items)+1] = new

        }

      }

    Monkeys[[m]]$items=list()
}
  }

}

for(m in 1:8){
  temp = Monkeys[[m]]$inspected
  if(m==1){inspected.out = temp}else{inspected.out = c(inspected.out,temp)}

}
inspected.out[order(inspected.out,decreasing = T)][1] * inspected.out[order(inspected.out,decreasing = T)][2]

######### doesn't work ###############
gcd <- function(x,y) {
  r <- x%%y;
  return(ifelse(r, gcd(y, r), y))
}


for(rounds in 1:10000){
  print(rounds)
  for(m in 1:8){

    if(length(Monkeys[[m]]$items) > 0){
      Monkeys[[m]]$inspected = Monkeys[[m]]$inspected + length(Monkeys[[m]]$items)


      for(i in 1: length(Monkeys[[m]]$items) ){

        old =  Monkeys[[m]]$items[[i]]
        eval(parse(text=Monkeys[[m]]$operation))
        #new=floor(new/3)
        Monkeys[[m]]$items[[i]] = new
        }

      items.out = unique(unlist(Monkeys[[m]]$items))
      items.out=items.out[order(items.out)]

      for(k in 1:(length(items.out)-1)){
        common.t = gcd(items.out[k],items.out[(k+1)])
        if(k==1){common = common.t}else{common = c(common,common.t)}
      }

if(!is.na(common)){
      if(length(unique(common)) == 1 && unique(common)[1] != 1){


        for(i in 1: length(Monkeys[[m]]$items) ){
          Monkeys[[m]]$items[[i]] = Monkeys[[m]]$items[[i]]/common

          }
      }
}

      for(i in 1: length(Monkeys[[m]]$items) ){

        # old =  Monkeys[[m]]$items[[i]]
        # eval(parse(text=Monkeys[[m]]$operation))
        # #new=floor(new/3)

        new = Monkeys[[m]]$items[[i]]
        if((new/ Monkeys[[m]]$test) == floor(new/ Monkeys[[m]]$test)){
          Monkeys[[Monkeys[[m]]$if.true]]$items[length( Monkeys[[Monkeys[[m]]$if.true]]$items)+1] = new
        }else{
          Monkeys[[Monkeys[[m]]$if.false]]$items[length( Monkeys[[Monkeys[[m]]$if.false]]$items)+1] = new

        }

      }

      #Monkeys[[m]]$items=list()

       }
  }




      }





for(m in 1:8){
  temp = Monkeys[[m]]$inspected
  if(m==1){inspected.out = temp}else{inspected.out = c(inspected.out,temp)}

}
inspected.out[order(inspected.out,decreasing = T)][1] * inspected.out[order(inspected.out,decreasing = T)][2]

#32520712144
#14402639160
#14480835790
#14424332860
#14402639160
#14419902240
#14417868780
