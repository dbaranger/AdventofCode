library(data.table)

setwd("R/Day1")

input = fread(file = "input",stringsAsFactors = F,na.strings = "",data.table = F)

cal=0
elf=1

for(i in 1:(dim(input)[1] +1)){

  if(!is.na(input[i,])){
    cal = cal + input[i,]
      }

  if(is.na(input[i,])){

    if(elf ==1){elf.out=cal}else{elf.out = c(elf.out,cal)}
    cal = 0
    elf = elf+1

  }


}
max(elf.out)

elf.out=elf.out[order(elf.out,decreasing = T)]
sum(elf.out[c(1:3)])
