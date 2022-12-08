library(data.table)
library(dplyr)
library(stringr)
library(data.tree)

setwd("~/AdventofCode2022/R/Day7")

input = read.table(file = "input",sep = " ",stringsAsFactors = F,header =  F,fill = T)



tree = Node$new("/")
tree$type = "folder"
current.node="tree"

for(i in 2:dim(input)[1]){
  command.temp = input[i,]
      if(command.temp$V2 != "ls"){
          if(command.temp$V2 == "cd"){

              if(command.temp$V3 == ".."){
              current.node = current.node[-length(current.node)]
              }else{current.node = c(current.node,command.temp$V3)}

          }else{
            eval(parse(text = paste(paste(current.node,collapse = "$",sep=""),"$AddChild(\"",command.temp$V2,"\")",sep="")))

          if(command.temp$V1 != "$" & command.temp$V1 != "dir" ){
              eval (parse(text = paste(paste(current.node,collapse = "$",sep=""),"$",command.temp$V2,"$size = ",as.numeric(command.temp$V1),sep="")))
              eval (parse(text = paste(paste(current.node,collapse = "$",sep=""),"$",command.temp$V2,"$type = \"file\"  ",sep="")))
          }
          if(command.temp$V1 == "dir" ){
              eval (parse(text = paste(paste(current.node,collapse = "$",sep=""),"$",command.temp$V2,"$type = \"folder\"  ",sep="")))
          }

        }

      }
}

print(tree,"type","size")

tree$Do(function(x) {
  x$fullsize <- Aggregate(node = x,
                            attribute = "size",
                            aggFun = sum)
},
traversal = "post-order")

Sort(tree, attribute = "fullsize", decreasing = TRUE, recursive = TRUE)

print(tree,"fullsize","size")

tree.frame <- ToDataFrameNetwork(tree,"size","fullsize","type")
tree.frame %>% dplyr::filter(fullsize<=100000,type == "folder") %>% dplyr::select("fullsize") %>% sum()

#1,448,749 -- 164,498
#1307902 !

###

30000000-(70000000-47048086)

tree.frame %>% dplyr::filter(fullsize>=(30000000-(70000000-47048086)),type == "folder") %>% dplyr::select("fullsize") %>% min()
#7068748

