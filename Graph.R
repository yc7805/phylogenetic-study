
# set directory
setwd("~/Desktop/research")


#install all the packages we need to use
install.packages("gdata", dep = T)      # If you have not installed it before
library(gdata)
install.packages("ape")                 # If you have not installed it before
library(ape)
install.packages("phangorn")            # If you have not installed it before
library(phangorn)
install.packages("reshape2")            # If you have not installed it before
library(reshape2)
install.packages("xlsx")                # If you have not installed it before
library(xlsx)



# Read in the tree the names of the taxa and the total taxa values
# Change the directory to the location of the file
input.trees <- read.tree(paste0("~/Desktop/research/input.txt"))
totals <- read.delim("~/Desktop/research/totals", header = FALSE, sep = ":", dec = ".")
names <- read.delim("~/Desktop/research/names", header = FALSE, sep = ":", dec = ".")
test1 <- as.matrix(totals)#change table to matrix
test2 <- as.matrix(names)#change table to matrix
test3 <- cbind(test2[,1],test1[,2])#put the name and total taxa values together
test3 <- cbind(test2[,1],test1[,2],0,0,0)#add three columns in to the matrix



# Using our formula to find the taxa level
choose <- 3*choose(n=as.numeric(nrow(test2)-1),k=3)
for(a in 1:nrow(test2)){
  test3[a,3] <- as.numeric(test3[a,2])/choose
}


# Count the percentage of missing data
max <- 0 # get the total amount of 
for(b in 1:nrow(test3)){
  count<-0
  test3[b,4]<-0
  for (a in 1:763){   #change the number 763 to the total amount of trees 
    test<-as.matrix(RA.3.2.trees[[a]]$tip.label)
    for(c in 1:nrow(test)){
      if(test[c,1]==test3[b,1]){
        count<-count+1
      }
    }
  }
  test3[b,4]<-count
  if(count>max){
    max=count
  }
}
for(b in 1:nrow(test2)){
  test3[b,5] <- ((max-as.numeric(test3[b, 4]))/max)
}


#Graph the Percent of Missing Data vs. Taxa Level
level <- test3[,3]
missing <- test3[,5]
plot(missing, level,xlab="percent of missing data",ylab="taxa level",pch=1) 
title("Percent of Missing Data vs. Taxa Level") 