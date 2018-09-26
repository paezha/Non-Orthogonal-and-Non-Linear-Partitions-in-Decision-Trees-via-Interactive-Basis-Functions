rm(list = ls())

#Load packages used for analysis
#library(stringr)
#library(tidyverse)
#library(tree)
library(irr)
library(evtree)
#library(ggmap)
#library(readr)
#library(rgdal)
#library(knitr)
#library(kableExtra)
#library(mlbench)
#library(dprep)
#library(rattle.data)
#library(microbenchmark)
library(randomForest)
#library(ggthemes)
library("Rmpi")


sprintf("TEST mpi.universe.size() =  %i", mpi.universe.size())
ns <- mpi.universe.size() - 1
sprintf("TEST attempt to spawn %i slaves", ns)
mpi.spawn.Rslaves(nslaves=ns)
mpi.remote.exec(paste("I am",mpi.comm.rank(),"of",mpi.comm.size()))
mpi.remote.exec(paste(mpi.comm.get.parent()))
#Send execution commands to the slaves
x<-5
#These would all be pretty correlated one would think
x<-mpi.remote.exec(rnorm,x)
length(x)
x
mpi.close.Rslaves()
mpi.quit()

#Obtain the names of directories in /data:
dir.names <- list.dirs("./data", recursive = FALSE)

#Read the names of data files and read data files in turn:
file.names <- character()
data.sets <- list()
for (i in 1:length(dir.names)){
  files <- list.files(dir.names[i])
  files <- files[str_detect(files, "_R.dat")]
  if (!str_detect(files, "test_R.dat")){
    file.names[i] <- files
  } else {
    file.names[i] <- files[-str_detect(files, "test_R.dat")]
  }
}

#Create a function to compute the interactions:

basis_functions <- function(df){
  df0 <- df
  a <- sapply(df0, is.numeric)
  df0 <- df0[,a]
  nc <- ncol(df0)
  for ( i in 1:nc ) { 
    for (j in i:nc) {
      if (i != j){
        df[[paste0(names(df0[i]),"p",names(df0[j]))]] <- df0[,i] + df0[,j]
        df[[paste0(names(df0[i]),"x",names(df0[j]))]] <- df0[,i] * df0[,j]
        df[[paste0(names(df0[i]),"c",names(df0[j]))]] <- df0[,i]^2 * df0[,j]^2
        df[[paste0(names(df0[i]),"xexp",names(df0[j]))]] <- df0[,i] * exp(df0[,j])
      }
    }
  }
  rm(df0)
  return(df)
}