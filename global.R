# Packages importation
if(!require(knitr)) install.packages("shiny")
if(!require(tidyverse)) install.packages("shinythemes")
if(!require(caret)) install.packages("ggplot2")
if(!require(evtree)) install.packages("FactoMineR")
if(!require(evtree)) install.packages("missMDA")
if(!require(evtree)) install.packages("foreach")
if(!require(gam)) install.packages("parallel")
if(!require(matrixStats)) install.packages("doParallel")

library(shiny)
library(shinythemes)
library(ggplot2)
library(FactoMineR)
library(missMDA)
library(foreach)
library(parallel)
library(doParallel)

# Class definition
Environnement <- setRefClass("Loading Data",
                             fields = list(path = "character", file = "character", strVec = "character", factVect = "factor",
                                           numVec = "numeric", doubleVec = "double", intVec = "integer", dateVec = "Date"),
                             methods = list(loadingData = function(path, file, strVec, factVect, numVec, doubleVec, intVec, dateVec){
                               cl <- parallel::makeCluster(2)
                               doParallel::registerDoParallel(cl)
                               data <- openxlsx::read.xlsx(paste(path, file, sep = "/"), check.names = FALSE, na.strings = c("NA", "<NA>", "na", "Na", "nA", "NULL", "null", "Null"))
                               foreach::foreach(i = 1:length(strVec)) %:% when (length(strVec) != 0) %do% (data[strVec[i]] = as.character(unlist(data[strVec[i]])))
                               foreach::foreach(i = 1:length(factVect)) %:% when (length(factVect) != 0) %do% (data[factVect[i]] = as.factor(unlist(data[factVect[i]])))
                               foreach::foreach(i = 1:length(numVec)) %:% when (length(numVec) != 0) %do% (data[numVec[i]] = as.numeric(unlist(data[numVec[i]])))
                               foreach::foreach(i = 1:length(doubleVec)) %:% when (length(doubleVec) != 0) %do% (data[doubleVec[i]] = as.double(unlist(data[doubleVec[i]])))
                               foreach::foreach(i = 1:length(intVec)) %:% when (length(intVec) != 0) %do% (data[intVec[i]] = as.integer(unlist(data[intVec[i]])))
                               foreach::foreach(i = 1:length(dateVec)) %:% when (length(dateVec) != 0) %do% (data[dateVec[i]] = as.Date(unlist(data[dateVec[i]])))
                               parallel::stopCluster(cl)
                               data
                             }
                             ))

# Function definition

# Loading data
path <- paste(getwd(), "/Data", sep = "")
suppressWarnings(data <- Environnement(path, "Name_file.xlsx", c(), c(), c(), c("Lat", "Long"), c(), c())$loadingData(path, "Name_file.xlsx", c(), c(), c(), c("Lat", "Long"), c(), c()))

# Vector

