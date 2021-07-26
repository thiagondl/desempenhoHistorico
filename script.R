#install.packages("pdftools")
library(pdftools)
library(tidyverse)
library(here)
library(numbers)

alteraCaminho <- function(file) {
    paste("data", file, sep="/")
}

files <- list.files(path = here("data"), pattern = ("pdf$")) %>% 
    map_chr(alteraCaminho)

pdfFile <- pdf_text(files) %>% str_split("\n")
dataPdf <- c()

for(i in 1:(length(pdfFile)-1)) {
    if (i == 1) {
        dataPdf[[i]] <- pdfFile[[i]][14:(length(pdfFile[[i]])-3)]
    } else if (i == 2) {
        dataPdf[[i]] <- pdfFile[[i]][3:(length(pdfFile[[i]])-5)]
    }
}

dataPdf <- str_squish(dataPdf)
dataPdf <- strsplit(dataPdf, split= "\\,\\s\\\"")

 for(i in 1:length(dataPdf)) {
    dataPdf[[i]][1] <- dataPdf[[i]][1] %>%
        stringr::str_extract("(?<=c[:punct:]\\\").*")
}

for(i in 1:length(dataPdf)) {
    for(j in 1:length(dataPdf[[i]])) {
        dataPdf[[i]][j] <- dataPdf[[i]][j] %>%
            stringr::str_extract(".*(?=\")")
    }
}

index <- length(dataPdf)
cleanLines <- vector(mode="list", index)

for(i in 1:length(dataPdf)) {
    for(j in 1:length(dataPdf[[i]])) {
        column <- dataPdf[[i]][j] %>% str_split(" ")
        matricula <- as.numeric(column[[1]][2])
        if(!is.na(matricula)) {
            cleanLines[[i]][j] <- dataPdf[[i]][j]
        }
    }
}

index <- length(dataPdf)
cleanLines <- vector(mode="list", index)

for(i in 1:length(dataPdf)) {
    for(j in 1:length(dataPdf[[i]])) {
        column <- dataPdf[[i]][j] %>% str_split(" ")
        matricula <- as.numeric(column[[1]][2])
        if(!is.na(matricula)) {
            cleanLines[[i]][j] <- dataPdf[[i]][j]
        }
    }
}

cleanData <- vector(mode="list", index)

cleanData[[1]] <- cleanLines[[1]][!is.na(cleanLines[[1]])]
cleanData[[2]] <- cleanLines[[2]][!is.na(cleanLines[[2]])]