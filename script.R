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
cleanLines <- list()

for(i in 1:length(dataPdf)) {
    for(j in 1:length(dataPdf[[i]])) {
        column <- dataPdf[[i]][j] %>% str_split(" ")
        matricula <- as.numeric(column[[1]][2])
        if(!is.na(matricula)) {
            cleanLines <- c(cleanLines, dataPdf[[i]][j])
        }
    }
}

cleanLines <- unlist(cleanLines)
column_index <- list()

for(e in cleanLines) {
    matches <- regexpr("[0-9][0-9][0-9][0-9][0-9][0-9][0-9]", e)
    length <- attr(matches,"match.length")
    column1 <- substr(e, matches[1], length + 1)
    e <- substring(e, matches[1] + length + 1)
    
    matches <- gregexpr("^.*(?=( Optativa | Obrigatória))", e, perl = TRUE)[[1]]
    length <- attr(matches,"match.length")
    column2 <- substr(e, matches[1], length)
    e <- substring(e, matches[1] + length + 1)

}


