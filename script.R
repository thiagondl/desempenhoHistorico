#install.packages("pdftools")
library(pdftools)
library(tidyverse)
library(here)

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
