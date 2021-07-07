#install.packages("pdftools")
library(pdftools)
library(tidyverse)
library(here)

files <- list.files(path = here("data"), pattern = ("pdf$")) %>% 
    map_chr(alteraCaminho)

pdfFile <- pdf_text(files) %>% str_split("\n")