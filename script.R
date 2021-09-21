change_path <- function(file) {
    paste("data", file, sep="/")
}

get_file <- function() {
    files <- list.files(path = here("data"), pattern = ("pdf$")) %>% 
        map_chr(change_path)
    file <- pdf_text(files) %>% str_split("\n")
    return(file)
}

clean_file <- function() {
    file <- get_file()
    data <- c()
    for(i in 1:(length(file)-1)) {
        if (i == 1) {
            data[[i]] <- file[[i]][14:(length(file[[i]])-3)]
        } else if (i == 2) {
            data[[i]] <- file[[i]][3:(length(file[[i]])-5)]
        }
    }
    
    data <- str_squish(data)
    data <- strsplit(data, split= "\\,\\s\\\"")
    
    for(i in 1:length(data)) {
        data[[i]][1] <- data[[i]][1] %>%
            stringr::str_extract("(?<=c[:punct:]\\\").*")
    }
    
    for(i in 1:length(data)) {
        for(j in 1:length(data[[i]])) {
            data[[i]][j] <- data[[i]][j] %>%
                stringr::str_extract(".*(?=\")")
        }
    }
    return(data)
}

remove_unnecessary_value <- function(data) {
    clean_lines <- list()
    
    for(i in 1:length(data)) {
        for(j in 1:length(data[[i]])) {
            column <- data[[i]][j] %>% str_split(" ")
            matricula <- as.numeric(column[[1]][2])
            if(!is.na(matricula)) {
                clean_lines <- c(clean_lines, data[[i]][j])
            }
        }
    }
    
    clean_lines <- unlist(clean_lines)
    return(clean_lines)
}

split_columns <- function(clean_lines) {
    column1 <- c()
    column2 <- c()
    column3 <- c()
    column4 <- c()
    column5 <- c()
    column6 <- c()
    column7 <- c()
    column8 <- c()
    
    for(e in clean_lines) {
        matches <- regexpr("[0-9][0-9][0-9][0-9][0-9][0-9][0-9]", e)
        length <- attr(matches,"match.length")
        column1 <- c(column1, substr(e, matches[1], length + 1))
        e <- substring(e, matches[1] + length + 1)
        
        matches <- gregexpr("^.*(?=( Optativa | Obrigatória | Extracurricular ))", e, perl = TRUE)[[1]]
        length <- attr(matches,"match.length")
        column2 <- c(column2, substr(e, matches[1], length))
        e <- substring(e, matches[1] + length + 1)
        
        line_split <- e %>% str_split(" ")
        column_split <- line_split[[1]]
        
        column3 <- c(column3, column_split[1])
        column4 <- c(column4, column_split[2])
        column5 <- c(column5, column_split[3])
        column6 <- c(column6, column_split[4])
        column7 <- c(column7, column_split[5])
        column8 <- c(column8, column_split[6])
    }
    list_column <- list(column1, column2, column3, column4, column5, column6, column7, column8)
    return(list_column)
}

format_data <- function() {
    data <- clean_file()
    clean_lines <- remove_unnecessary_value(data)
    columns <- split_columns(clean_lines)
    
    formatted_data <-
        as_tibble(
            list(
                "codigo_disc" = columns[[1]],
                "nome_disc" = columns[[2]],
                "tipo_disc" = as.factor(columns[[3]]),
                "credito" = as.double(columns[[4]]),
                "ch" = as.double(columns[[5]]),
                "media" = as.double(str_replace_all(columns[[6]], ",", ".")),
                "situacao" = as.factor(columns[[7]]),
                "periodo" = columns[[8]]
            )
        )
    return(formatted_data)
}

get_data <- function() {
    formatted_data <- format_data()
    return(formatted_data)
}