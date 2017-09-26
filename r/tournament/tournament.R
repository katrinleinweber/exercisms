library(dplyr)
library(magrittr)

tournament <- function(input) {

  # clean input data (oust empty rows)
  input <- input[lapply(input, nchar) > 0]
  
  # parse input into data frame
  input %>% 
    stringi::stri_extract_all(regex = "[^;]+") ->  # greedy everything except ;
    input
  
  # oust lines with incorrect number of cells
  input <- input[lapply(input, length) == 3]
  
  df <- data.frame()
  for (r in 1:length(input))
    for (c in 1:length(input[[r]]))
      df[r, c] <- input[[r]][c]
  names(df) <- c("Team", "Team2", "Result")
  
  # separate team pairs, marking invalid result strings
  Result2 <- list()
  for (r in 1:length(df$Result)) {
    if (df$Result[r] == "win") Result2[r] <- "loss"
    else if (df$Result[r] == "loss") Result2[r] <- "win"
    else if (df$Result[r] == "draw") Result2[r] <- "draw"
    else if (df$Result[r] != "draw") Result2[r] <- "NA"
  }
  
  Result2 %>% 
    unlist() %>% 
    data.frame(df$Team2, .) -> 
    df2
  names(df2) <- c("Team", "Result")
  
  # oust rows with invalid results/cells
  df2 <- df2[df2$Result != "NA",]
  
  df$Team2 <- NULL
  df <- rbind(df, df2)
  
  # calculate values & output data frame
  df %>% 
    mutate(W = ifelse(Result == "win", 1, 0),
           D = ifelse(Result == "draw", 1, 0),
           L = ifelse(Result == "loss", 1, 0),
           P = 3 * W + D) %>% 
    select(-Result) %>% 
    group_by(Team) %>% 
    summarise_all(sum) %>% 
    mutate(MP = W + D + L) %>% 
    select(Team, MP, W:P) %>% 
    arrange(desc(P))
}
