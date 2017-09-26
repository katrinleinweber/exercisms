library(dplyr)
library(magrittr)

tournament <- function(input) {

  # clean input data (oust empty rows) & parse
  input[lapply(input, nchar) > 0] %>%  
    stringi::stri_split_fixed(";") ->
    input
  
  # oust lines with incorrect number of cells
  input <- input[lapply(input, length) == 3]
  
  df <- data.frame()
  for (r in 1:length(input))
    for (c in 1:length(input[[r]]))
      df[r, c] <- input[[r]][c]
  names(df) <- c("Team", "Team2", "Result")
  
  # separate team pairs, marking invalid result strings
  df$Result %>% 
    purrr::map(function(x) case_when(x == "loss" ~ "win",
                                     x == "win" ~ "loss",
                                     x == "draw" ~ "draw",
                                     x != "draw" ~ "NA")) %>% 
    unlist() %>% 
    data.frame(df$Team2, .) -> 
    df2
  names(df2) <- c("Team", "Result")
  
  # oust rows with invalid results/cells
  df2 <- df2[df2$Result != "NA",]
  
  # calculate values & output data frame
  df$Team2 <- NULL
  df %>% 
    rbind(df2) %>% 
    mutate(W = ifelse(Result == "win", 1, 0),
           D = ifelse(Result == "draw", 1, 0),
           L = ifelse(Result == "loss", 1, 0),
           MP = W + D + L,
           P = 3 * W + D) %>% 
    select(-Result) %>% 
    group_by(Team) %>% 
    summarise_all(sum) %>% 
    select(Team, MP, W:P) %>% 
    arrange(desc(P))
}
