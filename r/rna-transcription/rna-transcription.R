to_rna <- function(dna) {
  
  # handle invalid sequences
  if (stringi::stri_detect_regex(dna, "[^GATC]"))
    stop("That's not a valid nucleobase!")
  
  # return complement
  transcribe <- function(base) {
    dplyr::case_when(base == "G" ~ "C",
                     base == "A" ~ "U",
                     base == "T" ~ "A",
                     base == "C" ~ "G")
  }
  
  # swap bases 1-by-1, then concatenate to sequence
  strsplit(dna, "") %>%
    unlist() %>%
    lapply(transcribe) %>%
    paste(collapse = "")
}
