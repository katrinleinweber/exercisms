library(dplyr)

rotate <- function(text, key) {
  
  # convert text to ASCII decimals
  bytes <- utf8ToInt(text)
  
  # separately for upper- and lowercase alphabet:
  # 1. shift decimal range to 1
  # 2. add key to each decimal
  # 3. ensure wrap-around by modulus
  # 4. reset range
  for (b in 1:length(bytes)) {
    if (between(bytes[b], 65, 90))
      bytes[b] <- (bytes[b] - 65 + key) %% 26 + 65
    else-if (between(bytes[b], 97, 122))
      bytes[b] <- (bytes[b] - 97 + key) %% 26 + 97
    else
      bytes[b] <- bytes[b]
  }
  # [ ] b in bytes, without length?
  # [ ] purr::map or ::walk?
  
  # convert back to (cipher)text
  intToUtf8(bytes)
  
}
