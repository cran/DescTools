Vigenere <-
function(x, key = NULL, decrypt = FALSE) {
  
  # hold that constant, as it makes the function too flexible else
  # in cases you maybe remind your password, but lost the charlist definition....
  charlist <- c(LETTERS, letters, 0:9)
  
  if(is.null(key)) key <- PasswordDlg()
  
  .mod1 <- function(v, n) {
    # mod1(1:20, 6)   =>   1 2 3 4 5 6 1 2 3 4 5 6 1 2 3 4 5 6 1 2
    ((v - 1) %% n) + 1
  }
  
  .str2ints <- function(s) {
    
    as.integer(Filter(Negate(is.na),
                      factor(levels = charlist, strsplit(s, "")[[1]])))
  }
  
  x <- .str2ints(x)
  key <- rep(.str2ints(key), len = length(x)) - 1
  paste(collapse = "", charlist[
    .mod1(x + (if (decrypt) -1 else 1)*key, length(charlist))])
}
