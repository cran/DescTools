IsNumeric <-
function (x, length.arg = Inf, integer.valued = FALSE, positive = FALSE, na.rm = FALSE){

  if (na.rm) 
    x <- na.omit(x)
  
  if (all(is.numeric(x)) && all(is.finite(x)) && (if (is.finite(length.arg)) length(x) == 
                                                    length.arg else TRUE) && (if (integer.valued) all(x == round(x)) else TRUE) && 
        (if (positive) all(x > 0) else TRUE)) TRUE else FALSE
}
