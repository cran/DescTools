Recode <-
function(x, newlevels, elselevel=NA, use.empty=FALSE){ 
  
  if( sum(duplicated(unlist(newlevels))) > 0) stop ("!Recode! newlevels contain non unique values!")
  
  if(is.null(elselevel)) { # leave elselevels as they are
    elselevels <- setdiff(levels(x), unlist(newlevels))
    names(elselevels) <- elselevels
    newlevels <- c(newlevels, elselevels)
  } else {
    if(!is.na(elselevel)){
      newlevels[[length(newlevels)+1]] <- setdiff(levels(x), unlist(newlevels))
      names(newlevels)[[length(newlevels)]] <- elselevel
    }  
  }
  levels(x) <- newlevels
  if(!use.empty) x <- factor(x)  # delete potentially empty levels
  return(x)
}
