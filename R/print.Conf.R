print.Conf <-
function(x, digits = max(3, getOption("digits") - 3), ...) {
  cat("Confusion Matrix and Statistics\n\n") 
  
  names(attr(x$table, "dimnames")) <- c("Prediction","Reference")
  print(x$table, ...)
  
  if(nrow(x$table)!=2) cat("\nOverall Statistics\n")
  
  txt <- gettextf("
               Accuracy : %s          
                 95%s CI : (%s, %s)
    No Information Rate : %s           
    P-Value [Acc > NIR] : %s  
                  
                  Kappa : %s          
 Mcnemar's Test P-Value : %s\n\n", 
                  Format(x$acc, digits=digits), "%", 
                  Format(x$acc.lci, digits=digits), Format(x$acc.uci, digits=digits),
                  Format(x$nri, digits=digits), Format(x$acc.pval, fmt="p", na.form="NA"),
                  Format(x$kappa, digits=digits), Format(x$mcnemar.pval, fmt="p", na.form="NA")
                  )   
  cat(txt)
  
  rownames(x$byclass) <- c("Sensitivity", "Specificity", "Pos Pred Value", "Neg Pred Value", "Prevalence",
                           "Detection Rate", "Detection Prevalence", "Balanced Accuracy")
  
  if(nrow(x$table)==2){
    cat(
      paste(StrPad(paste(rownames(x$byclass), ":"), width=25, adj = "right"), 
            Format(x$byclass, digits=digits))
      , sep="\n")
    
    txt <- gettextf("\n       'Positive' Class : %s\n\n", x$pos)
    cat(txt)
    
  } else {
    
    cat("\nStatistics by Class:\n\n")
    print(Format(x$byclass, digits = digits, na.form="NA"), quote = FALSE)
    cat("\n")
    
  }
  
}
