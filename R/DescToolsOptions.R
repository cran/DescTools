DescToolsOptions <-
function(){
 cat(gettextf("\nCurrently defined DescTools options:
 footnote1 = %s
 footnote2 = %s
 plotit    = %s
 col1      = %s
 col2      = %s
 col3      = %s\n\n"
 , Coalesce(getOption("footnote1"), "' (default)")
 , Coalesce(getOption("footnote2"), '" (default)')
 , Coalesce(getOption("plotit"), "FALSE (default)")
 , Coalesce(getOption("col1"), "hblue (default)")
 , Coalesce(getOption("col2"), "hred (default)")
 , Coalesce(getOption("col3"), "horange (default)")
  ))
  
}
