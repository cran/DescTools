ImportFileDlg <-
function(auto_type = TRUE, env = .GlobalEnv)  {

  requireNamespace("tcltk", quietly = FALSE)
  
  filename <- tcltk::tclvalue(tcltk::tkgetOpenFile(filetypes= "{{All files} *} 
     {{SPSS Files} {.sav}} {{SAS xport files} {.xpt, .xport}} 
     {{SYSTAT} {*.sys, *.syd}} {{MiniTab} {.mtp}} 
     {{Stata Files} {.dta}}"))
  
  # nicht topmost, aber wie mach ich das dann??
  # tcl("wm", "attributes", root, topmost=TRUE)
  
  if (filename=="") return
  
  path <- SplitPath(filename)
  
  fformats <- c("SPSS","SAS","SYSTAT", "Minitab","Stata")
  
  if(auto_type){
    xsel <- switch(toupper(path$extension),
                   "SAV"="SPSS",
                   "DTA"="Stata",
                   "SYD"="SYSTAT",
                   "SYS"="SYSTAT",
                   "MTP"="MiniTab",
                   "XPT"="SAS",
                   "XPORT"="SAS",
                   "SAS"="SAS",
                   select.list(fformats, multiple = FALSE, graphics = TRUE))
  } else {
    xsel <- select.list(fformats, multiple = FALSE, graphics = TRUE)
  }  
  
  switch(xsel, 
         "MiniTab"={
           zz <- foreign::read.mtp(file=filename) 
         },
         "SYSTAT"={
           dlg <- .ImportSYSTAT(paste("d.", path$filename, sep=""))
           if(is.null(dlg)) return()
           zz <- foreign::read.systat(file=filename, to.data.frame = dlg$to.data.frame) 
         },
         "SPSS"={  
           dlg <- .ImportSPSS(paste("d.", path$filename, sep=""))
           if(is.null(dlg)) return()
           zz <- foreign::read.spss(file=filename, use.value.labels = dlg$use.value.labels, 
                           to.data.frame = dlg$to.data.frame, 
                           max.value.labels = dlg$max.value.labels, 
                           trim.factor.names = dlg$trim.factor.names, 
                           trim_values = dlg$trim_value, 
                           reencode = ifelse(dlg$reencode=="", NA, dlg$reencode), 
                           use.missings = dlg$use.missings) 
         },
         "SAS"={
           print("not yet implemented.")
         },
         "Stata"={
           dlg <- .ImportStataDlg(paste("d.", path$filename, sep=""))
           if(is.null(dlg)) return()
           zz <- foreign::read.dta(file=filename, convert.dates = dlg[["convert.dates"]], convert.factors = dlg[["convert.factors"]], 
                                   missing.type = dlg[["missing.type"]], convert.underscore = dlg[["convert.underscore"]],
                                   warn.missing.labels = dlg[["warn.missing.labels"]])
         })
  assign(dlg[["dsname"]], zz, envir=env)
  message(gettextf("Dataset %s has been successfully created!\n\n", dlg[["dsname"]]))
  # Exec(gettextf("print(str(%s, envir = %s))", dlg[["dsname"]],  deparse(substitute(env)))) 
}
