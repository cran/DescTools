PasswordDlg <-
function() {
  
#  require(tcltk)
  
  e1 = environment()
  pw = character()
  
  # define tclVar
  tfpw <- tclVar("")
  
  # define functions
  OnOK <- function() {
    assign("pw", tclvalue(tfpw), envir = e1)
    tkdestroy(root)
  }
  
  # create window

  geom <- gettextf('%sx%s+%s+%s'
     , 200, 110
     , as.numeric(substr(capture.output(system("wmic desktopmonitor get screenwidth", 
                                               intern=TRUE)), 24,34))/2 - 50 
     , as.numeric(substr(capture.output(system("wmic desktopmonitor get screenheight", 
                                               intern=TRUE)), 25,34))/2 - 25
     )
  
  tclServiceMode(on = FALSE) 
  
  root <- tktoplevel()
  tkwm.title(root, "Login")
  tkwm.resizable(root, FALSE, FALSE)
  tkwm.iconbitmap(root, file.path(find.package("DescTools"), "data", "key.ico"))
  
  # define widgets
  content = tkframe(root, padx=5, pady=10)
  tfEntrPW = tkentry(content, width="30", textvariable=tfpw, show="*" )
  tfButOK = tkbutton(content,text="OK",command=OnOK)
  tfButCanc = tkbutton(content,text="Cancel", command=function() tkdestroy(root))
  
  # build GUI
  tkgrid(content, column=0, row=0)
  tkgrid(tklabel(content, text="Enter Password"), column=0, row=0, columnspan=3, sticky="w")
  tkgrid(tfEntrPW, column=0, row=1, columnspan=3, pady=10)
  tkgrid(tfButOK, column=0, row=2, ipadx=15)
  tkgrid(tfButCanc, column=2, row=2, ipadx=5)
  
  tkwm.geometry(root, geom)
              
  # binding event-handler
  tkbind(tfEntrPW, "<Return>", OnOK)
  
  tkfocus(tfEntrPW)
  tclServiceMode(on = TRUE) 
  
  tcl("wm", "attributes", root, topmost=TRUE)

  tkwait.window(root)
  
  return(pw)
  
}
