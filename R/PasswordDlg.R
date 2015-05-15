PasswordDlg <-
function() {

  requireNamespace("tcltk", quietly = FALSE)
  
  e1 = environment()
  pw = character()
  
  tfpw <- tcltk::tclVar("")

  OnOK <- function() {
    assign("pw", tcltk::tclvalue(tfpw), envir = e1)
    tcltk::tkdestroy(root)
  }
  
  # do not update screen
  tcltk::tclServiceMode(on = FALSE) 
  # create window
  root <- .InitDlg(205, 110, resizex=FALSE, resizey=FALSE, main="Login", ico="key")

  # define widgets
  content = tcltk::tkframe(root, padx=10, pady=10)
  tfEntrPW = tcltk::tkentry(content, width="30", textvariable=tfpw, show="*" )
  tfButOK = tcltk::tkbutton(content,text="OK",command=OnOK, width=6)
  tfButCanc = tcltk::tkbutton(content, text="Cancel", width=7, command=function() tcltk::tkdestroy(root))
  
  # build GUI
  tcltk::tkgrid(content, column=0, row=0)
  tcltk::tkgrid(tcltk::tklabel(content, text="Enter Password"), column=0, row=0, columnspan=3, sticky="w")
  tcltk::tkgrid(tfEntrPW, column=0, row=1, columnspan=3, pady=10)
  tcltk::tkgrid(tfButOK, column=0, row=2, ipadx=15, sticky="w")
  tcltk::tkgrid(tfButCanc, column=2, row=2, ipadx=5, sticky="e")

  # binding event-handler
  tcltk::tkbind(tfEntrPW, "<Return>", OnOK)
  
  tcltk::tkfocus(tfEntrPW)
  tcltk::tclServiceMode(on = TRUE) 
  
  tcltk::tcl("wm", "attributes", root, topmost=TRUE)

  tcltk::tkwait.window(root)
  
  return(pw)
  
}
