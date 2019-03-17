
# http://infohost.nmt.edu/tcc/help/pubs/tkinter/web/ttk-Label.html
# good documentation
# http://infohost.nmt.edu/tcc/help/pubs/tkinter/web/index.html

ModelDlg <- function(x, ...){

  # require(DescTools)
  requireNamespace("tcltk")

  .GetModTxt <- function()
    tcltk::tclvalue(tcltk::tkget(tfModx, "0.0", "end"))

  .EmptyListBox <- function(){
    n <- as.character(tcltk::tksize(tlist.var))
    for (i in (n:0)) tcltk::tkdelete(tlist.var, i)
  }

  .AddVar <- function(sep, pack = NULL) {

    var.name <- as.numeric(tcltk::tkcurselection(tlist.var))
    lst <- as.character(tcltk::tkget(tlist.var, 0, "end"))

    if (length(var.name) == 0)
      tcltk::tkmessageBox(message = "No variable selected",
                          icon = "info", type = "ok")

    if (length(var.name) > 0) {

      txt <- DescTools::StrTrim(.GetModTxt())
      if(is.null(pack))
        vn <- DescTools::StrTrim(lst[var.name + 1])
      else
        vn <- DescTools::StrTrim(gettextf(pack, lst[var.name + 1]))

      tcltk::tkinsert(tfModx, "insert",
                      StrTrim(paste(ifelse(txt=="", "", "+"), paste(vn, collapse=sep), ""), method="left")
                      , "notwrapped")
    }
  }

  .BtnAddVar <- function() .AddVar(" + ")
  .BtnAddMult <- function() .AddVar(" * ")
  .BtnAddInt <- function() .AddVar(" : ")
  .BtnAddPoly <- function() .AddVar(sep=" + ", pack="poly(%s, 2)")



  imgAsc <-  tcltk::tclVar()
  tclimgAsc <-  tcltk::tkimage.create("photo", imgAsc, file = file.path(find.package("DescTools"), "extdata", "SortListAsc.gif"))
  imgDesc <-  tcltk::tclVar()
  tclimgDesc <-  tcltk::tkimage.create("photo", imgDesc, file = file.path(find.package("DescTools"), "extdata", "SortListDesc.gif"))
  imgNone <-  tcltk::tclVar()
  tclimgNone <-  tcltk::tkimage.create("photo", imgNone, file = file.path(find.package("DescTools"), "extdata", "SortListNo.gif"))

  .BtnSortVarListAsc <- function() .SortVarList("a")
  .BtnSortVarListDesc <- function() .SortVarList("d")
  .BtnSortVarListNone <- function() .SortVarList("n")


  .InsertLHS <- function() {

    var.name <- as.numeric(tcltk::tkcurselection(tlist.var))
    lst <- as.character(tcltk::tkget(tlist.var, 0, "end"))

    if (length(var.name) == 0)
      tcltk::tkmessageBox(message = "No variable selected",
                          icon = "info", type = "ok")

    if (length(var.name) > 0) {
      tcltk::tclvalue(tflhs) <- paste(lst[var.name + 1], collapse=", ")
    }
  }

  .SortVarList <- function(ord){

    lst <- DescTools::StrTrim(as.character(tcltk::tkget(tlist.var, 0, "end")))

    # for (i in (length(names(x)):0)) tkdelete(tlist.var, i)
    .EmptyListBox()

    if(ord == "a"){
      v <- DescTools::StrTrim(sort(lst, decreasing = FALSE))
    } else if(ord == "d"){
      v <- DescTools::StrTrim(sort(lst, decreasing = TRUE))
    } else {
      v <- DescTools::StrTrim(names(x)[names(x) %in% lst])
    }

    for (z in v) {
      tcltk::tkinsert(tlist.var, "end", paste0(" ", z))
    }

  }

  .FilterVarList <- function(){

    pat <- DescTools::StrTrim(tcltk::tclvalue(tffilter))
    # print(pat)
    if(pat=="")
      v <- DescTools::StrTrim(names(x))
    else
      v <- grep(pattern = pat, DescTools::StrTrim(names(x)), value=TRUE, fixed=TRUE)

    for (i in (length(names(x)):0)) tcltk::tkdelete(tlist.var, i)

    for (z in v) {
      tcltk::tkinsert(tlist.var, "end", paste0(" ", z))
    }

    # tcltk::tclvalue(frmVar$text) <- gettextf("Variables (%s/%s):", length(v), length(names(x)))
    tcltk::tkconfigure(frmVar, text=gettextf("Variables (%s/%s):", length(v), length(names(x))))
  }

  .SelectVarList <- function(){

    var.name <- as.numeric(tcltk::tkcurselection(tlist.var))
    lst <- as.character(tcltk::tkget(tlist.var, 0, "end"))

    if (length(var.name) > 0) {
      txt <- StrTrunc(Label(x[, StrTrim(lst[var.name + 1])]), 30)
      if(length(txt) == 0) txt <- " "
      cltxt <- class(x[, StrTrim(lst[var.name + 1])])
      if(any(cltxt %in% c("factor","ordered")))
        cltxt <- paste0(cltxt, "(", max(nlevels(x[, StrTrim(lst[var.name + 1])])), ")")
      tcltk::tclvalue(tflbl) <- gettextf("%s\n  %s", paste(cltxt, collapse=", "), txt)
    } else {
      tcltk::tclvalue(tflbl) <- "\n"
    }
  }


  fam <- "comic"
  size <- 10
  myfont <- tcltk::tkfont.create(family = fam, size = size)

  tfmodx <- tcltk::tclVar("")
  tflhs <- tcltk::tclVar("")
  tffilter <- tcltk::tclVar("")
  tflbl <- tcltk::tclVar("\n")
  tfframe <- tcltk::tclVar("Variables:")
  # gettextf("Variables (%s):", length(names(x)))
  mod_x <- NA_character_

  e1 <- environment()
  modx <- character()
  # old, repl. by 0.99.22: xname <- deparse(substitute(x))
  xname <- paste(StrTrim(deparse(substitute(x))), collapse=" ")

  if (!missing(x)) {
    if(class(x) == "formula") {

      # would be nice to pick up a formula here, to be able to edit the formula
      # https://rviews.rstudio.com/2017/02/01/the-r-formula-method-the-good-parts/

      # try to extract the name of the data.frame from match.call
      xname <- StrExtract(gsub("^.+data = ", "\\1", paste(deparse(match.call()), collapse=" ")), ".+[[:alnum:]]")

      tcltk::tclvalue(tflhs) <- deparse(x[[2]])
      mod_x <- deparse(x[[3]])

      x <- eval(parse(text=xname, parent.env()))

    } else if(!is.data.frame(x))
      stop("x must be a data.frame")


  } else {
    stop("Some data must be provided, example: ModelDlg(iris)")
  }


  OnOK <- function() {
    assign("modx", paste(
      DescTools::StrTrim(tcltk::tclvalue(tflhs)), " ~ ",
      DescTools::StrTrim(.GetModTxt()), ", data=", xname, sep=""), envir = e1)
    tcltk::tkdestroy(root)
  }

  # do not update screen
  tcltk::tclServiceMode(on = FALSE)

  # create window
  root <- .InitDlg(width = 880, height = 532, resizex=TRUE, resizey=TRUE,
                   main=gettextf("Build Model Formula (%s)", deparse(substitute(x))), ico="R")

  # define widgets
  content <- tcltk::tkframe(root, padx=10, pady=10)


  # Variable list
  frmVar <- tcltk::tkwidget(content, "labelframe", text=gettextf("Variables (%s/%s):", length(names(x)), length(names(x))),
                           fg = "black", padx = 10, pady = 10, font = myfont)


  tfFilter <- tcltk::tkentry(frmVar, textvariable=tffilter, width= 20, bg="white")
  tfButSortAsc <- tcltk::tkbutton(frmVar, image = tclimgAsc, compound="none",
                                  command = .BtnSortVarListAsc, height = 21, width = 21)
  tfButSortDesc <- tcltk::tkbutton(frmVar, image = tclimgDesc, compound="none",
                                   command = .BtnSortVarListDesc, height = 21, width = 21)
  tfButSortNone <- tcltk::tkbutton(frmVar, image=tclimgNone, compound="none",
                                   command = .BtnSortVarListNone, height = 21, width = 21)
  var.scr <- tcltk::tkscrollbar(frmVar, repeatinterval = 5,
                                command = function(...) tcltk::tkyview(tlist.var, ...))

  tlist.var <- tcltk::tklistbox(frmVar, selectmode = "extended",
                                yscrollcommand = function(...)
                                  tcltk::tkset(var.scr, ...), background = "white",
                                exportselection = FALSE, activestyle= "none", highlightthickness=0,
                                height=20, width=20, font = myfont)
  tfVarLabel <- tcltk::tklabel(frmVar, justify="left", width=26, anchor="w", textvariable=tflbl, font=myfont)

  for (z in names(x)) {
    tcltk::tkinsert(tlist.var, "end", paste0(" ", z))
  }


  tcltk::tkbind(tlist.var)
  tcltk::tkgrid(tfFilter, row=0, padx=0, sticky = "n")
  tcltk::tkgrid(tcltk::tklabel(frmVar, text="  "), row=0, column=1)
  tcltk::tkgrid(tfButSortAsc, row=0, column=2, padx=0, sticky = "n")
  tcltk::tkgrid(tfButSortDesc, row=0, column=3,  sticky = "n")
  tcltk::tkgrid(tfButSortNone, row=0, column=4, sticky = "n")
  tcltk::tkgrid(tcltk::tklabel(frmVar, text=" "))
  tcltk::tkgrid(tlist.var, var.scr, row=2, columnspan=5, sticky = "news")
  tcltk::tkgrid(tfVarLabel, row=3, columnspan=5, pady=3, sticky = "es")
  tcltk::tkgrid.configure(var.scr, sticky = "news")
  # tcltk2::tk2tip(tlist.var, "List of variables in data frame")

  # Buttons
  frmButtons <- tcltk::tkwidget(content, "labelframe", text = "",  bd=0,
                                fg = "black", padx = 5, pady = 25)

  tfButLHS <- tcltk::tkbutton(frmButtons, text = ">",
                              command = .InsertLHS, height = 1, width = 2, font=myfont)

  tfButAdd <- tcltk::tkbutton(frmButtons, text = "+",
                              command = .BtnAddVar, height = 1, width = 2, font=myfont)
  tfButMult <- tcltk::tkbutton(frmButtons, text = "*",
                               command = .BtnAddMult, height = 1, width = 2, font=myfont)
  tfButInt <- tcltk::tkbutton(frmButtons, text = ":",
                              command = .BtnAddInt,
                              height = 1, width = 2, font=myfont)
  tfButPoly <- tcltk::tkbutton(frmButtons, text = "x\U00B2",
                               command = .BtnAddPoly,
                               height = 1, width = 2, font=myfont)

  tcltk::tkgrid(tfButLHS, row = 0, rowspan=10, padx = 5, sticky = "s")
  tcltk::tkgrid(tcltk::tklabel(frmButtons, text="\n\n"))
  tcltk::tkgrid(tfButAdd, row = 40, padx = 5, sticky = "s")
  tcltk::tkgrid(tfButMult, row = 50, padx = 5, sticky = "s")
  tcltk::tkgrid(tfButInt, row = 60, padx = 5, sticky = "s")
  tcltk::tkgrid(tfButPoly, row = 70, padx = 5, sticky = "s")


  # Model textbox
  frmModel <- tcltk::tkwidget(content, "labelframe", text = "Model:",
                              fg = "black", padx = 10, pady = 10, font = myfont)

  tfLHS <- tcltk::tkentry(frmModel, textvariable=tflhs, bg="white")
  tfModx <- tcltk::tktext(frmModel, bg="white", height=20, width=70, wrap="word", padx=7, pady=5, font=myfont)
  tcltk::tkgrid(tfLHS, column=0, row=0, pady=10, sticky="nwes")
  tcltk::tkgrid(tcltk::tklabel(frmModel, text="~"), row=1, sticky="w")
  tcltk::tkgrid(tfModx, column=0, row=2, pady=10, sticky="nws")
  if(!all(is.na(mod_x)))
    tcltk::tkinsert(tfModx, "insert", mod_x, "notwrapped")

  # root
  tfButOK = tcltk::tkbutton(content, text="OK", command=OnOK, width=6)
  tfButCanc = tcltk::tkbutton(content, text="Cancel", width=7,
                              command=function() tcltk::tkdestroy(root))

  tcltk::tkbind(tfFilter, "<KeyRelease>", .FilterVarList)
  tcltk::tkbind(tlist.var, "<ButtonRelease>", .SelectVarList)
  tcltk::tkbind(tlist.var, "<KeyRelease>", .SelectVarList)
  tcltk::tkbind(tlist.var, "<Double-1>", .InsertLHS)


  # build GUI
  tcltk::tkgrid(content, column=0, row=0, sticky = "nwes")
  tcltk::tkgrid(frmVar, padx = 5, pady = 5, row = 0, column = 0,
                rowspan = 20, columnspan = 1, sticky = "ns")

  tcltk::tkgrid(frmButtons, padx = 5, pady = 5, row = 0, column = 2,
                rowspan = 20, columnspan = 1, sticky = "ns")

  tcltk::tkgrid(frmModel, padx = 5, pady = 5, row = 0, column = 3,
                rowspan = 20,
                sticky = "nes")

  tcltk::tkgrid(tfButOK, column=3, row=30, ipadx=15, padx=5, sticky="es")
  tcltk::tkgrid(tfButCanc, column=0, row=30, ipadx=15, padx=5, sticky="ws")

  tcltk::tkfocus(tlist.var)
  tcltk::tclServiceMode(on = TRUE)

  tcltk::tcl("wm", "attributes", root, topmost=TRUE)

  tcltk::tkwait.window(root)

  return(modx)

}

# Label(d.pizza$temperature) <- "die Temperature"
# Label(d.pizza$quality) <- "die qualitaet mit langem Text und weiteren Beschreibungen"

# Old version up to v. 0.99.27

# ModelDlg <- function(x, ...){
#
#   .GetModTxt <- function()
#     tcltk::tclvalue(tcltk::tkget(tfModx, "0.0", "end"))
#
#
#   .AddVar <- function(sep, pack = NULL) {
#
#     var.name <- as.numeric(tcltk::tkcurselection(tlist.var))
#
#     if (length(var.name) == 0)
#       tcltk::tkmessageBox(message = "No variable selected",
#                           icon = "info", type = "ok")
#
#     if (length(var.name) > 0) {
#
#       txt <- DescTools::StrTrim(.GetModTxt())
#       if(is.null(pack))
#         vn <- colnames(x)[var.name + 1]
#       else
#         vn <- gettextf(pack, colnames(x)[var.name + 1])
#
#       tcltk::tkinsert(tfModx, "insert",
#                       paste(ifelse(txt=="", "", " +"), paste(vn, collapse=sep))
#                       , "notwrapped")
#     }
#   }
#
#   .BtnAddVar <- function() .AddVar(" + ")
#   .BtnAddMult <- function() .AddVar(" * ")
#   .BtnAddInt <- function() .AddVar(" : ")
#   .BtnAddPoly <- function() .AddVar(sep=" + ", pack="poly(%s, 2)")
#
#   .InsertLHS <- function() {
#
#     var.name <- as.numeric(tcltk::tkcurselection(tlist.var))
#
#     if (length(var.name) == 0)
#       tcltk::tkmessageBox(message = "No variable selected",
#                           icon = "info", type = "ok")
#
#     if (length(var.name) > 0) {
#       tcltk::tclvalue(tflhs) <- paste(colnames(x)[var.name + 1], collapse=", ")
#     }
#   }
#
#   fam <- "comic"
#   size <- 10
#   myfont <- tcltk::tkfont.create(family = fam, size = size)
#
#   tfmodx <- tcltk::tclVar("")
#   tflhs <- tcltk::tclVar("")
#
#   mod_x <- NA_character_
#
#   e1 <- environment()
#   modx <- character()
#   # old, repl. by 0.99.22: xname <- deparse(substitute(x))
#   xname <- paste(StrTrim(deparse(substitute(x))), collapse=" ")
#
#   if (!missing(x)) {
#     if(class(x) == "formula") {
#
#       # would be nice to pick up a formula here, to be able to edit the formula
#       # https://rviews.rstudio.com/2017/02/01/the-r-formula-method-the-good-parts/
#
#       # try to extract the name of the data.frame from match.call
#       xname <- StrExtract(gsub("^.+data = ", "\\1", paste(deparse(match.call()), collapse=" ")), ".+[[:alnum:]]")
#
#       tcltk::tclvalue(tflhs) <- deparse(x[[2]])
#       mod_x <- deparse(x[[3]])
#
#       x <- eval(parse(text=xname, parent.env()))
#
#     } else if(!is.data.frame(x))
#       stop("x must be a data.frame")
#
#
#   } else {
#     stop("Some data must be provided, example: ModelDlg(iris)")
#   }
#
#
#   OnOK <- function() {
#     assign("modx", paste(
#       DescTools::StrTrim(tcltk::tclvalue(tflhs)), " ~ ",
#       DescTools::StrTrim(.GetModTxt()), ", data=", xname, sep=""), envir = e1)
#     tcltk::tkdestroy(root)
#   }
#
#   # do not update screen
#   tcltk::tclServiceMode(on = FALSE)
#
#   # create window
#   root <- .InitDlg(width = 840, height = 480, resizex=TRUE, resizey=TRUE,
#                    main="Build Model Formula", ico="R")
#
#   # define widgets
#   content = tcltk::tkframe(root, padx=10, pady=10)
#
#
#   # Variable list
#   frmVar <- tcltk::tkwidget(content, "labelframe", text = "Variables:",
#                             fg = "black", padx = 10, pady = 10, font = myfont)
#
#   var.scr <- tcltk::tkscrollbar(frmVar, repeatinterval = 5,
#                                 command = function(...) tcltk::tkyview(tlist.var, ...))
#
#   tlist.var <- tcltk::tklistbox(frmVar, selectmode = "extended",
#                                 yscrollcommand = function(...)
#                                   tcltk::tkset(var.scr, ...), background = "white",
#                                 exportselection = FALSE,
#                                 height=21, width = 25, font = myfont)
#
#   for (z in names(x)) {
#     tcltk::tkinsert(tlist.var, "end", z)
#   }
#
#
#   tcltk::tkbind(tlist.var)
#   tcltk::tkgrid(tlist.var, var.scr, sticky = "ns")
#   tcltk::tkgrid.configure(var.scr, sticky = "ns")
#   # tcltk2::tk2tip(tlist.var, "List of variables in data frame")
#
#   # Buttons
#   frmButtons <- tcltk::tkwidget(content, "labelframe", text = "",  bd=0,
#                               fg = "black", padx = 5, pady = 25)
#
#   tfButLHS <- tcltk::tkbutton(frmButtons, text = ">",
#     command = .InsertLHS, height = 1, width = 2, font=myfont)
#
#   tfButAdd <- tcltk::tkbutton(frmButtons, text = "+",
#      command = .BtnAddVar, height = 1, width = 2, font=myfont)
#   tfButMult <- tcltk::tkbutton(frmButtons, text = "*",
#      command = .BtnAddMult, height = 1, width = 2, font=myfont)
#   tfButInt <- tcltk::tkbutton(frmButtons, text = ":",
#              command = .BtnAddInt,
#              height = 1, width = 2, font=myfont)
#   tfButPoly <- tcltk::tkbutton(frmButtons, text = "x\U00B2",
#                               command = .BtnAddPoly,
#                               height = 1, width = 2, font=myfont)
#
#   tcltk::tkgrid(tfButLHS, row = 0, rowspan=10, padx = 5, sticky = "s")
#   tcltk::tkgrid(tcltk::tklabel(frmButtons, text="\n\n"))
#   tcltk::tkgrid(tfButAdd, row = 40, padx = 5, sticky = "s")
#   tcltk::tkgrid(tfButMult, row = 50, padx = 5, sticky = "s")
#   tcltk::tkgrid(tfButInt, row = 60, padx = 5, sticky = "s")
#   tcltk::tkgrid(tfButPoly, row = 70, padx = 5, sticky = "s")
#
#
#   # Model textbox
#   frmModel <- tcltk::tkwidget(content, "labelframe", text = "Model:",
#                               fg = "black", padx = 10, pady = 10, font = myfont)
#
#   tfLHS <- tcltk::tkentry(frmModel, textvariable=tflhs, bg="white")
#   tfModx <- tcltk::tktext(frmModel, bg="white", height=18, width=70, font=myfont)
#   tcltk::tkgrid(tfLHS, column=0, row=0, pady=10, sticky="nwes")
#   tcltk::tkgrid(tcltk::tklabel(frmModel, text="~"), row=1, sticky="w")
#   tcltk::tkgrid(tfModx, column=0, row=2, pady=10, sticky="nws")
#   if(!is.na(mod_x))
#     tcltk::tkinsert(tfModx, "insert", mod_x, "notwrapped")
#
#   # root
#   tfButOK = tcltk::tkbutton(content, text="OK", command=OnOK, width=6)
#   tfButCanc = tcltk::tkbutton(content, text="Cancel", width=7,
#                               command=function() tcltk::tkdestroy(root))
#
#   # build GUI
#   tcltk::tkgrid(content, column=0, row=0, sticky = "nwes")
#   tcltk::tkgrid(frmVar, padx = 5, pady = 5, row = 0, column = 0,
#                 rowspan = 20, columnspan = 1, sticky = "ns")
#
#   tcltk::tkgrid(frmButtons, padx = 5, pady = 5, row = 0, column = 2,
#                 rowspan = 20, columnspan = 1, sticky = "ns")
#
#   tcltk::tkgrid(frmModel, padx = 5, pady = 5, row = 0, column = 3,
#                 rowspan = 20,
#                 sticky = "nes")
#
#   tcltk::tkgrid(tfButOK, column=3, row=30, ipadx=15, padx=5, sticky="es")
#   tcltk::tkgrid(tfButCanc, column=0, row=30, ipadx=15, padx=5, sticky="ws")
#
#   tcltk::tkfocus(tlist.var)
#   tcltk::tclServiceMode(on = TRUE)
#
#   tcltk::tcl("wm", "attributes", root, topmost=TRUE)
#
#   tcltk::tkwait.window(root)
#
#   return(modx)
#
# }
#
#
#

