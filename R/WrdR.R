WrdR <-
function(x,  wrd = getOption("lastWord") ){
  
  WrdText(paste("> ", x, sep=""), wrd=wrd, fontname="Courier New", fontsize=10, bold=TRUE, italic=TRUE)
  txt <- capture.output(eval(parse(text=x)))
  if(sum(nchar(txt))>0) WrdText(txt, wrd=wrd, fontname="Courier New", fontsize=10, bold=TRUE)
}
