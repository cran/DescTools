WhichCharacters <-
function (d.frm) {
  # returns the names of all character vectors of a data.frame
  names(d.frm)[grep(pattern = "character", x = lapply(d.frm, class))]
}
