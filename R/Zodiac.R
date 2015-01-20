Zodiac <-
function(x, lang = c("engl","deu"), stringsAsFactors = TRUE) {

  switch(match.arg(lang, choices=c("engl","deu"))
    , engl = {z <- c("Capricorn","Aquarius","Pisces","Aries","Taurus","Gemini","Cancer","Leo","Virgo","Libra","Scorpio","Sagittarius","Capricorn") }
    , deu =  {z <- c("Steinbock","Wassermann","Fische","Widder","Stier","Zwillinge","Krebs","Loewe","Jungfrau","Waage","Skorpion","Schuetze","Steinbock") }
  )
  
  i <- cut(DescTools::Month(x)*100 + DescTools::Day(x), breaks=c(0,120,218,320,420,520,621,722,822,923,1023,1122,1221,1231))
  if(stringsAsFactors){
    res <- i
    levels(res) <- z
  } else {
    res <- z[i]
  }  
  return(res)   
}
