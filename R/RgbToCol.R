RgbToCol <-
function(col, method="rgb", metric="euclidean") {

  switch( match.arg( arg=method, choices=c("rgb","hsv") )
     , "rgb" = {
            # accepts either a matrix with 3 columns RGB or a hexstr
            
          if(!is.matrix(col)) {
            col <- lapply(col, function(x) c(strtoi(substr(x,2,3), 16L), strtoi(substr(x,4,5), 16L), strtoi(substr(x,6,7), 16L)))
            col <- do.call("cbind", col)
          }  
          coltab <- col2rgb(colors())
          
          switch( match.arg( arg=metric, choices=c("euclidean","manhattan") )
                  , "euclidean" = {
                    colors()[apply(col, 2, function(x) which.min(apply(apply(coltab, 2, "-", x)^2, 2, sum)))]
                  }
                  , "manhattan" = {
                    colors()[apply(col, 2, function(x) which.min(apply(abs(apply(coltab, 2, "-", x)), 2, sum)))]
                  }
          )  
     }
     , "hsv" ={       
            # accepts either a matrix with 3 columns RGB or a hexstr
            col <- ColToHsv(col)
            if(!is.matrix(col)) {
              col <- lapply(col, function(x) c(strtoi(substr(x,2,3), 16L), strtoi(substr(x,4,5), 16L), strtoi(substr(x,6,7), 16L)))
              col <- do.call("cbind", col)
            }  
            coltab <- ColToHsv(colors())
            
            switch( match.arg( arg=metric, choices=c("euclidean","manhattan") )
                    , "euclidean" = {
                      colors()[apply(col, 2, function(x) which.min(apply(apply(coltab, 2, "-", x)^2, 2, sum)))]
                    }
                    , "manhattan" = {
                      colors()[apply(col, 2, function(x) which.min(apply(abs(apply(coltab, 2, "-", x)), 2, sum)))]
                    }
            )  
     }
  )
  
  # alternative?
  # Identify closest match to a color:  plotrix::color.id
  
  # old:
  # coltab <- col2rgb(colors())
  # cdist <- apply(coltab, 2, function(z) sum((z - col)^2))
  # colors()[which(cdist == min(cdist))]
}
