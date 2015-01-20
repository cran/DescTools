SysInfo <-
function() {

  ## description <<  getSysinfo is a convenience function to compile some information about the
  ##                 computing system and environment used.

  package.names <- sapply(sessionInfo()[['otherPkgs']],'[[','Package')
  package.versions <- sapply(sessionInfo()[['otherPkgs']],'[[','Version')
  packages.all <- paste(gettextf("%s (%s)", package.names, package.versions), collapse=", ")
  
  pars.sys <- c('user', 'nodename', 'sysname', 'release')
  R.system <- paste(sessionInfo()[[1]]$version.string)
  
  sys.info <- paste(pars.sys, Sys.info()[pars.sys], collapse=', ', sep=': ')
  all.info <- paste(c(sys.info,', ', R.system,', installed Packages: ', packages.all), 
                    sep='', collapse='')
  
  cat(gettextf("\nSystem: %s\nNodename: %s, User: %s", 
               paste(Sys.info()[c("sysname","release","version")], collapse=" ")
               , Sys.info()["nodename"], Sys.info()["user"], "\n\n"))
  cat(gettextf("\nTotal Memory: %s MB\n\n", memory.limit()))
  cat(StrTrim(sessionInfo()$R.version$version.string), "\n")
  cat(sessionInfo()$platform, "\n")
  cat("\nLoaded Packages: \n", packages.all, "\n")
  
  DescToolsOptions()
  
  invisible(all.info)
  
}
