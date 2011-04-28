varUsage <- function(x)
  {

    x <- strsplit(x, "\n")[[1]]
    startVars <- grep("\tAttribute usage", x)
    if(length(startVars) != 1) stop("cannot find attribute usage data")
    x <- x[startVars:(length(x) - 1)]
    
    ## making some assumptions here
    x <- gsub("^\t", "", x)
    hasPct <- grep("%", x)
    if(length(hasPct) < 1) stop("cannot find attribute usage data")
    x <- x[hasPct]
    x <- gsub(" ", "", x)
    x <- strsplit(x, "%")
    getCond <- function(x)
      {
        if(length(x) == 2) return(0) else return(as.numeric(x[1]))
      }

    getModel <- function(x)
      {
        if(length(x) == 2) return(as.numeric(x[1])) else return(as.numeric(x[2]))
      }
    getVar <- function(x) x[length(x)]

    conds <- unlist(lapply(x, getCond))
    mods <- unlist(lapply(x, getModel))
    var <- unlist(lapply(x, getVar))
    
    data.frame(Conditions = conds,
               Model = mods,
               Variable = var)
    
  }
