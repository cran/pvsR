pvsRequest4.1 <-
function (request,inputs) {
  pvs.url <- paste("http://api.votesmart.org/",request,"key=",get('pvs.key',envir=.GlobalEnv),inputs,sep="") #generate url for request
  
  if (names(xmlRoot(xmlTreeParse(pvs.url,useInternalNodes=TRUE)))[1]=="errorMessage") {
    
    # if the requested data is not available, return an empty (NA) data frame and give a warning
    warning(gsub(pattern="&", replacement=" ", x=paste("No data available for: ", inputs,". The corresponding rows in the data frame are filled with NAs.", sep=""), fixed=TRUE), call.=FALSE)
    
    
    output.df <- data.frame(matrix(nrow=1,ncol=0))
    output.df
    

    } else {
  
  output <- xmlRoot(xmlTreeParse(pvs.url,useInternalNodes=TRUE))
  
  
  nodenames <- names(output) # get names of nodes
  
  # remove unnecessary child-nodes
    if (nodenames[1]=="generalInfo") {
    
    
    output <- removeChildren(output,kids="generalInfo")
    
  } else { if (nodenames[1]=="generalinfo") {
    
    output <- removeChildren(output,kids="generalinfo")
    
  }
  }
  
  
  
  output.df <- data.frame(t(xmlSApply(output,xmlValue)))
  output.df

  }
}
