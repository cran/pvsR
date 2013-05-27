pvsRequest7 <-
function (request,inputs) {
  pvs.url <- paste("http://api.votesmart.org/",request,"key=",get('pvs.key',envir=.GlobalEnv),inputs,sep="") #generate url for request
  
  if (names(xmlRoot(xmlTreeParse(pvs.url,useInternalNodes=TRUE)))[1]=="errorMessage") {
    
    # if the requested data is not available, return an empty (NA) data frame and give a warning
    warning(gsub(pattern="&", replacement=" ", x=paste("No data available for: ", inputs,". The corresponding rows in the data frame are filled with NAs.", sep=""), fixed=TRUE), call.=FALSE)
    
    
    output.df <- data.frame(matrix(nrow=1,ncol=0))
    output.df
    
    
  } else {
    
    
  
    
    output <- xmlRoot(xmlTreeParse(pvs.url,useInternalNodes=TRUE))
    
    nodenames <- unique(names(output)) # get names of nodes
    
    freq.names <- summary(as.factor(names(output))) # check frequency,
    
    freq.subnames <- sapply(names(output), FUN=function(x) sum(summary(as.factor(names(output[[x]]))) )  )
    
    
       
     
        # if the subnodes have more than one entry (child) scrap them (individually), then cbind
    
    nodenames <- nodenames[nodenames!="generalInfo"]
    nodenames <- nodenames[nodenames!="generalinfo"]
    
    if (length(unique(names(output)))>1) {
        
        output.list <- lapply(nodenames, FUN=function(i) {
          
          
          if (freq.subnames[[i]]>1) {
            
            x <- output[[i]]
            
            data.frame(t(xmlSApply(x, xmlValue)))
            
          } else {
            
            NA
          }
          
          
        })
        
        
        output.df <- do.call("cbind", output.list)
        
        output.df
        
      
      
      
    } else {
      
      output <- t(xmlSApply(removeChildren(xmlRoot(xmlTreeParse(pvs.url,useInternalNodes=TRUE)),kids=1), function(x) xmlSApply(x, xmlValue)))
      output.df <- data.frame(output, row.names=NULL)
      output.df
      
    }
    
    
  }
}
