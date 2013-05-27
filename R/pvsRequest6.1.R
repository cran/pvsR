pvsRequest6.1 <-
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
    
    
    # if there are some nodes with one entry, and others with many, the ones with one contain data that belongs to every single entry of the one
    # with several entries (like invariant variable over different observations, --> macrodata)
    
    # hence, the following approach: 
    
    
    # check if there are several nodes with the same name
    
    if (sum(freq.names)>length(nodenames)) { 
      
      
      freq0 <- unique(names(freq.names[freq.names==0]))
      freq1 <- unique(names(freq.names[freq.names==1]))
      freqh <- unique(names(freq.names[freq.names>1]))
      
      # ignore generalInfo:
      freq1b <- freq1[freq1!="generalInfo"]
      freq1b <- freq1[freq1!="generalinfo"]
      
      
      
      # scrap all data from nodes that come up once, and cbind the resulting dfs
      freq1.list <- lapply(freq1b, FUN=function(i) {
        
        x <- output[[i]]
        
                
        i.df <- data.frame(t(xmlSApply(x, xmlValue)))
        
        try(if (names(x)=="text")     names(i.df) <- i, silent=TRUE )
        
        i.df
        
      })
      
      freq1.df <- do.call("cbind", freq1.list)
      
      
      # remove freq1-nodes from output.
      
      for (i in freq1)   output <- removeChildren(output,i)
      
      
      
      # now scrap the remaining output
      
      output.list <- lapply(1:length(names(output)), FUN=function(i) {
        
        x <- output[[i]]
        
        data.frame(t(xmlSApply(x, xmlValue)))
        
        
      })
      
      
      output2 <- dfList(output.list)
      
      output2 <- cbind(freq1.df,output2)
      output2
      
    } else {
      
      output <- t(xmlSApply(removeChildren(xmlRoot(xmlTreeParse(pvs.url,useInternalNodes=TRUE)),kids=1), function(x) xmlSApply(x, xmlValue)))
      output.df <- data.frame(output, row.names=NULL)
      output.df
      
    }
    
    
  }
}
