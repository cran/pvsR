pvsRequest <-
function (request,inputs) {
  pvs.url <- paste("http://api.votesmart.org/",request,"key=",get("get('pvs.key',envir=.GlobalEnv)",envir=.GlobalEnv),inputs,sep="") #generate url for request
  output <- t(xmlSApply(removeChildren(xmlRoot(xmlTreeParse(pvs.url,useInternalNodes=TRUE)),kids=1), function(x) xmlSApply(x, xmlValue)))
  data.frame(output, row.names=NULL)
  data.frame(output, row.names=NULL)

}
