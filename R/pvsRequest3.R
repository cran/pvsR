pvsRequest3 <-
function (request,inputs) {
  pvs.url <- paste("http://api.votesmart.org/",request,"key=",get('pvs.key',envir=.GlobalEnv),inputs,sep="") #generate url for request
output.base <- xmlRoot(xmlTreeParse(pvs.url, useInternalNodes=TRUE))
output <- xmlSApply(output.base, function(x) data.frame(t(xmlSApply(x, xmlValue))))
output  
}
