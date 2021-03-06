\name{Officials.getByZip}
\alias{Officials.getByZip}
\title{Get a list of officials according to the ZIP code}
\usage{
Officials.getByZip(zip5, zip4=NULL)
}
\arguments{
  \item{zip5}{a character string or list of character
  strings with the five-digit ZIP code}

  \item{zip4}{(optional) a character string or list of
  character strings with the expanded ZIP+4 code (default:
  all)}
}
\value{
A data frame with a row for each official and columns with
the following variables describing the official:\cr
candidateList.zipMessage,\cr
candidateList.candidate*.candidateId,\cr
candidateList.candidate*.firstName,\cr
candidateList.candidate*.nickName,\cr
candidateList.candidate*.middleName,\cr
candidateList.candidate*.lastName,\cr
candidateList.candidate*.suffix,\cr
candidateList.candidate*.title,\cr
candidateList.candidate*.electionParties,\cr
candidateList.candidate*.electionstatus,\cr
candidateList.candidate*.officeParties,\cr
candidatelist.candidate*.officeStatus,\cr
candidateList.candidate*.officeDistrictId,\cr
candidateList.candidate*.officeDistrictName,\cr
candidateList.candidate*.officeTypeId,\cr
candidateList.candidate*.officeId,\cr
candidateList.candidate*.officeName,\cr
candidateList.candidate*.officeStateId.
}
\description{
This function is a wrapper for the Officials.getByZip()
method of the PVS API Officials class which grabs a list of
officials according to the ZIP code of the area they
represent. The function sends a request with this method to
the PVS API for all ZIP Codes given as a function input,
extracts the XML values from the returned XML file(s) and
returns them arranged in one data frame.
}
\examples{
# First, make sure your personal PVS API key is saved as character string in the pvs.key variable:
\dontrun{pvs.key <- "yourkey"}
# get a list of officials by ZIP code
\dontrun{officials <- Officials.getByZip(list(10001,10002))}
\dontrun{head(officials)}
}
\author{
Ulrich Matter <ulrich.matter-at-unibas.ch>
}
\references{
http://api.votesmart.org/docs/Officials.html
}

