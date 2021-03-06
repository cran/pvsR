\name{Election.getElectionByZip}
\alias{Election.getElectionByZip}
\title{Get district basic election data according to the ZIP code}
\usage{
Election.getElectionByZip(zip5, zip4=NULL, year=NULL)
}
\arguments{
  \item{zip5}{a character string or list of character
  strings with the five-digit ZIP code}

  \item{zip4}{(optional) a character string or list of
  character strings with the expanded ZIP+4 code (default:
  NULL)}

  \item{year}{a character string or list of character
  strings with the year (defaults to current year)}
}
\value{
A data frame with a row for each election and columns with
the following variables describing the election:\cr
elections.election*.electionId,\cr
elections.election*.name,\cr
elections.election*.stateId,\cr
elections.election*.officeTypeId,\cr
elections.election*.special,\cr
elections.election*.electionYear.
}
\description{
This function is a wrapper for the
Election.getElectionByZip() method of the PVS API Election
class which grabs district basic election data according to
the ZIP code. If another year than the current year is
chosen, all election data from that year up to the current
year is returned. The function sends a request with this
method to the PVS API for all ZIP codes given as a function
input, extracts the XML values from the returned XML
file(s) and returns them arranged in one data frame.
}
\examples{
# First, make sure your personal PVS API key is saved as character string in the pvs.key variable:
\dontrun{pvs.key <- "yourkey"}
# get election data by ZIP code
\dontrun{election <- Election.getElectionByZip(zip5=list(10001,10002), year="2012")}
\dontrun{election}
}
\author{
Ulrich Matter <ulrich.matter-at-unibas.ch>
}
\references{
http://api.votesmart.org/docs/Election.html
}

