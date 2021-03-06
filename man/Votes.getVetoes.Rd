\name{Votes.getVetoes}
\alias{Votes.getVetoes}
\title{Get a list of vetoes according to candidate}
\usage{
Votes.getVetoes(candidateId)
}
\arguments{
  \item{candidateId}{a character string or list of
  character strings with the candidate ID(s) (see
  references for details)}
}
\value{
A data frame with a row for each veto and columns with the
following variables describing the veto:\cr
bills.bill*.vetoId,\cr bills.bill*.statusDate,\cr
bills.bill*.billId,\cr bills.bill*.billNumber,\cr
bills.bill*.billTitle,\cr bills.bill*.vetoCode,\cr
bills.bill*.vetoType,\cr bills.bill*.billSummary,\cr
bills.bill*.billLink,\cr bills.bill*.vetoLetterLink.
}
\description{
This function is a wrapper for the Votes.getVetoes() method
of the PVS API Votes class which returns a list of vetoes
according to candidate. The function sends a request with
this method to the PVS API for all candidate IDs given as a
function input, extracts the XML values from the returned
XML file(s) and returns them arranged in one data frame.
}
\examples{
# First, make sure your personal PVS API key is saved as character string in the pvs.key variable:
\dontrun{pvs.key <- "yourkey"}
# get vetoes by Barack Obama
\dontrun{vetoes <- Votes.getVetoes(9490)}
\dontrun{vetoes}
}
\author{
Ulrich Matter <ulrich.matter-at-unibas.ch>
}
\references{
http://api.votesmart.org/docs/Votes.html\cr Use
Candidates.getByOfficeState(),
Candidates.getByOfficeTypeState(),
Candidates.getByLastname(), Candidates.getByLevenshtein(),
Candidates.getByElection(), Candidates.getByDistrict() or
Candidates.getByZip() to get a list of candidate IDs.
}

