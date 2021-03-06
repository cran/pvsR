\name{Committee.getCommitteeMembers}
\alias{Committee.getCommitteeMembers}
\title{Get a list of members of a committee}
\usage{
Committee.getCommitteeMembers(committeeId)
}
\arguments{
  \item{committeeId}{a character string or list of
  character strings with the committee ID(s) (see
  references for details)}
}
\value{
A data frame with a row for each committee member and
columns with the following variables describing the
committee member:\cr
committeeMembers.committee.committeeId,\cr
committeeMembers.committee.parentId,\cr
committeeMembers.committee.name,\cr
committeeMembers.member*.candidateId,\cr
committeeMembers.member*.title,\cr
committeeMembers.member*.firstName,\cr
committeeMembers.member*.middleName,\cr
committeeMembers.member*.lastName,\cr
committeeMembers.member*.suffix,\cr
committeeMembers.member*.party,\cr
committeeMembers.member*.position.
}
\description{
This function is a wrapper for the
Committee.getCommitteeMembers() method of the PVS API
Committee class which returns a list of members of a
committee. The function sends a request with this method to
the PVS API for all committee IDs given as a function
input, extracts the XML values from the returned XML
file(s) and returns them arranged in one data frame.
}
\examples{
# First, make sure your personal PVS API key is saved as character string in the pvs.key variable:
\dontrun{pvs.key <- "yourkey"}
# get a list of members of certain committees
\dontrun{comember <- Committee.getCommitteeMembers(1)}
\dontrun{comember}
}
\author{
Ulrich Matter <ulrich.matter-at-unibas.ch>
}
\references{
http://api.votesmart.org/docs/Committee.html\cr Use
CandidateBio.getBio(), Committee.getCommitteesByTypeState()
or Votes.getBill() to get committee ID(s).
}

