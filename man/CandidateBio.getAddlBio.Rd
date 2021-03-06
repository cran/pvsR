\name{CandidateBio.getAddlBio}
\alias{CandidateBio.getAddlBio}
\title{Get a candidate's additional biographical information}
\usage{
CandidateBio.getAddlBio(candidateId)
}
\arguments{
  \item{candidateId}{a character string or list of
  character strings with the candidate ID(s) (see
  references for details)}
}
\value{
A data frame with a row for each candidate and columns with
the following variables describing the candidate:\cr
addlBio.candidate.shortTitle,\cr
addlBio.candidate.firstName,\cr
addlBio.candidate.nickName,\cr
addlBio.candidate.middleName,\cr
addlBio.candidate.lastName,\cr addlBio.candidate.suffix,\cr
addlBio.additional.item*.name,\cr
addlBio.additional.item*.data
}
\description{
This function is a wrapper for the
CandidateBio.getAddlBio() method of the PVS API
CandidateBio class which grabs the extended biographical
information for each candidate that has one. The function
sends a request with this method to the PVS API for all
candidate-IDs given as a function input, extracts the XML
values from the returned XML file(s) and returns them
arranged in one data frame.
}
\examples{
# First, make sure your personal PVS API key is saved as character string in the pvs.key variable:
\dontrun{pvs.key <- "yourkey"}
# get additional biographical data on Barack Obama
\dontrun{obama <- CandidateBio.getAddlBio(9490)}
\dontrun{obama}
# get additional biographical data on Barack Obama and Mitt Romney
\dontrun{onr <- CandidateBio.getAddlBio(list(9490,21942))}
\dontrun{onr}
}
\author{
Ulrich Matter <ulrich.matter-at-unibas.ch>
}
\references{
http://api.votesmart.org/docs/CandidateBio.html\cr Use
Candidates.getByOfficeState(),
Candidates.getByOfficeTypeState(),
Candidates.getByLastname(), Candidates.getByLevenshtein(),
Candidates.getByElection(), Candidates.getByDistrict() or
Candidates.getByZip() to get a list of candidate IDs.
}

