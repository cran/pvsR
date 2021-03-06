\name{CandidateBio.getBio}
\alias{CandidateBio.getBio}
\title{Get a candidate's main biographical information}
\usage{
CandidateBio.getBio(candidateId)
}
\arguments{
  \item{candidateId}{a character string or list of
  character strings with the candidate ID(s) (see
  references for details)}
}
\value{
A data frame with a row for each candidate and columns with
the following variables describing the candidate:\cr
bio.candidate.crpId (OpenSecrets ID),\cr
bio.candidate.firstName,\cr bio.candidate.nickName,\cr
bio.candidate.middleName,\cr bio.candidate.lastName,\cr
bio.candidate.suffix,\cr bio.candidate.birthDate,\cr
bio.candidate.birthPlace,\cr
bio.candidate.pronunciation,\cr bio.candidate.gender,\cr
bio.candidate.family,\cr bio.candidate.photo,\cr
bio.candidate.homeCity,\cr bio.candidate.homeState,\cr
bio.candidate.education,\cr bio.candidate.profession,\cr
bio.candidate.political,\cr bio.candidate.religion,\cr
bio.candidate.congMembership,\cr
bio.candidate.orgMembership,\cr
bio.candidate.specialMsg,\cr bio.office.parties,\cr
bio.office.title,\cr bio.office.shortTitle,\cr
bio.office.name,\cr bio.office.type,\cr
bio.office.status,\cr bio.office.firstElect,\cr
bio.office.lastElect,\cr bio.office.nextElect,\cr
bio.office.termStart,\cr bio.office.termEnd,\cr
bio.office.district,\cr bio.office.districtId,\cr
bio.office.stateId,\cr
bio.office.committee*.committeeId,\cr
bio.office.committee*.committeeName,\cr
bio.election*.office,\cr bio.election*.officeId,\cr
bio.election*.officeType,\cr bio.election*.parties,\cr
bio.election*.district,\cr bio.election*.districtId,\cr
bio.election*.status,\cr bio.election*.ballotName.
}
\description{
This function is a wrapper for the CandidateBio.getBio()
method of the PVS API CandidateBio class which grabs the
main biographical information for each candidate. The
function sends a request with this method to the PVS API
for all candidate IDs given as a function input, extracts
the XML values from the returned XML file(s) and returns
them arranged in one data frame.
}
\examples{
# First, make sure your personal PVS API key is saved as character string in the pvs.key variable:
\dontrun{pvs.key <- "yourkey"}
# get main biographical data on Barack Obama and Mitt Romney
\dontrun{bio <- CandidateBio.getBio(list(9490,21942))}
\dontrun{bio}
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

