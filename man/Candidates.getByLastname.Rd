\name{Candidates.getByLastname}
\alias{Candidates.getByLastname}
\title{Get a list of candidates according to their lastname}
\usage{
Candidates.getByLastname(lastName, electionYear=NULL)
}
\arguments{
  \item{lastName}{a character string or list of character
  strings with the last name(s) (see references for
  details)}

  \item{electionYear}{(optional) a character string or list
  of character strings with the election year(s) (default:
  >= current year)}
}
\value{
A data frame with a row for each candidate and year and
columns with the following variables describing the
candidate:\cr candidateList.candidate*.candidateId,\cr
candidateList.candidate*.firstName,\cr
candidateList.candidate*.nickName,\cr
candidateList.candidate*.middleName,\cr
candidateList.candidate*.preferredName,\cr
candidateList.candidate*.lastName,\cr
candidateList.candidate*.suffix,\cr
candidateList.candidate*.title,\cr
candidateList.candidate*.ballotName,\cr
candidateList.candidate*.electionParties,\cr
candidateList.candidate*.electionStatus,\cr
candidateList.candidate*.electionStage,\cr
candidateList.candidate*.electionDistrictId,\cr
candidateList.candidate*.electionDistrictName,\cr
candidateList.candidate*.electionOffice,\cr
candidateList.candidate*.electionOfficeId,\cr
candidateList.candidate*.electionStateId,\cr
candidateList.candidate*.electionOfficeTypeId,\cr
candidateList.candidate*.electionYear,\cr
candidateList.candidate*.electionSpecial,\cr
candidateList.candidate*.electionDate,\cr
candidateList.candidate*.officeParties,\cr
candidateList.candidate*.officeStatus,\cr
candidateList.candidate*.officeDistrictId,\cr
candidateList.candidate*.officeDistrictName,\cr
candidateList.candidate*.officeStateId,\cr
candidateList.candidate*.officeId,\cr
candidateList.candidate*.officeName,\cr
candidateList.candidate*.officeTypeId,\cr
candidateList.candidate*.runningMateId,\cr
candidateList.candidate*.runningMateName.
}
\description{
This function is a wrapper for the
Candidates.getByLastname() method of the PVS API Candidates
class which grabs a list of candidates according to a
lastname match. The function sends a request with this
method to the PVS API for all last names and election years
given as a function input, extracts the XML values from the
returned XML file(s) and returns them arranged in one data
frame.
}
\examples{
# First, make sure your personal PVS API key is saved as character string in the pvs.key variable:
\dontrun{pvs.key <- "yourkey"}
# get candidates by last names
\dontrun{names <- Candidates.getByLastname(list("Obama","Romney"),2012)}
\dontrun{names}
}
\author{
Ulrich Matter <ulrich.matter-at-unibas.ch>
}
\references{
http://api.votesmart.org/docs/Candidates.html\cr Use
CandidateBio.getBio(), Candidates.getByOfficeState(),
Candidates.getByOfficeTypeState(),
Candidates.getByElection(), Candidates.getByDistrict(),
Candidates.getByZip(), Committee.getCommitteeMembers(),
Election.getStageCandidates(), Leadership.getOfficials(),
Local.getOfficials(), Officials.getStatewide(),
Officials.getByOfficeState(),
Officials.getByOfficeTypeState(), Officials.getByDistrict()
or Officials.getByZip() to get last name(s).
}

