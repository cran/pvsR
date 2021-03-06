\name{Election.getStageCandidates}
\alias{Election.getStageCandidates}
\title{Get district basic election data according to election ID and stage ID}
\usage{
Election.getStageCandidates(stageId, electionId)
}
\arguments{
  \item{electionId}{a character string or list of character
  strings with the election ID(s) (see references for
  details)}

  \item{stageId}{a character string or list of character
  strings with the stage ID(s)}
}
\value{
A data frame with a row for each combination of stage ID
and election and columns with the following variables
describing the election:\cr
stagecandidates.election*.electionId,\cr
stagecandidates.election*.name,\cr
stagecandidates.election*.stage,\cr
stagecandidates.election*.stateId,\cr
stagecandidates.candidate*.candidateId,\cr
stagecandidates.candidate*.officeId,\cr
stagecandidates.candidate*.district,\cr
stagecandidates.candidate*.firstName,\cr
stagecandidates.candidate*.middleName,\cr
stagecandidates.candidate*.lastName,\cr
stagecandidates.candidate*.suffix,\cr
stagecandidates.candidate*.party,\cr
stagecandidates.candidate*.status,\cr
stagecandidates.candidate*.voteCount,\cr
stagecandidates.candidate*.votePercent.
}
\description{
This function is a wrapper for the
Election.getStageCandidates() method of the PVS API
Election class which grabs district basic election data
according to the election and stage ID. The function sends
a request with this method to the PVS API for all election
and stage IDs given as a function input, extracts the XML
values from the returned XML file(s) and returns them
arranged in one data frame.
}
\examples{
# First, make sure your personal PVS API key is saved as character string in the pvs.key variable:
\dontrun{pvs.key <- "yourkey"}
# get election data for certain election IDs
\dontrun{election <- Election.getStageCandidates(stageId=list("P"),electionId=list(2582,2607))}
\dontrun{election}
}
\author{
Ulrich Matter <ulrich.matter-at-unibas.ch>
}
\references{
http://api.votesmart.org/docs/Election.html\cr Use
Election.getElectionByYearState() or
Election.getElectionByZip() to get election ID(s).
}

