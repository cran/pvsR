\name{getAllBios}
\alias{getAllBios}
\title{Get several candidates' biographical information}
\usage{
getAllBios(candidateId, batchsize=100, pause=0, backupfile="bios.list.Rdata")
}
\arguments{
  \item{candidateId}{a character string or list of
  character strings with the candidate ID(s) (see
  references for details)}

  \item{batchsize}{numerical, indicating how many
  candidateIds should be processed in one batch (defaults
  to 100).}

  \item{pause}{numerical, indicating how long (in seconds)
  the download process should be paused after each batch
  (defaults to 0)}

  \item{backupfile}{character string for the path/file-name
  of the Rdata-file where the data should be saved
  (batch-wise) during the download process (default:
  "bios.list.Rdata").}
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
This function is essentially a wrapper around
CandidateBio.getBio() specified for large amount of
requests.
}
\details{
This functions splits large requests into several batches.
The requests are then processed batch-wise and are saved on
the local disc to make sure that not too much RAM is
assigned to the pvsR task.
}
\examples{
# First, make sure your personal PVS API key is saved as character string in the pvs.key variable:
\dontrun{pvs.key <- "yourkey"}
# get all officials of a certain state
\dontrun{officials <- Officials.getStatewide("FL")}
# get all biographical information on those officials
\dontrun{bios <- getAllBios(officials$candidateId[1:100], batchsize=20)}
\dontrun{head(bios)}
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

