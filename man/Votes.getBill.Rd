\name{Votes.getBill}
\alias{Votes.getBill}
\title{Get general information on a bill}
\usage{
Votes.getBill(billId,...)
}
\arguments{
  \item{billId}{a character string or list of character
  strings with the bill ID(s) (see references for details)}

  \item{...}{further arguments that are passed on to
  internal functions. Currently the argument separate can
  be defined: separate is a vector of character strings
  defining subnodes that should be returned separately
  (e.g., "sponsors").}
}
\value{
If separate is not specified, a data frame with a row for
each bill and columns with variables describing the bill.
If separate is specified, a list containing several data
frames, one for each subnode mentioned in separate and
additionally one data frame containing all remaining nodes
not mentioned in separate. The returned data frame contains
a row for each bill and columns with the following
variables describing the bill:\cr bill.billnumber,\cr
bill.parentbill,\cr bill.title,\cr bill.officialtitle,\cr
bill.dateintroduced,\cr bill.type,\cr
bill.categories.category*.categoryId,\cr
bill.categories.category*.name,\cr bill.billtextLink,\cr
bill.sponsors.sponsor*.candidateId,\cr
bill.sponsors.sponsor*.name,\cr
bill.sponsors.sponsor*.type,\cr
bill.committeeSponsors.committeeSponsor*.committeeId,\cr
bill.committeeSponsors.committeeSponsor*.name,\cr
bill.actions.action*.actionId,\cr
bill.actions.action*.level,\cr
bill.actions.action*.stage,\cr
bill.actions.action*.outcome,\cr
bill.actions.action*.statusDate,\cr
bill.actions.action*.rollNumber,\cr
bill.actions.action*.yea,\cr bill.actions.action*.nay,\cr
bill.actions.action*.voiceVote,\cr
bill.amendments.amendment*.billNumber,\cr
bill.amendments.amendment*.actionId,\cr
bill.amendments.amendment*.title,\cr
bill.amendments.amendment*.statusDate.
}
\description{
This function is a wrapper for the Votes.getBill() method
of the PVS API Votes class which grabs the general
information on a bill. The function sends a request with
this method to the PVS API for all bill IDs given as a
function input, extracts the XML values from the returned
XML file(s) and returns them arranged in one data frame.
}
\examples{
# First, make sure your personal PVS API key is saved as character string in the pvs.key variable:
\dontrun{pvs.key <- "yourkey"}
# get information about certain bills
\dontrun{billinfo <- Votes.getBill(list(2819,6427))}
\dontrun{billinfo}
# let some variables with subnodes be returned separately (here: "sponsors" and "actions")
\dontrun{billinfo2 <- Votes.getBill(billId=list(2819,6427,6590),
separate=c("sponsors","actions"))}
\dontrun{billinfo2}
# check the sponsors of the requested bill (argument of separate)...
\dontrun{billinfo2$sponsors}
# ... and the usual variables describing the bill (nodes not mentioned in separate)
\dontrun{billinfo2$main}
}
\author{
Ulrich Matter <ulrich.matter-at-unibas.ch>
}
\references{
http://api.votesmart.org/docs/Votes.html\cr Use
Votes.getByBillNumber(),
Votes.getBillsByCategoryYearState(),
Votes.getBillsByYearState(),
Votes.getBillsByOfficialYearOffice(),
Votes.getBillsByOfficialCategoryOffice(),
Votes.getByOfficial(), Votes.getBillsBySponsorYear(),
Votes.getBillsBySponsorCategory() or
Votes.getBillsByStateRecent() to get a list of bill IDs.
}

