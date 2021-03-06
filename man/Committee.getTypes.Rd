\name{Committee.getTypes}
\alias{Committee.getTypes}
\title{Get the committee types (house, senate, joint, etc.)}
\usage{
Committee.getTypes()
}
\value{
A data frame with a row for each committee type and columns
with the following variables describing the committee
type:\cr committeeTypes.type*.committeeTypeId,\cr
committeeTypes.type*.name.
}
\description{
This function is a wrapper for the Committee.getTypes()
method of the PVS API Committee class which returns the
existing committee types.
}
\examples{
# First, make sure your personal PVS API key is saved as character string in the pvs.key variable:
\dontrun{pvs.key <- "yourkey"}
# get the committee types
\dontrun{comtypes <- Committee.getTypes()}
\dontrun{comtypes}
}
\author{
Ulrich Matter <ulrich.matter-at-unibas.ch>
}
\references{
http://api.votesmart.org/docs/Committee.html
}

