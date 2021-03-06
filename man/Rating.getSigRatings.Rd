\name{Rating.getSigRatings}
\alias{Rating.getSigRatings}
\title{Get all ratings (scorecards) by a special interest group}
\usage{
Rating.getSigRatings(sigId)
}
\arguments{
  \item{sigId}{a character string or list of character
  strings with the special interest group's ID(s) (see
  references for details)}
}
\value{
A data frame with a row for each special interest group and
columns with the following variables describing the
rating:\cr sig.sigId,\cr sig.name,\cr rating*.ratingId,\cr
rating*.timespan,\cr rating*.ratingName,\cr
rating*.ratingText.
}
\description{
This function is a wrapper for the Rating.getSigRatings()
method of the PVS API Rating class which dumps all ratings
(scorecards) by a special interest group. The function
sends a request with this method to the PVS API for all
candidate and SIG IDs given as a function input, extracts
the XML values from the returned XML file(s) and returns
them arranged in one data frame.
}
\examples{
# First, make sure your personal PVS API key is saved as character string in the pvs.key variable:
\dontrun{pvs.key <- "yourkey"}
# get ratings of certain special interest groups
\dontrun{rating <- Rating.getSigRatings(list(568,1120,1704))}
\dontrun{rating}
}
\author{
Ulrich Matter <ulrich.matter-at-unibas.ch>
}
\references{
http://api.votesmart.org/docs/Rating.html\cr Use
Rating.getSigList() to get a list of special interest
group's IDs.
}

