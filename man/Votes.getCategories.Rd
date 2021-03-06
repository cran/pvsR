\name{Votes.getCategories}
\alias{Votes.getCategories}
\title{Get a list of categories that contain released bills according to year and state}
\usage{
Votes.getCategories(year, stateId="NA")
}
\arguments{
  \item{year}{a character string or list of character
  strings with the year(s)}

  \item{stateId}{(optional) a character string or list of
  character strings with the state ID(s) (default: "NA",
  for national) (see references for details)}
}
\value{
A data frame with a row for each category and columns with
the following variables describing the category:\cr
categories.category*.categoryId,\cr
categories.category*.name.
}
\description{
This function is a wrapper for the Votes.getCategories()
method of the PVS API Votes class which dumps categories
that contain released bills according to year and state.
The function sends a request with this method to the PVS
API for all years and state IDs given as a function input,
extracts the XML values from the returned XML file(s) and
returns them arranged in one data frame.
}
\examples{
# First, make sure your personal PVS API key is saved as character string in the pvs.key variable:
\dontrun{pvs.key <- "yourkey"}
# get a list of categories
\dontrun{categories <- Votes.getCategories(2012)}
\dontrun{categories}
}
\author{
Ulrich Matter <ulrich.matter-at-unibas.ch>
}
\references{
http://api.votesmart.org/docs/Votes.html\cr Use
State.getStateIDs() to get a list of state IDs.
}

