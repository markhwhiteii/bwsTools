% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prscoring.R
\name{prscoring}
\alias{prscoring}
\title{Page Rank Method to Calculate Individual Best-Worst Scores}
\usage{
prscoring(data, id, block, item, choice, ..., wide = FALSE)
}
\arguments{
\item{data}{A data.frame of the type described in details.}

\item{id}{A string of the name of the id column.}

\item{block}{A string of the name of the block column.}

\item{item}{A string of the name of the item column.}

\item{choice}{A string of the name of the choice column.}

\item{...}{Additional arguments to igraph::page_rank()}

\item{wide}{Logical of whether or not one wants the data returned in long
(each row is an item-respondent combination and all best-worst scores are
in the same column) format (FALSE) or in wide format (where each row is a 
respondent, and the best-worst scores for the items are in their own 
columns). See the `indiv` data as an example.}
}
\value{
A data.frame containing the id and item columns as well as a "walk" column
  that indicates the best worst score. If `wide = TRUE`, then each item
  has its own column and the walkscore is filled-in those columns.
}
\description{
Calculate best-worst scores for each respondent-item combination. This uses
  the page rank method. It works virtually the same as the walkscoring
  method; the only difference is that there is a parameter for randomly
  "teleporting" to a random node. This can help prevent random walks
  from getting "stuck" in certain areas. Page rank scores using are
  calculated and synthesized in the same way as the method used in
  walkscoring(). See White (2019).
}
\details{
This function requires data to be in a specified format. Each row must
  represent a respondent-block-label combination. That is, it indicates
  the person, the block (or trial), the item that was judged, and a column
  indicating whether it was chosen as best (+1), worst (-1), or wasn't 
  selected as either (0).
}
\examples{
\dontrun{
data(indiv)
head(indiv)
prscoring(indiv, "id", "block", "label", "value")
}

}
\references{
Brin, S., & Page, L. (1998). The anatomy of a large-scale hypertextual
  Web search engine. Computer Networks and ISDN Systems, 30(1), 107-117.

Gleich, D. F. (2014). PageRank beyond the Web. arxiv.org/abs/1407.5107

White, M. H., II. (2019). bwsTools: An R package for case 1 best-worst
  scaling. Retrieved from https://osf.io/xftvq/
}
