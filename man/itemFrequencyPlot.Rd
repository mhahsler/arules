\name{itemFrequencyPlot}
\alias{itemFrequencyPlot}
\alias{itemFrequencyPlot,itemMatrix-method}
\title{
  Creating a Item Frequencies/Support Bar Plot
}
\description{
  Provides the generic function \code{itemFrequencyPlot} and the S4 method to
  create an item frequency bar plot for inspecting the item frequency
  distribution for objects based on \code{\linkS4class{itemMatrix}} (e.g.,
  \code{\linkS4class{transactions}}, or items in \code{\linkS4class{itemsets}}
  and \code{\linkS4class{rules}}).
}
\usage{
itemFrequencyPlot(x, \dots)
\S4method{itemFrequencyPlot}{itemMatrix}(x, type = c("relative", "absolute"), 
    weighted = FALSE, support = NULL, topN = NULL,
    population = NULL, popCol = "black", popLwd = 1,
    lift = FALSE, horiz = FALSE, 
    names = TRUE, cex.names =  graphics::par("cex.axis"), 
    xlab = NULL, ylab = NULL, mai = NULL, \dots)
}
\arguments{
  \item{x}{the object to be plotted.}
  \item{\dots}{further arguments are passed on (see  
  	\code{\link[graphics]{barplot}} from possible arguments).}
  \item{type}{ a character string indicating whether 
	item frequencies should be displayed 
  relative of absolute.}
  \item{weighted}{ should support be weighted by transactions weights stored
   as column \code{"weight"} in transactionInfo?} 
  \item{support}{a numeric value. Only display items which have a support of 
    at least \code{support}. If no population is given, support is calculated 
    from \code{x} otherwise from the population. Support is interpreted relative
    or absolute according to the setting of \code{type}.}
  \item{topN}{a integer value. Only plot the \code{topN} 
    items with the highest item frequency or lift (if \code{lift = TRUE}).
    The items are plotted ordered by descending support. }
  \item{population}{ object of same class  as \code{x}; if \code{x}
    is a segment of a population, the population mean frequency for
    each item can be shown as a line in the plot.}
  \item{popCol}{plotting color for population.}
  \item{popLwd}{line width for population.}
  \item{lift}{a logical indicating whether to plot the lift ratio between
  instead of frequencies. The lift ratio is gives how many times an item is
  more frequent in \code{x} than in \code{population}.}
  \item{horiz}{a logical. If \code{horiz = FALSE} (default), 
    the bars are drawn vertically. If \code{TRUE}, the bars are
    drawn horizontally.}
  \item{names}{a logical indicating if the names (bar labels) should be 
    displayed?}
  \item{cex.names}{a numeric value for the expansion factor for axis names (bar
    labels).}
  \item{xlab}{a character string with the label for the x axis 
    (use an empty string to force no label).}
  \item{ylab}{a character string with the label for the y axis (see xlab).}
  \item{mai}{a numerical vector giving the plots margin sizes in inches (see `? par').}
}
\value{
A numeric vector with the midpoints of the drawn bars; useful for
adding to the graph.
}
\seealso{
  \code{\link{itemFrequency}},
  \code{\link{itemMatrix-class}}
}
\author{Michael Hahsler}
\examples{
data(Adult)

## the following example compares the item frequencies
## of people with a large income (boxes) with the average in the data set
Adult.largeIncome <- Adult[Adult \%in\% 
	"income=large"]

## simple plot
itemFrequencyPlot(Adult.largeIncome)

## plot with the averages of the population plotted as a line 
## (for first 72 variables/items)
itemFrequencyPlot(Adult.largeIncome[, 1:72], 
	population = Adult[, 1:72])

## plot lift ratio (frequency in x / frequency in population)
## for items with a support of 20\% in the population
itemFrequencyPlot(Adult.largeIncome, 
        population = Adult, support = 0.2, 
	lift = TRUE, horiz = TRUE)
}
\keyword{hplot}
