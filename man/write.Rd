\name{write}
\alias{write}
\alias{write.csv}
\alias{write.table}
\alias{write,transactions-method}
\alias{write,associations-method}
\alias{write,ANY-method} % redirects to base::write
\title{
  Writes transactions or associations to disk
}
\description{
  Provides the generic function \code{write} and the S4 methods to 
  write transactions or associations (itemsets, rules) to disk.  
}
\usage{
write(x, file = "",\dots)
\S4method{write}{transactions}(x, file="", format = c("basket", "single"),
                sep=" ", quote=TRUE, ...)
\S4method{write}{associations}(x, file="", sep=" ", quote=TRUE, ...)
}
\arguments{
  \item{x}{the transactions or associations (rules, itemsets, etc.) object.}
  \item{file}{either a character string naming a file or a connection open
            for writing.  '""' indicates output to the console.}
 \item{format}{format to write transactions.}
 \item{sep}{the field separator string. Values within each row of x
           are separated by this string. Use \code{quote=TRUE} and 
	   \code{sep=","} 
	   for saving data as in csv format.}
 \item{quote}{a logical value. Quote fields?}
  \item{\dots}{further arguments passed on to \code{\link{write.table}}
  or \code{\link{write}}. Use \code{fileEncoding} to set the encoding used for
  writing the file.}
}
\details{
  For associations (rules and itemsets) \code{write} first uses coercion 
  to data.frame to obtain a printable form of
  \code{x} and then uses \code{\link{write.table}} to write the data to disk.

  Transactions can be saved in basket (one line per transaction) or in 
  single (one line per item) format. 

  Note: To save and load associations in compact form, use \code{save} and
  \code{load} from the \pkg{base} package.  Alternatively, association can be
  written to disk in PMML (Predictive Model Markup Language)
  via \code{write.PMML}. This requiresvpackages \pkg{pmml}.
}
\seealso{
\code{\link{read.transactions}} for reading transactions from a file,
\code{\link{read.PMML}} and \code{\link{write.PMML}}
for reading/writing associations in PMML format,
\code{\link{write.table}} (in \pkg{base}),
 \code{\link{transactions-class}},
 \code{\link{associations-class}}
}
\author{Michael Hahsler}
\examples{
data("Epub")

## write the formated transactions to screen (basket format)
write(head(Epub))

## write the formated transactions to screen (single format)
write(head(Epub), format="single")

## write the formated result to file in CSV format
write(Epub, file = "data.csv", format="single", sep = ",")

## write rules in CSV format
rules <- apriori(Epub, parameter=list(support=0.0005, conf=0.8))
write(rules, file = "data.csv", sep = ",")

unlink("data.csv") # tidy up
}
\keyword{file}
