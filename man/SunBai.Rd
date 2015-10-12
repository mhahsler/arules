\name{SunBai}
\alias{SunBai}
\alias{sunbai}
\docType{data}
\title{The SunBai Data Set}
\description{
    A small example database for weighted association rule mining
    provided as an object of class
    \code{\link[arules:transactions-class]{transactions}}.
}
\usage{data(SunBai)}
\details{
    The data set contains the example database described in the
    paper by K. Sun and F.Bai for illustration of the concepts of
    weighted association rule mining. \code{weight} 
    stored in \code{transactionInfo}
    denotes the transaction weights obtained using the HITS algorithm.
}
\source{
    K. Sun and F. Bai (2008). Mining Weighted Association Rules
    without Preassigned Weights. \emph{IEEE Transactions on Knowledge
    and Data Engineering}, 4 (30), 489--495.
}
\seealso{
    Class
    \code{\link[arules:transactions-class]{transactions}},
    method 
    \code{\link[arules:transactionInfo]{transactionInfo}},
    function 
    \code{\link{hits}}.
}
\examples{
data(SunBai)
summary(SunBai)
inspect(SunBai)

transactionInfo(SunBai)
}
\keyword{datasets}
