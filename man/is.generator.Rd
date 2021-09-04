\name{is.generator}
\alias{is.generator}
\alias{is.generator,itemsets-method}
%
\title{Find Generator Itemsets}
\description{
Provides the generic function and the S4 method \code{is.generator} for finding
generator itemsets. Generators are part of concise representations for frequent itemsets. 
A generator in a set of itemsets is an itemset that has no
subset with the same support (Liu et al, 2008). Note that the empty set is by definition 
a generator, but it is typicaly not stored in the itemsets in \pkg{arules}.
}
\usage{
is.generator(x)
}
\arguments{
  \item{x}{ a set of itemsets. }
}
\value{a logical vector with the same length as \code{x} indicating
for each element in \code{x} if it is a generator itemset.
}
\seealso{
\code{\link{itemsets-class}},
\code{\link{is.closed}},
\code{\link{is.maximal}}
}
\references{
Yves Bastide, Niolas Pasquier, Rafik Taouil, Gerd Stumme, Lotfi Lakhal (2000).
Mining Minimal Non-redundant Association Rules Using Frequent Closed Itemsets.
In \emph{International Conference on Computational Logic}, Lecture Notes in Computer Science (LNCS 1861). pages  972--986. \doi{10.1007/3-540-44957-4_65}

Guimei Liu, Jinyan Li, Limsoon Wong (2008).
A new concise representation of frequent itemsets using generators and a positive border. 
\emph{Knowledge and Information Systems} 17(1):35-56.
\doi{10.1007/s10115-007-0111-5}
}
\author{Michael Hahsler}
\examples{
# Example from Liu et al (2008)
trans_list <- list(
      t1 = c("a","b","c"),
      t2 = c("a","b", "c", "d"),
      t3 = c("a","d"),
      t4 = c("a","c")
      )

trans <- transactions(trans_list)
its <- apriori(trans, support = 1/4, target = "frequent itemsets")

is.generator(its)
}
\keyword{models}
