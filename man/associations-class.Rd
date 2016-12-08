\name{associations-class}
\docType{class}
\alias{associations-class}
\alias{associations}
%
\alias{initialize,associations-method}
\alias{summary.associations-class}
\alias{show,associations-method}
\alias{quality<-}
\alias{quality<-,associations-method}
\alias{quality}
\alias{quality,associations-method}
\alias{info<-}
\alias{info<-,associations-method}
\alias{info}
\alias{info,associations-method}
\alias{items,associations-method}
\alias{labels,associations-method}
\alias{length,associations-method}
\alias{t,associations-method}
\alias{\%in\%,associations,associations-method}
\alias{plot.associations}

\title{Class associations - A Set of Associations}
\description{
  The \code{associations} class is a virtual class which is extended to
  represent mining result (e.g., sets of itemsets or rules).  The class
  provides accessors for the quality slot and a method for sorting the
  associations.
}
\section{Objects from the Class}{
  A virtual class: No objects may be created from it.}
\section{Slots}{
  \describe{
    \item{\code{quality}:}{a data.frame for quality
      measures (e.g., interest measures as support or confidence).  Each
      quality measure is a named vector with the same length as the
      number of elements in the set of associations and each vector
      element belongs to the association with the same index.} 
    \item{\code{info}:}{a list which is used to store algorithm specific
      mining information. Typically it contains a least the elements
      \code{"data"} (name of the transaction data set), 
      \code{"ntransactions"} (length of the data set), 
      \code{"support"} (the minimum support used for mining).} 
  }
}
\section{Methods}{
  \describe{
    \item{info<-}{\code{signature(x = "associations")};
      replaces the info list.}
    \item{info}{\code{signature(x = "associations")};
      returns the info list.}
    \item{items}{\code{signature(x = "associations")};
      dummy method. This method has to be implemented by all subclasses
      of \code{associations} and return the items which make up each
      association as an object of class 
      \code{itemMatrix}.}
    \item{labels}{\code{signature(object = "associations")};
      dummy method. This method has to be implemented by all subclasses
      of \code{associations} and return a vector
      of \code{length(object)} of labels
      for the elements in the association.}
    \item{length}{\code{signature(x = "associations")};
      dummy method. This method has to be implemented by all subclasses
      of \code{associations} and return the number of elements in the 
      association.}
    \item{quality<-}{\code{signature(x = "associations")};
      replaces the quality data.frame.  The lengths of the vectors
      in the data.frame have to equal the number of associations
      in the set.}
    \item{quality}{\code{signature(x = "associations")};
      returns the quality data.frame.}
    \item{show}{\code{signature(object = "associations")}}
  }
}
\section{Subclasses}{
  \code{\link{itemsets-class}},
  \code{\link{rules-class}}
}
\seealso{
  \code{\link{sort}},
  \code{\link{write}},
  \code{\link{length}},
  \code{\link{is.subset}},
  \code{\link{is.superset}},
  \code{\link{sets}},
  \code{\link{unique}},
  \code{\link{itemMatrix-class}}
}
\author{Michael Hahsler}
\keyword{classes}
