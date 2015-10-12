\name{AScontrol-classes}
\docType{class}
\alias{AScontrol-class}
\alias{AScontrol}
\alias{APcontrol-class}
\alias{APcontrol}
\alias{ECcontrol-class}
\alias{ECcontrol}
\alias{show,AScontrol-method}
\alias{initialize,AScontrol-method}
\alias{coerce,NULL,APcontrol-method}
\alias{coerce,list,APcontrol-method}
\alias{coerce,NULL,ECcontrol-method}
\alias{coerce,list,ECcontrol-method}

\title{Classes ``AScontrol'', ``APcontrol'', ``ECcontrol'' ---
  Specifying the `control' Argument of apriori() and eclat()}
\description{
  The \code{AScontrol} class holds the algorithmic parameters for the used
  mining algorithms.  \code{APcontrol} and \code{ECcontrol} directly extend
  \code{AScontrol} with additional slots for parameters only suitable for the
  algorithms Apriori (\code{APcontrol}) and Eclat (\code{ECcontrol}).
}
\section{Objects from the Class}{
  A suitable default control object will be automatically created by
  the \code{\link{apriori}} or the \code{\link{eclat}} function.
  By specifying a named list (names equal to slots) as \code{control}
  argument for the \code{\link{apriori}} or the \code{\link{eclat}}
  function, default values can be replaced by the values in the list.
  Objects can also be created by calls of the form
  \code{new("APcontrol", ...)} or \code{new("ECcontrol", ...)}.
}
\section{Slots}{
  Common slots defined in \code{AScontrol}:
  \describe{
    \item{\code{sort}:}{ an integer scalar indicating how to 
      sort items with respect to their frequency: (default: 2)
      \describe{
	\item{ 1:}{ascending}
	\item{-1:}{descending}
	\item{ 0:}{do not sort}
        \item{ 2:}{ascending}
	\item{-2:}{descending with respect to transaction size sum}
      }
    }
    \item{\code{verbose}:}{a logical indicating whether to
      report progress}
  }
  
  Additional slots for Apriori in \code{APcontrol}:
  \describe{
    \item{\code{filter}:}{a numeric scalar indicating how to
      filter unused items from transactions (default: 0.1)
      \describe{
	\item{\eqn{=0}:}{do not filter items with respect to. usage in
	  sets}
	\item{\eqn{<0}:}{fraction of removed items for filtering}
	\item{\eqn{>0}:}{take execution times ratio into account}
      }
    }
    \item{\code{tree}:}{a logical indicating whether to
      organize transactions as a prefix tree (default: \code{TRUE})}
    \item{\code{heap}:}{a logical indicating whether to
      use heapsort instead of quicksort to sort the transactions
      (default: \code{TRUE})}
    \item{\code{memopt}:}{a logical indicating whether to 
      minimize memory usage instead of maximize speed (default:
      \code{FALSE})}
    \item{\code{load}:}{a logical indicating whether to
      load transactions into memory (default: \code{TRUE})}
  }
  
  Additional slots for Eclat in \code{ECcontrol}:
  \describe{
    \item{\code{sparse}:}{a numeric value for the
      threshold for sparse representation (default: 7)}
  }
}
\section{Methods}{
  \describe{
    \item{coerce}{\code{signature(from = "NULL", to = "APcontrol")}}
    \item{coerce}{\code{signature(from = "list", to = "APcontrol")}}
    \item{coerce}{\code{signature(from = "NULL", to = "ECcontrol")}}
    \item{coerce}{\code{signature(from = "list", to = "ECcontrol")}}
  }
}
\references{
  Christian Borgelt (2004)
  \emph{Apriori --- Finding Association Rules/Hyperedges with the Apriori
    Algorithm}.
  \url{www.borgelt.net/apriori.html}
}
\seealso{
  \code{\link{apriori}},
  \code{\link{eclat}}
}
\author{Michael Hahsler and Bettina Gruen}
\keyword{classes}
