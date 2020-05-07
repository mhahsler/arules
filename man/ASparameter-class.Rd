\name{ASparameter-classes}
\docType{class}
\alias{ASparameter-class}
\alias{ASparameter}
\alias{APparameter-class}
\alias{APparameter}
\alias{ECparameter-class}
\alias{ECparameter}
\alias{initialize,ASparameter-method}
\alias{initialize,APparameter-method}
\alias{initialize,ECparameter-method}
\alias{show,ASparameter-method}
\alias{show,APparameter-method}
\alias{coerce,NULL,APparameter-method}
\alias{coerce,list,APparameter-method}
\alias{coerce,NULL,ECparameter-method}
\alias{coerce,list,ECparameter-method}

\title{Classes ASparameter, APparameter, ECparameter ---
  Specifying the parameter Argument of apriori() and eclat()}
\description{
  The \code{ASparameter} class holds the mining parameters (e.g., minimum
  support) for the used mining algorithms.  \code{APparameter} and
  \code{ECparameter} directly extend \code{ASparameter} with additional slots
  for parameters only suitable for the Apriori (\code{APparameter}) or the
  Eclat algorithms (\code{ECparameter}).
}
\section{Objects from the Class}{
  A suitable default parameter object will be automatically created by
  the \code{\link{apriori}} or the \code{\link{eclat}} function.  By
  specifying a named list (names equal to slots) as \code{parameter}
  argument for the \code{\link{apriori}} or the  \code{\link{eclat}}
  function, default values can be replaced by the values in the list.
  Objects can be created by calls of the form \code{new("APparameter",
    ...)} or \code{new("ECparameter", ...)}.
}
\section{Slots}{
  Common slots defined in \code{ASparameter}:
  \describe{
    \item{\code{support}:}{a numeric value for the 
      minimal support of an item set (default: \eqn{0.1})}
    \item{\code{minlen}:}{an integer value for the
      minimal number of items per item set (default: 1 item)}
    \item{\code{maxlen}:}{an integer value for the
      maximal number of items per item set (default: 10 items)}
    \item{\code{target}:}{a character string 
      indicating the type of association mined.
      One of
      \itemize{
	\item \code{"frequent itemsets"} 
	\item \code{"maximally frequent itemsets"}
	\item \code{"closed frequent itemsets"}
	\item \code{"rules"} (only available for Apriori; 
	     use \code{\link{ruleInduction}} for \code{eclat}.) 
 	\item \code{"hyperedgesets"} (only available for Apriori; 
	see references for the definition of association hyperedgesets)
      }
    }
    \item{\code{ext}:}{a logical indicating whether to
      report coverage (i.e., LHS-support) as an extended quality measure (default: \code{TRUE})}
  }

  Additional slots for Apriori in \code{APparameter}:
  \describe{
    \item{\code{confidence}:}{a numeric value for the
      minimal confidence of rules/association hyperedges (default:
      \eqn{0.8}). For frequent itemsets it is set to \code{NA}.}
    \item{\code{smax}:}{a numeric value for the
      maximal support of itemsets/rules/hyperedgesets (default: 1)}
    \item{\code{arem}:}{a character string indicating the used
      additional rule evaluation measure (default: \code{"none"}) given 
        by one of
      \describe{
	\item{\code{"none"}:}{no additional evaluation measure}
	\item{\code{"diff"}:}{absolute confidence difference}
	\item{\code{"quot"}:}{difference of confidence quotient to 1}
	\item{\code{"aimp"}:}{absolute difference of improvement to 1}
	\item{\code{"info"}:}{information difference to prior}
	\item{\code{"chi2"}:}{normalized \eqn{\chi^2} measure}
      }
    \bold{Note:} The measure is only reported if \code{aval} is set to \code{TRUE}. Use
    \code{minval} to set minimum thresholds on the measures.
    }
    \item{\code{aval}:}{a logical indicating whether to
      return the additional rule evaluation measure selected with
      \code{arem}.}
    \item{\code{minval}:}{a numeric value for the
      minimal value of additional evaluation measure selected with
      \code{arem} (default: \eqn{0.1})}
    \item{\code{originalSupport}:}{a logical indicating whether to
      use the original definition of minimum support 
      (support of the LHS and RHS of the rule). If set to \code{FALSE}
      then a minimum threshold on coverage (i.e., the support of the LHS) is used 
      instead. (default: \code{TRUE})}
    \item{\code{maxtime}:}{ Time limit in seconds for checking subsets. 
    \code{maxtime = 0} disables the time limit.  
      (default: 5 seconds)}
  }
  
  Additional slots for Eclat in \code{ECparameter}:
  \describe{
    \item{\code{tidLists}:}{a logical indicating whether to
      return also a list of supporting transactions (transaction IDs)
      (default: \code{FALSE})}
  }
}
\section{Methods}{
  \describe{
    \item{coerce}{\code{signature(from = "NULL", to = "APparameter")}}
    \item{coerce}{\code{signature(from = "list", to = "APparameter")}}
    \item{coerce}{\code{signature(from = "NULL", to = "ECparameter")}}
    \item{coerce}{\code{signature(from = "list", to = "ECparameter")}}
    \item{show}{\code{signature(object = "ASparameter")}}
  }
}
\references{
  Christian Borgelt (2004)
  \emph{Apriori --- Finding Association Rules/Hyperedges with the Apriori
    Algorithm}.
  \url{http://www.borgelt.net/apriori.html}
}
\seealso{
  \code{\link{apriori}},
  \code{\link{eclat}},
  \code{\link{weclat}} (for weighted rule mining),
  \code{\link{ruleInduction}}
}
\author{Michael Hahsler and Bettina Gruen}
\keyword{classes}
