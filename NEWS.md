# arules 1.7-3.1

## Changes

* We install now the latest version of fim4r


# arules 1.7-3 (1/9/2022)

## Changes

* Manual pages are now created using roxygen2.
* support() uses now explicit arguments instead of control.
* ruleInduction() uses now explicit arguments instead of control.

## New Feature

* Added interface to fim4r algorithms.

#  arules 1.7-2 (12/09/2021)

## New Feature

* The C APRIORI implementation can now mine frequent generator itemsets (contributed by Makh2018).

## Bugfixes

* deparse1() needs R 4.0.0

# arules 1.7-1 (11/18/2021)

## Bugfixes

* Fixed some C issues: unsigned int in bitmat.c (function static int _exists) to ensure bit shifting works. The bitmap support buffer is now initialized with zeros. 

# arules 1.7-0 (11/12/2021)

## New Feature

* Constructors and conversion
  * constructor transactions() can now also create transactions from data in long format (tid, item).
  * rules and itemsets have now a constructor.
  * toLongFormat converts transactions into a long format data.frame.

* Interest measures
  * interestMeasure for rules has now measure "table" which returns the contingency table. 
  * new interest measure "riskRatio" was added. 
  * interestMeasure for contingency table-based measures now accept the additional parameter smoothCounts 
    which is added to each count to avoid counts of zero (Laplace smoothing). 
  * new method for stats confint to calculate confidence intervals for some interest measures added.
  * is_redundant can now also use confidence intervals to determine statistical redundancy.
  * removed option "chiSquared" from crossTable.

* Mining algorithms
  * apriori and eclat gain ... additional arguments are now added to the parameter list.
  * added new function is.generators to find itemset generators.
  * apriori and eclat now store the call in the info slot of the created associations.

## Changes
* we use now a better check for installed suggested packages.
* inspect uses now a space after the comma.
* interestMeasures: reuse = TRUE now only reuses the basic measures of "support", "confidence", "coverage" and
  "lift". All other measures are recalculated to account for possible differences in additional parameters.
* set methods are now also exported as S3 methods using package generics so they do not conflict with tidyverse (dplyr). 

## Bug Fixes
* fixed mistake in man page for weclat. Weight column needs to be called weight (reported by Alexander Ruth).
* frequent itemsets now do not report "transIdenticalToItemsets" (reported by galadrielbriere). 
* fixed read.transactions reading in single format with header from a connection. First item is no longer dropped.

# arules 1.6-8 (05/17/2021)

## New Feature
* transactions have now a constructor function called transactions().
* Added new method compatible() to itemMatrix to check if the item coding is compatible 
  between two objects.
* c() now produces a warning if two itemMatrices with different itemCoding are combined.
* encode and recode accept now for itemLabels also objects with an itemLabels method.
* recode is now also available for associations (itemsets and rules). 

## Changes
* recode: parameter match is now deprecated. Use itemLabels

## Bug Fixes
* fixed addAggregate problem with character (reported by javiercoh).

# arules 1.6-7 (03/12/2021)

## New Features
* Added interest measure confidence boost (boost) with help from Jens Harbers, Cordes & Graefe KG.

## Bug Fixes
* interest measure improvement now does not produce inf values if no subset rule can be found. 


# arules 1.6-6 (05/14/2020)

## New Features
* added interestMeasure rhsSupport.
* added interestMeasure stdLift.
* addComplement now adds variables and levels to indicate what items are complments.

## Changes
* apriori and eclat now by default also reports coverage (ext defaults to TRUE and lhs.support is now called coverage).
* addComplement now adds variables and levels itemInfo.
* discretize now drops non-unique breaks with a warning (instead of producing an error).

## Bug Fixes
* DATAFRAME now works if itemsetInfo is empty.
* interestMeasure hyperlift now works without specifying transactions. 
* Changed c() to as.integer() for factors (R has changed).

# arules 1.6-5 (04/03/2020)

## New Features
* improved speed for calculating interestMeasures for rules and itemsets with no available 
  quality information or reuse = FALSE.
* Manual pages for associations were improved with examples for itemCoding.
* Manual page for interestMeasures is now linked with the associated web page.
* interest measure laplace (Laplace confidence) gained parameter k for the number of classes.

## Bug Fixes
* read.transactions: Parameter encoding is now correctly passed on to scan (reported by Sisi-Wiciel).
* interestMeasure was not calculating count for itemsets (reported by clcazer).
* removed the duplicated interest measure: "descriptiveConfirm" use "confirmedConfidence" instead.

# arules 1.6-4 (08/27/2019)

## Bug Fixes
* Fixed rounding bug affecting is.closed (reported by clcazer).
* Fixed TYPO in Ralambondrainy Measure (reported by andreijl).


# arules 1.6-3 (03/06/2019)

## New Features
* read.transactions gained parameter header to read files with column headers.

## Bug Fixes
* Fixed PROTECT placement in C code discovered by rchk.
* S4 objects use now show instead of print.


# arules 1.6-2 (12/02/2018)

## New Features
* discretizeDF now understands the method "none" which skips discretization.
* discretizeDF now reports which column produces the problem.

## Changes
* transactions: numeric columns are now discretized during coercion using discretizeDF (with a warning).

## Bug Fixes
* The spurious warning for reaching maxlen in apriori is now removed (reported by Ryan J. Cole).
* Fixed matrix check in function dissimilarity. 

# arules 1.6-1 (04/04/2018)

## Bug Fixes
* discretize now handles NAs in equal frequency (reported by yarik1988).
* interestMeasure: fixed error when an itemset/rules object of length 0 is provided.

## New Features
* rules and itemsets gained a method for nitems.
  

# arules 1.6-0 (2/28/2018)

## Major Changes
* discretize: the default method is now "frequency" and categories was 
    renamed breaks to be consistent with cut in R-base.

## New Features
* Added interest measure "importance".
* Added method items for transactions.
* Added discretizeDF to apply discretization to all numeric 
  columns in a data.frame.

## Bug Fixes
* Fixed typo in inspect for tidLists (reported by Carlos Chavarria). 
* Fixed bug in %in% for itemMatrix (reported by Henrique Lemos)

# arules 1.5-5 (01/09/2018)

## New Features
* Added (absolute support) "count" as an interest measure. 
* itemLabels can now be assigned for rules and itemsets. 

## Bug Fixes
* Fixed bug in subset with signature itemMatrix, itemMatrix (reported by rwdvc).
* Fixed pointer punning warning.

# arules 1.5-4 (10/12/2017)

## New Features
* Improved speed for read.transactions with format = "single" significantly.
* Appearance for apriori now guesses the default parameter automatically and 
  does some more checking, making the specification of templates easier.

## Bug Fixes
* Fixed null pointer in error message code.
* head does now not result in an error for empty rule sets 
  (bug reported by cornejom).

# arules 1.5-3 (08/31/2017)

## New Features
* apriori and eclat return now count (absolute support count) in the 
  quality data.frame.
* Added %oin% to find transactions/itemsets that ONLY contain certain items.

## Bug Fixes
* Improved PROTECT placement in C source code.
* itemMeasures for single rules/itemsets now returns a proper data.frame 
   (reported by lordbitin).
* itemMeasures: Added missing parentheses in kappa calculation and fixed
    equation for least contradiction (reported by Feng Chen). 


# arules 1.5-2 (03/12/2017)

## New Features
* apriori: maxtime = 0 disables the time limit. 
* is.subset/is.superset uses now fast and memory efficient C code 
   for sparse computation (contributed by Ian Johnson). 
   sparse = TRUE is now the default. Note that the result is now a 
   sparse matrix.

# arules 1.5-1 (01/23/2017)

## New Features

* Added interest measure maxConf.
* is.significant now supports in addition to Fisher's exact test,
  the chi-squared test.
* interest measures Fisher's exact test and chi-squared 
  (using significance = TRUE) can now produce p-values for
  substitutes (with complements = FALSE).
* Added function DATAFRAME for more control over coercion to data.frame 
  (e.g., use separate columns for LHS and RHS of rules).

## Bug Fixes

* Error message for sorting with an unknown interest measure.
* abbreviate works now for rules correctly.

## Internal Changes

* Added registration code for native routines. This requires R 3.3.2.

# arules 1.5-0 (09/23/2016)

## Major Changes

* apriori uses now a time limit set in the parameter list with 
  maxtime. The default is 5 seconds. Running out of time or maxlen results 
  in a warning. The warning for low absolute support was removed.

## Bug Fixes

* is.redundant now also marks rules with the same confidence as redundant.
* plot for associations and transactions produces now a better 
  error/warning message.
* improved argument check for %pin%. Warns now for multiple patterns 
  (was an error) and give an error for empty pattern.
* inspect prints now consistently the index of rules/itemsets using brackets 
  and starting from 1.

# arules 1.4-2 (08/06/2016)

## Bug Fixes

* is.redundant returned !is.redundant (reported by brisbia)
* Duplicate items when coercing from list to transactions are now 
  removed with a warning.

# arules 1.4-1 (04/10/2016)

## New Features

* added tail method for associations.
* added/fixed encoding for read.transactions

## Bug Fixes

* Mutual information is now calculated correctly
  (reported by ddessommes).

# arules 1.4-0 (03/18/2016)

## New Features

* The transaction class lost slot transactionInfo (we use the 
  itemsetInfo slot now). Note that you may have to rebuild some 
  transaction sets if you are using transactionInfo.
* interestMeasure: performance improvement for "improvement" measure.
* sort: speed up sort by always sorting NAs last.
* head: added method head for associations for getting the best rules 
  according to an interest measure faster than sorting all the 
  associations first.
* abbreviate is now a S4 generic with S4 methods.

## Bug Fixes

* combining item matrices with 0 rows (reported by C. Buchta).
* itemLabel recoding in is.subset (reported by sjain777). 
* NAMESPACE export for %in%
* is.redundant: fixed and performance improvement.
* Groceries: fixed typo in dataset.

# arules 1.3-1 (12/13/2015)


## Major Changes

* we now require R 3.2.0 so cbind in Matrix works.

## New Features

* is.maximal is now also available for rules.
* added is.significant for rules (uses Fishers exact test with correction).
* added is.redundant for rules.
* added support for multi-level analysis (aggregate). 
* APparameter: confidence shows now NA for frequent itemsets.

# arules 1.3-0 (11/11/2015)

## New Features

* removed deprecated WRITE and SORT functions.
* subset extraction: added checks, handles now NAs and recycles for logical.
* read.transactions gained arguments skip and quote and some defaults for
  read and write (uses now quotes and no rownames by default) have changed.
* itemMatrix: coercion from matrix checks now for 0-1 matrix with a warning.
* APRIORI and ECLAT report now absolute minimum support.
* APRIORI: out-of-memory while rule building does now result in an error and
  not a memory fault.
* aggregate uses now 'by' instead of 'itemLabels' to conform to 
      aggregate in base.

## Bug Fixes

* ruleInduction: bug fix for missing confidence values and better checking 
  (by C. Buchta). 
    
# arules 1.2-1 (09/20/2015)

## New Features

* Added many new interest measures.
* interestMeasure: the formal argument method is now called measure 
  (method is now deprecated).
* Added Mushroom dataset.
* Moved abbreviate from arulesViz to arules.

## Bug Fixes

* fixed undefined behavior for left shift in reclat.c 
  (reported by B. Ripley)

# arules 1.2-0 (09/14/2015)

## Major Changes

* added support for weighted association rule mining (by C. Buchta):
    - transactions can store weights a column called "weight" in 
        transactionInfo.
    - support, itemFrequency and itemFrequencyPlot gained a parameter
        called weighted.
    - weclat extends eclat with transaction weights.
    - hits can be used to calculate weights from transact ions.
* We are transitioning to internally use consistently data.frames 
  with the correct number of rows for quality, itemInfo, 
  transactionInfo and itemsetInfo. These data.frames possibly have 
  0 columns.
* arules uses now testthat (tests are in tests/testthat).

## New Features

* sort can now sort by several columns (used to break ties) in quality. 
  It also gained an order parameter to return a permutation vector
  (order) instead.
* inspect gained parameters setStart, setEnd, itemSep, ruleSep and
  linebreak to control output better.
* read.transactions now ignores empty items (e.g., caused by trailing 
  commas and leading or trailing white spaces).
* labels now returns not a list but consistent labels for objects       
  (transactions, itemMatrix, rules, itemsets, and tidLists). 
* tidLists has now an inspect method, gained coercion from "list", and
  has now a replacement method for dimnames(). 
* Coercion from itemMatrix to matrix results now in a logical matrix.
* fixed as(transactions, "data.frame"). The column names do now have
  no prefix (except if transactionInfo contains an item called "items").
* transactions has now its own dimnames function which correctly returns 
  transactionID from transactionInfo as rownames.
* replacement method for dimnames() checks now dimensions.
* item labels are now internally handled as character using 
  stringAsFactor = FALSE in data.frames and not AsIs with I(character).
* rules can now have no item in the RHS.

## Bug Fixes

* fixed missing row labels for is.subset().

# arules 1.1-9 (7/13/2015)

* More work on namespace.
* Fixed tests.

# arules 1.1-7 (6/29/2015)

* itemUnion: fixed bug for large amounts of dense rules.
* crossTable gained arguments measure and sort.
* Fixed namespace imports for non-base default packages.

# arules 1.1-6 (12/07/2014)

* dissimilarity method "pearson" is now set to 1 (max) for neg. 
      correlation. Also added phi correlation coefficient.
* discretize method "cluster" accepts now ... passed on to k-means
	    (e.g., for nstart)
* merge for itemMatrix checks now for conformity
* as(..., "transactions"): binary attributes are now translated into items
      only if TRUE. 

# arules 1.1-5 (8/19/2014)

* Import drop0 from Matrix

# arules 1.1-4 (7/25/2014)

* C code: fixed problem in error message generation in apriori and eclat
	    (this fixes the trio library problem under Windows)
* C code: rapriori uses now STRING_ELT to be compatible with TERR (TIBCO)
* C code: removed some unused variables.

# arules 1.1-3 (6/17/2014)

* Fixed dependency on XML and pmml
* the interest measure chi-squared does now also report p-values 
        (with significance=TRUE)
* interestMeasure calculation checks now better for missing transactions
* interestMeasure consistently returns now NA if not defined for a 
        certain rule

# arules 1.1-2 (2/21/2014)

* discretize gained the parameter ordered.
* itemwise set operations itemUnion, itemSetdiff and itemIntersect added.
* validObject checks now rules more thoroughly
* aggregate removes duplicate items from the lhs

# arules 1.1-1 (1/16/2014)

* is.superset/is.subset now makes sure that the two arguments conform using 
        recode (number and order of items)
* is.superset/is.subset returns now a matrix with appropriate dimnames
* bug fix: fixed dimname bug in as(..., "dgCMatrix") for tidLists
* image: labels are now passed on correctly.
* tidLists has now c(). 

# arules 1.1-0 (12/10/2013)

* bug fix: reuse in now passed on correctly in interestMeasures 
	      (bug reported by Ying Leung)
* direct coercions from and to dgCMatrix is no longer supported use 
        ngCMatrix instead
* coercion from ngCMatrix to itemMatrix and transactions is now possible
* C code: fixed misaligned address on 64-bit systems

# arules 1.0-15 (9/6/2013)

* service release

# arules 1.0-14 (5/24/2013)

* discretize handles now NAs correctly
* bug fix in is.subset

# arules 1.0-13 (4/7/2013)

* transactions: coercion form data.frame now handles logical automatically.
* discretize replaces categorize and offers several additional methods

# arules 1.0-12 (11/28/2012)

* Added read and write for PMML.
* 'WRITE' is now deprecated, use 'write' instead
* C code: Added a copy of the C subscript code from R for 
	better performance and compatibility with arulesSequences

# arules 1.0-11 (11/19/2012)

* Fixed vignette.
* Internal Changes for dimnames and subsetting

# arules 1.0-9 and 1.0-10 (9/3/2012)

* Added PACKAGE argument to C calls.
* C code: Added C routine symbols to NAMESPACE for arulesSequence

# arules 1.0-8 (8/23/2012)

* fixed memory problem in eclat with tidLists=TRUE
* added supportedTransactions()
* is.subset/is.superset can not return a sparse matrix
* added support to categorize continuous variables.

# arules 1.0-7 (11/4/2011)

* minor fixes (removed factor in dimnames for itemMatrix, warning in WRITE)
* read.transactions now accepts column names to specify user and item 
	columns (by F. Leisch)

# arules 1.0-0 (3/24/2009)

* Initial stable release version

# arules 0.1-0 (4/15/2005)

* Alpha and beta versions

