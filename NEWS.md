# arules 1.5-0.1 (xx/xx/2016)

* is.significant now supports in addition to Fisher's exact test,
  the chi-squared test.
* Added function DATAFRAME for more control over coercion to data.frame 
  (e.g., use separate columns for LHS and RHS of rules).
* FIX: Error message for sorting with an unknown interest measure.
* FIX: abbreviate works now for rules correctly.


# arules 1.5-0 (09/23/2016)

* IMPORTANT CHANGE: apriori uses now a time limit set in parameter with 
  maxtime. The default is 5 seconds. Running out of time or maxlen results 
  in a warning. The warning for low absolute support was removed.
* is.redundant now also marks rules with the same confidence as redundant.
* plot for associations and transactions produces now a better 
  error/warning message.
* improved argument check for %pin%. Warns now for multiple patterns 
  (was an error) and give an error for empty pattern.
* inspect prints now consistently the index of rules/itemsets using brackets 
  and starting from 1.

# arules 1.4-2 (08/06/2016)

* Bugfix: is.redundant returned !is.redundant (reported by brisbia)
* Duplicate items when coercing from list to transactions are now 
  removed with a warning.

# arules 1.4-1 (04/10/2016)

* added tail method for associations.
* added/fixed encoding for read.transactions
* Bugfix for interestMeasure. Mutual information is now calculated correctly
  (reported by ddessommes).

# arules 1.4-0 (03/18/2016)

* The transaction class lost slot transactionInfo (we use the 
  itemsetInfo slot now). Note that you may have to rebuild some 
  transaction sets if you are using transactionInfo.
* Bugfix for combining item matrices with 0 rows (reported by C. Buchta).
* Bugfix for itemLabel recoding in is.subset (reported by sjain777). 
* Bugfix for NAMESPACE export for %in%
* is.redundant: fixed and performance improvement.
* interestMeasure: performance improvement for "improvement" measure.
* sort: speed up sort by always sorting NAs last.
* head: added method head for associations for getting the best rules 
  according to an interest measure faster than sorting all the 
  associations first.
* Groceries: fixed typo in dataset.
* abbreviate is now a S4 generic with S4 methods.

# arules 1.3-1 (12/13/2015)

* we now require R 3.2.0 so cbind in Matrix works.
* is.maximal is now also available for rules.
* added is.significant for rules (uses Fishers exact test with correction).
* added is.redundant for rules.
* added support for multi-level analysis (aggregate). 
* APparameter: confidence shows now NA for frequent itemsets.

# arules 1.3-0 (11/11/2015)

* removed deprecated WRITE and SORT functions.
* ruleInduction: bug fix for missing confidence values and better checking 
  (by C. Buchta). 
* subset extraction: added checks, handles now NAs and recycles for logical.
* read.transactions gained arguments skip and quote and some defaults for
  read and write (uses now quotes and no rownames by default) have changed.
* itemMatrix: coersion from matrix checks now for 0-1 matrix with a warning.
* APRIORI and ECLAT report now absolute minimum support.
* APRIORI: out-of-memory while rule building does now result in an error and
  not a memory fault.
* aggregate uses now 'by' instead of 'itemLabels' to conform to 
      aggregate in base.
    
# arules 1.2-1 (09/20/2015)

* Added many new interest measures.
* interestMeasure: the formal argument method is now called measure 
  (method is now deprecated).
* Added Mushroom dataset.
* Moved abbreviate from arulesViz to arules.
* fixed undefined behavior for left shift in reclat.c 
  (reported by B. Ripley)

# arules 1.2-0 (09/14/2015)

* added support for weighted association rule mining (by C. Buchta):
    - transactions can store weights a column called "weight" in 
        transactionInfo.
    - support, itemFrequency and itemFrequencyPlot gained a parameter
        called weighted.
    - weclat extends eclat with transaction weights.
    - hits can be used to calculate weights from transact ions.
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
* fixed missing row labels for is.subset().
* replacement method for dimnames() checks now dimensions.
* item labels are now internally handled as character using 
  stringAsFactor = FALSE in data.frames and not AsIs with I(character).
* rules can now have no item in the RHS.
* We are transitioning to internally use consistently data.frames 
  with the correct number of rows for quality, itemInfo, 
  transactionInfo and itemsetInfo. These data.frames possibly have 
  0 columns.
* arules uses now testthat (tests are in tests/testthat).

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