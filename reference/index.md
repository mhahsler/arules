# Package index

## Package overview

Infrastructure for representing, manipulating, and analyzing transaction
data, frequent itemsets, and association rules.

- [`arules`](http://michael.hahsler.net/arules/reference/arules-package.md)
  [`arules-package`](http://michael.hahsler.net/arules/reference/arules-package.md)
  : Infrastructure for representing transaction data, mining frequent
  itemsets and association rules, and evaluating the resulting patterns.
  The package includes efficient C implementations of the Apriori and
  Eclat algorithms.

## Transaction data and item matrices

Represent, inspect, visualize, and manipulate sparse binary incidence
matrices, transactions, and transaction ID lists.

- [`transactions()`](http://michael.hahsler.net/arules/reference/transactions-class.md)
  [`summary(`*`<transactions>`*`)`](http://michael.hahsler.net/arules/reference/transactions-class.md)
  [`toLongFormat(`*`<transactions>`*`)`](http://michael.hahsler.net/arules/reference/transactions-class.md)
  [`items(`*`<transactions>`*`)`](http://michael.hahsler.net/arules/reference/transactions-class.md)
  [`transactionInfo()`](http://michael.hahsler.net/arules/reference/transactions-class.md)
  [`` `transactionInfo<-`() ``](http://michael.hahsler.net/arules/reference/transactions-class.md)
  [`dimnames(`*`<transactions>`*`)`](http://michael.hahsler.net/arules/reference/transactions-class.md)
  [`` `dimnames<-`( ``*`<transactions>`*`,`*`<list>`*`)`](http://michael.hahsler.net/arules/reference/transactions-class.md)
  : Class transactions — Binary Incidence Matrix for Transactions
- [`abbreviate()`](http://michael.hahsler.net/arules/reference/abbreviate.md)
  : Abbreviate item labels in transactions, itemMatrix and associations
- [`c(`*`<itemMatrix>`*`)`](http://michael.hahsler.net/arules/reference/c.md)
  [`c(`*`<transactions>`*`)`](http://michael.hahsler.net/arules/reference/c.md)
  [`c(`*`<tidLists>`*`)`](http://michael.hahsler.net/arules/reference/c.md)
  [`c(`*`<rules>`*`)`](http://michael.hahsler.net/arules/reference/c.md)
  [`c(`*`<itemsets>`*`)`](http://michael.hahsler.net/arules/reference/c.md)
  : Combining Association and Transaction Objects
- [`crossTable()`](http://michael.hahsler.net/arules/reference/crossTable.md)
  : Cross-tabulate joint occurrences across pairs of items
- [`duplicated()`](http://michael.hahsler.net/arules/reference/duplicated.md)
  : Find Duplicated Elements
- [`` `[`( ``*`<itemMatrix>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<ANY>`*`)`](http://michael.hahsler.net/arules/reference/extract.md)
  [`` `[`( ``*`<transactions>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<ANY>`*`)`](http://michael.hahsler.net/arules/reference/extract.md)
  [`` `[`( ``*`<tidLists>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<ANY>`*`)`](http://michael.hahsler.net/arules/reference/extract.md)
  [`` `[`( ``*`<rules>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<ANY>`*`)`](http://michael.hahsler.net/arules/reference/extract.md)
  [`` `[`( ``*`<itemsets>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<ANY>`*`)`](http://michael.hahsler.net/arules/reference/extract.md)
  : Methods for "\[": Extraction or Subsetting arules Objects
- [`addAggregate()`](http://michael.hahsler.net/arules/reference/hierarchy.md)
  [`filterAggregate()`](http://michael.hahsler.net/arules/reference/hierarchy.md)
  [`aggregate()`](http://michael.hahsler.net/arules/reference/hierarchy.md)
  : Support for Item Hierarchies
- [`image(`*`<itemMatrix>`*`)`](http://michael.hahsler.net/arules/reference/image.md)
  [`image(`*`<transactions>`*`)`](http://michael.hahsler.net/arules/reference/image.md)
  [`image(`*`<tidLists>`*`)`](http://michael.hahsler.net/arules/reference/image.md)
  : Visual Inspection of Binary Incidence Matrices
- [`inspect()`](http://michael.hahsler.net/arules/reference/inspect.md)
  : Display Associations and Transactions in Readable Form
- [`is.superset()`](http://michael.hahsler.net/arules/reference/is.superset.md)
  [`is.subset()`](http://michael.hahsler.net/arules/reference/is.superset.md)
  : Find Super and Subsets
- [`itemFrequency()`](http://michael.hahsler.net/arules/reference/itemFrequency.md)
  : Getting Frequency/Support for Single Items
- [`itemFrequencyPlot()`](http://michael.hahsler.net/arules/reference/itemFrequencyPlot.md)
  : Creating a Item Frequencies/Support Bar Plot
- [`summary(`*`<itemMatrix>`*`)`](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)
  [`dim(`*`<itemMatrix>`*`)`](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)
  [`nitems()`](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)
  [`length(`*`<itemMatrix>`*`)`](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)
  [`toLongFormat()`](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)
  [`labels(`*`<itemMatrix>`*`)`](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)
  [`itemLabels()`](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)
  [`` `itemLabels<-`() ``](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)
  [`itemInfo()`](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)
  [`` `itemInfo<-`() ``](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)
  [`itemsetInfo()`](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)
  [`` `itemsetInfo<-`() ``](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)
  [`dimnames(`*`<itemMatrix>`*`)`](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)
  [`` `dimnames<-`( ``*`<itemMatrix>`*`,`*`<list>`*`)`](http://michael.hahsler.net/arules/reference/itemMatrix-class.md)
  : Class itemMatrix — Sparse Binary Incidence Matrix to Represent Sets
  of Items
- [`itemUnion()`](http://michael.hahsler.net/arules/reference/itemwiseSetOps.md)
  [`itemSetdiff()`](http://michael.hahsler.net/arules/reference/itemwiseSetOps.md)
  [`itemIntersect()`](http://michael.hahsler.net/arules/reference/itemwiseSetOps.md)
  : Itemwise Set Operations
- [`match()`](http://michael.hahsler.net/arules/reference/match.md)
  [`` `%in%`( ``*`<itemMatrix>`*`,`*`<itemMatrix>`*`)`](http://michael.hahsler.net/arules/reference/match.md)
  [`` `%in%`( ``*`<itemMatrix>`*`,`*`<character>`*`)`](http://michael.hahsler.net/arules/reference/match.md)
  [`` `%in%`( ``*`<associations>`*`,`*`<associations>`*`)`](http://michael.hahsler.net/arules/reference/match.md)
  [`` `%pin%`( ``*`<itemMatrix>`*`,`*`<character>`*`)`](http://michael.hahsler.net/arules/reference/match.md)
  [`` `%ain%`( ``*`<itemMatrix>`*`,`*`<character>`*`)`](http://michael.hahsler.net/arules/reference/match.md)
  [`` `%oin%`( ``*`<itemMatrix>`*`,`*`<character>`*`)`](http://michael.hahsler.net/arules/reference/match.md)
  : Value Matching
- [`merge()`](http://michael.hahsler.net/arules/reference/merge.md) :
  Adding Items to Data
- [`random.transactions()`](http://michael.hahsler.net/arules/reference/random.transactions.md)
  [`random.patterns()`](http://michael.hahsler.net/arules/reference/random.transactions.md)
  : Simulate a Random Transactions
- [`sample(`*`<itemMatrix>`*`)`](http://michael.hahsler.net/arules/reference/sample.md)
  [`sample(`*`<associations>`*`)`](http://michael.hahsler.net/arules/reference/sample.md)
  : Random Samples and Permutations
- [`union(`*`<itemMatrix>`*`)`](http://michael.hahsler.net/arules/reference/sets.md)
  [`union(`*`<associations>`*`)`](http://michael.hahsler.net/arules/reference/sets.md)
  [`intersect(`*`<itemMatrix>`*`)`](http://michael.hahsler.net/arules/reference/sets.md)
  [`intersect(`*`<associations>`*`)`](http://michael.hahsler.net/arules/reference/sets.md)
  [`setequal(`*`<itemMatrix>`*`)`](http://michael.hahsler.net/arules/reference/sets.md)
  [`setequal(`*`<associations>`*`)`](http://michael.hahsler.net/arules/reference/sets.md)
  [`setdiff(`*`<itemMatrix>`*`)`](http://michael.hahsler.net/arules/reference/sets.md)
  [`setdiff(`*`<associations>`*`)`](http://michael.hahsler.net/arules/reference/sets.md)
  [`is.element(`*`<itemMatrix>`*`)`](http://michael.hahsler.net/arules/reference/sets.md)
  [`is.element(`*`<associations>`*`)`](http://michael.hahsler.net/arules/reference/sets.md)
  : Set Operations
- [`size()`](http://michael.hahsler.net/arules/reference/size.md) :
  Number of Items in Sets
- [`supportingTransactions()`](http://michael.hahsler.net/arules/reference/supportingTransactions.md)
  : Supporting Transactions
- [`tidLists()`](http://michael.hahsler.net/arules/reference/tidLists-class.md)
  [`summary(`*`<tidLists>`*`)`](http://michael.hahsler.net/arules/reference/tidLists-class.md)
  [`dim(`*`<tidLists>`*`)`](http://michael.hahsler.net/arules/reference/tidLists-class.md)
  [`dimnames(`*`<tidLists>`*`)`](http://michael.hahsler.net/arules/reference/tidLists-class.md)
  [`` `dimnames<-`( ``*`<tidLists>`*`,`*`<list>`*`)`](http://michael.hahsler.net/arules/reference/tidLists-class.md)
  [`length(`*`<tidLists>`*`)`](http://michael.hahsler.net/arules/reference/tidLists-class.md)
  [`t(`*`<tidLists>`*`)`](http://michael.hahsler.net/arules/reference/tidLists-class.md)
  [`transactionInfo(`*`<tidLists>`*`)`](http://michael.hahsler.net/arules/reference/tidLists-class.md)
  [`` `transactionInfo<-`( ``*`<tidLists>`*`)`](http://michael.hahsler.net/arules/reference/tidLists-class.md)
  [`itemInfo(`*`<tidLists>`*`)`](http://michael.hahsler.net/arules/reference/tidLists-class.md)
  [`` `itemInfo<-`( ``*`<tidLists>`*`)`](http://michael.hahsler.net/arules/reference/tidLists-class.md)
  [`itemLabels(`*`<tidLists>`*`)`](http://michael.hahsler.net/arules/reference/tidLists-class.md)
  [`labels(`*`<tidLists>`*`)`](http://michael.hahsler.net/arules/reference/tidLists-class.md)
  : Class tidLists — Transaction ID Lists for Items/Itemsets
- [`unique()`](http://michael.hahsler.net/arules/reference/unique.md) :
  Remove Duplicated Elements from a Collection
- [`decode()`](http://michael.hahsler.net/arules/reference/itemCoding.md)
  [`encode()`](http://michael.hahsler.net/arules/reference/itemCoding.md)
  [`recode()`](http://michael.hahsler.net/arules/reference/itemCoding.md)
  [`compatible()`](http://michael.hahsler.net/arules/reference/itemCoding.md)
  : Item Coding — Conversion between Item Labels and Column IDs
- [`addComplement()`](http://michael.hahsler.net/arules/reference/addComplement.md)
  : Add Complement-items to Transactions
- [`subset()`](http://michael.hahsler.net/arules/reference/subset.md) :
  Subsetting Itemsets, Rules and Transactions

## Data preprocessing

Prepare data for mining by discretizing variables, managing item coding
and hierarchies, adding items, and sampling data.

- [`discretize()`](http://michael.hahsler.net/arules/reference/discretize.md)
  [`discretizeDF()`](http://michael.hahsler.net/arules/reference/discretize.md)
  : Convert a Continuous Variable into a Categorical Variable
- [`addAggregate()`](http://michael.hahsler.net/arules/reference/hierarchy.md)
  [`filterAggregate()`](http://michael.hahsler.net/arules/reference/hierarchy.md)
  [`aggregate()`](http://michael.hahsler.net/arules/reference/hierarchy.md)
  : Support for Item Hierarchies
- [`decode()`](http://michael.hahsler.net/arules/reference/itemCoding.md)
  [`encode()`](http://michael.hahsler.net/arules/reference/itemCoding.md)
  [`recode()`](http://michael.hahsler.net/arules/reference/itemCoding.md)
  [`compatible()`](http://michael.hahsler.net/arules/reference/itemCoding.md)
  : Item Coding — Conversion between Item Labels and Column IDs
- [`merge()`](http://michael.hahsler.net/arules/reference/merge.md) :
  Adding Items to Data
- [`sample(`*`<itemMatrix>`*`)`](http://michael.hahsler.net/arules/reference/sample.md)
  [`sample(`*`<associations>`*`)`](http://michael.hahsler.net/arules/reference/sample.md)
  : Random Samples and Permutations

## Association mining

Mine frequent itemsets and association rules, configure the mining
algorithms, and induce rules from itemsets.

- [`apriori()`](http://michael.hahsler.net/arules/reference/apriori.md)
  : Mining Associations with the Apriori Algorithm
- [`eclat()`](http://michael.hahsler.net/arules/reference/eclat.md) :
  Mining Associations with Eclat
- [`weclat()`](http://michael.hahsler.net/arules/reference/weclat.md) :
  Mining Associations from Weighted Transaction Data with Eclat (WARM)
- [`fim4r()`](http://michael.hahsler.net/arules/reference/fim4r.md) :
  Interface to Mining Algorithms from fim4r
- [`ruleInduction()`](http://michael.hahsler.net/arules/reference/ruleInduction.md)
  : Association Rule Induction from Itemsets
- [`APappearance-class`](http://michael.hahsler.net/arules/reference/APappearance-class.md)
  [`APappearance`](http://michael.hahsler.net/arules/reference/APappearance-class.md)
  [`coercion-APappearance`](http://michael.hahsler.net/arules/reference/APappearance-class.md)
  [`coerce,NULL,APappearance-method`](http://michael.hahsler.net/arules/reference/APappearance-class.md)
  [`coerce,list,APappearance-method`](http://michael.hahsler.net/arules/reference/APappearance-class.md)
  : Class APappearance — Specifying the appearance Argument of Apriori
  to Implement Rule Templates
- [`coerce(`*`<NULL>`*`,`*`<APcontrol>`*`)`](http://michael.hahsler.net/arules/reference/AScontrol-classes.md)
  : Classes AScontrol, APcontrol, ECcontrol — Specifying the control
  Argument of Apriori and Eclat
- [`ASparameter-classes`](http://michael.hahsler.net/arules/reference/ASparameter-classes.md)
  [`parameter`](http://michael.hahsler.net/arules/reference/ASparameter-classes.md)
  [`initialize,ASparameter-method`](http://michael.hahsler.net/arules/reference/ASparameter-classes.md)
  [`show,ASparameter-method`](http://michael.hahsler.net/arules/reference/ASparameter-classes.md)
  [`ASparameter-class`](http://michael.hahsler.net/arules/reference/ASparameter-classes.md)
  [`ASparameter`](http://michael.hahsler.net/arules/reference/ASparameter-classes.md)
  [`APparameter-class`](http://michael.hahsler.net/arules/reference/ASparameter-classes.md)
  [`APparameter`](http://michael.hahsler.net/arules/reference/ASparameter-classes.md)
  [`initialize,APparameter-method`](http://michael.hahsler.net/arules/reference/ASparameter-classes.md)
  [`ECparameter-class`](http://michael.hahsler.net/arules/reference/ASparameter-classes.md)
  [`ECparameter`](http://michael.hahsler.net/arules/reference/ASparameter-classes.md)
  [`initialize,ECparameter-method`](http://michael.hahsler.net/arules/reference/ASparameter-classes.md)
  [`coercion`](http://michael.hahsler.net/arules/reference/ASparameter-classes.md)
  [`coerce,NULL,APparameter-method`](http://michael.hahsler.net/arules/reference/ASparameter-classes.md)
  [`coerce,list,APparameter-method`](http://michael.hahsler.net/arules/reference/ASparameter-classes.md)
  [`coerce,NULL,ECparameter-method`](http://michael.hahsler.net/arules/reference/ASparameter-classes.md)
  [`coerce,list,ECparameter-method`](http://michael.hahsler.net/arules/reference/ASparameter-classes.md)
  : Classes ASparameter, APparameter, ECparameter — Specifying the
  parameter Argument of APRIORI and ECLAT

## Weighted association mining

Compute transaction weights and mine frequent itemsets from weighted
transaction data.

- [`SunBai`](http://michael.hahsler.net/arules/reference/SunBai.md)
  [`sunbai`](http://michael.hahsler.net/arules/reference/SunBai.md) :
  The SunBai Weighted Transactions Data Set
- [`hits()`](http://michael.hahsler.net/arules/reference/hits.md) :
  Computing Transaction Weights With HITS
- [`weclat()`](http://michael.hahsler.net/arules/reference/weclat.md) :
  Mining Associations from Weighted Transaction Data with Eclat (WARM)

## Associations and set operations

Represent and manipulate sets of itemsets and association rules.

- [`inspect()`](http://michael.hahsler.net/arules/reference/inspect.md)
  : Display Associations and Transactions in Readable Form
- [`abbreviate()`](http://michael.hahsler.net/arules/reference/abbreviate.md)
  : Abbreviate item labels in transactions, itemMatrix and associations
- [`quality(`*`<associations>`*`)`](http://michael.hahsler.net/arules/reference/associations-class.md)
  [`` `quality<-`( ``*`<associations>`*`)`](http://michael.hahsler.net/arules/reference/associations-class.md)
  [`info(`*`<associations>`*`)`](http://michael.hahsler.net/arules/reference/associations-class.md)
  [`` `info<-`( ``*`<associations>`*`)`](http://michael.hahsler.net/arules/reference/associations-class.md)
  [`head(`*`<associations>`*`)`](http://michael.hahsler.net/arules/reference/associations-class.md)
  [`tail(`*`<associations>`*`)`](http://michael.hahsler.net/arules/reference/associations-class.md)
  [`items(`*`<associations>`*`)`](http://michael.hahsler.net/arules/reference/associations-class.md)
  [`length(`*`<associations>`*`)`](http://michael.hahsler.net/arules/reference/associations-class.md)
  [`labels(`*`<associations>`*`)`](http://michael.hahsler.net/arules/reference/associations-class.md)
  [`plot(`*`<associations>`*`)`](http://michael.hahsler.net/arules/reference/associations-class.md)
  [`plot(`*`<itemMatrix>`*`)`](http://michael.hahsler.net/arules/reference/associations-class.md)
  : Class associations — A Set of Associations
- [`c(`*`<itemMatrix>`*`)`](http://michael.hahsler.net/arules/reference/c.md)
  [`c(`*`<transactions>`*`)`](http://michael.hahsler.net/arules/reference/c.md)
  [`c(`*`<tidLists>`*`)`](http://michael.hahsler.net/arules/reference/c.md)
  [`c(`*`<rules>`*`)`](http://michael.hahsler.net/arules/reference/c.md)
  [`c(`*`<itemsets>`*`)`](http://michael.hahsler.net/arules/reference/c.md)
  : Combining Association and Transaction Objects
- [`duplicated()`](http://michael.hahsler.net/arules/reference/duplicated.md)
  : Find Duplicated Elements
- [`` `[`( ``*`<itemMatrix>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<ANY>`*`)`](http://michael.hahsler.net/arules/reference/extract.md)
  [`` `[`( ``*`<transactions>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<ANY>`*`)`](http://michael.hahsler.net/arules/reference/extract.md)
  [`` `[`( ``*`<tidLists>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<ANY>`*`)`](http://michael.hahsler.net/arules/reference/extract.md)
  [`` `[`( ``*`<rules>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<ANY>`*`)`](http://michael.hahsler.net/arules/reference/extract.md)
  [`` `[`( ``*`<itemsets>`*`,`*`<ANY>`*`,`*`<ANY>`*`,`*`<ANY>`*`)`](http://michael.hahsler.net/arules/reference/extract.md)
  : Methods for "\[": Extraction or Subsetting arules Objects
- [`is.closed()`](http://michael.hahsler.net/arules/reference/is.closed.md)
  : Find Closed Itemsets
- [`is.generator()`](http://michael.hahsler.net/arules/reference/is.generator.md)
  : Find Generator Itemsets
- [`is.maximal()`](http://michael.hahsler.net/arules/reference/is.maximal.md)
  : Find Maximal Itemsets
- [`is.redundant()`](http://michael.hahsler.net/arules/reference/is.redundant.md)
  : Find Redundant Rules
- [`is.significant()`](http://michael.hahsler.net/arules/reference/is.significant.md)
  : Find Significant Rules
- [`is.superset()`](http://michael.hahsler.net/arules/reference/is.superset.md)
  [`is.subset()`](http://michael.hahsler.net/arules/reference/is.superset.md)
  : Find Super and Subsets
- [`itemsets()`](http://michael.hahsler.net/arules/reference/itemsets-class.md)
  [`summary(`*`<itemsets>`*`)`](http://michael.hahsler.net/arules/reference/itemsets-class.md)
  [`length(`*`<itemsets>`*`)`](http://michael.hahsler.net/arules/reference/itemsets-class.md)
  [`nitems(`*`<itemsets>`*`)`](http://michael.hahsler.net/arules/reference/itemsets-class.md)
  [`labels(`*`<itemsets>`*`)`](http://michael.hahsler.net/arules/reference/itemsets-class.md)
  [`itemLabels(`*`<itemsets>`*`)`](http://michael.hahsler.net/arules/reference/itemsets-class.md)
  [`` `itemLabels<-`( ``*`<itemsets>`*`)`](http://michael.hahsler.net/arules/reference/itemsets-class.md)
  [`itemInfo(`*`<itemsets>`*`)`](http://michael.hahsler.net/arules/reference/itemsets-class.md)
  [`items(`*`<itemsets>`*`)`](http://michael.hahsler.net/arules/reference/itemsets-class.md)
  [`` `items<-`( ``*`<itemsets>`*`)`](http://michael.hahsler.net/arules/reference/itemsets-class.md)
  [`tidLists(`*`<itemsets>`*`)`](http://michael.hahsler.net/arules/reference/itemsets-class.md)
  : Class itemsets — A Set of Itemsets
- [`match()`](http://michael.hahsler.net/arules/reference/match.md)
  [`` `%in%`( ``*`<itemMatrix>`*`,`*`<itemMatrix>`*`)`](http://michael.hahsler.net/arules/reference/match.md)
  [`` `%in%`( ``*`<itemMatrix>`*`,`*`<character>`*`)`](http://michael.hahsler.net/arules/reference/match.md)
  [`` `%in%`( ``*`<associations>`*`,`*`<associations>`*`)`](http://michael.hahsler.net/arules/reference/match.md)
  [`` `%pin%`( ``*`<itemMatrix>`*`,`*`<character>`*`)`](http://michael.hahsler.net/arules/reference/match.md)
  [`` `%ain%`( ``*`<itemMatrix>`*`,`*`<character>`*`)`](http://michael.hahsler.net/arules/reference/match.md)
  [`` `%oin%`( ``*`<itemMatrix>`*`,`*`<character>`*`)`](http://michael.hahsler.net/arules/reference/match.md)
  : Value Matching
- [`rules()`](http://michael.hahsler.net/arules/reference/rules-class.md)
  [`summary(`*`<rules>`*`)`](http://michael.hahsler.net/arules/reference/rules-class.md)
  [`length(`*`<rules>`*`)`](http://michael.hahsler.net/arules/reference/rules-class.md)
  [`nitems(`*`<rules>`*`)`](http://michael.hahsler.net/arules/reference/rules-class.md)
  [`labels(`*`<rules>`*`)`](http://michael.hahsler.net/arules/reference/rules-class.md)
  [`itemLabels(`*`<rules>`*`)`](http://michael.hahsler.net/arules/reference/rules-class.md)
  [`` `itemLabels<-`( ``*`<rules>`*`)`](http://michael.hahsler.net/arules/reference/rules-class.md)
  [`itemInfo(`*`<rules>`*`)`](http://michael.hahsler.net/arules/reference/rules-class.md)
  [`lhs()`](http://michael.hahsler.net/arules/reference/rules-class.md)
  [`` `lhs<-`() ``](http://michael.hahsler.net/arules/reference/rules-class.md)
  [`rhs()`](http://michael.hahsler.net/arules/reference/rules-class.md)
  [`` `rhs<-`() ``](http://michael.hahsler.net/arules/reference/rules-class.md)
  [`items(`*`<rules>`*`)`](http://michael.hahsler.net/arules/reference/rules-class.md)
  [`generatingItemsets()`](http://michael.hahsler.net/arules/reference/rules-class.md)
  : Class rules — A Set of Rules
- [`sample(`*`<itemMatrix>`*`)`](http://michael.hahsler.net/arules/reference/sample.md)
  [`sample(`*`<associations>`*`)`](http://michael.hahsler.net/arules/reference/sample.md)
  : Random Samples and Permutations
- [`union(`*`<itemMatrix>`*`)`](http://michael.hahsler.net/arules/reference/sets.md)
  [`union(`*`<associations>`*`)`](http://michael.hahsler.net/arules/reference/sets.md)
  [`intersect(`*`<itemMatrix>`*`)`](http://michael.hahsler.net/arules/reference/sets.md)
  [`intersect(`*`<associations>`*`)`](http://michael.hahsler.net/arules/reference/sets.md)
  [`setequal(`*`<itemMatrix>`*`)`](http://michael.hahsler.net/arules/reference/sets.md)
  [`setequal(`*`<associations>`*`)`](http://michael.hahsler.net/arules/reference/sets.md)
  [`setdiff(`*`<itemMatrix>`*`)`](http://michael.hahsler.net/arules/reference/sets.md)
  [`setdiff(`*`<associations>`*`)`](http://michael.hahsler.net/arules/reference/sets.md)
  [`is.element(`*`<itemMatrix>`*`)`](http://michael.hahsler.net/arules/reference/sets.md)
  [`is.element(`*`<associations>`*`)`](http://michael.hahsler.net/arules/reference/sets.md)
  : Set Operations
- [`size()`](http://michael.hahsler.net/arules/reference/size.md) :
  Number of Items in Sets
- [`sort(`*`<associations>`*`)`](http://michael.hahsler.net/arules/reference/sort.md)
  : Sort Associations
- [`unique()`](http://michael.hahsler.net/arules/reference/unique.md) :
  Remove Duplicated Elements from a Collection

## Interest measures

Calculate support, coverage, confidence intervals, and other interest
measures for itemsets and association rules.

- [`confint(`*`<rules>`*`)`](http://michael.hahsler.net/arules/reference/confint.md)
  : Confidence Intervals for Interest Measures for Association Rules
- [`coverage()`](http://michael.hahsler.net/arules/reference/coverage.md)
  : Calculate coverage for rules
- [`interestMeasure()`](http://michael.hahsler.net/arules/reference/interestMeasure.md)
  : Calculate Additional Interest Measures
- [`is.redundant()`](http://michael.hahsler.net/arules/reference/is.redundant.md)
  : Find Redundant Rules
- [`is.significant()`](http://michael.hahsler.net/arules/reference/is.significant.md)
  : Find Significant Rules
- [`support()`](http://michael.hahsler.net/arules/reference/support.md)
  : Support Counting for Itemsets

## Postprocessing

Induce and prune rules and identify closed, generator, maximal,
redundant, significant, and related itemsets or rules.

- [`interestMeasure()`](http://michael.hahsler.net/arules/reference/interestMeasure.md)
  : Calculate Additional Interest Measures
- [`is.closed()`](http://michael.hahsler.net/arules/reference/is.closed.md)
  : Find Closed Itemsets
- [`is.generator()`](http://michael.hahsler.net/arules/reference/is.generator.md)
  : Find Generator Itemsets
- [`is.maximal()`](http://michael.hahsler.net/arules/reference/is.maximal.md)
  : Find Maximal Itemsets
- [`is.redundant()`](http://michael.hahsler.net/arules/reference/is.redundant.md)
  : Find Redundant Rules
- [`is.significant()`](http://michael.hahsler.net/arules/reference/is.significant.md)
  : Find Significant Rules
- [`is.superset()`](http://michael.hahsler.net/arules/reference/is.superset.md)
  [`is.subset()`](http://michael.hahsler.net/arules/reference/is.superset.md)
  : Find Super and Subsets
- [`ruleInduction()`](http://michael.hahsler.net/arules/reference/ruleInduction.md)
  : Association Rule Induction from Itemsets

## Proximities and prediction

Compute and represent affinities, similarities, and dissimilarities and
use them for nearest-neighbor prediction.

- [`affinity()`](http://michael.hahsler.net/arules/reference/affinity.md)
  : Computing Affinity Between Items
- [`dissimilarity()`](http://michael.hahsler.net/arules/reference/dissimilarity.md)
  : Dissimilarity Matrix Computation for Associations and Transactions
- [`predict()`](http://michael.hahsler.net/arules/reference/predict.md)
  : Model Predictions
- [`proximity-classes`](http://michael.hahsler.net/arules/reference/proximity-classes.md)
  [`ar_similarity-class`](http://michael.hahsler.net/arules/reference/proximity-classes.md)
  [`ar_cross_dissimilarity-class`](http://michael.hahsler.net/arules/reference/proximity-classes.md)
  : Classes dist, ar_cross_dissimilarity and ar_similarity — Proximity
  Matrices

## Import and export

Read, write, and convert transactions and associations using data
frames, lists, text formats, and PMML.

- [`DATAFRAME()`](http://michael.hahsler.net/arules/reference/DATAFRAME.md)
  : Data.frame Representation for arules Objects
- [`LIST()`](http://michael.hahsler.net/arules/reference/LIST.md) : List
  Representation for Objects Based on Class itemMatrix
- [`write.PMML()`](http://michael.hahsler.net/arules/reference/pmml.md)
  [`read.PMML()`](http://michael.hahsler.net/arules/reference/pmml.md) :
  Read and Write PMML
- [`read.transactions()`](http://michael.hahsler.net/arules/reference/read.md)
  : Read Transaction Data
- [`write()`](http://michael.hahsler.net/arules/reference/write.md) :
  Write Transactions or Associations to a File

## Data sets

Example transaction and survey data sets for association mining and
preprocessing.

- [`Adult`](http://michael.hahsler.net/arules/reference/Adult.md)
  [`adult`](http://michael.hahsler.net/arules/reference/Adult.md)
  [`AdultUCI`](http://michael.hahsler.net/arules/reference/Adult.md) :
  Adult Data Set
- [`Epub`](http://michael.hahsler.net/arules/reference/Epub.md) : The
  Epub Transactions Data Set
- [`Groceries`](http://michael.hahsler.net/arules/reference/Groceries.md)
  [`groceries`](http://michael.hahsler.net/arules/reference/Groceries.md)
  : The Groceries Transactions Data Set
- [`Income`](http://michael.hahsler.net/arules/reference/Income.md)
  [`income`](http://michael.hahsler.net/arules/reference/Income.md)
  [`IncomeESL`](http://michael.hahsler.net/arules/reference/Income.md) :
  The Income Data Set
- [`Mushroom`](http://michael.hahsler.net/arules/reference/Mushroom.md)
  [`mushroom`](http://michael.hahsler.net/arules/reference/Mushroom.md)
  : The Mushroom Data Set as Transactions
- [`SunBai`](http://michael.hahsler.net/arules/reference/SunBai.md)
  [`sunbai`](http://michael.hahsler.net/arules/reference/SunBai.md) :
  The SunBai Weighted Transactions Data Set
