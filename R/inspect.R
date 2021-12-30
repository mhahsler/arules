#######################################################################
# arules - Mining Association Rules and Frequent Itemsets
# Copyright (C) 2011-2015 Michael Hahsler, Christian Buchta,
#			Bettina Gruen and Kurt Hornik
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

#' Display Associations and Transactions in Readable Form
#'
#' Provides the generic function `inspect()` and methods to display
#' [associations] and [transactions] plus additional information formatted for
#' online inspection.
#'
#' `inspect()` prints the results directly. If you need to create a data.frame
#' with a human readable version, then you can use [DATAFRAME()].
#' 
#' @aliases inspect
#' @family associations functions
#' @family itemMatrix and transactions functions
#' 
#' @param x a set of [associations] or [transactions] or an [itemMatrix].
#' @param ... additional arguments.
#'  can be used to customize the output:
#' @param setStart set start symbol
#' @param setEnd set end symbol 
#' @param itemSep item separator
#' @param ruleSep rule separator
#' @param linebreak print only one element per line in case the output lines get very long? 
#'
#' @return Nothing is returned (see the Details Section).
#' @author Michael Hahsler and Kurt Hornik
#' @keywords print
#' @examples
#' data("Adult")
#' rules <- apriori(Adult)
#'
#' ## display some rules
#' inspect(rules[1000:1001])
#' inspect(rules[1000:1001], ruleSep = "~~>", itemSep = " + ", setStart = "", setEnd = "",
#'   linebreak = FALSE)
#'
#' ## to get rules in readable format, use coercion or DATAFRAME with additional parameters.
#' as(rules[1000:1001], "data.frame")
#' DATAFRAME(rules[1000:1001])
#' DATAFRAME(rules[1000:1001], separate = TRUE, setStart = "", setEnd = "")
#'
setGeneric("inspect",
  function(x, ...)
    standardGeneric("inspect"))

#' @rdname inspect
setMethod("inspect", signature(x = "itemsets"),
  function(x,
    itemSep = ", ",
    setStart = "{",
    setEnd = "}",
    linebreak = NULL,
    ...) {
    n_of_itemsets <- length(x)
    
    if (n_of_itemsets == 0)
      return(invisible(NULL))
    ## Nothing to inspect here ...
    
    l <- labels(x, itemSep, setStart, setEnd)
    if (is.null(linebreak))
      linebreak <- any(nchar(l) > options("width")$width * 2 / 3)
    
    if (!linebreak) {
      out <- data.frame(items = l)
      if (nrow(quality(x)) > 0)
        out <- cbind(out, quality(x))
      rownames(out) <- paste0('[', 1:n_of_itemsets, ']')
      print(out, right = FALSE)
      
    } else {
      ## number of rows + fix empty itemsets
      items <- unlist(lapply(
        as(items(x), "list"),
        FUN = function(x)
          if (length(x) == 0)
            ""
        else
          x
      ))
      n_of_items <- size(items(x))
      n_of_items[n_of_items == 0] <- 1
      
      ## calculate begin and end positions
      entry_beg_pos <- cumsum(c(1, n_of_items[-n_of_itemsets]))
      entry_end_pos <- entry_beg_pos + n_of_items - 1
      
      ## prepare output
      n_of_rows <- sum(n_of_items)
      quality <- quality(x)
      ## Output.
      out <-
        matrix("", nrow = n_of_rows + 1, ncol = 2 + NCOL(quality))
      
      ## Column 1: itemset nr.
      tmp <- rep.int("", n_of_rows + 1)
      tmp[entry_beg_pos + 1] <- paste0('[', 1:n_of_itemsets, ']')
      out[, 1] <- format(tmp)
      
      ## Column 2: items in the item sets, one per line.
      pre <- rep.int(" ", n_of_rows)
      pre[entry_beg_pos] <- rep.int(setStart, length(entry_beg_pos))
      post <- rep.int(itemSep, n_of_rows)
      post[entry_end_pos] <- rep.int(setEnd, length(entry_end_pos))
      out[, 2] <- format(c("items",
        paste(pre, unlist(items), post, sep = "")))
      
      
      ## Remaining columns: quality measures.
      for (i in seq(length = NCOL(quality))) {
        tmp <- rep.int("", n_of_rows + 1)
        tmp[1] <- names(quality)[i]
        tmp[entry_end_pos + 1] <- format(quality[[i]])
        out[, i + 2] <- format(tmp, justify = "right")
      }
      
      ## Output.
      cat(t(out), sep = c(rep.int(" ", NCOL(out) - 1), "\n"))
    }
  })

#' @rdname inspect
setMethod("inspect", signature(x = "rules"),
  function(x,
    itemSep = ", ",
    setStart = "{",
    setEnd = "}",
    ruleSep = "=>",
    linebreak = NULL,
    ...) {
    n_of_rules <- length(x)
    
    if (n_of_rules == 0)
      return(invisible(NULL))
    ## Nothing to inspect here ...
    
    lhs <- labels(lhs(x), itemSep, setStart, setEnd)
    rhs <- labels(rhs(x), itemSep, setStart, setEnd)
    
    if (is.null(linebreak))
      linebreak <- any(nchar(lhs) + nchar(rhs) + nchar(ruleSep) >
          options("width")$width * 2 / 3)
    
    if (!linebreak) {
      out <- data.frame(lhs = lhs, "." = ruleSep, rhs = rhs)
      if (nrow(quality(x)) > 0)
        out <- cbind(out, quality(x))
      colnames(out)[2] <- ""
      rownames(out) <- paste0('[', 1:n_of_rules, ']')
      print(out, right = FALSE)
      
    } else {
      items_lhs <- as(lhs(x), "list")
      items_rhs <- as(rhs(x), "list")
      
      ### Empty RHS!
      items_rhs <- lapply(
        items_rhs,
        FUN = function(r)
          if (length(r) == 0)
            ""
        else
          r
      )
      
      quality <- quality(x)
      
      ## Rewrite empty LHSs.
      ind <- sapply(items_lhs, length) == 0
      if (any(ind))
        items_lhs[ind] <- ""
      
      ## Various lengths ...
      n_of_items_lhs <- sapply(items_lhs, length)
      n_of_items_rhs <- sapply(items_rhs, length)
      entry_end_pos <-
        cumsum(n_of_items_lhs + n_of_items_rhs - 1) + 1
      entry_beg_pos <- c(1, entry_end_pos[-n_of_rules]) + 1
      entry_mid_pos <- entry_beg_pos + n_of_items_lhs - 1
      lhs_pos <- unlist(mapply(seq, entry_beg_pos, entry_mid_pos,
        SIMPLIFY = FALSE))
      rhs_pos <- unlist(mapply(seq, entry_mid_pos, entry_end_pos,
        SIMPLIFY = FALSE))
      n_of_rows <- entry_end_pos[n_of_rules]
      
      out <- matrix("", nrow = n_of_rows, ncol = 4 + NCOL(quality))
      
      ## Column 1: Rule number
      tmp <- rep.int("", n_of_rows)
      tmp[entry_beg_pos] <- paste0('[', 1:n_of_rules, ']')
      out[, 1] <- format(tmp)
      
      ## Column 2: lhs.
      pre <- rep.int(" ", n_of_rows)
      pre[entry_beg_pos] <- setStart
      post <- rep.int("", n_of_rows)
      post[lhs_pos] <- itemSep
      post[entry_mid_pos] <- setEnd
      tmp <- rep.int("", n_of_rows)
      tmp[lhs_pos] <- unlist(items_lhs)
      out[, 2] <-
        format(c("lhs", paste(pre, tmp, post, sep = "")[-1]))
      
      ## Column 3: '=>'
      tmp <- rep.int("", n_of_rows)
      tmp[entry_mid_pos] <- ruleSep
      out[, 3] <- format(tmp)
      
      ## Column 4: rhs.
      pre <- rep.int(" ", n_of_rows)
      pre[entry_mid_pos] <- setStart
      post <- rep.int("", n_of_rows)
      post[rhs_pos] <- itemSep
      post[entry_end_pos] <- setEnd
      tmp <- rep.int("", n_of_rows)
      tmp[rhs_pos] <- unlist(items_rhs)
      out[, 4] <-
        format(c("rhs", paste(pre, tmp, post, sep = "")[-1]))
      
      ## Remaining columns: quality measures.
      for (i in seq(length = NCOL(quality))) {
        tmp <- rep.int("", n_of_rows)
        tmp[1] <- names(quality)[i]
        tmp[entry_end_pos] <- format(quality[[i]])
        out[, i + 4] <- format(tmp, justify = "right")
      }
      ## Output.
      cat(t(out), sep = c(rep.int(" ", NCOL(out) - 1), "\n"))
    }
  })


#' @rdname inspect
setMethod("inspect", signature(x = "transactions"),
  function(x,
    itemSep = ", ",
    setStart = "{",
    setEnd = "}",
    linebreak = NULL,
    ...) {
    n_of_itemsets <- length(x)
    
    if (n_of_itemsets == 0)
      return(invisible(NULL))
    ## Nothing to inspect here ...
    
    l <- labels(x, itemSep, setStart, setEnd)
    if (is.null(linebreak))
      linebreak <- any(nchar(l) > options("width")$width * 2 / 3)
    
    if (!linebreak) {
      out <- data.frame(items = l)
      if (nrow(transactionInfo(x)) > 0)
        out <- cbind(out, transactionInfo(x))
      rownames(out) <- paste0('[', 1:n_of_itemsets, ']')
      print(out, right = FALSE)
      
    } else {
      ## number of rows + fix empty itemsets
      items <- unlist(lapply(
        as(x, "list"),
        FUN = function(x)
          if (length(x) == 0)
            ""
        else
          x
      ))
      n_of_items <- size(x)
      n_of_items[n_of_items == 0] <- 1
      
      ## calculate begin and end positions
      entry_beg_pos <- cumsum(c(1, n_of_items[-n_of_itemsets]))
      entry_end_pos <- entry_beg_pos + n_of_items - 1
      
      ## prepare output
      n_of_rows <- sum(n_of_items)
      transactionInfo <- transactionInfo(x)
      ## Output.
      out <-
        matrix("",
          nrow = n_of_rows + 1,
          ncol = 2 + NCOL(transactionInfo))
      
      ## Column 1: itemset nr.
      tmp <- rep.int("", n_of_rows + 1)
      tmp[entry_beg_pos + 1] <- paste0('[', 1:n_of_itemsets, ']')
      out[, 1] <- format(tmp)
      
      ## Column 2: items in the item sets, one per line.
      pre <- rep.int(" ", n_of_rows)
      pre[entry_beg_pos] <- rep.int(setStart, length(entry_beg_pos))
      post <- rep.int(itemSep, n_of_rows)
      post[entry_end_pos] <- rep.int(setEnd, length(entry_end_pos))
      out[, 2] <- format(c("items",
        paste(pre, unlist(items), post, sep = "")))
      
      
      ## Remaining columns: transactionInfo.
      for (i in seq(length = NCOL(transactionInfo))) {
        tmp <- rep.int("", n_of_rows + 1)
        tmp[1] <- names(transactionInfo)[i]
        tmp[entry_end_pos + 1] <- format(transactionInfo[[i]])
        out[, i + 2] <- format(tmp, justify = "right")
      }
      
      ## Output.
      cat(t(out), sep = c(rep.int(" ", NCOL(out) - 1), "\n"))
    }
    
  })

#' @rdname inspect
setMethod("inspect", signature(x = "itemMatrix"),
  function(x,
    itemSep = ", ",
    setStart = "{",
    setEnd = "}",
    linebreak = NULL,
    ...) {
    n_of_itemsets <- length(x)
    
    if (n_of_itemsets == 0)
      return(invisible(NULL))
    ## Nothing to inspect here ...
    
    l <- labels(x, itemSep, setStart, setEnd)
    if (is.null(linebreak))
      linebreak <- any(nchar(l) > options("width")$width * 2 / 3)
    
    if (!linebreak) {
      out <- data.frame(items = l)
      rownames(out) <- paste0('[', 1:n_of_itemsets, ']')
      print(out, right = FALSE)
      
    } else {
      items <- unlist(lapply(
        as(x, "list"),
        FUN = function(x)
          if (length(x) == 0)
            ""
        else
          x
      ))
      
      n_of_items <- size(x)
      n_of_items[n_of_items == 0] <- 1
      
      ## calculate begin and end positions
      entry_beg_pos <- cumsum(c(1, n_of_items[-n_of_itemsets]))
      entry_end_pos <- entry_beg_pos + n_of_items - 1
      
      ## prepare output
      n_of_rows <- sum(n_of_items)
      ## Output.
      out <- matrix("", nrow = n_of_rows + 1, ncol = 2)
      
      ## Column 1: itemset nr.
      tmp <- rep.int("", n_of_rows + 1)
      tmp[entry_beg_pos + 1] <- paste0('[', 1:n_of_itemsets, ']')
      out[, 1] <- format(tmp)
      
      ## Column 2: items in the item sets, one per line.
      pre <- rep.int(" ", n_of_rows)
      pre[entry_beg_pos] <- rep.int(setStart, length(entry_beg_pos))
      post <- rep.int(itemSep, n_of_rows)
      post[entry_end_pos] <- rep.int(setEnd, length(entry_end_pos))
      out[, 2] <- format(c("items",
        paste(pre, unlist(items), post, sep = "")))
      
      
      ## Output.
      cat(t(out), sep = c(rep.int(" ", NCOL(out) - 1), "\n"))
    }
  })


#' @rdname inspect
setMethod("inspect", signature(x = "tidLists"),
  function(x, ...) {
    print(data.frame(
      items = itemLabels(x),
      transactionIDs = labels(x),
      row.names = NULL
    ),
      right = FALSE)
  })
