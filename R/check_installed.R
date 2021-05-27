## this is a modified version from package rlang that only uses base R functionality.

check_installed <- function (pkg, reason = NULL)
{
  if (!is.character(pkg)) {
    stop("`pkg` must be a package name or a vector of package names.")
  }
  
  needs_install <-
    sapply(pkg, function(x)
      !requireNamespace(x,
        quietly = TRUE))
  
  if (any(needs_install)) {
    missing_pkgs <- pkg[needs_install]
    missing_pkgs_enum <- paste(missing_pkgs, collapse = ", ")
    n <- length(missing_pkgs)
    
    info <-
      paste0("The ", missing_pkgs_enum,
        " package(s) is/are required")
    if (is.null(reason)) {
      info <- paste0(info, ".")
    }
    else {
      info <- paste(info, reason)
    }
    
    if (!interactive()) {
      stop(info)
    }
    
    question <-
      "Would you like to install the package(s)?"
    cat(info, "\n", question)
    if (utils::menu(c("Yes", "No")) != 1) {
      invokeRestart("abort")
    }
    
    utils::install.packages(missing_pkgs)
    
  }
  invisible(TRUE)
}