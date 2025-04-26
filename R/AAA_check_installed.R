## This is a modified version from package rlang that only uses base R functionality.

## action can be "install" (from CRAN), "stop" (with message), "check" (returns TRUE/FALSE)
## manual can be either TRUE or a string with installation instructions.
check_installed <-
  function(
      pkg,
      action = "install",
      message = NULL) {
    action <- match.arg(action, choices = c("install", "stop", "check"))

    if (!is.character(pkg)) {
      stop("`pkg` must be a package name or a vector of package names.")
    }

    needs_install <-
      sapply(pkg, function(x) {
        !requireNamespace(x,
          quietly = TRUE
        )
      })

    if (action == "check") {
      return(!any(needs_install))
    }

    if (any(needs_install)) {
      if (!interactive()) {
        stop(info)
      }

      missing_pkgs <- pkg[needs_install]
      missing_pkgs_enum <- paste(missing_pkgs, collapse = ", ")

      info <-
        paste(
          "The", missing_pkgs_enum,
          "package(s) is/are required."
        )

      if (action == "install") {
        question <-
          "Would you like to install the package(s)?"
        cat(info, "\n", question, sep = "")
        if (utils::menu(c("Yes", "No")) != 1) {
          invokeRestart("abort")
        }

        utils::install.packages(missing_pkgs)
      } else {
        ### this is stop
        cat(info,
          "\n",
          message,
          sep = ""
        )

        invokeRestart("abort")
      }
    }

    invisible(TRUE)
  }
