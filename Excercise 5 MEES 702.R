# Excercise 5 MEES 702
library(cli)

# Base R validation reference
is_leap_year <- function(year) {
    if (!is.numeric(year)) {
        stop("Argument 'year' must be a numeric vector.", call. = FALSE)
    }
    (year %% 4 == 0) & ((year %% 100 != 0) | (year %% 400 == 0))
}

scaled_power <- function(x, power = 2, scale = 10,
                         method = c("simple", "precise"),
                         plot = TRUE, verbose = TRUE, ...) {
    if (!is.numeric(x)) {
        stop("Input 'x' must be a numeric vector.", call. = FALSE)
    }
    method <- match.arg(method)
    res <- (x^power) * scale
    if (method == "simple") res <- round(res)
    if (verbose) message("Method selected: '", method, "' | Processing ", length(x), " elements.")
    if (plot) plot(x, res, ...)
    return(res)
}

# Updated function
    is_leap_year_cli <- function(year) {
        if(!is.numeric(year)) {
            cli::cli_abort(c(
                "x" = "Argument {.arg year} must be a numeric vector.",
                "i" = "You supplied an object of class {.cls {class(year)}}."
            ))
        }
        (year %% 4 == 0) & ((year %% 100 != 0) | (year %% 400 == 0))
    }

# Invalid input test
is_leap_year_cli("2024")


# Scaled power function changes
scaled_power_cli <- function(x, power = 2, scale = 10,
                             method = c("simple", "precise"),
                             plot = TRUE, verbose = TRUE, ...) {
    # X
    if (!is.numeric(x)) {
        cli::cli_abort(c(
            "x" = "Argument {.arg x} must be a numeric vector.",
                "i" = "You supplied an object of class {.cls {class(x)}}."
        ))
    }

    # Power
    if (!is.numeric(power) || length(power) != 1) {
        cli::cli_abort(c(
            "x" = "Argument {.arg power} must be a single numeric value.",
            "i" = "You supplied an object of class {.cls {class(power)}} with length {.val {length(power)}}."
        ))
    }

    # Scale
    if (!is.numeric(scale) || length(scale) != 1) {
        cli::cli_abort(c(
            "x" = "Argument {.arg scale} must be a single numeric value.",
            "i" = "You supplied an object of class {.cls {class(scale)}} with length {.val {length(scale)}}."
        ))
    }

    # Method
    method <- rlang::arg_match(method)

    # Plot
    if (!is.logical(plot) || length(plot) != 1 || is.na(plot)) {
        cli::cli_abort(c(
            "x" = "Argument {.arg plot} must be either {.val {TRUE}} or {.val {FALSE}}.",
            "i" = "You supplied an object of class {.cls {class(plot)}}
            with length {.val {length(plot)}}."
        ))
    }

    # Verbose
    if (!is.logical(verbose) || length(verbose) != 1 || is.na(verbose)) {
        cli::cli_abort(c(
            "x" = "Argument {.arg verbose} must be either {.val {TRUE}} or {.val {FALSE}}.",
            "i" = "You supplied an object of class {.cls {class(verbose)}} with
            length {.val {length(verbose)}}."
        ))
    }

     res <- (x^power) * scale
    if (method == "simple") res <- round(res)
    if (verbose) message("Method selected: '", method, "' | Processing ", length(x), " elements.")
    if (plot) plot(x, res, ...)
    return(res)
}


# Changing the "message" of the scaled function - enhancing consol messaging
scaled_power_cli <- function(x, power = 2, scale = 10,
                             method = c("simple", "precise"),
                             plot = TRUE, verbose = TRUE, ...) {
    # X
    if (!is.numeric(x)) {
        cli::cli_abort(c(
            "x" = "Argument {.arg x} must be a numeric vector.",
            "i" = "You supplied an object of class {.cls {class(x)}}."
        ))
    }}

    # Power
    if (!is.numeric(power) || length(power) != 1) {
        cli::cli_abort(c(
            "x" = "Argument {.arg power} must be a single numeric value.",
            "i" = "You supplied an object of class {.cls {class(power)}} with length {.val {length(power)}}."
        ))
    }

    # Scale
    if (!is.numeric(scale) || length(scale) != 1) {
        cli::cli_abort(c(
            "x" = "Argument {.arg scale} must be a single numeric value.",
            "i" = "You supplied an object of class {.cls {class(scale)}} with length {.val {length(scale)}}."
        ))
    }

    # Method
    method <- rlang::arg_match(method)

    # Plot
    if (!is.logical(plot) || length(plot) != 1 || is.na(plot)) {
        cli::cli_abort(c(
            "x" = "Argument {.arg plot} must be either {.val {TRUE}} or {.val {FALSE}}.",
            "i" = "You supplied an object of class {.cls {class(plot)}}
            with length {.val {length(plot)}}."
        ))
    }

    # Verbose
    if (!is.logical(verbose) || length(verbose) != 1 || is.na(verbose)) {
        cli::cli_abort(c(
            "x" = "Argument {.arg verbose} must be either {.val {TRUE}} or {.val {FALSE}}.",
            "i" = "You supplied an object of class {.cls {class(verbose)}} with
            length {.val {length(verbose)}}."
        ))
    }

    res <- (x^power) * scale
    if (method == "simple") res <- round(res)
    if (verbose) {
        cli::cli_inform(c(
            "v" = "Confirming successful method selection by {.val {method}}.",
            "i" = "Values were multipled by a scale of {.val {scale}}.",
            "*" = "Processed {length(x)} element{?s} using scale = {.val {scale}}."
        ))
    if (plot) plot(x, res, ...)
    return(res)
    }

# Verifying and comparing developer ergonomics
scaled_power_cli(x = "invalid", power = 2)
scaled_power_cli(x = 5, verbose = TRUE, plot = FALSE)
scaled_power_cli(x = 1:5, verbose = TRUE, plot = FALSE)



