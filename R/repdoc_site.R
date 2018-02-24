#' Custom site generator for repdoc websites
#'
#' \code{repdoc_site} is a
#' \href{http://rmarkdown.rstudio.com/rmarkdown_site_generators.html}{custom
#' site generator} to be used in combination with the R Markdown output format
#' \code{\link{repdoc_html}}.
#'
#' Do not call the function \code{repdoc_site} directly. Instead insert the line
#' below directly into the YAML header of the file \code{index.Rmd}:
#'
#' \code{site: repdoc:repdoc_site}
#'
#' Then you can build the website by running \code{\link[rmarkdown]{render_site}}
#' in the R console or clicking the Knit button in RStudio.
#'
#' If you receive an error when using the RStudio Knit button (the error is
#' about an unused argument), make sure the Knit Directory is set to Document
#' Directory (you can set this with the dropdown menu next to the Knit button).
#'
#' @param input character. The name of the website directory or a specific R
#'   Markdown file in the website directory.
#' @param encoding character. The
#'   \href{https://en.wikipedia.org/wiki/Character_encoding}{character encoding}
#'   to use to read the file.
#' @param ... Placeholder for potential future use.
#'
#' @seealso \code{\link{repdoc_html}}, \code{\link[rmarkdown]{render_site}}
#'
#' @import rmarkdown
#' @export
repdoc_site <- function(input, encoding = getOption("encoding"), ...) {

  render <- function(input_file,
                     output_format,
                     envir,
                     quiet,
                     encoding, ...) {

    input <- normalizePath(input)

    # Get repdoc options
    repdoc_root <- try(rprojroot::find_root(rprojroot::has_file("_repdoc.yml"),
                                        path = input), silent = TRUE)
    if (class(repdoc_root) != "try-error") {
      repdoc_yml <- file.path(repdoc_root, "_repdoc.yml")
      repdoc_opts <- yaml::yaml.load_file(repdoc_yml)
    } else {
      repdoc_opts <- list()
    }

    # Set knit_root_dir
    if (is.null(repdoc_opts$knit_root_dir)) {
      knit_root_dir <- NULL
    } else {
      knit_root_dir <- normalizePath(file.path(repdoc_root, repdoc_opts$knit_root_dir))
    }

    if (is.null(input_file)) {
      files <- list.files(input, pattern = "^[^_].*\\.[Rr]md$")
      files <- file.path(input, files)
    } else {
      files <- input_file
    }

    # Get output directory if it exists
    site_fname <- file.path(input, "_site.yml")
    site_yml <- yaml::yaml.load_file(site_fname)
    if (is.null(site_yml$output_dir)) {
      output_dir <- input
    } else {
      output_dir <- file.path(input, site_yml$output_dir)
      dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
      output_dir <- normalizePath(output_dir)
    }

    # For an R Markdown website, the output_options self_contained and lib_dir
    # must be set. Force them here instead of temporarily editing the _site.yml
    # file.
    output_options <- list(self_contained = FALSE,
                           lib_dir = "site_libs")

    for (f in files) {
      suppressMessages(
        output_file <- rmarkdown::render(f,
                                         output_format = output_format,
                                         output_options = output_options,
                                         knit_root_dir = knit_root_dir,
                                         envir = envir,
                                         quiet = quiet,
                                         encoding = encoding)
      )

      if (output_dir != input) {
        # Move HTML file
        file.copy(output_file, output_dir, overwrite = TRUE)
        unlink(output_file)
        output_file <- file.path(output_dir, basename(output_file))

        # Move figures
        fig_dir <- file.path(input, "figure", basename(f))
        if (dir.exists(fig_dir)) {
          fig_output_dir <- file.path(output_dir, "figure")
          dir.create(fig_output_dir, showWarnings = FALSE)
          file.copy(fig_dir, fig_output_dir, recursive = TRUE)
          unlink(fig_dir, recursive = TRUE)
        }
      }
    }

    # Clean up source directory
    if (output_dir != input) {
      # Move site libraries
      site_libs <- file.path(input, "site_libs")
      file.copy(site_libs, output_dir, recursive = TRUE)
      unlink(site_libs, recursive = TRUE)
      # Remove figure directory
      unlink(file.path(input, "figure"), recursive = TRUE)
    }

    # Open in RStudio Viewer
    if (!quiet) {
      message("\nOutput created: ", output_file)
    }
  }

  # return site generator
  list(
    name = "not implemented",
    output_dir = "not implemented",
    render = render,
    clean = function() stop("Not implemented", call. = FALSE)
  )
}
