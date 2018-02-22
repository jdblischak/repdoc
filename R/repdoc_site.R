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

    move_safe <- function(from, to, overwrite = TRUE, recursive = dir.exists(to)) {
      file.copy(from, to, overwrite = overwrite, recursive = recursive)
      unlink(from, recursive = TRUE)
    }

    for (f in files) {
      suppressMessages(
        output_file <- rmarkdown::render(f,
                                         output_format = output_format,
                                         output_options = output_options,
                                         knit_root_dir = NULL,
                                         envir = envir,
                                         quiet = quiet,
                                         encoding = encoding)
      )
      move_safe(output_file, output_dir)
      output_file <- file.path(output_dir, basename(output_file))
      # Move site libraries
      move_safe(file.path(input, "site_libs"), output_dir)
      # Move figures
      fig_dir <- file.path(input, "figure", basename(f))
      if (dir.exists(fig_dir)) {
        fig_output_dir <- file.path(output_dir, "figure")
        dir.create(fig_output_dir,showWarnings = FALSE)
        move_safe(fig_dir, fig_output_dir)
      }
    }

    # Delete figure directory
    if (output_dir != input) {
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
