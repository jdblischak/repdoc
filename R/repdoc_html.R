#' Reproducible HTML document
#'
#' The output format \code{repdoc_html} automatically 1) sets a seed with
#' \code{\link{set.seed}}, 2) inserts version of Git repo, and 3) inserts
#' \code{\link{sessionInfo}}.
#'
#' @param ... Arguments passed to \code{\link[rmarkdown]{html_document}}
#'
#' @return \code{\link[rmarkdown]{output_format}}
#'
#' @import rmarkdown
#' @export
repdoc_html <- function(...) {

  # knitr options --------------------------------------------------------------

  # Save the figures in "figure/<basename-of-Rmd-file>/"
  # https://yihui.name/knitr/hooks/#option-hooks
  hook_fig_path <- function(options) {
    options$fig.path <- file.path("figure", knitr::current_input(), "")
    return(options)
  }

  knitr <- rmarkdown::knitr_options(opts_chunk = list(comment = NA,
                                                      fig.align = "center",
                                                      tidy = FALSE),
                                    opts_hooks = list(fig.path = hook_fig_path))

  # pre_knit function ----------------------------------------------------------

  # This function copies the R Markdown file to a temporary directory and then
  # modifies it.
  pre_knit <- function(input, ...) {

    # Access parent environment. Have to go up 2 frames because of the function
    # that combines pre_knit function from the current and base output_formats.
    #
    # Inspired by rmarkdowntown by Romain François
    # https://github.com/romainfrancois/rmarkdowntown/blob/deef97a5cd6f0592318ecc6e78c6edd7612eb449/R/html_document2.R#L12
    frames <- sys.frames()
    e <- frames[[length(frames) - 2]]

    lines_in <- readLines(input)
    tmpfile <- file.path(tempdir(), basename(input))
    e$knit_input <- tmpfile

    # Default repdoc options
    repdoc_opts <- list(knit_root_dir = NULL,
                        seed = 12345,
                        github = get_github_from_remote(dirname(input)),
                        sessioninfo = "sessionInfo()")

    # Get options from a potential _repdoc.yml file
    repdoc_root <- try(rprojroot::find_root(rprojroot::has_file("_repdoc.yml"),
                                            path = dirname(input)), silent = TRUE)
    if (class(repdoc_root) != "try-error") {
      repdoc_yml <- file.path(repdoc_root, "_repdoc.yml")
      repdoc_yml_opts <- yaml::yaml.load_file(repdoc_yml)
      for (opt in names(repdoc_yml_opts)) {
        repdoc_opts[[opt]] <- repdoc_yml_opts[[opt]]
      }
      # If knit_root_dir is a relative path, interpret it as relative to the
      # location of _repdoc.yml
      if (!is.null(repdoc_opts$knit_root_dir)) {
        if (!fs::is_absolute_path(repdoc_opts$knit_root_dir)) {
          repdoc_opts$knit_root_dir <- fs::path_abs(file.path(repdoc_root,
                                                              repdoc_opts$knit_root_dir))
        }
      }
    }

    # Get potential options from YAML header. These override the options
    # specified in _repdoc.yml.
    header <- rmarkdown::yaml_front_matter(input)
    header_opts <- header$repdoc
    for (opt in names(header_opts)) {
      repdoc_opts[[opt]] <- header_opts[[opt]]
    }
    # If knit_root_dir was specified as a relative path in the YAML header,
    # interpret it as relative to the location of the file
    if (!is.null(repdoc_opts$knit_root_dir)) {
      if (!fs::is_absolute_path(repdoc_opts$knit_root_dir)) {
        repdoc_opts$knit_root_dir <- fs::path_abs(file.path(dirname(input),
                                                            repdoc_opts$knit_root_dir))
      }
    }

    # If knit_root_dir hasn't been configured in _repdoc.yml or the YAML header,
    # set it to the location of the original file
    if (is.null(repdoc_opts$knit_root_dir)) {
      repdoc_opts$knit_root_dir <- dirname(normalizePath(input))
    }

    # Set the knit_root_dir option for rmarkdown::render. However, the user can
    # override the knit_root_dir option by passing it directly to render.
    if (is.null(e$knit_root_dir)) {
      e$knit_root_dir <- repdoc_opts$knit_root_dir
    } else {
      repdoc_opts$knit_root_dir <- e$knit_root_dir
    }

    # Find the end of the YAML header for inserting new lines
    header_delims <- stringr::str_which(lines_in, "^-{3}|^\\.{3}")
    header_end <- header_delims[2]
    insert_point <- header_end

    # Get output directory if it exists
    output_dir <- get_output_dir(directory = dirname(input))

    report_stats <- generate_report(input,
                                    output_dir = repdoc_opts$knit_root_dir,
                                    sessioninfo = repdoc_opts$sessioninfo)

    report <- format_report(date = report_stats$date,
                            sha = report_stats$sha,
                            status = report_stats$status,
                            rmd_status = report_stats$rmd_status,
                            blobs_html = report_stats$blobs_html,
                            git_html = report_stats$git_html,
                            blobs_rmd = report_stats$blobs_rmd,
                            git_rmd = report_stats$git_rmd,
                            ls_globalenv = report_stats$ls_globalenv,
                            seed = repdoc_opts$seed,
                            github = repdoc_opts$github,
                            sessioninfo = report_stats$sessioninfo)

    # Set seed at beginning
    seed_chunk <- c("",
                    "```{r seed-set-by-repdoc, echo = FALSE}",
                    sprintf("set.seed(%d)", repdoc_opts$seed),
                    "```",
                    "")

    # Add session information at the end
    sessioninfo <- c("",
                     "## Session information",
                     "",
                     "```{r session-info-chunk-inserted-by-repdoc}",
                     repdoc_opts$sessioninfo,
                     "```",
                     "")

    lines_out <- c(lines_in[1:header_end],
                   report,
                   "---",
                   seed_chunk,
                   lines_in[(header_end + 1):length(lines_in)],
                   sessioninfo)

    writeLines(lines_out, tmpfile)
  }

  # post_knit function ---------------------------------------------------------

  # This function adds the navigation bar for websites defined in either
  # _navbar.html or _site.yml. Below I just fix the path to the input file that
  # I had changed for pre_knit and then execute the post_knit from
  # rmarkdown::html_document.
  post_knit <- function(metadata, input_file, runtime, encoding, ...) {

    # Change the input_file back to its original so that the post_knit defined
    # in rmarkdown::html_document() can find the navbar defined in _site.yml.
    input_file_original <- file.path(getwd(), basename(input_file))
    # I tried to find a better solution than directly calling it myself (since
    # it is run afterwards anyways since html_document() is the base format),
    # but nothing I tried worked.
    rmarkdown::html_document()$post_knit(metadata, input_file_original,
                                         runtime, encoding, ...)
  }

  # pre_processor function -----------------------------------------------------

  # Pass additional arguments to Pandoc. I use this to add a custom footer.
  pre_processor <- function(metadata, input_file, runtime, knit_meta,
                            files_dir, output_dir) {
    fname_footer <- tempfile("footer", fileext = ".html")
    footer <- c("<hr>",
                "<p>",
                "This reproducible <a href=\"http://rmarkdown.rstudio.com\">R Markdown</a> analysis was created with <a href=\"https://github.com/jdblischak/repdoc\">repdoc</a>",
                "</p>",
                "<hr>")
    writeLines(footer, con = fname_footer)
    args <- c("--include-after-body", fname_footer)
    return(args)
  }

  # Return ---------------------------------------------------------------------

  o <- rmarkdown::output_format(knitr = knitr,
                                pandoc = pandoc_options(to = "html"),
                                pre_knit = pre_knit,
                                post_knit = post_knit,
                                pre_processor = pre_processor,
                                base_format = rmarkdown::html_document(...))
  return(o)
}
