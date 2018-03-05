

generate_report <- function(input, github = NA_character_, seed = NULL, output_dir = NULL, sessioninfo = "") {

  input <- normalizePath(input)

  # Output list to contain results
  o <- list()

  o$date <- Sys.Date()

  uses_git <- git2r::in_repository()
  if (uses_git) {
    r <- git2r::repository()
    log <- git2r::commits(r)
    sha <- log[[1]]@sha
    sha7 <- stringr::str_sub(sha, 1, 7)
    o$sha <- sha7
    if (!is.na(github)) {
      o$github <- github
    }
    s <- git2r::status(r, ignored = TRUE, all_untracked = TRUE)
    o$status <- s
    s_simpler <- Map(unlist, s)
    add_git_path <- function(x) if (!is.null(x)) paste0(git2r::workdir(r), x) else NA_character_
    s_simpler <- Map(add_git_path, s_simpler)
    # Determine current status of R Markdown file
    if (input %in% s$staged) {
      o$rmd_status <- "staged"
    } else if (input %in% s$unstaged) {
      o$rmd_status <- "unstaged"
    } else if (input %in% s$untracked) {
      o$rmd_status <- "untracked"
    } else if (input %in% s$ignored) {
      o$rmd_status <- "ignored"
    } else {
      o$rmd_status <- "up-to-date"
    }
    # Add past versions of R Markdown file and HTML file
    blobs <- git2r::odb_blobs(r)
    blobs$fname <- file.path(git2r::workdir(r), blobs$path, blobs$name)
    blobs$fname <- fs::path_abs(blobs$fname)
    # HTML
    html <- to_html(input, outdir = output_dir)
    o$blobs_html <- blobs[blobs$fname == html,
                        c("commit", "author", "when")]
    # R Markdown
    o$blobs_rmd <- blobs[blobs$fname == input,
                       c("commit", "author", "when")]
    }

  # Check the global environment for objects
  o$ls_globalenv <- ls(name = .GlobalEnv)

  # Seed
  o$seed <- seed

  # Session information
  lines <- readLines(input)
  any_sessioninfo <- stringr::str_detect(lines, c("sessionInfo", "session_info"))
  if (!any(any_sessioninfo) && sessioninfo == "") {
    o$sessioninfo <- FALSE
  } else {
    o$sessioninfo <- TRUE
  }

  return(o)
}



  # Set seed at beginning
  seed_chunk <- c("",
                  "```{r seed-set-by-repdoc, echo = FALSE}",
                  sprintf("set.seed(%d)", repdoc_opts$seed),
                  "```",
                  "")
  seed_status <- paste(clisymbols::symbol$tick,
                       sprintf("**SUCCESS:** This analysis was run with the seed %d",
                               repdoc_opts$seed))
  report <- c(report, seed_status)

  # Add session information at the end
  sessioninfo <- c("",
                   "## Session information",
                   "",
                   "```{r session-info-chunk-inserted-by-repdoc}",
                   repdoc_opts$sessioninfo,
                   "```",
                   "")







}

format_report <- function() {
  # Start reproducibility report
  report <- c("**Reproducibility report:**",
              sprintf("Results last updated on %s", Sys.Date()))

  # Insert information on the status of the Git repo
  if (git2r::in_repository()) {
    r <- git2r::repository()
    log <- git2r::commits(r)
    sha <- log[[1]]@sha
    sha7 <- stringr::str_sub(sha, 1, 7)
    if (!is.na(repdoc_opts$github)) {
      commit_status <- sprintf("These results were generated with revision [%s](%s)",
                               sha7,
                               paste0(repdoc_opts$github, "/tree/", sha))
    } else {
      commit_status <- sprintf("These results were generated with revision %s",
                               sha7)
    }
    report <- c(report, commit_status)
    s <- git2r::status(r, ignored = TRUE, all_untracked = TRUE)
    s <- Map(unlist, s)
    s <- Map(function(x) if (!is.null(x)) paste0(git2r::workdir(r), x) else NA_character_, s)
    # Determine current status of R Markdown file
    if (normalizePath(input) %in% s$staged) {
      rmd_status <- paste(clisymbols::symbol$cross,
                          "**WARNING:** The R Markdown file had staged changes when the HTML was built")
    } else if (normalizePath(input) %in% s$unstaged) {
      rmd_status <- paste(clisymbols::symbol$cross,
                          "**WARNING:** The R Markdown file had unstaged changes when the HTML was built")
    } else if (normalizePath(input) %in% s$untracked) {
      rmd_status <- paste(clisymbols::symbol$cross,
                          "**WARNING:** The R Markdown file is untracked by Git")
    } else if (normalizePath(input) %in% s$ignored) {
      rmd_status <- paste(clisymbols::symbol$cross,
                          "**WARNING:** The R Markdown file is ignored by Git")
    } else {
      rmd_status <- paste(clisymbols::symbol$tick,
                          "**SUCCESS:** The R Markdown file is up-to-date")
    }
  } else {
    rmd_status <- paste(clisymbols::symbol$cross,
                        "**WARNING:** This project has no version control")
  }
  report <- c(report, rmd_status)

  # Add past versions of R Markdown file and HTML file
  if (git2r::in_repository()) {
    blobs <- git2r::odb_blobs(r)
    blobs$fname <- file.path(git2r::workdir(r), blobs$path, blobs$name)
    blobs$fname <- fs::path_abs(blobs$fname)
    if (!is.na(repdoc_opts$github)) {
      blobs$commit <- paste0("[", blobs$commit, "](", repdoc_opts$github,
                             "/blob/", blobs$commit, "/", blobs$path, "/",
                             blobs$name, ")")
    }
    # HTML
    html <- to_html(input, outdir = output_dir)
    blobs_html <- blobs[blobs$fname == html,
                        c("commit", "author", "when")]
    if (nrow(blobs_html) > 0) {
      # Add HTML preview from https://htmlpreview.github.io/
      if (!is.na(repdoc_opts$github)) {
        blobs_html$commit <- stringr::str_replace(blobs_html$commit,
                                                  "https://github.com",
                                                  "https://htmlpreview.github.io/?https://github.com")
      }
      blobs_html_table <- knitr::kable(blobs_html, format = "html", padding = 10,
                                       row.names = FALSE)
      blobs_html_report <- c("<details>",
                             "<summary>Click here to see past versions of the HTML file:</summary>",
                             "<br>",
                             blobs_html_table,
                             "<br>",
                             "</details>")
      report <- c(report, blobs_html_report)
    }
    # R Markdown
    blobs_rmd <- blobs[blobs$fname == normalizePath(input),
                       c("commit", "author", "when")]
    if (nrow(blobs_rmd) > 0) {
      blobs_rmd_table <- knitr::kable(blobs_rmd, format = "html", padding = 10,
                                      row.names = FALSE)
      blobs_rmd_report <- c("<details>",
                            "<summary>Click here to see past versions of the R Markdown file:</summary>",
                            "<br>",
                            blobs_rmd_table,
                            "<br>",
                            "</details>")
      report <- c(report, blobs_rmd_report)
    }
  }

  # Add Git status
  if (git2r::in_repository()) {
    s <- git2r::status(r)
    s_report <- c("<details>",
                  "<summary>Click here to see the status of the Git repository:</summary>",
                  "<br>",
                  utils::capture.output(print(s)),
                  "<br>",
                  "</details>")
    report <- c(report, s_report)
  }

  # Check the global environment for objects
  ls_globalenv <- ls(name = .GlobalEnv)
  if (length(ls_globalenv) == 0) {
    ls_globalenv_report <- paste(clisymbols::symbol$tick,
                                 "**SUCCESS:** These results were generated in a clean R environment")
  } else {
    ls_globalenv_report <- paste(clisymbols::symbol$cross,
                                 "**WARNING:** These results were **not** generated in a clean R environment",
                                 "<details>",
                                 "<summary>Click here to see the objects defined in the global environment:</summary>",
                                 "<br>",
                                 paste(ls_globalenv, collapse = " "),
                                 "<br>",
                                 "</details>")
  }
  report <- c(report, ls_globalenv_report)

  # Set seed at beginning
  seed_chunk <- c("",
                  "```{r seed-set-by-repdoc, echo = FALSE}",
                  sprintf("set.seed(%d)", repdoc_opts$seed),
                  "```",
                  "")
  seed_status <- paste(clisymbols::symbol$tick,
                       sprintf("**SUCCESS:** This analysis was run with the seed %d",
                               repdoc_opts$seed))
  report <- c(report, seed_status)

  # Add session information at the end
  sessioninfo <- c("",
                   "## Session information",
                   "",
                   "```{r session-info-chunk-inserted-by-repdoc}",
                   repdoc_opts$sessioninfo,
                   "```",
                   "")
  sinfo_status <- paste(clisymbols::symbol$tick,
                        "**SUCCESS:** The session information was recorded at the end of the analysis")
  report <- c(report, sinfo_status)

  # Add paragraph HTML tag to any lines that don't already have an HTML tag
  report <- ifelse(stringr::str_sub(report, 1, 1) != "<",
                   paste0("<p>", report, "</p>"), report)

}
