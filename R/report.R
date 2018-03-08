
create_report <- function(input, output_dir, opts) {

  input <- normalizePath(input)
  input_dir <- dirname(input)

  has_code <- detect_code(input)

  uses_git <- git2r::in_repository(input_dir)
  if (uses_git) {
    r <- git2r::repository(input_dir, discover = TRUE)
    s <- git2r::status(r)
  }

  #repdoc checks ---------------------------------------------------------------
  checks <- list()

  # Check R Markdown status
  if (uses_git) {
    checks$result_rmd <- check_rmd(input, r, s)
  }

  # Check environment
  checks$result_environment <- check_environment()

  # Version history ------------------------------------------------------------

  # Formatting -----------------------------------------------------------------

  checks_formatted <- Map(format_check, checks)
  template <-
"
<strong>repdoc checks:</strong> <small>(Click a bullet for more information)</small>
<ul>
{{{checks}}}
</ul>
"
  data <- list(checks = paste(unlist(checks_formatted), collapse = "\n"))
  report <- whisker::whisker.render(template, data)

  return(report)
}

check_environment <- function() {
  ls_globalenv <- ls(name = .GlobalEnv)
  if (length(ls_globalenv) == 0) {
    pass <- TRUE
    summary <- "<strong>Environment:</strong> empty"
    details <-
"
Great job! The global environment was empty. Objects defined in the global
environment can affect the analysis in your R Markdown file in unknown ways.
For reproduciblity it's best to always run the code in an empty environment.
"
  } else {
    pass <- FALSE
    summary <- "<strong>Environment:</strong> objects present"
    details <-
"
The global environment had objects present when the code in the R Markdown
file was run. These objects can affect the analysis in your R Markdown file in
unknown ways. For reproduciblity it's best to always run the code in an empty
environment. Use <code>wflow_publish</code> or <code>wflow_build</code> to
ensure that the code is always run in an empty environment.
"
    objects_table <- create_objects_table(.GlobalEnv)
    details <- paste(collapse = "\n",
                     details,
                     "<br><br>",
                     "<p>The following objects were defined in the global
                     environment when these results were created:</p>",
                     objects_table)
  }

  return(list(pass = pass, summary = summary, details = details))
}

create_objects_table <- function(env) {
  objects <- ls(name = env)
  classes <- vapply(env, class, character(1))
  sizes <- vapply(env, function(x) format(object.size(x), units = "auto"),
                  character(1))
  df <- data.frame(Name = objects, Class = classes, Size = sizes)
  table <- knitr::kable(df, format = "html", row.names = FALSE)
  # Add table formatting
  table <- stringr::str_replace(table, "<table>",
            "<table style = \"border-collapse:separate; border-spacing:5px;\">")
  return(as.character(table))
}

format_check <- function(check) {
  if (check$pass) {
    symbol <- sprintf("<strong style=\"color:blue;\">%s</strong>",
                      clisymbols::symbol$tick)
  } else {
    symbol <- sprintf("<strong style=\"color:red;\">%s</strong>",
                      clisymbols::symbol$cross)
  }
  template <-
    "
  <li>
  <details>
  <summary>
  {{{symbol}}} {{{summary}}}
  </summary>
  {{{details}}}
  </details>
  </li>
  "
  data <- list(symbol = symbol, summary = check$summary,
               details = check$details)
  text <- whisker::whisker.render(template, data)
  return(text)
}


check_rmd <- function(input, r, s) {

  s_simpler <- lapply(s, unlist)
  s_simpler <- lapply(s_simpler, add_git_path, r = r)

  # Determine current status of R Markdown file
  if (input %in% s_simpler$staged) {
    rmd_status <- "staged"
  } else if (input %in% s_simpler$unstaged) {
    rmd_status <- "unstaged"
  } else if (input %in% s_simpler$untracked) {
    rmd_status <- "untracked"
  } else if (input %in% s_simpler$ignored) {
    rmd_status <- "ignored"
  } else {
    rmd_status <- "up-to-date"
  }

  if (rmd_status == "up-to-date") {
    pass <- TRUE
    summary <- "<strong>R Markdown file:</strong> up-to-date"
    details <-
"
Great! Since the R Markdown file has been committed to the Git repository, you
know the exact version of the code that produced these results.
"
  } else {
    pass <- FALSE
    summary <- "<strong>R Markdown file:</strong> uncommitted changes"
    if (rmd_status %in% c("staged", "unstaged")) {
      details <- sprintf("The R Markdown file has %s changes.", rmd_status)
    } else {
      details <- sprintf("The R Markdown is %s by Git.", rmd_status)
    }
    details <- paste(collapse = " ", details,
"
To know which version of the R Markdown file created these
results, you'll want to first commit it to the Git repo. If
you're still working on the analysis, you can ignore this
warning. When you're finished, you can run
<code>wflow_publish</code> to commit the R Markdown file and
build the HTML.
"
                    )
  }

  return(list(pass = pass, summary = summary, details = details))
}

add_git_path <- function(x, r) {
  if (!is.null(x)) {
    paste0(git2r::workdir(r), x)
  } else {
   NA_character_
  }
}

detect_code <- function(input) {
  stopifnot(file.exists(input))
  lines <- readLines(input)
  code_chunks <- stringr::str_detect(lines, "^```\\{r")
  # Inline code can span multiple lines, so concatenate first. Only interprets
  # as code if at least two characters after the r. A new line counts as a
  # character, which is the same as the space inserted by the collapse.
  code_inline <- stringr::str_detect(paste(lines, collapse = " "),
                                     "`r.{2,}`")
  return(any(code_chunks) || code_inline)
}

generate_report <- function(input, output_dir = NULL, sessioninfo = "") {

  input <- normalizePath(input)

  # Output list to contain results
  o <- list()

  o$date <- Sys.Date()

  uses_git <- git2r::in_repository()
  if (uses_git) {
    r <- git2r::repository()
    log <- git2r::commits(r)
    sha <- log[[1]]@sha
    o$sha <- sha

    s <- git2r::status(r, ignored = TRUE, all_untracked = TRUE)
    o$status <- s
    s_simpler <- Map(unlist, s)
    add_git_path <- function(x) if (!is.null(x)) paste0(git2r::workdir(r), x) else NA_character_
    s_simpler <- Map(add_git_path, s_simpler)
    # Determine current status of R Markdown file
    if (input %in% s_simpler$staged) {
      o$rmd_status <- "staged"
    } else if (input %in% s_simpler$unstaged) {
      o$rmd_status <- "unstaged"
    } else if (input %in% s_simpler$untracked) {
      o$rmd_status <- "untracked"
    } else if (input %in% s_simpler$ignored) {
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
    colnames(o$blobs_html) <- c("Version", "Author", "Date")
    o$blobs_html$Date <- as.Date(o$blobs_html$Date)
    o$blobs_html <- o$blobs_html[order(o$blobs_html$Date, decreasing = TRUE), ]
    o$git_html <- stringr::str_replace(html, git2r::workdir(r), "")
    # R Markdown
    o$blobs_rmd <- blobs[blobs$fname == input,
                       c("commit", "author", "when")]
    colnames(o$blobs_rmd) <- c("Version", "Author", "Date")
    o$blobs_rmd$Date <- as.Date(o$blobs_rmd$Date)
    o$blobs_rmd <- o$blobs_rmd[order(o$blobs_rmd$Date, decreasing = TRUE), ]
    o$git_rmd <- stringr::str_replace(input, git2r::workdir(r), "")
    }

  # Check the global environment for objects
  o$ls_globalenv <- ls(name = .GlobalEnv)

  # Session information
  lines <- readLines(input)
  any_sessioninfo <- stringr::str_detect(lines, "session(_i|I)nfo")
  if (!any(any_sessioninfo) && sessioninfo == "") {
    o$sessioninfo <- FALSE
  } else {
    o$sessioninfo <- TRUE
  }

  return(o)
}


shorten_sha <- function(sha) {
  stringr::str_sub(sha, 1, 7)
}

format_report <- function(date, sha, status, rmd_status, blobs_html, git_html, blobs_rmd, git_rmd, ls_globalenv, seed, github, sessioninfo) {

  if (is.na(github)) {
    sha <- shorten_sha(sha)
    blobs_html$Version <- shorten_sha(blobs_html$Version)
    blobs_rmd$Version <- shorten_sha(blobs_rmd$Version)
  } else {
    sha <- sprintf("<a href=\"%s/tree/%s\" target=\"_blank\">%s</a>", github, sha, shorten_sha(sha))
    html_preview <- "https://htmlpreview.github.io/?"
    blobs_html$Version <- sprintf("<a href=\"%s%s/blob/%s/%s\" target=\"_blank\">%s</a>",
                                  html_preview, github, blobs_html$Version, git_html,
                                  shorten_sha(blobs_html$Version))
    blobs_rmd$Version <- sprintf("<a href=\"%s/blob/%s/%s\" target=\"_blank\">%s</a>",
                                 github, blobs_rmd$Version, git_rmd,
                                 shorten_sha(blobs_rmd$Version))
  }

  if (is.null(rmd_status)) {
    rmd_status_message <- sprintf("<p><strong style=\"color:red;\">%s WARNING:</strong> No Git repository detected</p>", clisymbols::symbol$cross)
  } else if (rmd_status == "up-to-date") {
    rmd_status_message <- sprintf("<p><strong style=\"color:blue;\">%s SUCCESS:</strong> The R Markdown file is up-to-date</p>", clisymbols::symbol$tick)
  } else if (rmd_status == "staged") {
    rmd_status_message <- sprintf("<p><strong style=\"color:red;\">%s WARNING:</strong> The R Markdown file has staged changes</p>", clisymbols::symbol$cross)
  } else if (rmd_status == "unstaged") {
    rmd_status_message <- sprintf("<p><strong style=\"color:red;\">%s WARNING:</strong> The R Markdown file has unstaged changes</p>", clisymbols::symbol$cross)
  } else if (rmd_status == "untracked") {
    rmd_status_message <- sprintf("<p><strong style=\"color:red;\">%s WARNING:</strong> The R Markdown file is untracked by Git</p>", clisymbols::symbol$cross)
  } else if (rmd_status == "ignored") {
    rmd_status_message <- sprintf("<p><strong style=\"color:red;\">%s WARNING:</strong> The R Markdown file is ignored by Git</p>", clisymbols::symbol$cross)
  }

  if (length(ls_globalenv) == 0) {
    ls_globalenv_message <- sprintf("<p><strong style=\"color:blue;\">%s SUCCESS:</strong> These results were generated in a clean R environment</p>", clisymbols::symbol$tick)
  } else {
    ls_globalenv_message <- sprintf("<p><strong style=\"color:red;\">%s WARNING:</strong> These results were <strong>not</strong> generated in a clean R environment</p>", clisymbols::symbol$cross)
  }

  # Format Git status
  status_message <- utils::capture.output(print(status))
  status_message <- c("<pre><code>", status_message, "</code></pre>")
  status_message <- paste(status_message, collapse = "\n")

  # Format seed message
  seed_message <- sprintf("<p><strong style=\"color:blue;\">%s SUCCESS:</strong> These results were generated with the seed %d</p>",
                          clisymbols::symbol$tick, seed)

  # Format sessioninfo message
  if (sessioninfo) {
    sessioninfo_message <- sprintf("<p><strong style=\"color:blue;\">%s SUCCESS:</strong> The session information was recorded at the end of the analysis</p>",
            clisymbols::symbol$tick)
  } else {
    sessioninfo_message <- sprintf("<p><strong style=\"color:red;\">%s WARNING:</strong> The session information was <strong>not</strong> recorded at the end of the analysis</p>",
                                   clisymbols::symbol$cross)
  }

  template <-
"
<strong>Reproducibility report:</strong>
<p>These results were generated with version {{{sha}}} on {{date}}</p>
{{{rmd_status_message}}}
{{#if_html}}
<details>
<summary>Click here to see past versions of the HTML file:</summary>
<table style = \"border-collapse:separate; border-spacing:5px;\">
 <thead>
  <tr>
   <th style=\"text-align:left;\"> Version </th>
   <th style=\"text-align:left;\"> Author </th>
   <th style=\"text-align:left;\"> Date </th>
  </tr>
 </thead>
<tbody>
{{#blobs_html}}
  <tr>
   <td style=\"text-align:left;\"> {{{Version}}} </td>
   <td style=\"text-align:left;\"> {{Author}} </td>
   <td style=\"text-align:left;\"> {{Date}} </td>
  </tr>
  {{/blobs_html}}
</tbody>
</table>
</details>
{{/if_html}}
{{#if_rmd}}
<details>
<summary>Click here to see past versions of the R Markdown file:</summary>
<table style = \"border-collapse:separate; border-spacing:5px;\">
 <thead>
  <tr>
   <th style=\"text-align:left;\"> Version </th>
   <th style=\"text-align:left;\"> Author </th>
   <th style=\"text-align:left;\"> Date </th>
  </tr>
 </thead>
<tbody>
{{#blobs_rmd}}
  <tr>
   <td style=\"text-align:left;\"> {{{Version}}} </td>
   <td style=\"text-align:left;\"> {{Author}} </td>
   <td style=\"text-align:left;\"> {{Date}} </td>
  </tr>
  {{/blobs_rmd}}
</tbody>
</table>
</details>
{{/if_rmd}}
{{{ls_globalenv_message}}}
{{#if_globalenv}}
<details>
<summary>Click here to see the objects defined in the global environment:</summary>
{{ls_globalenv}}
</details>
  {{/if_globalenv}}
<details>
<summary>Click here to see the status of the Git repository:</summary>
{{{status_message}}}
</details>
{{{seed_message}}}
{{{sessioninfo_message}}}
"

  report <- whisker::whisker.render(template,
                     data = list(sha = sha, date = date,
                     rmd_status_message = rmd_status_message,
                     if_html = nrow(blobs_html) > 0,
                     blobs_html = unname(whisker::rowSplit(blobs_html)),
                     if_rmd = nrow(blobs_rmd) > 0,
                     blobs_rmd = unname(whisker::rowSplit(blobs_rmd)),
                     ls_globalenv_message = ls_globalenv_message,
                     if_globalenv = length(ls_globalenv) > 0,
                     ls_globalenv = paste(ls_globalenv, collapse = " "),
                     status_message = status_message,
                     seed_message = seed_message,
                     sessioninfo_message = sessioninfo_message))
  return(report)
}
