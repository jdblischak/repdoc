
create_report <- function(input, output_dir, has_code, opts) {

  input <- normalizePath(input)
  input_dir <- dirname(input)

  uses_git <- git2r::in_repository(input_dir)
  if (uses_git) {
    r <- git2r::repository(input_dir, discover = TRUE)
    s <- git2r::status(r)
  } else {
    r <- NULL
    s <- NULL
  }

  # repdoc checks --------------------------------------------------------------
  checks <- list()

  # Check R Markdown status
  if (uses_git) {
    checks$result_rmd <- check_rmd(input, r, s)
  }

  if (has_code) {
    # Check environment
    checks$result_environment <- check_environment()

    # Check seed
    checks$result_seed <- check_seed(opts$seed)

    # Check sessioninfo
    checks$result_sessioninfo <- check_sessioninfo(input, opts$sessioninfo)
  }

  # Check version control
  checks$result_vc <- check_vc(input, output_dir, r, s, opts$github)

  # Formatting checks ----------------------------------------------------------

  checks_formatted <- Map(format_check, checks)
  template_checks <-
"
<strong>repdoc checks:</strong> <small>(Click a bullet for more information)</small>
<ul>
{{{checks}}}
</ul>
"
  data_checks <- list(checks = paste(unlist(checks_formatted), collapse = "\n"))
  report_checks <- whisker::whisker.render(template_checks, data_checks)

  # Version history ------------------------------------------------------------

  if (uses_git) {
    blobs <- git2r::odb_blobs(r)
    versions <- get_versions(input, output_dir, blobs, r, opts$github)
    if (versions == "") {
      report_versions <- versions
    } else {
      template_versions <-
"
<details>
<summary>
<small><strong>Expand here to see past versions:</strong></small>
</summary>
<ul>
{{{versions}}}
</ul>
</details>
"
      report_versions <- whisker::whisker.render(template_versions,
                                                 data = list(versions = versions))
    }
  } else {
    report_versions <- ""
  }

  # Return ---------------------------------------------------------------------

  report <- paste(report_checks, report_versions, collapse = "\n")

  return(report)
}

get_versions <- function(input, output_dir, blobs, r, github) {

  blobs$fname <- file.path(git2r::workdir(r), blobs$path, blobs$name)
  blobs$fname <- fs::path_abs(blobs$fname)
  blobs$ext <- tools::file_ext(blobs$fname)

  html <- to_html(input, outdir = output_dir)
  blobs_file <- blobs[blobs$fname %in% c(input, html),
                      c("ext", "commit", "author", "when")]
  # Exit early if there are no past versions
  if (nrow(blobs_file) == 0) {
    return("")
  }
  colnames(blobs_file) <- c("File", "Version", "Author", "Date")
  blobs_file <- blobs_file[order(blobs_file$Date, decreasing = TRUE), ]
  blobs_file$Date <- as.Date(blobs_file$Date)
  git_html <- stringr::str_replace(html, git2r::workdir(r), "")
  git_rmd <- stringr::str_replace(input, git2r::workdir(r), "")

  if (is.na(github)) {
    blobs_file$Version <- shorten_sha(blobs_file$Version)
  } else {
    html_preview <- "https://htmlpreview.github.io/?"
    blobs_file$Version <- ifelse(blobs_file$File == "html",
                                 # HTML preview URL
                                 sprintf("<a href=\"%s%s/blob/%s/%s\" target=\"_blank\">%s</a>",
                                         html_preview, github, blobs_file$Version,
                                         git_html, shorten_sha(blobs_file$Version)),
                                 # R Markdown GitHub URL
                                 sprintf("<a href=\"%s/blob/%s/%s\" target=\"_blank\">%s</a>",
                                         github, blobs_file$Version, git_rmd,
                                         shorten_sha(blobs_file$Version)))
  }

  template <-
"
<table style = \"border-collapse:separate; border-spacing:5px;\">
<thead>
<tr>
<th style=\"text-align:left;\"> File </th>
<th style=\"text-align:left;\"> Version </th>
<th style=\"text-align:left;\"> Author </th>
<th style=\"text-align:left;\"> Date </th>
</tr>
</thead>
<tbody>
{{#blobs_file}}
<tr>
<td style=\"text-align:left;\"> {{{File}}} </td>
<td style=\"text-align:left;\"> {{{Version}}} </td>
<td style=\"text-align:left;\"> {{Author}} </td>
<td style=\"text-align:left;\"> {{Date}} </td>
</tr>
{{/blobs_file}}
</tbody>
</table>
</details>
  "
  data <- list(blobs_file = unname(whisker::rowSplit(blobs_file)))
  text <- whisker::whisker.render(template, data)

  return(text)
}

check_vc <- function(input, output_dir, r, s, github) {
 if (!is.null(r)) {
   pass <- TRUE
   log <- git2r::commits(r)
   sha <- log[[1]]@sha
   sha7 <- shorten_sha(sha)
   if (!is.na(github)) {
     sha_display <- sprintf("<a href=\"%s/tree/%s\" target=\"_blank\">%s</a>",
                            github, sha, sha7)
   } else {
     sha_display <- sha7
   }
   summary <- sprintf("<strong>Repository version:</strong> %s", sha_display)
   status <- utils::capture.output(print(s))
   status <- c("<pre><code>", status, "</code></pre>")
   status <- paste(status, collapse = "\n")
   details <- paste(collpase = "\n",
"
Great! You are using Git for version control. Tracking code development and
connecting the code version to the results is critical for reproducibility.
The version displayed above was the version of the Git repository at the time
these results were generated.
<br><br>
Note that you need to be careful to ensure that all relevant files for the
analysis have been committed to Git prior to generating the results (you can
use <code>wflow_publish</code> or <code>wflow_commit</code>). repdoc only
checks the R Markdown file, but you know if there are other scripts or data
files that it depends on. Below is the status of the Git repository when the
results were generated:
"
                , status)
 } else {
   pass <- FALSE
   summary <- "<strong>Repository version:</strong> no version control"
   details <-
"
Tracking code development and connecting the code version to the results is
critical for reproducibility. To start using Git, open the Terminal and type
<code>git init</code> in your project directory.
"
 }

  return(list(pass = pass, summary = summary, details = details))
}

check_sessioninfo <- function(input, sessioninfo) {
  # Check if the user manually inserted sessionInfo or session_info (from
  # devtools or sessioninfo packages)
  lines <- readLines(input)
  any_sessioninfo <- stringr::str_detect(lines, "session(_i|I)nfo")
  if (any(any_sessioninfo) || sessioninfo != "") {
    pass <- TRUE
    summary <- "<strong>Session information:</strong> recorded"
    details <-
"
Great job! Recording the operating system, R version, and package versions is
critical for reproducibility.
"
  } else {
    pass <- FALSE
    summary <- "<strong>Session information:</strong> unavailable"
    details <-
"
Recording the operating system, R version, and package versions is critical
for reproducibility. To record the session information, add <code>sessioninfo:
\"sessionInfo()\"</code> to _repdoc.yml. Alternatively, you could use
<code>devtools::session_info()</code> or
<code>sessioninfo::session_info()</code>. Lastly, you can manually add a code
chunk to this file to run any one of these commands and then disable to
automatic insertion by changing the repdoc setting to <code>sessioninfo:
\"\"</code>.
"
  }

  return(list(pass = pass, summary = summary, details = details))
}

check_seed <- function(seed) {
  if (is.numeric(seed) && length(seed) == 1) {
    pass <- TRUE
    seed_code <- sprintf("<code>set.seed(%d)</code>", seed)
    summary <- sprintf("<strong>Seed:</strong> %s", seed_code)
    details <- sprintf(
"
The command %s was run prior to running the code in the R Markdown file.
Setting a seed ensures that any results that rely on randomness, e.g.
subsampling or permutations, are reproducible.
"
                       , seed_code)
  } else {
    pass <- FALSE
    summary <- "<strong>Seed:</strong> none"
    details <-
"
No seed was set with <code>set.seed</code> prior to running the code in the R
Markdown file. Setting a seed ensures that any results that rely on
randomness, e.g. subsampling or permutations, are reproducible. To set a seed,
specify an integer value for the option seed in _repdoc.yml or the YAML header
of the R Markdown file.
"
  }

  return(list(pass = pass, summary = summary, details = details))
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
  classes <- vapply(objects, function(x) paste(class(env[[x]]), collapse = ";"),
                    character(1))
  sizes <- vapply(objects, function(x) format(object.size(env[[x]]), units = "auto"),
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
