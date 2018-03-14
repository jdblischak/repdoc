# If the user doesn't define a URL for a GitHub repo in the YAML header or
# _repdoc.yml, determine the URL from the remote "origin". If this remote
# doesn't exist, return NA.
get_github_from_remote <- function(path) {
  # HTTPS: https://github.com/jdblischak/repdoc.git
  # SSH: git@github.com:jdblischak/repdoc.git
  if (!git2r::in_repository(path = path)) {
    return(NA_character_)
  }
  r <- git2r::repository(path = path, discover = TRUE)
  remotes <- git2r::remotes(r)
  if (!("origin" %in% remotes)) {
    return(NA_character_)
  }
  origin <- git2r::remote_url(r, remote = "origin")
  if (!stringr::str_detect(origin, "github")) {
    return(NA_character_)
  }
  github <- origin
  # Remove trailing .git
  github <- stringr::str_replace(github, "\\.git$", "")
  # If SSH, replace with HTTPS URL
  github <- stringr::str_replace(github, "^git@github.com:", "https://github.com/")
  return(github)
}


# Get output directory if it exists
get_output_dir <- function(directory, yml = "_site.yml") {

  stopifnot(dir.exists(directory))

  site_fname <- file.path(directory, "_site.yml")
  if (!file.exists(site_fname)) {
    return(NULL)
  }
  site_yml <- yaml::yaml.load_file(site_fname)

  if (is.null(site_yml$output_dir)) {
    output_dir <- directory
  } else {
    output_dir <- file.path(directory, site_yml$output_dir)
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
    output_dir <- normalizePath(output_dir)
  }

  return(output_dir)
}

# Function copied from workflowr:
#
# https://github.com/jdblischak/workflowr/blob/f151235d724e6daffd159df56f00cb5315375c9a/R/utility.R#L65
#
# Convert R Markdown file to corresponding HTML
to_html <- function(files, outdir = NULL) {
  ext <- tools::file_ext(files)
  if (!all(stringr::str_detect(ext, "[Rr]md$")))
    stop("Invalid file extension")
  html <- stringr::str_replace(files, "[Rr]md$", "html")
  if (!is.null(outdir)) {
    # Remove trailing slash if present
    outdir <- stringr::str_replace(outdir, "/$", "")
    # Only prepend outdir if it's not "." for current working directory
    if (outdir == ".") {
      html <- basename(html)
    } else {
      html <- file.path(outdir, basename(html))
    }
  }
  return(html)
}

r <- git2r::repository()
s <- git2r::status(r)

# Convert named nested list to data frame
status_to_df <- function(s) {
  stopifnot(class(s) == "git_status")
  s_vec <- unlist(s)
  if (length(s_vec) > 0) {
    categories <- stringr::str_split(names(s_vec), pattern = "\\.", n = 2,
                                     simplify = TRUE)
    d <- data.frame(categories, s_vec, stringsAsFactors = FALSE)
    colnames(d) <- c("state1", "state2", "file")
  } else {
    d <- data.frame(state1 = character(0), state2 = character(0),
                    file = character(0))
  }

  return(d)
}

# Convert data frame to git_status
df_to_status <- function(d) {
  stopifnot(is.data.frame(d),
            colnames(d) == c("state1", "state2", "file"))
  status <- list(staged = list(), unstaged = list(), untracked = list())
  for (i in seq_along(d$file)) {
    status[[d$state1[i]]] <- c(status[[d$state1[i]]], list(d$file[i]))
    names(status[[d$state1[i]]])[length(status[[d$state1[i]]])] <- d$state2[i]
  }
  class(status) <- "git_status"
  return(status)
}
