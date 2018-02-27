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
  github <- stringr::str_extract(origin, "[:alnum:]+/[:alnum:]+\\.git$")
  github <- stringr::str_replace(github, "\\.git$", "")
  github <- paste0("https://github.com/", github)
  return(github)
}
