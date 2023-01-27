#' Get all badged people who consent to publish their name
#'
#' This will use the data used to populate https://feeds.carpentries.org and our
#' main websites to cross-reference against whose names we can publish on the 
#' lesson releases. 
#'
#' This requires the REDASH_QUERY128 API key to be set in your envvars.
#'
#' @return a data frame with the following columns:
#'   - person_name_with_middle: person's first, middle, and last names, 
#'       if provided
#'   - pub_name: person's first and last names, if provided
#'   - email: person's primary email
#'   - github: person's primary github account name
#'   - twitter: person's twitter identity
#'   - orcid: person's orcID
#'   - url: url of person's website
#'   - country: self-reported country
#'   - iata: Nearest Airport Code
#'   - latitude: latitude of Nearest Airport
#'   - longitude: longitutde of Nearest Airport
#'   - badges: comma separated list of badge IDs (integers) from AMY
#'   - publish_profile: an integer. 1 indicates publishing consent, 
#'       0 indicates revocation
#'   - lesson_publication_consent: one of "no", "amy" "orcid", 
#'       "github" or "unset"
#'
#' @noRd
all_people <- function() {

  api_key <- get_env_var("REDASH_QUERY128")
  url <- paste0(
    "https://redash.carpentries.org/api/queries/128/results.csv?api_key=",
    trimws(api_key)
  )

  res <- readr::read_csv(
    file = url,
    show_col_types = FALSE
  ) 

  dplyr::rename(res,
    email = person_email,
    pub_name =  person_name
  )
}

#' Extract git username from an email
#'
#' GitHub emails are often XXXXXX+[username]@users.noreply.github.com to
#' mask user's email addresses and it messes with our matching in AMY, so
#' if we have this pattern, we can get the github username from the email
#' and then match it against our AMY database
#'
#' @param email a character vector of emails
#' @return a character string of github usernames where they could be found
git_user_from_email <- function(email) {
  email[!grepl("@users.noreply.github.com", email, fixed = TRUE)] <- NA
  sub("^([0-9]{6,}[+])?(.+?)[@]users.noreply.github.com", "\\2", email)
}
