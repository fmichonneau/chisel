all_people <- function() {

  api_key <- get_env_var("REDASH_QUERY128")
  url <- paste0(
    "https://redash.carpentries.org/api/queries/128/results.csv?",
    api_key
  )

  readr::read_csv(
    file = url
  ) %>%
  dplyr::rename(
    email = person_email,
    pub_name =  person_name
  )
}
