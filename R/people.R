## Uses the AMY API to get the full list of people
get_amy_people <- function() {
  endpoint <- "api/v1/persons"
  amy <- crul::HttpClient$new(
    url = "https://amy.software-carpentry.org/",
    auth = crul::auth(Sys.getenv("AMY_USER"),
                      Sys.getenv("AMY_PASS"))
  )

  first_page_raw  <- amy$get(endpoint)
  first_page <- jsonlite::fromJSON(first_page_raw$parse(encoding = "utf-8"))
  n_pages <- ceiling(first_page$count/nrow(first_page$results))

  if (n_pages < 2) return(first_page$results)

  other_pages <- purrr::map_df(seq.int(2, n_pages), function(.p) {
    message(.p, " out of ",  n_pages, " ... ", appendLF = FALSE)
    .res <- amy$get(paste0(endpoint, "/?page=", .p))
    message("DONE.")
    jsonlite::fromJSON(.res$parse(encoding = "utf-8"))$results
  })

  other_pages

}

## caches the AMY persons data for 1 week, and redownload the data if cache is
## older
all_people <- function(path = "local_data/people") {
  st <- storr::storr_rds(path)

  initialize <- function() {
    key <- as.character(as.POSIXct(Sys.time()))
    st$set(key, get_amy_people())
  }

  ## if the storr is empty, we initialize it
  if (identical(length(st$list()), 0L)) {
    message("initializing storr")
    initialize()
  } else if (identical(length(st$list()), 1L)) {
    ## if it's not, we check the last time it was created; and if it is more
    ## than one week old, we redownload the data
    curr_time <- as.POSIXct(Sys.time())
    cache_time <- as.POSIXct(st$list()[[1L]])
    one_week_in_sec <- 60 * 60 * 24 * 7
    diff_time <- difftime(curr_time, cache_time, units = "secs")
    ## refresh old cache
    if (diff_time > one_week_in_sec) {
      st$del(st$list()[[1L]])
      initialize()
    }
  } else {
    stop("too many elements in the storr")
  }

  st$get(st$list()[[1]])

}
