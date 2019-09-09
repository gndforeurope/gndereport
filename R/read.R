#' @export
parse_user_comment <- function(my_file) {

  my_regex <-
    paste0(
      paste0(rep('([^\\t]+)\\t', 7), collapse = ''),
      '(.+)$'
    )

  file_contents <- readr::read_file(my_file)
  unparsed_fields <- stringr::str_match(file_contents, stringr::regex('Text\\n(.+)$', dotall = TRUE))[1,2]
  fields <- stringr::str_match(unparsed_fields, stringr::regex(my_regex, dotall = TRUE))[1, -1]

  tbl <- tibble::tibble(
    date = lubridate::as_datetime(fields[1]),
    chapter = stringr::str_squish(fields[2]),
    name = stringr::str_squish(fields[3]),
    email = stringr::str_squish(fields[4]),
    organization = stringr::str_squish(fields[5]),
    country = stringr::str_squish(fields[6]),
    choice = stringr::str_squish(fields[7]),
    text = stringr::str_squish(fields[8])
  )

  # Returns a one-row tibble
  return(tbl)
}

#' @export
read_feedback <- function(directory) {

  # Check that `directory` is a single directory path
  if (!rlang::is_scalar_character(directory))
    stop('directory must be a single directory path.')

  # Check that `directory` exists
  if (!fs::dir_exists(directory))
    stop('directory path does not exist!')

  # Check that `directory` contains files
  files <- fs::dir_ls(directory)
  if (rlang::is_empty(files))
     stop('directory is empty!')

  my_tbl <- tibble::as_tibble(data.table::rbindlist(fs::dir_map(directory, fun = parse_user_comment)))

  return(my_tbl)
}
