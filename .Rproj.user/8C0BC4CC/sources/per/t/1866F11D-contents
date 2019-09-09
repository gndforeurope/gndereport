#' @export
str_to_title2 <- function(strings) {

  is_single_word <- stringr::str_detect(strings, '^[:upper:]\\w+$')

  strings2 <- stringr::str_to_title(strings)
  strings2[is_single_word] <- strings[is_single_word]

  return(strings2)
}

#' @export
fix_prepositions_case <- function(strings) {

  strings2 <- strings
  strings2 <- stringr::str_replace(strings2, '\\bOf\\b', 'of')
  strings2 <- stringr::str_replace(strings2, '\\bThe\\b', 'the')
  strings2 <- stringr::str_replace(strings2, '\\bAnd\\b', 'and')
  strings2 <- stringr::str_replace(strings2, '\\bFor\\b', 'for')
  strings2 <- stringr::str_replace(strings2, '\\bOn\\b', 'on')
  strings2 <- stringr::str_replace(strings2, '\\bDe\\b', 'de')
  strings2 <- stringr::str_replace(strings2, '\\bDes\\b', 'des')

  return(strings2)

}


#' @export
country_remap <- function(my_tbl) {

  country_name <- my_tbl$country

  country_map <-
    c('uk' = 'United Kingdom',
      'united kigdom' = 'United Kingdom',
      'uk and others' = 'United Kingdom',
      'uk and others (internationals network)' = 'United Kingdom',
      'usa' = 'United States of America',
      'united states' = 'United States of America',
      'the netherlands' = 'Netherlands',
      'espa\u00F1a' = 'Spain',
      'frnce' = 'France',
      'espanya' = 'Spain',
      'portugal/france' = 'France',
      'deutschland' = 'Germany'
    )

  country_names2 <- country_map[tolower(country_name)]
  country_names2[is.na(country_names2)] <- country_name[is.na(country_names2)]

  names(country_names2) <- NULL

  my_tbl$country <- stringr::str_to_title(country_names2)
  my_tbl$country <- fix_prepositions_case(my_tbl$country)

  return(my_tbl)
}

#' @export
organization_remap <- function(my_tbl) {

  organization_name <- my_tbl$organization

  # Left-hand values must be lowercase
  organization_map <-
    c('diem25' = 'DiEM25',
      'diem' = 'DiEM25',
      'diem25, doctors without borders' = 'DiEM25',
      'diem25, aufstehen' = 'DiEM25',
      'aufstehen, diem25' = 'DiEM25',
      'regular member of diem25' = 'DiEM25',
      'cleantech21 foundation' = 'Cleantech21',
      'individual' = 'individual',
      '-' = 'individual',
      'normal citizen' = 'individual',
      'na' = 'individual',
      'none' = 'individual',
      'scientist' = 'individual',
      'independent economist' = 'individual',
      'alaskamanspeaks.org@word press.com' = 'individual',
      'energy transition student' = 'individual',
      'private practice medical doctor - neurologist' = 'individual',
      'independent' = 'individual'
    )

  organization_names2 <- organization_map[tolower(organization_name)]
  organization_names2[is.na(organization_names2)] <- organization_name[is.na(organization_names2)]

  names(organization_names2) <- NULL
  my_tbl$organization <- str_to_title2(organization_names2)
  my_tbl$organization <- fix_prepositions_case(my_tbl$organization)
  return(my_tbl)
}

#' @export
remove_na_lines <- function(tbl) {
  dplyr::filter_all(tbl, dplyr::any_vars(!is.na(.)))
}

#' @export
names_to_titlecase <- function(my_tbl) {
  my_tbl$name <- stringr::str_to_title(my_tbl$name)
  return(my_tbl)
}

#' @export
tidy_feedback <- function(my_tbl) {

  my_tbl %>%
    remove_na_lines() %>%
    country_remap %>%
    organization_remap %>%
    names_to_titlecase %>%
    dplyr::arrange(date) -> my_tbl2

  my_tbl3 <- tibble::add_column(my_tbl2, Identifier = 1:nrow(my_tbl2), .before = TRUE)

  return(my_tbl3)
}
