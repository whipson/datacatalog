catalog_template <- function() {
  dplyr::tibble(
    table = character(),
    column = character(),
    type = character(),
    desc = character()
  )
}
