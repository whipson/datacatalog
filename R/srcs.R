#' Use a local file src
#'
#' @param paths vector of paths to files and/or directories containing files
#' @param extension which file path extensions to look for
#' @param recurse whether to search recursively in directories
#' @param read_fun callable function capable of reading files
#' @param ... arguments passed to `read_fun`
local_file_src <- function(
  paths,
  extension,
  recurse = FALSE,
  read_fun = read.csv,
  ...
) {

  # Look through all the paths for local file
  files <- purrr::map(paths, ~get_files_with_ext(.x, ext = extension)) |>
    purrr::list_c()

  names <- tools::file_path_sans_ext(basename(files))

  # Read them using the read_fun
  tables <- purrr::map(files, \(x) read_fun(x, ...))

  # Create the catalog entries
  purrr::map2(tables, names, ~{
    col_cat <- data_frame_to_catalog(.x)
    dplyr::tibble(
      table = .y,
      col_cat
    )
  }) |>
    purrr::list_rbind()
}
