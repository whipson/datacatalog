is_file_path <- function(path) {
  !fs::dir_exists(.x) && grepl("\\.[^/\\\\]+$", .x)
}

get_files_with_ext <- function(path, ext) {

  if (substr(ext, 1, 1) != ".") {
    ext <- paste0(".", ext)
  }

  path <- normalizePath(path, mustWork = TRUE)

  if (dir.exists(path)) {
    files <- list.files(path, recursive = TRUE, full.names = TRUE)
    matching_files <- files[grepl(paste0("\\", ext, "$"), files, ignore.case = TRUE)]
    return(matching_files)
  }

  if (file.exists(path) && grepl(paste0("\\", ext, "$"), path, ignore.case = TRUE)) {
    return(path)
  }

  character(0)
}

data_frame_to_catalog <- function(df) {

  type_list <- lapply(df, function(x) {
    if (is.character(x)) return("string")
    if (is.numeric(x) && all(x == as.integer(x), na.rm = TRUE)) return("integer")
    if (is.numeric(x)) return("double")
    if (is.logical(x)) return("logical")
    if (inherits(x, "Date")) return("date")
    if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) return("timestamp")
    return("string")
  })

  dplyr::tibble(
    column = names(type_list),
    type = unlist(type_list)
  )
}

data_frame_to_yaml <- function(df, ...) {

  dots <- rlang::list2(...)

  type_list <- lapply(df, function(x) {
    if (is.character(x)) return("string")
    if (is.numeric(x) && all(x == as.integer(x), na.rm = TRUE)) return("integer")
    if (is.numeric(x)) return("double")
    if (is.logical(x)) return("logical")
    if (inherits(x, "Date")) return("date")
    if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) return("timestamp")
    return("string")
  })

  type_list <- lapply(type_list, function(x) {
    list(
      desc = "",
      type = x
    )
  }) |>
    stats::setNames(names(type_list))

  df_name <- deparse(substitute(df))

  list_2_convert <- list(
    ...,
    columns = type_list
  )

  list_2_convert <- list(
    list_2_convert
  ) |>
    stats::setNames(df_name)

  yaml::write_yaml(list_2_convert, file = "catalog.yml")
}
