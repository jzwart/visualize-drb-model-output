

#' modified from scipiper
#'
gd_get <- function(file, download_to, type = NULL, overwrite = TRUE, verbose = FALSE, folder)
{
  browser()
  require("googledrive")
  remote_path <- gd_locate_file(file, folder)
  remote_id <- tail(remote_path$id, 1)
  if (!is.na(remote_id)) {
    if (verbose)
      message("Downloading ", file, " from Google Drive")
    if (!dir.exists(dirname(download_to)))
      dir.create(dirname(download_to), recursive = TRUE)
    googledrive::drive_download(file = googledrive::as_id(remote_id),
                                path = download_to, type = type, overwrite = overwrite,
                                verbose = verbose)
  }
  else {
    stop(paste0("Could not locate ", file, " for download from Google Drive"))
  }
  return(file)
}




#' modified function from scipiper to pull from google drive
#'
#'
gd_locate_file <- function (file, folder)
{
  drive_resource <- parents <- name <- ".dplyr.var"
  relative_path <- get_relative_path(file)
  relative_path_escaped <- relative_path %>%
    gsub(pattern = "[", replacement = "\\[", fixed = TRUE) %>%
    gsub(pattern = "]", replacement = "\\]", fixed = TRUE) %>%
    gsub(pattern = ".", replacement = "\\.", fixed = TRUE) %>%
    gsub(pattern = "/", replacement = "$|^")

  relevant_files <- bind_rows(googledrive::drive_get(id = googledrive::as_id(folder)),
                              googledrive::drive_ls(path = googledrive::as_id(folder),
                                                    pattern = sprintf("^%s$", relative_path_escaped),
                                                    verbose = FALSE, recursive = TRUE)) %>%
    dplyr::mutate(parents = lapply(drive_resource,
                                   function(dr) {
                                     parent <- unlist(dr$parents)
                                     if (is.character(parent))
                                       parent
                                     else NA
                                   })) %>% tidyr::unnest(parents)
  path_elements <- strsplit(relative_path, split = "/")[[1]]
  path_df <- dplyr::filter(relevant_files, id == googledrive::as_id(folder))
  for (i in seq_along(path_elements)) {
    elem <- path_elements[i]
    parent <- path_df[[i, "id"]]
    elem_row <- filter(relevant_files, name == elem, parents ==
                         parent)
    if (nrow(elem_row) == 1) {
      path_exists <- TRUE
      path_df <- bind_rows(path_df, elem_row)
    }
    else if (nrow(elem_row) == 0) {
      path_exists <- FALSE
      path_df <- bind_rows(path_df, tibble(id = NA))
      break
    }
    else if (nrow(elem_row) > 0) {
      stop(sprintf("Found multiple copies of %s in Drive",
                   do.call(file.path, as.list(path_elements[seq_len(i)]))))
    }
  }
  return(path_df)
}

#'
#'
get_relative_path <- function(file)
{
  . <- ".dplyr.var"
  file %>% normalizePath(winslash = "/", mustWork = FALSE) %>%
    gsub(normalizePath(getwd(), winslash = "/"), "", .) %>%
    gsub("^/", "", .)
}
