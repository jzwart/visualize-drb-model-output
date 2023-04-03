#' collection of functions to help with downloading from ScienceBase. Borrowed from
#' https://code.usgs.gov/wma/proxies/habs/wq-data-prep/-/blob/main/1_fetch/src/sb_fetch_helpers.R


#' @title Download files from ScienceBase
#' @description Download specific ScienceBase files from one ScienceBase item to a local
#' directory. Set up to retry at least 3 times if there is an error to handle network flakiness.
#' @param sb_id character string of the ScienceBase item to inventory
#' @param sb_files_to_download character vector of the filenames on ScienceBase. Designed to work
#' with output from `inventory_sb_pcode_files()`.
#' @param dest_dir character string of the local directory (which should exist already) to
#' download the ScienceBase files to.
download_sb_files <- function(sb_id, sb_files_to_download, dest_dir) {

  # Download the files and save in the local directory
  files_local_name <- file.path(dest_dir, sb_files_to_download)
  # files_out <- retry(
  #   sbtools::item_file_download(
  #     sb_id,
  #     names = sb_files_to_download,
  #     destinations = files_local_name,
  #     overwrite_file = TRUE),
  #   when = "Error:",
  #   max_tries = 3)
  files_out <- sbtools::item_file_download(
    sb_id,
    names = sb_files_to_download,
    destinations = files_local_name,
    overwrite_file = TRUE)

  # `item_file_download()` returns the local filepaths
  # so pass these on to the user
  return(files_out)
}
