
read_numpy_files <- function(file_path, file_name, col_names, output_path) {
  np <- import("numpy")

  model_name <- sub("\\.npy$", "", file_name) # Drop the .npy extension to create the model name
  file_data <- np$load(file.path(file_path, file_name), allow_pickle=TRUE)
  out_tibble <- as_tibble(t(file_data))

  if(model_name %in% c("sim_temp_SNTemp", "sim_temp_composite")){
    start_date <- as.Date("1980-01-01")
    end_date <- as.Date("2020-07-31")
  }else{
    start_date <- as.Date("2006-12-26")
    end_date <- as.Date("2020-06-22")
  }
  date_range <- seq(start_date, end_date, by = "day")
  # browser()
  colnames(out_tibble) <- col_names
  out_tibble$date <- date_range

  if(grepl("sim", model_name)){
    out_tibble <- out_tibble %>%
      pivot_longer(cols = -date,
                   names_to = "seg_id_nat",
                   values_to = "stream_temp_c") %>%
      mutate(model_name = model_name,
             model = model_name,
             data_sparsity = 100)
  }else{
    out_tibble <- out_tibble %>%
      pivot_longer(cols = -date,
                   names_to = "seg_id_nat",
                   values_to = "stream_temp_c") %>%
      mutate(model_name = model_name,
             model = str_extract(model_name, "^[^_]+"),
             data_sparsity = as.numeric(str_extract(model_name, "(?<=_)\\d+")))
  }


  # Write the data frame to a Feather file
  output_file <- file.path(output_path, paste0(model_name, ".feather"))
  arrow::write_feather(out_tibble, output_file)
  return(output_file)
}
