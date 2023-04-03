library(targets)

options(tidyverse.quiet = TRUE,
        clustermq.scheduler = "multicore")

# set package needs
tar_option_set(packages = c("arrow",
                            "dataRetrieval",
                            "ggspatial",
                            "sf",
                            "tidyverse",
                            "reticulate",
                            "reticulate"))

source("src/read_data_files.R")
source("src/sb_utils.R")
source("src/data_munge_utils.R")
source("src/plotting_utils.R")

path_to_python <- "C:/ProgramData/Anaconda3"
reticulate::use_python(path_to_python)

# End this file with a list of target objects.
list(

  # Define the path to the folder containing the .npy files
  tar_target(
    file_path,
    "Data/in/model_files"
  ),

  # Define the names of the 9 files
  tar_target(
    files,
    list.files(file_path)
  ),

  tar_target(
    seg_id_nat,
    c("1435", "1436", "1437", "1438", "1439", "1440", "1441", "1442", "1443",
      "1444", "1445", "1446", "1447", "1448", "1449", "1450", "1451", "1452",
      "1453", "1454", "1455", "1456", "1457", "1458", "1459", "1460", "1461",
      "1462", "1463", "1545", "1546", "1547","1548", "1549", "1550", "1551",
      "1552", "1553", "1554", "1555", "1556", "1557", "1558", "1559", "1560",
      "1561", "1562", "1563", "1564", "1565", "1566", "1571", "1572", "1573",
      "1574", "1575")
  ),

  tar_target(
    feather_file,
    read_numpy_files(
      file_path = file_path,
      file_name = files,
      col_names = seg_id_nat,
      output_path = "Data/out"
    ),
    format = "file",
    pattern = map(files)
  ),

  tar_target(
    all_data_feather,
    {
      feather_file %>%
        map_df(~arrow::read_feather(.)) %>%
        arrow::write_feather(., "Data/out/all_data.feather")
      return("Data/out/all_data.feather")
    },
    format = "file"
  ),

  tar_target(
    drb_shapefile,{
      #  sbtools can now handle facets
      sbtools::item_file_download("5f6a285d82ce38aaa244912e",
                                  dest_dir = "Data/in") %>%
        .[grep('reaches.shp', .)] # only grabbing stream reach shapefile
    },
    format = "file"
  ),

  tar_target(
    drb_res_shapefile,{
      #  sbtools can now handle facets
      sbtools::item_file_download("5f6a285d82ce38aaa244912e",
                                  dest_dir = "Data/in") %>%
        .[grep('reservoirs.shp', .)] # only grabbing stream reach shapefile
    },
    format = "file"
  ),

  tar_target(
    p2_obs_temp_csv,
    download_sb_files(sb_id = "5f6a287382ce38aaa2449131",
                      sb_files_to_download = "temperature_observations_drb.zip",
                      dest_dir = "Data/in") %>%
      # have to unzip the file, returns csv in same location
      unzip(zipfile = .,
            overwrite = T,
            exdir = "Data/in"),
    format = "file"
  ),

  tar_target(
    temp_rmse_by_segment,
    calc_rmse(
      in_file = all_data_feather,
      obs_file = p2_obs_temp_csv,
      start_date = as.Date("2006-12-26"),
      end_date = as.Date("2020-04-22"),
      var = "temp",
      out_file = "out/temperature_rmse_segment.csv"
    ),
    format = "file"
  ),

  # tar_target(
  #   figure_temp_timeseries,
  #   plot_time_series(
  #     pred_file = all_data_feather,
  #     obs_file = p2_obs_temp_csv,
  #     start_date = as.Date("2006-12-26"),
  #     end_date = as.Date("2008-12-26"),
  #     output_file = "figures/fig_timeseries.png")
  # ),


  # We created some results of water temperature prediction over 56 river
  # segments in Lordville. Could you help visualize the difference of three
  # methods, heterogeneous graph network (HGN), graph diffusion network (GDN),
  # and the proposed method (proposed), using 100% or 2% training data? We named
  # the output file as method_xx(% training data).npy. Each file is the prediction
  # output matrix for 56 rivers over the period of 2006-12-26 to 2020-06-22
  # (4928 dates).
  tar_target(
    figure_temp_rmse_density,
    plot_rmse_density(
      rmse_file = temp_rmse_by_segment,
      min_n_obs = 2,
      #rmse_lims = c(0, 4),
      xlab = expression("RMSE ("*~degree*C*")"),
      out_file = "figures/fig_temp_rmse_density.png"),
    format = "file"
  ),

  # Another function of the proposed method is that it can create a new version
  # of simulated data combining SNTemp-based stream simulations and GLM-based
  # reservoir simulations. We also want to compare several different versions
  # of simulated data: original SNTemp (sim_temp_SNTemp), the composite data
  # in USGS data release (sim_temp_composite), and the proposed (sim_proposed).
  # We only want to compare their performance in the same testing period of
  # 2006-12-26 to 2020-06-22 (sim_proposed has been cut to this range while
  # sim_temp_SNtemp and sim_temp_composite are from 1980-01-01).
  tar_target(
    figure_simulation_data_density,
    plot_sim_density(
      rmse_file = temp_rmse_by_segment,
      min_n_obs = 2,
      #rmse_lims = c(0, 4),
      xlab = expression("RMSE ("*~degree*C*")"),
      out_file = "figures/fig_simulation_rmse_density.png"),
    format = "file"
  ),

  # Is it possible to also visualize the distribution of errors/ error difference
  #  in a map (showing reservoirs as well)?
  tar_target(
    figure_error_spatial_distribution,
    plot_rmse_map(
      rmse_file = temp_rmse_by_segment,
      shapefile = drb_shapefile,
      res_shapefile = drb_res_shapefile,
      seg_id_nats = seg_id_nat,
      min_n_obs = 2,
      lab = expression("RMSE ("*~degree*C*")"),
      out_file = "figures/fig_temp_rmse_map.png"
    ),
    format = "file"
  ),

  tar_target(
    figure_error_simulation_spatial_distribution,
    plot_rmse_simulation_map(
      rmse_file = temp_rmse_by_segment,
      shapefile = drb_shapefile,
      res_shapefile = drb_res_shapefile,
      seg_id_nats = seg_id_nat,
      min_n_obs = 2,
      lab = expression("RMSE ("*~degree*C*")"),
      out_file = "figures/fig_simulation_rmse_map.png"
    ),
    format = "file"
  )


)

