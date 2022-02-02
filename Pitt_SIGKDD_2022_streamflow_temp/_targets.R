# main target script for calling all subsequent targets
library(targets)

tar_option_set(packages = c(
                 "cowplot",
                 "ggspatial",
                 "googledrive",
                 "reticulate",
                 "sbtools",
                 "sf",
                 "tidyverse"
               )
)


source("src/gd_utils.R")
source("src/plotting_functions.R")
source("src/data_munge_utils.R")
np = reticulate::import("numpy")

# complete list of targets
targets_list <- list(

  # get the data from google drive
  # I attempted to download the files in the pipeline from google drive folder
  # but kept getting errors. So I manually downloaded and added the files
  # to the /in/ directory. Google drive folder is https://drive.google.com/drive/folders/1Nnx9rwwQy5_CTEyL0R2O8kxoSdDEXHVr
  # tar_target(
  #   google_drive_folder,
  #   "1Nnx9rwwQy5_CTEyL0R2O8kxoSdDEXHVr"
  # ),
  #
  # tar_target(
  #   test_dates,
  #   gd_get(
  #     file = "Streamflow_10/GMAML.npy",
  #     download_to = "in/dates_test.npy",
  #     overwrite = TRUE,
  #     folder = google_drive_folder)
  # )

  tar_target(
    test_dates,
    np$load("in/dates_test.npy", allow_pickle = TRUE) %>%
      as.Date()
  ),

  tar_target(
    seg_id_nats,
    np$load("in/seg_ids.npy", allow_pickle = TRUE) %>%
      as.character()
  ),

  tar_target(
    drb_shapefile,{
      #  sbtools can now handle facets
      sbtools::item_file_download("5f6a285d82ce38aaa244912e",
                                  dest_dir = "in") %>%
        .[grep('reaches.shp', .)] # only grabbing stream reach shapefile
    },
    format = "file"
  ),

  # A figure/map that shows these segments in DRB (just to show readers
  #  our study region).
  tar_target(
    figure_drb_map,
    plot_drb_map(
      shapefile = drb_shapefile,
      out_file = "out/drb_map.png"
    ),
    format = "file"
  ),

  # Comparison between three methods over different segments. In each folder from
  #  {"Stream 10", "Stream 100", "Temperature 1", "Temperature 100"}, we include
  #  three output files, HGN and GMAML are baselines, and MTHGN is our proposed
  #  method. We also include label_training and label_testing in each folder
  #  for your reference. All the output files and label_testing files are in
  #  the shape of 456 (segments)-by-4900 (dates), and 4900 testing dates are
  #  10/31/2006-03/30/2020. The training label file is in the shape of 456
  #  (segments)-by-9800 (dates), which is taken from 9800 dates before the
  #  testing period. For both training and testing labels, we use '-11' as the
  #  default value to mark missing observations. Each folder is an individual
  #  test, e.g., "temperature 1" represents the temperature prediction test using
  #  1% training data (stored in label_training).

  # was lazy and didn't make these targets based on file updates from in/ folder
  tar_target(
    temp_preds_obs,
    combine_temp_preds_obs(
      folders = c("Temperature_1", "Temperature_100"),
      models = c("HGN", "GMAML", "MTHGN"),
      obs_file = "label_temp_testing.npy",
      dates = test_dates,
      seg_ids = seg_id_nats,
      out_file = "out/temperature_preds_obs.rds"
    ),
    format = "file"
  ),

  tar_target(
    flow_preds_obs,
    combine_flow_preds_obs(
      folders = c("Streamflow_10", "Streamflow_100"),
      models = c("HGN", "GMAML", "MTHGN"),
      obs_file = "label_flow_testing.npy",
      dates = test_dates,
      seg_ids = seg_id_nats,
      out_file = "out/flow_preds_obs.rds"
    ),
    format = "file"
  ),

  tar_target(
    temp_rmse_by_segment,
    calc_rmse(
      in_file = temp_preds_obs,
      var = 'temp',
      out_file = "out/temperature_rmse_segment.csv"
    ),
    format = "file"
  ),

  tar_target(
    flow_rmse_by_segment,
    calc_rmse(
      in_file = flow_preds_obs,
      var = 'flow',
      out_file = "out/flow_rmse_segment.csv"
    ),
    format = "file"
  ),

  tar_target(
    figure_temp_rmse_density,
    plot_rmse_density(
      rmse_file = temp_rmse_by_segment,
      min_n_obs = 0,
      rmse_lims = c(0,4),
      xlab = expression("RMSE ("*~degree*C*")"),
      out_file = 'out/temp_rmse_density.png'
    ),
    format = "file"
  ),

  tar_target(
    figure_flow_rmse_density,
    plot_rmse_density(
      rmse_file = flow_rmse_by_segment,
      min_n_obs = 0,
      rmse_lims = c(0,100),
      xlab = expression(RMSE~(m^{3}~sec^{-1})),
      log_x_axis = T,
      out_file = 'out/flow_rmse_density.png'
    ),
    format = "file"
  ),

  # RMSE comparison between MTHGN and HGN on all the 456 segments
  tar_target(
    figure_temp_rmse_map_compare,
    plot_rmse_map(
      rmse_file = temp_rmse_by_segment,
      shapefile = drb_shapefile,
      min_n_obs = 0,
      lab = expression("RMSE ("*~degree*C*")"),
      out_file = 'out/temp_rmse_map_compare.png'
    ),
    format = "file"
  ),

  tar_target(
    figure_flow_rmse_map_compare,
    plot_rmse_map(
      rmse_file = flow_rmse_by_segment,
      shapefile = drb_shapefile,
      min_n_obs = 0,
      out_file = 'out/flow_rmse_map_compare.png'
    ),
    format = "file"
  ),

  # Clustering results: we did a clustering over 456 segments, and we wish to
  #  see how the clustering over segments looks like. The cluster labels are
  #  included in the "Clustering " folder. Each file is a 456 vector, where
  #  each entry is the group label of a segment. The three files are the
  #  clusterings obtained with different settings (1. 10 clusters based on
  #  temperature data, 2. 5 clusters based on temperature data, and 3. 10 clusters
  #  based on flow data ).

  tar_target(
    temp_rmse_by_cluster_5,
    calc_rmse_cluster(
      in_file = temp_preds_obs,
      var = 'temp',
      cluster_file = 'in/Clustering/cluster_5_temperature.npy',
      seg_ids = seg_id_nats,
      out_file = 'out/temperature_rmse_cluster_5.csv'
    ),
    format = "file"
  ),

  tar_target(
    temp_rmse_by_cluster_10,
    calc_rmse_cluster(
      in_file = temp_preds_obs,
      var = 'temp',
      cluster_file = 'in/Clustering/cluster_10_temperature.npy',
      seg_ids = seg_id_nats,
      out_file = 'out/temperature_rmse_cluster_10.csv'
    ),
    format = "file"
  ),

  tar_target(
    flow_rmse_by_cluster_10,
    calc_rmse_cluster(
      in_file = temp_preds_obs,
      var = 'temp',
      cluster_file = 'in/Clustering/cluster_10_streamflow.npy',
      seg_ids = seg_id_nats,
      out_file = 'out/flow_rmse_cluster_10.csv'
    ),
    format = "file"
  ),

  tar_target(
    figure_temp_rmse_cluster_5,
    plot_rmse_scatter(
      rmse_file = temp_rmse_by_cluster_5,
      min_n_obs = 0,
      ylab = expression("RMSE ("*~degree*C*")"),
      out_file = "out/temp_rmse_cluster_5_scatter.png"
    ),
    format = "file"
  ),

  tar_target(
    figure_temp_rmse_cluster_10,
    plot_rmse_scatter(
      rmse_file = temp_rmse_by_cluster_10,
      min_n_obs = 0,
      ylab = expression("RMSE ("*~degree*C*")"),
      out_file = "out/temp_rmse_cluster_10_scatter.png"
    ),
    format = "file"
  ),

  tar_target(
    figure_flow_rmse_cluster_10,
    plot_rmse_scatter(
      rmse_file = flow_rmse_by_cluster_10,
      min_n_obs = 0,
      ylab = expression(RMSE~(m^{3}~sec^{-1})),
      out_file = "out/flow_rmse_cluster_10_scatter.png"
    ),
    format = "file"
  )


)
