

plot_time_series <- function(
    pred_file,
    obs_file,
    start_date,
    end_date,
    output_file)
{

  obs <- read_csv(obs_file) %>%
    mutate(seg_id_nat = as.character(seg_id_nat),
           date = as.Date(date))
  preds_obs <- arrow::read_feather(pred_file) %>%
    left_join(select(obs, date, seg_id_nat, mean_temp_c)) %>%
    rename(temp_pred = stream_temp_c,
           temp_obs = mean_temp_c) %>%
    filter(date >= start_date, date <= end_date)

  browser()

  # Create the time series plot using ggplot2
  p <- ggplot(filter(preds_obs,
                     seg_id_nat == "1566",
                     model %in% c("GDN", "HGN", "proposed"),
                     data_sparsity == 100),
              aes(x = date,
                  y = temp_pred,
                  color = factor(model))) +
    geom_line() +
    geom_point(data = filter(preds_obs,
                             seg_id_nat == "1566",
                             model %in% c("GDN"),
                             data_sparsity == 100),
               aes(x = date,
                   y = temp_obs), color = "black")
    labs(x = "Date", y = "Value", color = "Variable") +
    scale_color_manual(values = c("red", "blue"),
                       labels = c("Predictions", "Observations"))

  # Save the plot to a file
  ggsave(output_file, p, width = 8, height = 6, dpi = 300)
}



plot_rmse_density <- function(
    rmse_file,
    min_n_obs = 0,
    rmse_lims = NULL,
    xlab = "",
    log_x_axis = F,
    out_file
){

  rmse <- read_csv(rmse_file) %>%
    filter(n_obs_test >= min_n_obs,
           model %in% c("GDN", "HGN", "proposed")) %>%
    mutate(data_sparsity = as.character(data_sparsity),
           model = case_when(model == "HGN" ~ "Heterogenous Graph Network",
                             model == "GDN" ~ "Graph Diffusion Network",
                             model == "proposed" ~ "Proposed"))

  if(is.null(rmse_lims)){
    rmse_lims = range(rmse$rmse, na.rm = T)
  }

  # New facet label names for percent train
  facet.labs <- c("2% Training Data", "100% Training Data")
  names(facet.labs) <- factor(c("2", "100"), levels = c("2", "100"))


  rmse_mean <- rmse %>%
    group_by(model, data_sparsity) %>%
    summarise(mean_rmse = mean(rmse, na.rm = T),
              .groups = 'drop')

  rmse$data_sparsity <- factor(rmse$data_sparsity, levels = c("2", "100"))
  # browser()

  out <- ggplot(rmse) +
    geom_density(aes(x = rmse, group = model,
                     color = model, fill = model),
                 outline.type = 'upper',
                 alpha = 0.2,
                 size = 2,
                 trim = F) +
    geom_vline(data = rmse_mean, aes(xintercept = mean_rmse,
                                     group = model,
                                     color = model),
               linetype = 'dashed', size =1.2)+
    facet_wrap(~data_sparsity,
               labeller = labeller(data_sparsity = facet.labs)) +
    xlim(rmse_lims) +
    ylab('Density') +
    xlab(xlab) +
    theme_minimal() +
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 16),
          strip.text = element_text(size = 16)) +
    guides(fill=guide_legend(title="Model Type"),
           color=guide_legend(title="Model Type")) +
    scale_fill_brewer(palette="Dark2") +
    scale_color_brewer(palette="Dark2")

  if(log_x_axis){
    out <- out + scale_x_log10()
  }


  ggsave(out_file, out, bg = "white",
         width = 10, height = 5, dpi = 300)

  return(out_file)
}


plot_sim_density <- function(
    rmse_file,
    min_n_obs = 0,
    rmse_lims = NULL,
    xlab = "",
    log_x_axis = F,
    out_file
){

  rmse <- read_csv(rmse_file) %>%
    filter(n_obs_test >= min_n_obs,
           model %in% c("sim_proposed", "sim_temp_composite", "sim_temp_SNTemp")) %>%
    mutate(data_sparsity = as.character(data_sparsity),
           model = case_when(model == "sim_proposed" ~ "GDN-composite",
                             model == "sim_temp_composite" ~ "Exp-Decay",
                             model == "sim_temp_SNTemp" ~ "SNTemp"))

  if(is.null(rmse_lims)){
    rmse_lims = range(rmse$rmse, na.rm = T)
  }

  # browser()
  # New facet label names for percent train
  facet.labs <- c("2% Training Data", "100% Training Data")
  names(facet.labs) <- factor(c("2", "100"), levels = c("2", "100"))


  rmse_mean <- rmse %>%
    group_by(model, data_sparsity) %>%
    summarise(mean_rmse = mean(rmse, na.rm = T),
              .groups = 'drop')

  rmse$data_sparsity <- factor(rmse$data_sparsity, levels = c("2", "100"))
  rmse$model <- factor(rmse$model, levels = c("SNTemp", "Exp-Decay", "GDN-composite"))
  # browser()

  out <- ggplot(rmse) +
    geom_density(aes(x = rmse, group = model,
                     color = model, fill = model),
                 outline.type = 'upper',
                 alpha = 0.2,
                 size = 2,
                 trim = F) +
    geom_vline(data = rmse_mean, aes(xintercept = mean_rmse,
                                     group = model,
                                     color = model),
               linetype = 'dashed', size =1.2)+
    # facet_wrap(~data_sparsity,
    #            labeller = labeller(data_sparsity = facet.labs)) +
    xlim(rmse_lims) +
    ylab('Density') +
    xlab(xlab) +
    theme_minimal() +
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 16),
          strip.text = element_text(size = 16)) +
    guides(fill=guide_legend(title="Simulation Dataset"),
           color=guide_legend(title="Simulation Dataset")) +
    scale_fill_manual(values = c("SNTemp" = "#00bfbf",
                                 "Exp-Decay" = "#c3c310",
                                 "GDN-composite" = "#c721c7")) +
    scale_color_manual(values = c("SNTemp" = "#00bfbf",
                                  "Exp-Decay" = "#c3c310",
                                  "GDN-composite" = "#c721c7"))

  if(log_x_axis){
    out <- out + scale_x_log10()
  }


  ggsave(out_file, out, bg = "white",
         width = 10, height = 5, dpi = 300)

  return(out_file)
}


plot_rmse_map <- function(
    rmse_file,
    shapefile,
    res_shapefile,
    seg_id_nats,
    min_n_obs = 0,
    rmse_lims = NULL,
    lab = "",
    log_x_axis = F,
    out_file
){

  reservoirs <- sf::read_sf(res_shapefile)
  segments <- sf::read_sf(shapefile)
  segments_filtered <- segments %>%
    filter(segidnat %in% as.numeric(seg_id_nats))

  states_all <- st_as_sf(maps::map("state", resolution = 0, fill=TRUE, plot=FALSE))
  states <- states_all %>%
    filter(ID %in% c('pennsylvania','new york','delaware','new jersey','maryland')) %>%
    mutate(name = c('pennsylvania'='Pennsylvania', 'new york'='New York',
                    'delaware'='Delaware', 'new jersey'='New Jersey',
                    'maryland'='Maryland')[ID])
  state_labels <- st_as_sf(tibble(
    name=c('New York', 'Pennsylvania', 'New Jersey'),
    lon = c(-74.0, -76.0, -74.38),
    lat = c(42.0, 41.52, 40.38)
  ), coords=c('lon','lat')) %>%
    sf::st_set_crs(sf::st_crs(segments))

  proj_str <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"

  map_bbox <- sf::st_bbox(sf::st_buffer(sf::st_transform(segments_filtered, crs = proj_str), dist=2000)) +
    c(-50000, -5000, 50000, -1000)

  rmse <- read_csv(rmse_file) %>%
    filter(n_obs_test >= min_n_obs,
           model %in% c("GDN", "HGN", "proposed")) %>%
    mutate(data_sparsity = as.character(data_sparsity))# ,
           # model = case_when(model == "HGN" ~ "Heterogenous Graph Network",
           #                   model == "GDN" ~ "Graph Diffusion Network",
           #                   model == "proposed" ~ "Proposed"))

  rmse <- rmse %>%
    rename(segidnat = seg_id_nat) %>%
    mutate(segidnat = as.integer(segidnat)) %>%
    pivot_wider(id_cols = c("segidnat","data_sparsity","n_obs_test"),
                names_from = model, values_from = rmse) %>%
    mutate(rmse_diff_GDN = proposed - GDN,
           rmse_diff_HGN = proposed - HGN)

  rmse <- left_join(segments, rmse, by = "segidnat")

  if(is.null(rmse_lims)){
    rmse_lims = range(rmse$rmse_diff, na.rm = T)
  }

  # New facet label names for percent train
  facet.labs <- c("2% Training Data", "100% Training Data",
                  "Proposed RMSE - GDN RMSE", "Proposed RMSE - HGN RMSE")
  names(facet.labs) <- c("2", "100", "rmse_diff_GDN", "rmse_diff_HGN")

  rmse <- filter(rmse,
                 data_sparsity %in% c("2", "100")) %>%
    pivot_longer(cols = starts_with("rmse_diff"),
                 names_to = "model",
                 values_to = "rmse_diff")

  out <- ggplot(rmse) +
    geom_sf(data = segments,
            color = "grey95") +
    geom_sf(data = rmse,
            aes(color = rmse_diff),
            size=1,
            alpha = 1) +
    geom_sf(data = reservoirs,
            color = "grey40",
            fill = "grey50") +
    coord_sf(crs = st_crs(proj_str), datum = NA,
             xlim = map_bbox[c('xmin','xmax')],
             ylim = map_bbox[c('ymin','ymax')]) +
    facet_grid(data_sparsity ~ model,
               labeller = labeller(data_sparsity = facet.labs, model = facet.labs)) +
    ggthemes::theme_map() +
    # ggspatial::annotation_north_arrow(which_north = 'true',
    #                                   style = north_arrow_fancy_orienteering)+
    # ggspatial::annotation_scale(location = 'br', style = 'ticks') +
    scico::scale_color_scico(palette = "vik",
                             midpoint = 0,
                             name = "RMSE Difference")

  ggsave(out_file,
         out,
         bg = "white",
         width = 10,
         height = 10,
         dpi = 300)

  return(out_file)
}


plot_rmse_simulation_map <- function(
    rmse_file,
    shapefile,
    res_shapefile,
    seg_id_nats,
    min_n_obs = 0,
    rmse_lims = NULL,
    lab = "",
    log_x_axis = F,
    out_file
){

  reservoirs <- sf::read_sf(res_shapefile)
  segments <- sf::read_sf(shapefile)
  segments_filtered <- segments %>%
    filter(segidnat %in% as.numeric(seg_id_nats))

  states_all <- st_as_sf(maps::map("state", resolution = 0, fill=TRUE, plot=FALSE))
  states <- states_all %>%
    filter(ID %in% c('pennsylvania','new york','delaware','new jersey','maryland')) %>%
    mutate(name = c('pennsylvania'='Pennsylvania', 'new york'='New York',
                    'delaware'='Delaware', 'new jersey'='New Jersey',
                    'maryland'='Maryland')[ID])
  state_labels <- st_as_sf(tibble(
    name=c('New York', 'Pennsylvania', 'New Jersey'),
    lon = c(-74.0, -76.0, -74.38),
    lat = c(42.0, 41.52, 40.38)
  ), coords=c('lon','lat')) %>%
    sf::st_set_crs(sf::st_crs(segments))

  proj_str <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"

  map_bbox <- sf::st_bbox(sf::st_buffer(sf::st_transform(segments_filtered, crs = proj_str), dist=2000)) +
    c(-50000, -5000, 50000, -1000)

  rmse <- read_csv(rmse_file) %>%
    filter(n_obs_test >= min_n_obs,
           model %in% c("sim_proposed", "sim_temp_composite", "sim_temp_SNTemp")) %>%
    mutate(data_sparsity = as.character(data_sparsity))# ,
  # model = case_when(model == "HGN" ~ "Heterogenous Graph Network",
  #                   model == "GDN" ~ "Graph Diffusion Network",
  #                   model == "proposed" ~ "Proposed"))

  rmse <- rmse %>%
    rename(segidnat = seg_id_nat) %>%
    mutate(segidnat = as.integer(segidnat)) %>%
    pivot_wider(id_cols = c("segidnat","data_sparsity","n_obs_test"),
                names_from = model, values_from = rmse) %>%
    mutate(rmse_diff_composite = sim_proposed - sim_temp_composite,
           rmse_diff_SNTemp = sim_proposed - sim_temp_SNTemp)

  rmse <- left_join(segments, rmse, by = "segidnat")

  if(is.null(rmse_lims)){
    rmse_lims = range(rmse$rmse_diff, na.rm = T)
  }

  # New facet label names for percent train
  facet.labs <- c("GDN-composite RMSE - Exp-Decay RMSE",
                  "GDN-composite RMSE - SNTemp RMSE")
  names(facet.labs) <- c("rmse_diff_composite", "rmse_diff_SNTemp")

  rmse <- filter(rmse,
                 data_sparsity %in% c("2", "100")) %>%
    pivot_longer(cols = starts_with("rmse_diff"),
                 names_to = "model",
                 values_to = "rmse_diff")

  out <- ggplot(rmse) +
    geom_sf(data = segments,
            color = "grey95") +
    geom_sf(data = rmse,
            aes(color = rmse_diff),
            size=1,
            alpha = 1) +
    geom_sf(data = reservoirs,
            color = "grey40",
            fill = "grey50") +
    coord_sf(crs = st_crs(proj_str), datum = NA,
             xlim = map_bbox[c('xmin','xmax')],
             ylim = map_bbox[c('ymin','ymax')]) +
    facet_wrap(~model,
               labeller = labeller(model = facet.labs)) +
    ggthemes::theme_map() +
    # ggspatial::annotation_north_arrow(which_north = 'true',
    #                                   style = north_arrow_fancy_orienteering)+
    # ggspatial::annotation_scale(location = 'br', style = 'ticks') +
    scico::scale_color_scico(palette = "vik",
                             midpoint = 0,
                             name = "RMSE Difference")

  ggsave(out_file,
         out,
         bg = "white",
         width = 10,
         height = 6,
         dpi = 300)

  return(out_file)
}
