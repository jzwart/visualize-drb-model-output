

# plot a map for DRB study region
plot_drb_map <- function(
  shapefile,
  out_file
){

  segments <- sf::read_sf(shapefile)

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

  map_bbox <- sf::st_bbox(sf::st_buffer(sf::st_transform(segments, crs = proj_str), dist=2000)) +
    c(-50000, -5000, 50000, -1000)

  inset_centroid_tbl <- tibble(lon = mean(st_bbox(segments)[c(1,3)]),
                               lat = mean(st_bbox(segments)[c(2,4)]))
  inset_centroid <- inset_centroid_tbl %>%
    sf::st_as_sf(coords=c('lon','lat')) %>%
    sf::st_set_crs(sf::st_crs(segments))

  us_map <- ggplot(states_all) +
    geom_sf(color='gray80', fill='white', size=0.3) +
    geom_sf(data = segments$geometry, size=1, color='blue') +
    geom_sf(data = inset_centroid, shape=21, size=7,
            color='blue', fill='blue', alpha=0.3) +
    theme(legend.position = 'none') +
    coord_sf(crs = st_crs(proj_str), datum = NA) +
    ggthemes::theme_map()

  study_map <- ggplot(states) +
    geom_sf(color = "gray80", fill = "white") +
    geom_sf_text(data = state_labels, aes(label=name), color='gray80') +
    geom_sf(data = segments, color = 'blue',
            alpha = .8, size=0.6) +
    coord_sf(crs = st_crs(proj_str), datum = NA,
             xlim = map_bbox[c('xmin','xmax')],
             ylim = map_bbox[c('ymin','ymax')]) +
    ggthemes::theme_map() +
    ggspatial::annotation_north_arrow(which_north = 'true',
                                      style = north_arrow_fancy_orienteering)+
    ggspatial::annotation_scale(location = 'br', style = 'ticks')


  combined <- ggdraw() +
    draw_plot(study_map) +
    draw_plot(us_map, x = 0.7, y = 0.75, width = 0.3, height = 0.2)

  ggsave(out_file,
         plot=combined,
         width=6, height=6, dpi=300)

  return(out_file)
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
    filter(n_obs_test >= min_n_obs) %>%
    mutate(percent_trn_data = as.character(percent_trn_data))

  if(is.null(rmse_lims)){
    rmse_lims = range(rmse$rmse, na.rm = T)
  }

  # New facet label names for percent train
  facet.labs <- c("1% Training Data", "10% Training Data", "100% Training Data")
  names(facet.labs) <- c("1", "10", "100")


  rmse_median <- rmse %>%
    group_by(model, percent_trn_data) %>%
    summarise(median_rmse = median(rmse, na.rm = T), .groups = 'drop')

  out <- ggplot(rmse) +
    geom_density(aes(x = rmse, group = model,
                     color = model, fill = model),
                 outline.type = 'upper',
                 alpha = 0.2,
                 size = 2,
                 trim = F) +
    geom_vline(data = rmse_median, aes(xintercept = median_rmse,
                                       group = model,
                                       color = model),
               linetype = 'dashed', size =1.2)+
    facet_wrap(~percent_trn_data,
               labeller = labeller(percent_trn_data = facet.labs)) +
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


  ggsave(out_file, out,
         width = 10, height = 5, dpi = 300)

  return(out_file)
}

plot_rmse_map <- function(
  rmse_file,
  shapefile,
  min_n_obs = 0,
  rmse_lims = NULL,
  lab = "",
  log_x_axis = F,
  out_file
){

  segments <- sf::read_sf(shapefile)

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

  map_bbox <- sf::st_bbox(sf::st_buffer(sf::st_transform(segments, crs = proj_str), dist=2000)) +
    c(-50000, -5000, 50000, -1000)

  rmse <- read_csv(rmse_file) %>%
    filter(n_obs_test >= min_n_obs) %>%
    mutate(percent_trn_data = as.character(percent_trn_data))
  # flow_rmse <- read_csv(flow_rmse_file) %>%
  #   filter(n_obs_test >= min_n_obs) %>%
  #   mutate(percent_trn_data = as.character(percent_trn_data),
  #          var = 'flow')

  rmse <- rmse %>%
    rename(segidnat = seg_id_nat) %>%
    mutate(segidnat = as.integer(segidnat)) %>%
    filter(model %in% c('MTHGN', 'HGN')) %>%
    pivot_wider(id_cols = c('segidnat','percent_trn_data','n_obs_test'),
                names_from = model, values_from = rmse) %>%
    mutate(rmse_diff = MTHGN - HGN)

  rmse <- left_join(segments, rmse, by = 'segidnat')

  if(is.null(rmse_lims)){
    rmse_lims = range(rmse$rmse_diff, na.rm = T)
  }

  # New facet label names for percent train
  facet.labs <- c("1% Training Data", "10% Training Data", "100% Training Data")
  names(facet.labs) <- c("1", "10", "100")

  # out <- ggplot(rmse) +
  #   geom_density(aes(x = rmse, group = model,
  #                    color = model, fill = model),
  #                outline.type = 'upper',
  #                alpha = 0.2,
  #                size = 2,
  #                trim = F) +
  #   geom_vline(data = rmse_median, aes(xintercept = median_rmse,
  #                                      group = model,
  #                                      color = model),
  #              linetype = 'dashed', size =1.2)+
  #   facet_wrap(~percent_trn_data,
  #              labeller = labeller(percent_trn_data = facet.labs)) +
  #   xlim(rmse_lims) +
  #   ylab('Density') +
  #   xlab(xlab) +
  #   theme_minimal() +
  #   theme(axis.title = element_text(size = 16),
  #         axis.text = element_text(size = 16),
  #         strip.text = element_text(size = 16)) +
  #   guides(fill=guide_legend(title="Model Type"),
  #          color=guide_legend(title="Model Type")) +
  #   scale_fill_brewer(palette="Dark2") +
  #   scale_color_brewer(palette="Dark2")

  rmse <- filter(rmse,
                 percent_trn_data %in% c('1', '10')) %>%
    mutate(line_size = abs(ifelse(is.na(rmse_diff), .1, rmse_diff)))

  out<-ggplot(states) +
    geom_sf(color = "gray80", fill = "white") +
    geom_sf_text(data = state_labels, aes(label=name), color='gray80') +
    geom_sf(data = rmse, aes(color = rmse_diff), size=1,
            alpha = 1) +
    coord_sf(crs = st_crs(proj_str), datum = NA,
             xlim = map_bbox[c('xmin','xmax')],
             ylim = map_bbox[c('ymin','ymax')]) +
    # facet_wrap(~var) +
    ggthemes::theme_map() +
    ggspatial::annotation_north_arrow(which_north = 'true',
                                      style = north_arrow_fancy_orienteering)+
    ggspatial::annotation_scale(location = 'br', style = 'ticks') +
    scico::scale_color_scico(palette = "vik",midpoint = 0)

  ggsave(out_file, out,
         width = 6, height = 7, dpi = 300)

  return(out_file)
}



plot_rmse_scatter <- function(
  rmse_file,
  min_n_obs = 0,
  rmse_lims = NULL,
  ylab = "",
  log_y_axis = F,
  out_file
){

  rmse <- read_csv(rmse_file) %>%
    filter(mean_n_obs_test >= min_n_obs) %>%
    mutate(percent_trn_data = as.character(percent_trn_data),
           cluster = factor(cluster))

  if(is.null(rmse_lims)){
    rmse_lims = range(rmse$mean_rmse, na.rm = T)
  }

  # New facet label names for percent train
  facet.labs <- c("1% Training Data", "10% Training Data", "100% Training Data")
  names(facet.labs) <- c("1", "10", "100")

  out <- ggplot(rmse, aes(x = cluster, y = mean_rmse,
                          group = model,
                          color = model)) +
    geom_point(size = 3, width = .1, height = 0) +
    # geom_errorbar(aes(ymin = mean_rmse-sd_rmse, ymax=mean_rmse+sd_rmse)) +
    facet_wrap(~percent_trn_data,
               labeller = labeller(percent_trn_data = facet.labs)) +
    ylim(rmse_lims) +
    ylab(ylab) +
    xlab('Cluster') +
    theme_minimal() +
    theme(axis.title = element_text(size = 16),
          axis.text = element_text(size = 16),
          strip.text = element_text(size = 16)) +
    guides(fill=guide_legend(title="Model Type"),
           color=guide_legend(title="Model Type")) +
    scale_fill_brewer(palette="Dark2") +
    scale_color_brewer(palette="Dark2")

  if(log_y_axis){
    out <- out + scale_y_log10()
  }


  ggsave(out_file, out,
         width = 10, height = 5, dpi = 300)

  return(out_file)
}

