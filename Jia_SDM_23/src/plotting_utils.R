

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
