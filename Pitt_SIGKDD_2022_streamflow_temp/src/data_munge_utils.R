


combine_temp_preds_obs <- function(
  folders,
  models,
  obs_file,
  dates,
  seg_ids,
  out_file
){

  all_preds <- tibble()
  for(folder in folders){
    for(model in models){
      cur <- np$load(file.path('in',folder, paste0(model,'.npy')),
                     allow_pickle = TRUE ) %>%
        as_tibble()
      colnames(cur) <- dates
      cur <- cur %>%
        mutate(seg_id_nat = seg_ids,
               model = model,
               percent_trn_data = as.numeric(strsplit(folder,'_')[[1]][2])) %>%
        pivot_longer(cols = -c(seg_id_nat, model, percent_trn_data),
                     names_to = 'date', values_to = 'temp_pred') %>%
        mutate(date = as.Date(date))

      all_preds <- bind_rows(all_preds, cur)
      rm(cur)
    }
  }

  all_obs <- tibble()
  for(folder in folders){
    obs <- np$load(file.path('in',folder, obs_file),
                   allow_pickle = TRUE ) %>%
      as_tibble()
    colnames(obs) <- dates
    obs <- obs %>%
      mutate(seg_id_nat = seg_ids,
             percent_trn_data = as.numeric(strsplit(folder,'_')[[1]][2])) %>%
      pivot_longer(cols = -c(seg_id_nat, percent_trn_data),
                   names_to = 'date', values_to = 'temp_obs') %>%
      mutate(date = as.Date(date),
             temp_obs = ifelse(temp_obs == -11, NA, temp_obs))
    all_obs <- bind_rows(all_obs, obs)
    rm(obs)
  }

  all <- left_join(all_preds, all_obs, by = c('seg_id_nat', 'date', 'percent_trn_data'))

  saveRDS(all, out_file)
  return(out_file)
}


combine_flow_preds_obs <- function(
  folders,
  models,
  obs_file,
  dates,
  seg_ids,
  out_file
){

  all_preds <- tibble()
  for(folder in folders){
    for(model in models){
      cur <- np$load(file.path('in',folder, paste0(model,'.npy')),
                     allow_pickle = TRUE ) %>%
        as_tibble()
      colnames(cur) <- dates
      cur <- cur %>%
        mutate(seg_id_nat = seg_ids,
               model = model,
               percent_trn_data = as.numeric(strsplit(folder,'_')[[1]][2])) %>%
        pivot_longer(cols = -c(seg_id_nat, model, percent_trn_data),
                     names_to = 'date', values_to = 'flow_pred') %>%
        mutate(date = as.Date(date))

      all_preds <- bind_rows(all_preds, cur)
      rm(cur)
    }
  }

  all_obs <- tibble()
  for(folder in folders){
    obs <- np$load(file.path('in',folder, obs_file),
                   allow_pickle = TRUE ) %>%
      as_tibble()
    colnames(obs) <- dates
    obs <- obs %>%
      mutate(seg_id_nat = seg_ids,
             percent_trn_data = as.numeric(strsplit(folder,'_')[[1]][2])) %>%
      pivot_longer(cols = -c(seg_id_nat, percent_trn_data),
                   names_to = 'date', values_to = 'flow_obs') %>%
      mutate(date = as.Date(date),
             flow_obs = ifelse(flow_obs == -11, NA, flow_obs))
    all_obs <- bind_rows(all_obs, obs)
    rm(obs)
  }

  all <- left_join(all_preds, all_obs, by = c('seg_id_nat', 'date', 'percent_trn_data'))

  saveRDS(all, out_file)
  return(out_file)
}

calc_rmse <- function(
  in_file,
  var,
  out_file
){

  preds_obs <- readRDS(in_file)

  if(var == 'temp'){
    rmse <- preds_obs %>%
      group_by(seg_id_nat, model, percent_trn_data) %>%
      summarise(rmse = sqrt(mean((temp_pred-temp_obs)^2,na.rm=T)),
                bias = mean(temp_pred-temp_obs, na.rm=T),
                n_obs_test = sum(!is.na(temp_obs)),
                .groups = 'drop')
  }else{
    rmse <- preds_obs %>%
      group_by(seg_id_nat, model, percent_trn_data) %>%
      summarise(rmse = sqrt(mean((flow_pred-flow_obs)^2,na.rm=T)),
                bias = mean(flow_pred-flow_obs, na.rm=T),
                n_obs_test = sum(!is.na(flow_obs)),
                .groups = 'drop')
  }

  write_csv(rmse, out_file)
  return(out_file)
}


calc_rmse_cluster <- function(
  in_file,
  var,
  cluster_file,
  seg_ids,
  out_file
){
  preds_obs <- readRDS(in_file)

  cluster <- np$load(cluster_file, allow_pickle = TRUE) %>%
    as_tibble() %>% rename(cluster = value) %>%
    mutate(seg_id_nat = seg_ids)

  preds_obs <- left_join(preds_obs, cluster, by = 'seg_id_nat')

  if(var == 'temp'){
    rmse <- preds_obs %>%
      group_by(seg_id_nat, cluster, model, percent_trn_data) %>%
      summarise(rmse = sqrt(mean((temp_pred-temp_obs)^2,na.rm=T)),
                bias = mean(temp_pred-temp_obs, na.rm=T),
                n_obs_test = sum(!is.na(temp_obs)),
                .groups = 'drop') %>%
      group_by(cluster, model, percent_trn_data) %>%
      summarise(mean_rmse = mean(rmse, na.rm=T),
                sd_rmse = sd(rmse, na.rm = T),
                mean_n_obs_test = mean(n_obs_test),
                sd_n_obs_test = sd(n_obs_test),
                .groups = 'drop')
  }else{
    rmse <- preds_obs %>%
      group_by(seg_id_nat, cluster, model, percent_trn_data) %>%
      summarise(rmse = sqrt(mean((flow_pred-flow_obs)^2,na.rm=T)),
                bias = mean(flow_pred-flow_obs, na.rm=T),
                n_obs_test = sum(!is.na(flow_obs)),
                .groups = 'drop') %>%
      group_by(cluster, model, percent_trn_data) %>%
      summarise(mean_rmse = mean(rmse, na.rm=T),
                sd_rmse = sd(rmse, na.rm = T),
                mean_n_obs_test = mean(n_obs_test),
                sd_n_obs_test = sd(n_obs_test),
                .groups = 'drop')
  }

  write_csv(rmse, out_file)
  return(out_file)
}

