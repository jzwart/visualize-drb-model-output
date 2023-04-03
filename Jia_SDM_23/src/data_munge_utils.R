
calc_rmse <- function(
    in_file,
    obs_file,
    start_date,
    end_date,
    var,
    out_file
){

  obs <- read_csv(obs_file) %>%
    mutate(seg_id_nat = as.character(seg_id_nat),
           date = as.Date(date))
  preds_obs <- arrow::read_feather(in_file) %>%
    left_join(select(obs, date, seg_id_nat, mean_temp_c)) %>%
    filter(date >= start_date, date <= end_date) %>%
    rename(temp_pred = stream_temp_c,
           temp_obs = mean_temp_c)
  # browser()

  if(var == 'temp'){
    rmse <- preds_obs %>%
      group_by(seg_id_nat, model, data_sparsity) %>%
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
