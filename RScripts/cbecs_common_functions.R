plot_fun <- function(in_model=NA, n_features=2, short_name=NA, alt_predict=FALSE, log_tran=FALSE, 
                     observed_df=NA, response=NA, fuel_type = NA, area_vector=NA) {
  imp_df <- varImp(in_model)$importance %>% 
    rownames_to_column(var='feature')
  plot_df <- imp_df %>% 
    arrange(desc(Overall)) %>% 
    head(n_features)
  p1 <- ggplot(data = data.frame(plot_df), aes(x=reorder(feature, desc(Overall)), y=Overall)) + 
    geom_bar(stat='identity') +
    ggtitle('Variable Importance') +
    labs(x='Feature') +
    theme(axis.text.x = element_text(angle=60, hjust=1),
          plot.title = element_text(hjust = 0.5),
          plot.background = element_rect(fill = "lightgrey"), 
          panel.background = element_rect(fill = "white"), 
          panel.grid.major.x = element_line(color = "lightgrey"), 
          panel.grid.major.y = element_line(color = "lightgrey"), 
          axis.text = element_text(size=8, color = "grey55"), 
          axis.title = element_text(size=10, color = "grey55"), 
          legend.title=element_text(size=7),
          legend.text=element_text(size=6),
          legend.key.height = unit(0.5, "cm"),
          title = element_text(size=10, color = "grey55"))
  ggsave(paste0('Documents/Images/', fuel_type, '_',short_name,'_vars.png'), p1, width = 6.5, height = 3.5, units = 'in')
  plots <- plot_pred_obs(in_model = in_model,
                         short_name_sub = short_name,
                         alt_predict_sub = alt_predict, 
                         log_tran_sub = log_tran,
                         observed_df_sub = observed_df,
                         response_sub = response,
                         fuel_type_sub = fuel_type,
                         area_vector_sub = area_vector)
  return(list(vars_plot = p1, 
              pvo_plot = plots$p,
              tmp_model = plots$tmp_mdl,
              imp_df = imp_df,
              RMSE = plots$RMSE_untransformed,
              R2 = plots$R2_untransformed,
              MAE = plots$MAE_untransformed,
              MAPE = plots$MAPE_untransformed,
              MSLE = plots$MSLE_untransformed,
              RMSE_total = plots$RMSE_total_untransformed,
              R2_total = plots$R2_total_untransformed,
              MAE_total = plots$MAE_total_untransformed,
              MAPE_total = plots$MAPE_total_untransformed,
              MSLE_total = plots$MSLE_total_untransformed)
  )
}

plot_pred_obs <- function(in_model_sub = NA, short_name_sub = short_name, alt_predict_sub=FALSE, 
                          log_tran_sub=FALSE, observed_df_sub=NA, response_sub=NA, fuel_type_sub = NA, area_vector_sub=NA){
  if(alt_predict_sub==FALSE){
    final_model <- in_model_sub$finalModel
  }
  if(alt_predict_sub!=FALSE){
    final_model <- in_model_sub
  }
  
  if(log_tran_sub==TRUE){
    tmp_model_sub <- lm(exp(predict(final_model, observed_df_sub)) ~ 
                          response_sub)
    p2 <- ggplot(data = data.frame(predicted = exp(predict(final_model, observed_df_sub)), 
                                   observed = response_sub))
    RMSE_sub <- caret::RMSE(exp(predict(final_model, observed_df_sub)), 
                            response_sub)
    R2_sub <- caret::R2(exp(predict(final_model, observed_df_sub)), 
                        response_sub)
    MAE_sub <- caret::MAE(exp(predict(final_model, observed_df_sub)), 
                          response_sub)
    MAPE_sub <- mean(abs(exp(predict(final_model, observed_df_sub)) - response_sub) / pmax(response_sub, 1)) * 100
    MSLE_sub <- mean((log(pmax(0,exp(predict(final_model, observed_df_sub)))) - log(pmax(response_sub,0)))^2)
    RMSE_total_sub <- caret::RMSE(exp(predict(final_model, observed_df_sub))*area_vector_sub / 1000000, 
                            response_sub*area_vector_sub / 1000000)
    R2_total_sub <- caret::R2(exp(predict(final_model, observed_df_sub))*area_vector_sub / 1000000, 
                        response_sub*area_vector_sub / 1000000)
    MAE_total_sub <- caret::MAE(exp(predict(final_model, observed_df_sub))*area_vector_sub / 1000000, 
                          response_sub*area_vector_sub / 1000000)
    MAPE_total_sub <- mean(abs(exp(predict(final_model, observed_df_sub))*area_vector_sub / 1000000 - 
                                 response_sub*area_vector_sub / 1000000) / pmax(response_sub*area_vector_sub / 1000000, 1)) * 100
    MSLE_total_sub <- mean((log(pmax(0,(exp(predict(final_model, observed_df_sub))*area_vector_sub / 1000000))) - 
                                 log(pmax(0,response_sub*area_vector_sub / 1000000)))^2)
  }
  if(log_tran_sub!=TRUE){
    tmp_model_sub <- lm(predict(final_model, observed_df_sub) ~ 
                          response_sub)
    p2 <- ggplot(data = data.frame(predicted = predict(final_model, observed_df_sub), 
                                   observed = response_sub))
    RMSE_sub <- caret::RMSE(predict(final_model, observed_df_sub),
                            response_sub)
    R2_sub <- caret::R2(predict(final_model, observed_df_sub),
                        response_sub)
    MAE_sub <- caret::MAE(predict(final_model, observed_df_sub),
                          response_sub)
    MAPE_sub <- mean(abs(predict(final_model, observed_df_sub) - response_sub) / pmax(response_sub, 1)) * 100
    MSLE_sub <- mean((log(pmax(0,predict(final_model, observed_df_sub))) - log(pmax(0,response_sub)))^2)
    RMSE_total_sub <- caret::RMSE(predict(final_model, observed_df_sub)*area_vector_sub / 1000000,
                            response_sub*area_vector_sub / 1000000)
    R2_total_sub <- caret::R2(predict(final_model, observed_df_sub)*area_vector_sub / 1000000,
                        response_sub*area_vector_sub / 1000000)
    MAE_total_sub <- caret::MAE(predict(final_model, observed_df_sub)*area_vector_sub / 1000000,
                          response_sub*area_vector_sub / 1000000)
    MAPE_total_sub <- mean(abs(predict(final_model, observed_df_sub)*area_vector_sub / 1000000 - 
                           response_sub*area_vector_sub / 1000000) / pmax(response_sub*area_vector_sub / 1000000, 1)) * 100
    MSLE_total_sub <- mean((log(pmax(0,predict(final_model, observed_df_sub)*area_vector_sub / 1000000)) - 
                                 log(pmax(0,response_sub*area_vector_sub / 1000000)))^2)
  }
  p2 <- p2 + 
    aes(x=log(predicted), y=log(observed)) + 
    geom_point(alpha=0.2) +
    ggtitle('Log Predicted vs. Log Observed') +
    geom_abline(slope = 1, intercept = 0) +
    theme(axis.text.x = element_text(angle=60, hjust=1),
          plot.title = element_text(hjust = 0.5),
          plot.background = element_rect(fill = "lightgrey"), 
          panel.background = element_rect(fill = "white"), 
          panel.grid.major.x = element_line(color = "lightgrey"), 
          panel.grid.major.y = element_line(color = "lightgrey"), 
          axis.text = element_text(size=12, color = "grey55"), 
          axis.title = element_text(size=12, color = "grey55"), 
          legend.title=element_text(size=7),
          legend.text=element_text(size=6),
          legend.key.height = unit(0.5, "cm"),
          title = element_text(size=12, color = "grey55"))
  ggsave(paste0('Documents/Images/', fuel_type_sub, '_',short_name_sub,'_pvo.png'), p2, width = 6.5, height = 3, units = 'in')
  png(paste0('Documents/Images/', fuel_type_sub, '_',short_name_sub,'_res_1.png'), width = 7, height = 4, 
      units = 'in', pointsize = 12,
      bg='white', res=100)
  par(mfrow=c(1,2))
  plot(tmp_model_sub, c(1,2))
  dev.off()
  png(paste0('Documents/Images/', fuel_type_sub, '_',short_name_sub,'_res_2.png'), width = 7, height = 4, 
      units = 'in', pointsize = 12,
      bg='white', res=100)
  par(mfrow=c(1,2))
  plot(tmp_model_sub, c(3,5))
  dev.off()
  return(list(p = p2,
              tmp_mdl = tmp_model_sub,
              RMSE_untransformed = RMSE_sub,
              R2_untransformed = R2_sub,
              MAE_untransformed = MAE_sub,
              MAPE_untransformed = MAPE_sub,
              MSLE_untransformed = MSLE_sub,
              RMSE_total_untransformed = RMSE_total_sub,
              R2_total_untransformed = R2_total_sub,
              MAE_total_untransformed = MAE_total_sub,
              MAPE_total_untransformed = MAPE_total_sub,
              MSLE_total_untransformed = MSLE_total_sub))
}

plot_vars <- function(df, response, response_char, i, file_index, trans_name, title_text, yname){
  pre_p <- ggplot(df) +
    ggtitle(paste(i, '-', title_text, sep = " ")) #+
  #theme(axis.title = element_text(size=2))
  if(length(unique(df[,i])) > 15){
    tmp_p <- pre_p + aes(x=df[,i], y=response) +
      geom_point() +
      labs(x=i, y=paste(yname, 'MBTU', sep=" ")) +
      theme(axis.text.x = element_text(angle=60, hjust=1),
            plot.title = element_text(hjust = 0.5,  size=16),
            plot.background = element_rect(fill = "lightgrey"), 
            panel.background = element_rect(fill = "white"), 
            panel.grid.major.x = element_line(color = "lightgrey"), 
            panel.grid.major.y = element_line(color = "lightgrey"),
            axis.text = element_text(size=12, color = "grey55"), 
            axis.title = element_text(size=12, color = "grey55"), 
            legend.title=element_text(size=7),
            legend.text=element_text(size=6),
            legend.key.height = unit(0.5, "cm"),
            title = element_text(size=12, color = "grey55"))
  }
  if(length(unique(df[,i])) <= 15){
    tmp_p <- pre_p +  aes(x=as.factor(df[,i]), y=response) +
      geom_violin() +
      labs(x=i, y=paste(yname, 'BTU', sep=" ")) +
      theme(axis.text.x = element_text(angle=60, hjust=1),
            plot.title = element_text(hjust = 0.5,  size=16),
            plot.background = element_rect(fill = "lightgrey"), 
            panel.background = element_rect(fill = "white"), 
            panel.grid.major.x = element_line(color = "lightgrey"), 
            panel.grid.major.y = element_line(color = "lightgrey"),
            axis.text = element_text(size=12, color = "grey55"), 
            axis.title = element_text(size=12, color = "grey55"), 
            legend.title=element_text(size=7),
            legend.text=element_text(size=6),
            legend.key.height = unit(0.5, "cm"),
            title = element_text(size=12, color = "grey55"))
  }
  ggsave(paste('Documents/Images/',response_char, '_var_', trans_name, "_", file_index, '.png', sep=""), tmp_p, width = 6.5, height = 3, units = 'in')
  return(tmp_p)
}