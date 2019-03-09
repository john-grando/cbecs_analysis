#Run Principal Component Analysis.

source('RScripts/ngbtu_base_training.R')

cbecs_pca <- prcomp(cbecs_ngbtu_encoded_center_scale_df %>% 
                    select(-NGBTU) %>%
                    select(-one_of(cbecs_dfs$response_cols)), center = TRUE, scale. = TRUE)

#save model
model_name <- 'ModelSaves/ngbtu_pca.RData'
save(cbecs_pca, file = model_name)
put_object(file = model_name, 
           bucket = 'cuny-msds-final-project-cbecs', 
           object = model_name)
