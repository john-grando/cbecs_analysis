#Run Principal Component Analysis.

source('RScripts/dhbtu_base_training_psf.R')

cbecs_pca <- prcomp(cbecs_dhbtu_encoded_center_scale_df %>% 
                    select(-DHBTUPerSf) %>%
                    select(-one_of(cbecs_dfs$response_cols)), center = TRUE, scale. = TRUE)

#save model
model_name <- 'ModelSaves/dhbtu_pca_psf.RData'
save(cbecs_pca, file = model_name)
put_object(file = model_name, 
           bucket = 'cuny-msds-final-project-cbecs', 
           object = model_name)
