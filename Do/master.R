# master.R
# John Kearns
# Goal: Write script to run all scripts needed to run google trends and debt models

library(fredr)

fred_key = Sys.getenv('FRED_KEY')
gt_key = Sys.getenv("GT_KEY")
bls_key = Sys.getenv("BLS_KEY")

fredr_set_key(fred_key)

reticulate::py_discover_config()

reticulate::py_require(c('google-api-python-client',
                         'pandas',
                         'dill'))

reticulate::py_run_file("Do/1a_pull_trends_data.py")

source('Do/0_model_functions.R')

source('Do/1_clean_google_trends.R')

source('Do/2_assemble_econ_data.R')

source('Do/3_construct_dataset_with_data_lags.R')

source('Do/4_feature_imputation.R')

source('Do/5_construct_GT_index.R')

source('Do/7_gt_deficit_modelling.R')

source('Do/8_daily_model.R')

source('Do/9_XDate_estimation.R')

source('Do/10_make_charts.R')


