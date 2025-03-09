# master.R
# John Kearns
# Goal: Write script to run all scripts needed to run google trends and debt models

library(fredr)

fred_key = Sys.getenv('FRED_KEY')
gt_key = Sys.getenv("GT_KEY")
bls_key = Sys.getenv("BLS_KEY")

fredr_set_key(fred_key)

reticulate::use_python(reticulate::py_discover_config()$python, required = TRUE)
reticulate::py_discover_config()

reticulate::py_require(c('google-api-python-client',
                         'pandas',
                         'dill'))
reticulate::py_run_file("Do/1a_pull_trends_data.py")

source('0_model_functions.R')

source('1_clean_google_trends.R')

source('2_assemble_econ_data.R')

source('3_construct_dataset_with_data_lags.R')

source('4_feature_imputation.R')

source('5_construct_GT_index.R')

source('7_gt_deficit_modelling.R')

source('8_daily_model.R')

source('9_XDate_estimation.R')

source('10_make_charts.R')


