TO Create Supplement table S2 from scratch, run the following files in this order: 1_calculate_keyvalues.R, 1_run_estimation_allmethods.R, and then 2_create_supplement_tabS2.R creates the table.

The first file will need to be run with arguments 1 through 720. Following is the bash code


for i in {1..720}
do
  Rscript 1_run_estimation_allmethods.R "$i"
done

One could run this code parallelly if needed



The following libraries are needed

	library(mvtnorm)
	library(ElasticIntegrative)
	library(SuperLearner)
	library(glmnet)
	library(stringr)
	library(caret)
	library(ncvreg)
	library(MatchIt)
	library(nloptr)
	library(randomForest)
	library(optmatch)
