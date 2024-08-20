Title of dissertation: A 30-year review of the impact of heat and cold stress on small for gestational age (SGA) on the Thai-Myanmar border.

R script related to the completion of this dissertation is uploaded:

-- "Final_outcomes_script_thesis.R": this script contains all code used to generate the final outputs that are presented in the dissertation. Please mind that the fake data "fakedata.csv" does not have exactly the same distribution as the real data, so the results will be different.

The following scripts are for cleaning and preparing the raw data (both ANC data selection and climatic metrics calculation) before finally getting to the analysis stage presented in script "Final_outcome_scripts_thesis.R". Since the raw data cannot be shared, these scripts cannot be run successfully and it is provided here for reference and can be ignored if not necessary.

-- "Data_cleaning1_weekly_climate_metrics.R": This script shows how the daily raw climatic metrics were summarized into metrics for each gestational week for each pregnancy and merged with the ANC raw data.

-- "Data_cleaning2_final_data.R": This is the script of how the data frame used in the final analysis ("Final_outcomes_script_thesis.R") was produced.
