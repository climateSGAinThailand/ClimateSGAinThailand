################################################################################
#
# Model script 17 - 0724 PCA/stillbirth/status
#
################################################################################

# Load packages 
library(dplyr)
library(ggplot2)
library(tidyr)
library(dlnm)
library(readxl)

# load data
ANCdata <- read.csv("ANCdata_climate_final.csv")
climate_data <- read.csv("weekly_climate_all_long.csv")
merged_data <- read.csv("merged_data.csv")

ANCdata <- ANCdata %>%
  mutate(Study_year = ifelse(z_yrdob >= 1986 & z_yrdob < 1991, "1986-1990",
                             ifelse(z_yrdob >= 1991 & z_yrdob < 1996, "1991-1995",
                                    ifelse(z_yrdob >= 1996 & z_yrdob < 2001, "1996-2000",
                                           ifelse(z_yrdob >= 2001 & z_yrdob < 2006, "2001-2005", 
                                                  ifelse(z_yrdob >= 2006 & z_yrdob < 2011, "2006-2010",
                                                         ifelse(z_yrdob >= 2011 & z_yrdob < 2016, "2011-2015", "2016-2020")))))))

ANCdata$ANAEMIA[ANCdata$ANAEMIA == 9] <- NA
ANCdata$wtless40[ANCdata$wtless40 == 9] <- NA

statusdata <- read_excel("status_data.xlsx")
ANCdata <- left_join(ANCdata, statusdata, by = "anc_code")
table(ANCdata$status)


columns_to_check <- c("WeightCentile")
# Remove rows with any missing values in the specified columns
ANCdata_GA <- ANCdata[complete.cases(ANCdata[, columns_to_check]), ]
ANCdata_GA <- ANCdata_GA %>%
  mutate(GA = ifelse(WeightCentile < 10, "SGA",
                     ifelse(WeightCentile > 90, "LGA", "AGA")),
         SGA = ifelse(WeightCentile < 10, 1, 0),
         LGA = ifelse(WeightCentile > 90, 1, 0))
ANCdata_GA <- ANCdata_GA %>%
  mutate(dfc_cat = ifelse(dfcT1 == 1, "T1",
                          ifelse(dfcT3 == 1, "T3", "T2")))

# The selection criteria for the SGA study (definition formally stated in thesis)
ANCdata_GA <- ANCdata_GA %>%
  #mutate(gestational_weeks = lengthpreg_days%/%7) %>%
  filter(del_abo == 1) %>%                                       # only delivery singleton
  filter(gestational_weeks >= 37 & gestational_weeks < 42) %>%  # gestational age between 28 and 42 weeks
  filter(NORMAL == 1) %>%                                        # normal baby (1=normal;2=abnormal)
  filter(ALIVE_SB == 1) %>%                                      # alive at birth (1=alive;2=dead)
  filter(incbw == 1) %>%                                         # birth weight can be used
  filter(!is.na(GA)) %>%                                         # SGA is not missing
  mutate(SGA = as.factor(SGA)) %>%                               
  mutate(LGA = as.factor(LGA)) %>%                               
  mutate(Study_year = as.factor(Study_year)) %>%                 
  mutate(lmp_month = as.factor(lmp_month)) %>%                   
  mutate(dfcT1 = as.factor(dfcT1)) %>%                           
  mutate(dfc_cat = as.factor(dfc_cat)) %>%                       
  mutate(non_malaria = as.factor(non_malaria)) %>%               
  mutate(mip = as.factor(mip)) %>%                               
  mutate(wtless40 = as.factor(wtless40)) %>%                     
  mutate(EC_PRE_EC = as.factor(EC_PRE_EC)) %>%                   
  mutate(ANAEMIA = as.factor(ANAEMIA)) %>%                       
  mutate(AGE_GRAV = as.factor(AGE_GRAV)) %>%                     
  filter(!is.na(AGE_GRAV) & # 15 missingness
           !is.na(ANAEMIA) &  # 2403 missingness                
           !is.na(EC_PRE_EC) & # 0 missingness
           !is.na(wtless40) & # 2 missingness
           !is.na(mip) &  # 0 missingness
           # !is.na(non_malaria) & # 23273 missingness
           !is.na(dfc_cat) & # 0 missingness
           !is.na(lmp_month) & # 0 missingness
           !is.na(Study_year)) # 0 missingness


Splitdata <- ANCdata_GA %>%
  # only keep the columns we need
  select(anc_code, SGA, LGA, GA, ANAEMIA, AGE_GRAV, EC_PRE_EC, wtless40, mip, dfcT1, dfc_cat, lmp_month, Study_year, gestational_weeks,status,
         weekly_HI_0, weekly_HI_1, weekly_HI_2, weekly_HI_3, weekly_HI_4, weekly_HI_5,
         weekly_HI_6, weekly_HI_7, weekly_HI_8, weekly_HI_9, weekly_HI_10, weekly_HI_11,
         weekly_HI_12, weekly_HI_13, weekly_HI_14, weekly_HI_15, weekly_HI_16, weekly_HI_17,
         weekly_HI_18, weekly_HI_19, weekly_HI_20, weekly_HI_21, weekly_HI_22, weekly_HI_23,
         weekly_HI_24, weekly_HI_25, weekly_HI_26, weekly_HI_27, weekly_HI_28, weekly_HI_29,
         weekly_HI_30, weekly_HI_31, weekly_HI_32, weekly_HI_33, weekly_HI_34, weekly_HI_35,
         weekly_HI_36, weekly_HI_37, weekly_HI_38, weekly_HI_39, weekly_HI_40, weekly_HI_41,
         weekly_max_temp_0, weekly_max_temp_1, weekly_max_temp_2, weekly_max_temp_3,
         weekly_max_temp_4, weekly_max_temp_5, weekly_max_temp_6, weekly_max_temp_7,
         weekly_max_temp_8, weekly_max_temp_9, weekly_max_temp_10, weekly_max_temp_11,
         weekly_max_temp_12, weekly_max_temp_13, weekly_max_temp_14, weekly_max_temp_15,
         weekly_max_temp_16, weekly_max_temp_17, weekly_max_temp_18, weekly_max_temp_19,
         weekly_max_temp_20, weekly_max_temp_21, weekly_max_temp_22, weekly_max_temp_23,
         weekly_max_temp_24, weekly_max_temp_25, weekly_max_temp_26, weekly_max_temp_27,
         weekly_max_temp_28, weekly_max_temp_29, weekly_max_temp_30, weekly_max_temp_31,
         weekly_max_temp_32, weekly_max_temp_33, weekly_max_temp_34, weekly_max_temp_35,
         weekly_max_temp_36, weekly_max_temp_37, weekly_max_temp_38, weekly_max_temp_39,
         weekly_max_temp_40, weekly_max_temp_41,
         weekly_minT_0, weekly_minT_1, weekly_minT_2, weekly_minT_3, weekly_minT_4,
         weekly_minT_5, weekly_minT_6, weekly_minT_7, weekly_minT_8, weekly_minT_9,
         weekly_minT_10, weekly_minT_11, weekly_minT_12, weekly_minT_13, weekly_minT_14,
         weekly_minT_15, weekly_minT_16, weekly_minT_17, weekly_minT_18, weekly_minT_19,
         weekly_minT_20, weekly_minT_21, weekly_minT_22, weekly_minT_23, weekly_minT_24,
         weekly_minT_25, weekly_minT_26, weekly_minT_27, weekly_minT_28, weekly_minT_29,
         weekly_minT_30, weekly_minT_31, weekly_minT_32, weekly_minT_33, weekly_minT_34,
         weekly_minT_35, weekly_minT_36, weekly_minT_37, weekly_minT_38, weekly_minT_39,
         weekly_minT_40, weekly_minT_41,
         weekly_humidity_0, weekly_humidity_1, weekly_humidity_2, weekly_humidity_3,
         weekly_humidity_4, weekly_humidity_5, weekly_humidity_6, weekly_humidity_7,
         weekly_humidity_8, weekly_humidity_9, weekly_humidity_10, weekly_humidity_11,
         weekly_humidity_12, weekly_humidity_13, weekly_humidity_14, weekly_humidity_15,
         weekly_humidity_16, weekly_humidity_17, weekly_humidity_18, weekly_humidity_19,
         weekly_humidity_20, weekly_humidity_21, weekly_humidity_22, weekly_humidity_23,
         weekly_humidity_24, weekly_humidity_25, weekly_humidity_26, weekly_humidity_27,
         weekly_humidity_28, weekly_humidity_29, weekly_humidity_30, weekly_humidity_31,
         weekly_humidity_32, weekly_humidity_33, weekly_humidity_34, weekly_humidity_35,
         weekly_humidity_36, weekly_humidity_37, weekly_humidity_38, weekly_humidity_39,
         weekly_humidity_40, weekly_humidity_41)



# create cum metrics
Splitdata <- Splitdata %>%
  mutate(HI_cum_10 = weekly_HI_0 + weekly_HI_1 + weekly_HI_2 + weekly_HI_3 + weekly_HI_4 + weekly_HI_5 + weekly_HI_6 + weekly_HI_7 + weekly_HI_8 + weekly_HI_9,
         Hi_cum_10_20 = weekly_HI_10 + weekly_HI_11 + weekly_HI_12 + weekly_HI_13 + weekly_HI_14 + weekly_HI_15 + weekly_HI_16 + weekly_HI_17 + weekly_HI_18 + weekly_HI_19,
         Hi_cum_20_30 = weekly_HI_20 + weekly_HI_21 + weekly_HI_22 + weekly_HI_23 + weekly_HI_24 + weekly_HI_25 + weekly_HI_26 + weekly_HI_27 + weekly_HI_28 + weekly_HI_29,
         Hi_cum_30_37 = weekly_HI_30 + weekly_HI_31 + weekly_HI_32 + weekly_HI_33 + weekly_HI_34 + weekly_HI_35 + weekly_HI_36,
         maxt_cum_10 = weekly_max_temp_0 + weekly_max_temp_1 + weekly_max_temp_2 + weekly_max_temp_3 + weekly_max_temp_4 + weekly_max_temp_5 + weekly_max_temp_6 + weekly_max_temp_7 + weekly_max_temp_8 + weekly_max_temp_9,
         maxt_cum_10_20 = weekly_max_temp_10 + weekly_max_temp_11 + weekly_max_temp_12 + weekly_max_temp_13 + weekly_max_temp_14 + weekly_max_temp_15 + weekly_max_temp_16 + weekly_max_temp_17 + weekly_max_temp_18 + weekly_max_temp_19,
         maxt_cum_20_30 = weekly_max_temp_20 + weekly_max_temp_21 + weekly_max_temp_22 + weekly_max_temp_23 + weekly_max_temp_24 + weekly_max_temp_25 + weekly_max_temp_26 + weekly_max_temp_27 + weekly_max_temp_28 + weekly_max_temp_29,
         maxt_cum_30_37 = weekly_max_temp_30 + weekly_max_temp_31 + weekly_max_temp_32 + weekly_max_temp_33 + weekly_max_temp_34 + weekly_max_temp_35 + weekly_max_temp_36,
         mint_cum_10 = weekly_minT_0 + weekly_minT_1 + weekly_minT_2 + weekly_minT_3 + weekly_minT_4 + weekly_minT_5 + weekly_minT_6 + weekly_minT_7 + weekly_minT_8 + weekly_minT_9,
         mint_cum_10_20 = weekly_minT_10 + weekly_minT_11 + weekly_minT_12 + weekly_minT_13 + weekly_minT_14 + weekly_minT_15 + weekly_minT_16 + weekly_minT_17 + weekly_minT_18 + weekly_minT_19,
         mint_cum_20_30 = weekly_minT_20 + weekly_minT_21 + weekly_minT_22 + weekly_minT_23 + weekly_minT_24 + weekly_minT_25 + weekly_minT_26 + weekly_minT_27 + weekly_minT_28 + weekly_minT_29,
         mint_cum_30_37 = weekly_minT_30 + weekly_minT_31 + weekly_minT_32 + weekly_minT_33 + weekly_minT_34 + weekly_minT_35 + weekly_minT_36,
         hum_cum_10 = weekly_humidity_0 + weekly_humidity_1 + weekly_humidity_2 + weekly_humidity_3 + weekly_humidity_4 + weekly_humidity_5 + weekly_humidity_6 + weekly_humidity_7 + weekly_humidity_8 + weekly_humidity_9,
         hum_cum_10_20 = weekly_humidity_10 + weekly_humidity_11 + weekly_humidity_12 + weekly_humidity_13 + weekly_humidity_14 + weekly_humidity_15 + weekly_humidity_16 + weekly_humidity_17 + weekly_humidity_18 + weekly_humidity_19,
         hum_cum_20_30 = weekly_humidity_20 + weekly_humidity_21 + weekly_humidity_22 + weekly_humidity_23 + weekly_humidity_24 + weekly_humidity_25 + weekly_humidity_26 + weekly_humidity_27 + weekly_humidity_28 + weekly_humidity_29,
         hum_cum_30_37 = weekly_humidity_30 + weekly_humidity_31 + weekly_humidity_32 + weekly_humidity_33 + weekly_humidity_34 + weekly_humidity_35 + weekly_humidity_36,
         min_hum = pmin(weekly_humidity_0, weekly_humidity_1, weekly_humidity_2, weekly_humidity_3,
                        weekly_humidity_4, weekly_humidity_5, weekly_humidity_6, weekly_humidity_7,
                        weekly_humidity_8, weekly_humidity_9, weekly_humidity_10, weekly_humidity_11,
                        weekly_humidity_12, weekly_humidity_13, weekly_humidity_14, weekly_humidity_15,
                        weekly_humidity_16, weekly_humidity_17, weekly_humidity_18, weekly_humidity_19,
                        weekly_humidity_20, weekly_humidity_21, weekly_humidity_22, weekly_humidity_23,
                        weekly_humidity_24, weekly_humidity_25, weekly_humidity_26, weekly_humidity_27,
                        weekly_humidity_28, weekly_humidity_29, weekly_humidity_30, weekly_humidity_31,
                        weekly_humidity_32, weekly_humidity_33, weekly_humidity_34, weekly_humidity_35,
                        weekly_humidity_36, weekly_humidity_37, weekly_humidity_38, weekly_humidity_39,
                        weekly_humidity_40, weekly_humidity_41, na.rm = TRUE),
         max_hum = pmax(weekly_humidity_0, weekly_humidity_1, weekly_humidity_2, weekly_humidity_3,
                        weekly_humidity_4, weekly_humidity_5, weekly_humidity_6, weekly_humidity_7,
                        weekly_humidity_8, weekly_humidity_9, weekly_humidity_10, weekly_humidity_11,
                        weekly_humidity_12, weekly_humidity_13, weekly_humidity_14, weekly_humidity_15,
                        weekly_humidity_16, weekly_humidity_17, weekly_humidity_18, weekly_humidity_19,
                        weekly_humidity_20, weekly_humidity_21, weekly_humidity_22, weekly_humidity_23,
                        weekly_humidity_24, weekly_humidity_25, weekly_humidity_26, weekly_humidity_27,
                        weekly_humidity_28, weekly_humidity_29, weekly_humidity_30, weekly_humidity_31,
                        weekly_humidity_32, weekly_humidity_33, weekly_humidity_34, weekly_humidity_35,
                        weekly_humidity_36, weekly_humidity_37, weekly_humidity_38, weekly_humidity_39,
                        weekly_humidity_40, weekly_humidity_41, na.rm = TRUE),
         min_HI = pmin(weekly_HI_0, weekly_HI_1, weekly_HI_2, weekly_HI_3,
                       weekly_HI_4, weekly_HI_5, weekly_HI_6, weekly_HI_7,
                       weekly_HI_8, weekly_HI_9, weekly_HI_10, weekly_HI_11,
                       weekly_HI_12, weekly_HI_13, weekly_HI_14, weekly_HI_15,
                       weekly_HI_16, weekly_HI_17, weekly_HI_18, weekly_HI_19,
                       weekly_HI_20, weekly_HI_21, weekly_HI_22, weekly_HI_23,
                       weekly_HI_24, weekly_HI_25, weekly_HI_26, weekly_HI_27,
                       weekly_HI_28, weekly_HI_29, weekly_HI_30, weekly_HI_31,
                       weekly_HI_32, weekly_HI_33, weekly_HI_34, weekly_HI_35,
                       weekly_HI_36, weekly_HI_37, weekly_HI_38, weekly_HI_39,
                       weekly_HI_40, weekly_HI_41, na.rm = TRUE),
         max_HI = pmax(weekly_HI_0, weekly_HI_1, weekly_HI_2, weekly_HI_3,
                       weekly_HI_4, weekly_HI_5, weekly_HI_6, weekly_HI_7,
                       weekly_HI_8, weekly_HI_9, weekly_HI_10, weekly_HI_11,
                       weekly_HI_12, weekly_HI_13, weekly_HI_14, weekly_HI_15,
                       weekly_HI_16, weekly_HI_17, weekly_HI_18, weekly_HI_19,
                       weekly_HI_20, weekly_HI_21, weekly_HI_22, weekly_HI_23,
                       weekly_HI_24, weekly_HI_25, weekly_HI_26, weekly_HI_27,
                       weekly_HI_28, weekly_HI_29, weekly_HI_30, weekly_HI_31,
                       weekly_HI_32, weekly_HI_33, weekly_HI_34, weekly_HI_35,
                       weekly_HI_36, weekly_HI_37, weekly_HI_38, weekly_HI_39,
                       weekly_HI_40, weekly_HI_41, na.rm = TRUE),
         max_temp = pmax(weekly_max_temp_0, weekly_max_temp_1, weekly_max_temp_2, weekly_max_temp_3,
                         weekly_max_temp_4, weekly_max_temp_5, weekly_max_temp_6, weekly_max_temp_7,
                         weekly_max_temp_8, weekly_max_temp_9, weekly_max_temp_10, weekly_max_temp_11,
                         weekly_max_temp_12, weekly_max_temp_13, weekly_max_temp_14, weekly_max_temp_15,
                         weekly_max_temp_16, weekly_max_temp_17, weekly_max_temp_18, weekly_max_temp_19,
                         weekly_max_temp_20, weekly_max_temp_21, weekly_max_temp_22, weekly_max_temp_23,
                         weekly_max_temp_24, weekly_max_temp_25, weekly_max_temp_26, weekly_max_temp_27,
                         weekly_max_temp_28, weekly_max_temp_29, weekly_max_temp_30, weekly_max_temp_31,
                         weekly_max_temp_32, weekly_max_temp_33, weekly_max_temp_34, weekly_max_temp_35,
                         weekly_max_temp_36, weekly_max_temp_37, weekly_max_temp_38, weekly_max_temp_39,
                         weekly_max_temp_40, weekly_max_temp_41, na.rm = TRUE),
         min_temp = pmin(weekly_minT_0, weekly_minT_1, weekly_minT_2, weekly_minT_3,
                         weekly_minT_4, weekly_minT_5, weekly_minT_6, weekly_minT_7,
                         weekly_minT_8, weekly_minT_9, weekly_minT_10, weekly_minT_11,
                         weekly_minT_12, weekly_minT_13, weekly_minT_14, weekly_minT_15,
                         weekly_minT_16, weekly_minT_17, weekly_minT_18, weekly_minT_19,
                         weekly_minT_20, weekly_minT_21, weekly_minT_22, weekly_minT_23,
                         weekly_minT_24, weekly_minT_25, weekly_minT_26, weekly_minT_27,
                         weekly_minT_28, weekly_minT_29, weekly_minT_30, weekly_minT_31,
                         weekly_minT_32, weekly_minT_33, weekly_minT_34, weekly_minT_35,
                         weekly_minT_36, weekly_minT_37, weekly_minT_38, weekly_minT_39,
                         weekly_minT_40, weekly_minT_41, na.rm = TRUE))

# calculate the cumulative for the last 10 weeks
Splitdata$Hi_cum_30_40 <- rowSums(Splitdata[, c("weekly_HI_30", "weekly_HI_31", "weekly_HI_32", "weekly_HI_33", "weekly_HI_34", "weekly_HI_35", "weekly_HI_36", "weekly_HI_37", "weekly_HI_38", "weekly_HI_40")], na.rm = TRUE)

write.csv(Splitdata, "sga_lga_new.csv") # which is the final dataset for the analysis, 
                                        # fakedata was generated based on this dataset.



# # construct a new column for the cumulative HI for the last 8 weeks (from not NA backward)
# cum_last_6_HI <- t(apply(ANCdata_GA, 1, function(sub) exphist(sub[paste0("weekly_HI_", 0:41)], as.numeric(sub["gestational_weeks"]), lag=c(0,5))))
# cum_last_6_HI <- apply(cum_last_6_HI, 2, as.numeric)
# colnames(cum_last_6_HI) <- paste("lag", 0:5, sep="")
# cumulative_last_6_HI <- rowSums(cum_last_6_HI)
# Splitdata$Hi_last_6 <- cumulative_last_6_HI
# # colnames(Splitdata)[colnames(Splitdata) == "cumulative_last_6_HI"] <- "Hi_last_6"
# 
# cum_last_6_maxt <- t(apply(ANCdata_GA, 1, function(sub) exphist(sub[paste0("weekly_max_temp_", 0:41)], as.numeric(sub["gestational_weeks"]), lag=c(0,5))))
# cum_last_6_maxt <- apply(cum_last_6_maxt, 2, as.numeric)
# colnames(cum_last_6_maxt) <- paste("lag", 0:5, sep="")
# cumulative_last_6_maxt <- rowSums(cum_last_6_maxt)
# Splitdata$maxt_last_6 <- cumulative_last_6_maxt
# 
# cum_last_6_mint <- t(apply(ANCdata_GA, 1, function(sub) exphist(sub[paste0("weekly_minT_", 0:41)], as.numeric(sub["gestational_weeks"]), lag=c(0,5))))
# cum_last_6_mint <- apply(cum_last_6_mint, 2, as.numeric)
# colnames(cum_last_6_mint) <- paste("lag", 0:5, sep="")
# cumulative_last_6_mint <- rowSums(cum_last_6_mint)
# Splitdata$mint_last_6 <- cumulative_last_6_mint
# 
# cum_last_6_hum <- t(apply(ANCdata_GA, 1, function(sub) exphist(sub[paste0("weekly_humidity_", 0:41)], as.numeric(sub["gestational_weeks"]), lag=c(0,5))))
# cum_last_6_hum <- apply(cum_last_6_hum, 2, as.numeric)
# colnames(cum_last_6_hum) <- paste("lag", 0:5, sep="")
# cumulative_last_6_hum <- rowSums(cum_last_6_hum)
# Splitdata$hum_cum_last_6 <- cumulative_last_6_hum
