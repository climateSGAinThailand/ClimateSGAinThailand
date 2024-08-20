################################################################################
#
# MGH Dissertation Final output R script
#
################################################################################
# library(caret)  
# library(e1071)
# library(rpart.plot)
# library(DiagrammeR)
# library(patchwork)
# library(EIX)
library(randomForest)
library(pdp)
library(reshape2)
library(ALEPlot)
library(ggplot2)
library(h2o)
library(iml)
library(DALEX)
library(ALEPlot)
library(tidyverse)
library(tidymodels)

################################################################################
#
# The code for data cleaning and other exploratory analysis are included in 
# another script called "datacleaning_script.R", this script only contains all 
# code used to generate the final output that is presented in the dissertation.
# Please mind that the fakedata does not have exactly the same distribution as 
# the real data, so the results might be different.
#
################################################################################

data <- read.csv("fakedata.csv")
str(data)
summary(data)
#data <- na.omit(data)

## factorise data
data$SGA <- as.factor(data$SGA)
data$ANAEMIA <- as.factor(data$ANAEMIA)
data$AGE_GRAV <- as.factor(data$AGE_GRAV)
data$EC_PRE_EC <- as.factor(data$EC_PRE_EC)
data$mip <- as.factor(data$mip)
data$Study_year <- as.factor(data$Study_year)
data$lmp_month <- as.factor(data$lmp_month)
data$status <- as.factor(data$status)


#
# Table 1: Characteristics of the study population
#
# SGA: Binary outcome (=1)
# status: refugee (=1), migrant (=2)
# AGE_GRAV: age and gravidity (6 levels)
# wtless40: weight<40kg (=1), weight≥40kg (=0)
# ANAEMIA: anaemia during pregnancy (=1), non-anaemia (=0)
# EC_PRE_EC: pre-eclampsia (=1), no pre-eclampsia (=0)
# mip: malaria in pregnancy (=1)
# dfcT1: first ANC visit in trimester 1 (=1)
# Study_year: every 5 years one category
prop.table(table(data$SGA))
table(data$SGA)
prop.table(table(data$status))
table(data$status)
prop.table(table(data$AGE_GRAV))
table(data$AGE_GRAV)
prop.table(table(data$wtless40))
table(data$wtless40)
prop.table(table(data$ANAEMIA))
table(data$ANAEMIA)
prop.table(table(data$EC_PRE_EC))
table(data$EC_PRE_EC)
prop.table(table(data$mip))
table(data$mip)
prop.table(table(data$dfcT1))
table(data$dfcT1)
prop.table(table(data$Study_year))
table(data$Study_year)


#
# Figure 2 (plotted in excel)
#
prop.table(table(data$Study_year, data$SGA), margin = 1) * 100
prop.table(table(data$Study_year, data$ANAEMIA), margin = 1) * 100
prop.table(table(data$Study_year, data$mip), margin = 1) * 100
prop.table(table(data$Study_year, data$wtless40), margin = 1) * 100
prop.table(table(data$Study_year, data$EC_PRE_EC), margin = 1) * 100
prop.table(table(data$Study_year, data$AGE_GRAV), margin = 1) * 100


#
# Figure S1: due to restriction license, the climate data cannot be share
# 
library(dplyr)
library(readxl)
library(ggplot2)
# climatedata <- read_excel("MaeSot_WeatherStatio_1986-2020.xlsx")
# humiditydata <- read.csv("daily_humidity_long.csv")[, -1]
# reorganize the climatedata
climatedata$DATE <- as.Date(climatedata$DATE, format = "%m/%d/%Y")
climatedata$Year <- as.numeric(format(climatedata$DATE, "%Y"))
climatedata <- climatedata %>%
  mutate(Study_year = ifelse(Year >= 1986 & Year < 1991, "1986-1990",
                             ifelse(Year >= 1991 & Year < 1996, "1991-1995",
                                    ifelse(Year >= 1996 & Year < 2001, "1996-2000",
                                           ifelse(Year >= 2001 & Year < 2006, "2001-2005", 
                                                  ifelse(Year >= 2006 & Year < 2011, "2006-2010",
                                                         ifelse(Year >= 2011 & Year < 2016, "2011-2015", "2016-2020")))))))

# plot the distribution of climatic variables within each study_year level 
climatedata$Study_year <- as.factor(climatedata$Study_year)
climatedata <- climatedata %>%
  select(Study_year, TMAX, TMIN, TAVG)
# turn TMAX TMIN TAVG into celsius degree, ignore the NAs
# climatedata$TMAX <- (climatedata$TMAX - 32) * 5 / 9
# climatedata$TMIN <- (climatedata$TMIN - 32) * 5 / 9
# climatedata$TAVG <- (climatedata$TAVG - 32) * 5 / 9
climatedata <- climatedata %>%
  gather(key = "variable", value = "value", -Study_year)
names(climatedata)[2] <- "Temperature"
# change the names of Temperautre to TMAX, TMIN, TAVG
climatedata$Temperature <- ifelse(climatedata$Temperature == "TMAX", "Daily Max",
                                  ifelse(climatedata$Temperature == "TMIN", "Daily Min", "Daily Avg"))
# add a line to show the mean value of each climatic variable
climatedata_mean <- climatedata %>%
  group_by(Study_year, Temperature) %>%
  summarize(mean_value = mean(value, na.rm = TRUE))
ggplot(climatedata, aes(value, fill = Temperature)) +
  geom_density(alpha = 0.5) +
  geom_vline(data = climatedata_mean, aes(xintercept = mean_value), color = "black") +
  facet_wrap(~Study_year, scales = "free_x", ncol = 7) +
  coord_flip() +
  labs(title = "Temperature distribution across 1986-2020 (5-year)",
       x = "Daily Temperature (F)",
       y = "Density")


#
# Figure S2. plot the time series plot of relative humidity
#
humiditydata$date <- as.Date(humiditydata$date)
ggplot(humiditydata, aes(date, humidity)) +
  geom_line() +
  # add a fit line in red
  geom_smooth(method = "loess", color = "red") +
  labs(title = "Daily Relative Humidity 1986-2020",
       x = "",
       y = "Relative Humidity (%)")


#############################################################################
# 
# Initial analysis: multiple logistic regression
#
levels(data$AGE_GRAV) <- c("<20prim", "<20multi", "20-34prim", "20-34multi", ">35prim", ">35multi")
data$AGE_GRAV <- relevel(data$AGE_GRAV, ref = "20-34multi")

# 1. logistic regression on full data
# Univariate ##
logi_model1 <- glm(SGA ~ ANAEMIA, data = data, family = binomial)
summary(logi_model1)
exp(cbind(OR = coef(logi_model1), confint(logi_model1)))
logi_model2 <- glm(SGA ~ AGE_GRAV, data = data, family = binomial)
summary(logi_model2)
exp(cbind(OR = coef(logi_model2), confint(logi_model2)))
logi_model3 <- glm(SGA ~ EC_PRE_EC, data = data, family = binomial)
summary(logi_model3)
exp(cbind(OR = coef(logi_model3), confint(logi_model3)))
logi_model4 <- glm(SGA ~ wtless40, data = data, family = binomial)
summary(logi_model4)
exp(cbind(OR = coef(logi_model4), confint(logi_model4)))
logi_model5 <- glm(SGA ~ mip, data = data, family = binomial)
summary(logi_model5)
exp(cbind(OR = coef(logi_model5), confint(logi_model5)))
logi_model6 <- glm(SGA ~ dfcT1, data = data, family = binomial)
summary(logi_model6)
exp(cbind(OR = coef(logi_model6), confint(logi_model6)))
logi_model7 <- glm(SGA ~ status, data = data, family = binomial)
summary(logi_model7)
exp(cbind(OR = coef(logi_model7), confint(logi_model7)))

# multivariate ##
logistic_model_all <- glm(SGA ~ ANAEMIA + AGE_GRAV + EC_PRE_EC + wtless40 + mip + dfcT1 + status + lmp_month + Study_year, data = data, family = binomial)
summary(logistic_model_all)
exp(cbind(OR = coef(logistic_model_all), confint(logistic_model_all)))

# Table S1: Contingency tables ##
prop.table(table(data[data$mip == 0,]$Study_year, data[data$mip == 0,]$SGA), margin = 1)
prop.table(table(data[data$mip == 1,]$Study_year, data[data$mip == 1,]$SGA), margin = 1)
# prop.table(table(data[data$AGE_GRAV != '20-34multi',]$Study_year, data[data$AGE_GRAV != '20-34multi',]$SGA), margin = 1)
# prop.table(table(data[data$AGE_GRAV == '20-34multi',]$Study_year, data[data$AGE_GRAV == '20-34multi',]$SGA), margin = 1)



#################################################################################
#
# Smaller cohort - Random forest
#
data$Study_year <- as.numeric(data$Study_year)
data <- data %>%
  filter(mip == 0, Study_year > 3) # non-malaria 2000-2020 cohort filter
data$Study_year <- as.factor(data$Study_year)
data <- data[, -6]  #remove malaria (mip) column

prop.table(table(data$SGA))
table(data$SGA)
prop.table(table(data$status))
table(data$status)
prop.table(table(data$AGE_GRAV))
table(data$AGE_GRAV)
prop.table(table(data$wtless40))
table(data$wtless40)
prop.table(table(data$ANAEMIA))
table(data$ANAEMIA)
prop.table(table(data$EC_PRE_EC))
table(data$EC_PRE_EC)
prop.table(table(data$Study_year))
table(data$Study_year)

## prepare data
set.seed(123)
ind <- sample(2, nrow(data),
              replace = TRUE,
              prob = c(0.6, 0.4))
training <- data[ind == 1, ]
testing <- data[ind == 2, ]


## logistic regression
model <- glm(SGA ~
               EC_PRE_EC + wtless40 + ANAEMIA + AGE_GRAV + status,
             # mip + EC_PRE_EC + ANAEMIA + Study_year +
             # mip:Study_year + EC_PRE_EC:Study_year + ANAEMIA:Study_year,
             # mip*Study_year*EC_PRE_EC * ANAEMIA,
             family = binomial(link = "logit"), data = training)
summary(model)
# exp(model$coefficients)
exp(cbind(OR = coef(model), confint(model)))


coeff <- tidy(model) %>%
  arrange(desc(abs(estimate))) %>%
  filter(abs(estimate) > 0.5)
ggplot(coeff, aes(x = term, y = exp(estimate), fill = term)) + geom_col() + coord_flip()



response <- as.factor(training$SGA)
features <- training[-which(names(training) == "SGA")]


## random forest
set.seed(123)
rf_model <- randomForest(response ~ ., data = features, type = "classification",
                         ntree = 1000, importance = TRUE)

# feature importance
imp <-  importance(rf_model)
barplot(imp[, "MeanDecreaseGini"])
impvar <- rownames(imp)[order(imp[, 4], decreasing = TRUE)]
imp[impvar, "MeanDecreaseGini"]
barplot(imp[impvar, "MeanDecreaseGini"])

# save.image(file = "rf_final.RData")

## Importance
yhat <- function(X_model, newdata) {
  p_hat <- as.numeric(predict(X_model, newdata, type = "response"))
  # log(p_hat / (1 - p_hat))
  # print(p_hat)
}
predictor_rf <- Predictor$new(
  model = rf_model,
  data = features,
  y = response,
  predict.fun = yhat,
  class = "classification"
)

imp_rf <- FeatureImp$new(predictor_rf, loss = "ce")
plot(imp_rf) + ggtitle("RF - Feature importance")
rf_ot <- FeatureEffect$new(predictor_rf, "AGE_GRAV") %>% plot() + ggtitle("RF")
rf_ot


#### Interactions (might took hours to run each plot)
interact.rf  <- Interaction$new(predictor_rf) %>% plot() + ggtitle("RF")
interact.rf
# save.image(file = "rf.RData")

interact.rf2  <- Interaction$new(predictor_rf, feature = "AGE_GRAV") %>% plot()
interact.rf2

interact.rf3  <- Interaction$new(predictor_rf, feature = "wtless40") %>% plot()
interact.rf3

interact.rf4  <- Interaction$new(predictor_rf, feature = "ANAEMIA") %>% plot()
interact.rf4

interact.rf5 <- Interaction$new(predictor_rf, feature = "EC_PRE_EC") %>% plot()
interact.rf5


#### PDP
partialPlot(rf_model, pred.data = features, x.var = "AGE_GRAV")
partialPlot(rf_model, pred.data = features, x.var = "wtless40")
partialPlot(rf_model, pred.data = features, x.var = "ANAEMIA")
partialPlot(rf_model, pred.data = features, x.var = "EC_PRE_EC")

# partialPlot(rf_model, pred.data = features, x.var = "hum_cum_20_30")
# partialPlot(rf_model, pred.data = features, x.var = "mint_cum_10_20")
# partialPlot(rf_model, pred.data = features, x.var = "maxt_cum_10")
# partialPlot(rf_model, pred.data = features, x.var = "mint_cum_20_30")
# partialPlot(rf_model, pred.data = features, x.var = "Hi_cum_10_20")
# partialPlot(rf_model, pred.data = features, x.var = "maxt_cum_10_20")
# partialPlot(rf_model, pred.data = features, x.var = "hum_cum_10_20")

explainer_rf <- DALEX::explain(model = rf_model,
                               data = features,
                               y = as.numeric(response)-1,
                               label = "Random Forest")

#
# Figure 3: Final PDP code with better formatting and legend ##
#
## Figure 3(a)
pdp_rf_1 <- model_profile(explainer = explainer_rf,
                          variables = "wtless40", groups = "AGE_GRAV")
# plot(pdp_rf_1, geom = "profiles") +
#   ggtitle("Partial-dependence profiles for weight<40kg, grouped by Age&Gravidity") +
#   xlab("Weight<40kg") +
#   ylab("Predicted Probability of SGA") +
#   scale_x_continuous(breaks = c(0, 1) ,labels = c("No", "Yes"))
p1 <- plot(pdp_rf_1, geom = "profiles") +
  labs(
    title = "PDP for weight<40kg, grouped by Age&Gravidity",
    subtitle = element_blank(),
    caption = element_blank()
  ) +
  theme(axis.title.x.top = element_blank(),
        strip.text = element_blank(),
  ) +
  scale_color_discrete(labels = c("<20 & primigravida", "<20 & multigravida", "20-34 & primigravida", "20-34 & multigravida", "≥35 & primigravida", "≥35 & multigravida")) +
  guides(color = guide_legend(title = "Age & Gravidity", theme(legend.title.position = "left"))) +
  ylab("Probability of SGA") + xlab("Weight<40kg") + 
  scale_x_continuous(breaks = c(0, 1) ,labels = c("No", "Yes"))
p1

## Figure 3(b)
pdp_rf_2 <- model_profile(explainer = explainer_rf,
                          variables = "hum_cum_10_20", groups = "AGE_GRAV")
# plot(pdp_rf_2, geom = "profiles") +
#   ggtitle("Partial-dependence profiles for hum_cum_10_20, grouped by age&gravidity") +
#   xlab("Cumulative relative humidity over weeks 10-19 (average weekly RH)") +
#   ylab("Predicted Probability of SGA") +
#   scale_x_continuous(breaks = c(600, 700, 800, 900), labels = c("600 (60%)", "700 (70%)", "800 (80%)", "900 (90%)")) +
#   # add a vertical line at the 800 mark
#   geom_vline(xintercept = 760, linetype = "dashed", color = "black")
p2 <- plot(pdp_rf_2, geom = "profiles") +
  labs(
    title = "PDP for relative humidity (weeks 10-19), grouped by Age&Gravidity",
    subtitle = element_blank(),
    caption = element_blank()
  ) +
  theme(axis.title.x.top = element_blank(),
        strip.text = element_blank(),
  ) +
  scale_color_discrete(labels = c("<20 & primigravida", "<20 & multigravida", "20-34 & primigravida", "20-34 & multigravida", "≥35 & primigravida", "≥35 & multigravida")) +
  guides(color = guide_legend(title = "Age&Gravidity", theme(legend.title.position = "left"))) +
  ylab("Probability of SGA") + xlab("Mean relative humidity during gestational weeks 10 to 19") +
  scale_x_continuous(breaks = c(600, 700, 800, 900), labels = c("60%", "70%", "80%", "90%")) +
  # add a vertical line at the 800 mark
  geom_vline(xintercept = 760, linetype = "dashed", color = "black") +
  # add tick on the x-axis for each break
  theme(axis.ticks.x = element_line(color = "darkgray", linewidth = 0.6),
        axis.ticks.length = unit(0.25, "cm"))
p2

## Figure 3(c)
pdp_rf_3 <- model_profile(explainer = explainer_rf,
                          variables = "maxt_cum_20_30", groups = "AGE_GRAV")
# plot(pdp_rf_3, geom = "profiles") +
#   ggtitle("Partial-dependence profiles for maxt_cum_20_30, grouped by age&gravidity") +
#   xlab("Cumulative max temp over weeks 20-29 (average weekly max temp in Celsius)") +
#   ylab("Predicted Probability of SGA") +
#   scale_x_continuous(breaks = c(850, 900, 950, 1000), labels = c("850F(29.4C)", "900F(32.2C)", "950F(35.0C)", "1000F(37.8C)")) +
#   # add a vertical line at the 910F mark
#   geom_vline(xintercept = 920, linetype = "dashed", color = "black")
p3 <- plot(pdp_rf_3, geom = "profiles") +
  labs(
    title = "PDP for maximum temperature (weeks 20-29), grouped by Age&Gravidity",
    subtitle = element_blank(),
    caption = element_blank()
  ) +
  theme(axis.title.x.top = element_blank(),
        strip.text = element_blank(),
  ) +
  scale_color_discrete(labels = c("<20 & primigravida", "<20 & multigravida", "20-34 & primigravida", "20-34 & multigravida", "≥35 & primigravida", "≥35 & multigravida")) +
  guides(color = guide_legend(title = "Age&Gravidity", theme(legend.title.position = "left"))) +
  ylab("Probability of SGA") + xlab("Mean maximum temperature during gestational weeks 20 to 29") +
  geom_vline(xintercept = 910, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = c(850, 900, 950, 1000), labels = c("85F(29.4C)", "90F(32.2C)", "95F(35.0C)", "100F(37.8C)")) +
  # add tick on the x-axis for each break
  theme(axis.ticks.x = element_line(color = "darkgray", linewidth = 0.6),
        axis.ticks.length = unit(0.25, "cm"))
p3


## Figure 3(d)
pdp_rf_4 <- model_profile(explainer = explainer_rf,
                          variables = "mint_last6", groups = "AGE_GRAV")
# plot(pdp_rf_4, geom = "profiles") +
#   ggtitle("Partial-dependence profiles for mint_last6, grouped by age&gravidity") +
#   xlab("Minimum temperature in Fahrenheit (average weekly min temp in Celsius)") +
#   ylab("Predicted Probability of SGA") +
#   scale_x_continuous(breaks = c(600, 650, 700, 750), labels = c("600F(15.6C)", "650F(18.3C)", "700F(21.1C)", "750F(23.9C)")) +
#   # add a vertical line at the 750F mark
#   geom_vline(xintercept = 740, linetype = "dashed", color = "black")
p4 <- plot(pdp_rf_4, geom = "profiles") +
  labs(
    title = "PDP for minimum temperature (weeks 30-36), grouped by Age&Gravidity",
    subtitle = element_blank(),
    caption = element_blank()
  ) +
  theme(axis.title.x.top = element_blank(),
        strip.text = element_blank(),
  ) +
  scale_color_discrete(labels = c("<20 & primigravida", "<20 & multigravida", "20-34 & primigravida", "20-34 & multigravida", "≥35 & primigravida", "≥35 & multigravida")) +
  guides(color = guide_legend(title = "Age&Gravidity", theme(legend.title.position = "left"))) +
  ylab("Probability of SGA") + xlab("Mean minimum temperature during gestational weeks 30 to 36") +
  geom_vline(xintercept = 740, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = c(600, 650, 700, 750), labels = c("60F(15.6C)", "65F(18.3C)", "70F(21.1C)", "75F(23.9C)")) +
  # add tick on the x-axis for each break
  theme(axis.ticks.x = element_line(color = "darkgray", linewidth = 0.6),
        axis.ticks.length = unit(0.25, "cm"))
p4


# Another way to plot PDP - the contour plots that can visualize better the changes

sgap4 <- pdp::partial(rf_model, pred.var = c("AGE_GRAV", "mint_last6"), prob = TRUE)
# plotPartial(sgap, zlab = "SGA", levelplot = FALSE, scale = list(arrows = FALSE))
ggplot(sgap, aes(x = mint_last6, y = AGE_GRAV)) +
  geom_raster(aes(fill = 1-yhat)) +
  # ylab() +
  # geom_contour(aes(z = 1-yhat), colour = "white", size = 0.2, alpha = 0.5) +
  scale_fill_gradient2("Probability of SGA",
                       low = "#0059ff", high = "red", midpoint  = 0.275, na.value = NA) +
  theme_minimal()
sgap4


# pdp_rf_grav7 <- model_profile(explainer = explainer_rf,
#                               variables = "max_temp", groups = "AGE_GRAV")
# plot(pdp_rf_grav7, geom = "profiles") +
#   ggtitle("Partial-dependence profiles for max_temp, grouped by age&gravidity")
# 
# pdp_rf_grav8 <- model_profile(explainer = explainer_rf,
#                               variables = "min_temp", groups = "AGE_GRAV")
# plot(pdp_rf_grav8, geom = "profiles") +
#   ggtitle("Partial-dependence profiles for min_temp, grouped by age&gravidity")
# 
# pdp_rf_grav9 <- model_profile(explainer = explainer_rf,
#                               variables = "lmp_month", groups = "AGE_GRAV")
# plot(pdp_rf_grav9, geom = "profiles") +
#   ggtitle("Partial-dependence profiles for lmp_month, grouped by age&gravidity")
# 
# pdp_rf_grav10 <- model_profile(explainer = explainer_rf,
#                                variables = "Study_year", groups = "AGE_GRAV")
# plot(pdp_rf_grav10, geom = "profiles") +
#   ggtitle("Partial-dependence profiles for Study_year, grouped by age&gravidity")

# pdp_rf_mint6last2 <- model_profile(explainer = explainer_rf,
#                                variables = "mint_last6", groups = "wtless40")
# plot(pdp_rf_mint6last2, geom = "profiles") +
#   ggtitle("Partial-dependence profiles for mint_last6, grouped by wt40")
# 
# pdp_rf_mint6last3 <- model_profile(explainer = explainer_rf,
#                                variables = "mint_last6", groups = "ANAEMIA")
# plot(pdp_rf_mint6last3, geom = "profiles") +
#   ggtitle("Partial-dependence profiles for mint_last6, grouped by anaemia")
# 
# pdp_rf_mint6last4 <- model_profile(explainer = explainer_rf,
#                                variables = "mint_last6", groups = "status")
# plot(pdp_rf_mint6last4, geom = "profiles") +
#   ggtitle("Partial-dependence profiles for mint_last6, grouped by status")
# 
# pdp_rf_mint6last5 <- model_profile(explainer = explainer_rf,
#                                variables = "mint_last6", groups = "EC_PRE_EC")
# plot(pdp_rf_mint6last5, geom = "profiles") +
#   ggtitle("Partial-dependence profiles for mint_last6, grouped by pre-eclampsia")
# 
# # wtless40
# pdp_rf_weight <- model_profile(explainer = explainer_rf,
#                                variables = "mint_cum_10_20", groups = "wtless40")
# plot(pdp_rf_weight, geom = "profiles") +
#   ggtitle("Partial-dependence profiles for mint_cum_10_20, grouped by wt40")
# 
# pdp_rf_weight2 <- model_profile(explainer = explainer_rf,
#                                 variables = "min_hum", groups = "wtless40")
# plot(pdp_rf_weight2, geom = "profiles") +
#   ggtitle("Partial-dependence profiles for age, grouped by wt40")
# 
# 


#
# Figure 4.
#
pdp_rf_5 <- model_profile(explainer = explainer_rf,
                          variables = "Hi_last6", groups = "wtless40")
# plot(pdp_rf_5, geom = "profiles") +
#   ggtitle("Partial-dependence profiles for Hi_last6, grouped by weight<40kg") +
#   xlab("Cumulative HI in weeks 30-36 in Fahrenheit (average weekly HI in Celsius)") +
#   scale_x_continuous(labels = c("750F(23.9C)", "800F(26.7C)", "850F(29.4C)", "900F(32.2C)", "950F(35.0C)"))
p5 <- plot(pdp_rf_5, geom = "profiles") +
  labs(
    title = "PDP for heat index (weeks 30-36), grouped by maternal weight",
    subtitle = element_blank(),
    caption = element_blank()
  ) +
  theme(axis.title.x.top = element_blank(),
        strip.text = element_blank(),
  ) +
  scale_color_discrete(labels = c("weight≥40kg", "weight<40kg")) +
  guides(color = guide_legend(title = "Maternal weight", theme(legend.title.position = "left"))) +
  ylab("Probability of SGA") + xlab("Mean heat index during gestational weeks 30 to 36") +
  scale_x_continuous(breaks = c(750, 800, 850, 900, 950), labels = c("75F(23.9C)", "80F(26.7C)", "85F(29.4C)", "90F(32.2C)", "95F(35.0C)")) +
  # add tick on the x-axis for each break
  theme(axis.ticks.x = element_line(color = "darkgray", linewidth = 0.6),
        axis.ticks.length = unit(0.25, "cm")) +
  geom_vline(xintercept = 840, linetype = "dashed", color = "black")
p5


pdp_rf_6 <- model_profile(explainer = explainer_rf,
                          variables = "Hi_last6", groups = "EC_PRE_EC")
# plot(pdp_rf_6, geom = "profiles") +
#   ggtitle("Partial-dependence profiles for Hi_last6, grouped by pre-eclampsia") +
#   xlab("Cumulative HI in weeks 30-36 in Fahrenheit (average weekly HI in Celsius)") +
#   scale_x_continuous(labels = c("750F(23.9C)", "800F(26.7C)", "850F(29.4C)", "900F(32.2C)", "950F(35.0C)"))
p6 <- plot(pdp_rf_6, geom = "profiles") +
  labs(
    title = "PDP for heat index (weeks 30-36), grouped by pre-eclampsia",
    subtitle = element_blank(),
    caption = element_blank()
  ) +
  theme(axis.title.x.top = element_blank(),
        strip.text = element_blank(),
  ) +
  scale_color_discrete(labels = c("no pre-eclampsia", "pre-eclampsia")) +
  guides(color = guide_legend(title = "Pre-eclampsia", theme(legend.title.position = "left"))) +
  ylab("Probability of SGA") + xlab("Mean heat index during gestational weeks 30 to 36") +
  scale_x_continuous(breaks = c(750, 800, 850, 900, 950), labels = c("75F(23.9C)", "80F(26.7C)", "85F(29.4C)", "90F(32.2C)", "95F(35.0C)")) +
  # add tick on the x-axis for each break
  theme(axis.ticks.x = element_line(color = "darkgray", linewidth = 0.6),
        axis.ticks.length = unit(0.25, "cm")) +
  geom_vline(xintercept = 840, linetype = "dashed", color = "black")
p6

# 
# pdp_rf_hilast6_3 <- model_profile(explainer = explainer_rf,
#                                   variables = "Hi_last6", groups = "ANAEMIA")
# plot(pdp_rf_hilast6_3, geom = "profiles") +
#   ggtitle("Partial-dependence profiles for Hi_last6, grouped by anaemia")
# 
# pdp_rf_hilast6_4 <- model_profile(explainer = explainer_rf,
#                                   variables = "Hi_last6", groups = "status")
# plot(pdp_rf_hilast6_4, geom = "profiles") +
#   ggtitle("Partial-dependence profiles for Hi_last6, grouped by status")
