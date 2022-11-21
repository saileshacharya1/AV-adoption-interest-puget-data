## Load the packages ###########################################################
################################################################################

library(mlogit)
library(pscl)
library(data.table)
library(nnet)
library(factoextra)
library(NbClust)
library(psych)
library(lavaan)
library(fastDummies)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

## Import data #################################################################
################################################################################

df <- readRDS("data/intermediate/clean_data.rds")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

### K-means clustering (Partitioning) ##########################################
################################################################################

# https://www.datanovia.com/en/lessons/determining-the-optimal-number-of-clusters-3-must-know-methods/

# new df with input columns for clustering
df_cluster <- df[c(
  "personid", "av_interest_nodriver",
  "av_interest_backupdriver", "av_interest_own",
  "av_interest_carshare", "av_interest_short"
)]

# keep row id
rownames(df_cluster) <- df_cluster$personid
df_cluster$personid <- NULL

# Elbow method
fviz_nbclust(df_cluster, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) +
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(df_cluster, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy.
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(df_cluster, kmeans, nstart = 25, method = "gap_stat", nboot = 50) +
  labs(subtitle = "Gap statistic method")

# k-means cluster analysis
set.seed(100)
fit <- kmeans(df_cluster, 3) # 3 cluster solution

# get cluster means
aggregate(df_cluster, by = list(fit$cluster), FUN = mean)

# append cluster assignment
df_cluster <- data.frame(df_cluster, fit$cluster)

# rename the clusters
setnames(df_cluster, "fit.cluster", "cluster")
df_cluster <- df_cluster[c("cluster")]
df_cluster$cluster <- as.numeric(df_cluster$cluster)
df_cluster$personid <- rownames(df_cluster)

# bind clusters to main data frame
df <- merge(df, df_cluster, by = "personid")
# remove intermediate files
rm(df_cluster)
rm(fit)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

### Confirmatory factor analysis ###############################################
################################################################################

# av_concern
model <- "
av_concern =~ av_concern_safeequip + av_concern_legal + av_concern_safeveh +
              av_concern_react + av_concern_perform
"
fit <- cfa(model, data = df, estimator = "DWLS")
summary(fit, rsquare = TRUE, fit.measures = TRUE, standardized = TRUE)

# av_interest
model <- "
av_interest =~ av_interest_nodriver + av_interest_backupdriver +
              av_interest_own + av_interest_carshare + av_interest_short"

fit <- cfa(model, data = df, estimator = "DWLS")
summary(fit, rsquare = TRUE, fit.measures = TRUE, standardized = TRUE)

# remove intermediate files
rm(model)
rm(fit)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

### Multinomial logit model - MNL ##############################################
################################################################################

# make data frame for MNL
df_mnl <- df

# repeat observations for three alternatives
df_mnl <- left_join(
  data.frame(
    personid = rep(df_mnl$personid, each = 3),
    choice = rep(1:3, nrow(df))
  ),
  df_mnl,
  by = "personid"
)

# specify chosen alternative
df_mnl$cluster <- ifelse(df_mnl$cluster == df_mnl$choice, 1, 0)

# add alternative specific variables
df_mnl$cluster_1 <- ifelse(df_mnl$choice == 1, 1, 0)
df_mnl$cluster_2 <- ifelse(df_mnl$choice == 2, 1, 0)
df_mnl$cluster_3 <- ifelse(df_mnl$choice == 3, 1, 0)

# format data for mlogit
df_mnl <- mlogit.data(df_mnl,
  choice = "cluster", shape = "long",
  alt.var = "choice"
)

# MNL model
model <- mlogit(cluster ~ 0 |
  age + gender + worker + employment + student + education +
    hhsize + numchildren + numworkers + hh_income +
    vehicle_count + license + smartphone + walk_freq + bike_freq +
    transit_freq + rideshare_freq + carshare_freq +
    av_concern_average + res_type + res_factors_walk +
    res_factors_transit + res_factors_30min +
    res_factors_school + res_factors_hwy |
  0, reflevel = 1, data = df_mnl)
summary(model)

# remove intermediate files
rm(df_mnl)
rm(model)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

### Prepare data for temporal analysis #########################################
################################################################################

# dummy code few variables
df <- dummy_cols(df,
  select_columns = c(
    "age", "gender", "education",
    "hh_income", "license", "survey_year"
  ),
  remove_selected_columns = FALSE
)

# rename few dummy variables
setnames(df, "age_25 to 44 years", "age_25_44")
setnames(df, "age_45 to 64 years", "age_45_65")
setnames(df, "age_65 years and over", "age_65")
setnames(df, "education_undergraducate degree", "education_undergrad")
setnames(df, "education_graduate or higher degree", "education_grad")
setnames(df, "hh_income_$25,000-$49,999", "hh_income_25_50k")
setnames(df, "hh_income_$50,000-$74,999", "hh_income_50_75k")
setnames(df, "hh_income_$75,000-$99,999", "hh_income_75_100k")
setnames(df, "hh_income_$100,000 or more", "hh_income_100k")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

### Temporal analysis using structural equation modeling #######################
################################################################################

# av interest
model <- "
av_interest =~ av_interest_nodriver + av_interest_backupdriver +
                av_interest_own + av_interest_carshare + av_interest_short
av_interest ~ survey_year_2017 + survey_year_2019 + age_25_44 + age_45_65 +
              age_65 + gender_male + license_yes + education_undergrad +
              education_grad + hh_income_25_50k + hh_income_50_75k +
              hh_income_75_100k + hh_income_100k
"

fit <- cfa(model, data = df, estimator = "MLM")
summary(fit, rsquare = TRUE, fit.measures = TRUE, standardized = TRUE)

# av concern
model <- "
av_concern =~ av_concern_safeequip + av_concern_legal + av_concern_safeveh +
              av_concern_react + av_concern_perform

av_concern ~  survey_year_2017 + survey_year_2019 + age_25_44 + age_45_65 +
              age_65 + gender_male + license_yes + education_undergrad +
              education_grad + hh_income_25_50k + hh_income_50_75k +
              hh_income_75_100k + hh_income_100k
"
fit <- cfa(model, data = df, estimator = "MLM")
summary(fit, rsquare = TRUE, fit.measures = TRUE, standardized = TRUE)

# remove intermediate files
rm(model)
rm(fit)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
