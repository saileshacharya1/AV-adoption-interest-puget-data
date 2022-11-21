## Load the packages ###########################################################
################################################################################

library(data.table)
library(tidyverse)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

### Import and prepare data ####################################################
################################################################################

# import raw data
df15h <- read.csv("data/raw/2015-hh.csv", header = TRUE) # 2015 household
df15p <- read.csv("data/raw/2015-per.csv", header = TRUE) # 2015 person
df1719h <- read.csv("data/raw/2017-19-hh.csv", header = TRUE) # 2017-19 household
df1719p <- read.csv("data/raw/2017-19-per.csv", header = TRUE) # 2017-19 person

# columns for final data frame
col <- c(
  "hhid", "personid", "survey_year", "age", "gender", "employment",
  "worker", "student", "education", "smartphone", "hhsize", "numadults",
  "numchildren", "numworkers", "hh_income", "res_type",
  "res_factors_walk", "res_factors_transit", "res_factors_30min",
  "res_factors_school", "res_factors_hwy", "vehicle_count",
  "license", "transit_freq", "bike_freq", "walk_freq", "carshare_freq",
  "rideshare_freq", "hours_work", "flex_worktime", "commute_freq",
  "telecommute_freq", "night_shift", "commute_mode",
  "av_interest_nodriver", "av_interest_backupdriver",
  "av_interest_commutesov", "av_interest_commutehov", "av_interest_own",
  "av_interest_carshare", "av_interest_short", "av_concern_safeequip",
  "av_concern_legal", "av_concern_safeveh", "av_concern_react",
  "av_concern_perform"
)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

### Working on 2015 data #######################################################
################################################################################

# merge household and person data
df15 <- merge(df15p, df15h, by = "hhid")

# add a column of survey year
df15$survey_year <- 2015

# remove observations of age less than 18 years
df15 <- df15[df15$age > 4, ]

# age
df15$age <- factor(df15$age,
  levels = c(5:12),
  labels = c(
    "18 to 24 years", "25 to 44 years", "25 to 44 years",
    "45 to 64 years", "45 to 64 years", "65 years and over",
    "65 years and over", "65 years and over"
  )
)

# gender
df15$gender <- factor(df15$gender,
  levels = c(1, 2),
  labels = c("male", "female")
)

# employment
df15$employment <- factor(df15$employment,
  levels = c(1:7),
  labels = c("employed full time", "employed part time", rep("others", 5))
)

# worker
df15$worker <- factor(df15$worker, levels = c(0, 1), labels = c("no", "yes"))

# student
df15$student <- factor(df15$student,
  levels = c(1:4),
  labels = c("no", rep("yes", 3))
)

# education
df15$education <- factor(df15$education,
  levels = c(1:7),
  labels = c(
    rep("no college degree", 5),
    "undergraducate degree",
    "graduate or higher degree"
  )
)

# smartphone
df15$smartphone <- factor(df15$smartphone,
  levels = c(3:1),
  labels = c(rep("no", 2), "yes")
)

# income
setnames(df15, "hh_income_broad", "hh_income")
df15$hh_income <- factor(df15$hh_income,
  levels = c(1:5),
  labels = c(
    "under $25,000", "$25,000-$49,999",
    "$50,000-$74,999", "$75,000-$99,999",
    "$100,000 or more"
  )
)

# residence type
df15$res_type <- factor(df15$res_type,
  levels = c(1:8),
  labels = c(
    "single-family house", "townhouse",
    "building with 3 or fewer apartments",
    "building with 3 or fewer apartments",
    "building with 4 or more apartments",
    "others", "others", "others"
  )
)

# driving license
df15$license <- factor(df15$license, levels = c(2, 1), labels = c("no", "yes"))

# transit, bike, and walk frequency
df15 <- df15 %>%
  mutate_at(c("transit_freq", "bike_freq", "walk_freq"), function(x) 8 - x)

# add car share and ride share frequency as NA
df15$carshare_freq <- as.numeric(NA)
df15$rideshare_freq <- as.numeric(NA)

# hours work
df15$hours_work <- factor(df15$hours_work,
  levels = c(1:6),
  labels = c(
    "more than 50 hours", "41-50 hours",
    "31-40 hours", "21-30 hours",
    "11-20 hours", "10 hours or fewer"
  )
)

# flexible work time
setnames(df15, "benefits_flextime", "flex_worktime")
df15$flex_worktime <- factor(df15$flex_worktime,
  levels = c(1:3),
  labels = c("no", rep("yes", 2))
)

# commute and telecommute frequency
df15 <- df15 %>%
  mutate_at(
    c("commute_freq", "telecommute_freq"),
    function(x) {
      (factor(x,
        levels = c(1:10),
        labels = c(
          "6-7 days/week", "5 days per week",
          "4 days per week", "3 days per week",
          "2 days per week", "1 day per week",
          "a few times per month",
          "less than monthly", "never",
          "never"
        )
      ))
    }
  )

# night shift work
df15$night_shift <- factor(df15$night_shift,
  levels = c(1:2),
  labels = c("no", "yes")
)

# commute mode
df15$commute_mode <- factor(df15$commute_mode,
  levels = c(1:17),
  labels = c(
    "drive alone", "drive/ride with other household members",
    "drive/ride with strangers", rep("others", 2), "bike", "walk",
    "bus", "train", rep("others", 3), "13 taxi and Uber/Lift", rep("others", 4)
  )
)

# av interests and concerns
av_var <- c(
  "av_interest_nodriver", "av_interest_backupdriver",
  "av_interest_commutesov", "av_interest_commutehov",
  "av_interest_own", "av_interest_carshare",
  "av_interest_short", "av_concern_safeequip",
  "av_concern_legal", "av_concern_safeveh",
  "av_concern_react", "av_concern_perform"
)
df15 <- df15 %>%
  mutate_at(av_var, function(x) ifelse(x == 6, NA, x)) %>%
  mutate_at(av_var, function(x) 6 - x)

# keep required columns only
df15 <- subset(df15, select = col)

# remove intermediate files
rm(df15h)
rm(df15p)
rm(var)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

### Working on 2017-19 data ####################################################
################################################################################

# merge 2017-19 household and person data
df1719 <- merge(df1719p, df1719h, by = "hhid")

# person id
setnames(df1719, "person_id", "personid")

# survey year
setnames(df1719, "survey_year.x", "survey_year")

# remove observations with age less than 18 years
df1719 <- df1719 %>%
  filter(age != "Under 5 years old" & age != "5-11 years" &
    age != "12-15 years" & age != "16-17 years")

# age
df1719 <- df1719 %>%
  mutate(age = recode_factor(age,
    "18-24 years" = "18 to 24 years",
    "25-34 years" = "25 to 44 years",
    "35-44 years" = "25 to 44 years",
    "45-54 years" = "45 to 64 years",
    "55-64 years" = "45 to 64 years",
    "65-74 years" = "65 years and over",
    "75-84 years" = "65 years and over",
    "85 or years older" = "65 years and over"
  ))

# gender
df1719$gender <- factor(df1719$gender,
  levels = c("Male", "Female"),
  labels = c("male", "female")
)

# employment
df1719$employment <- factor(df1719$employment,
  levels = c(
    "Employed full time (35+ hours/week, paid)",
    "Employed part time (fewer than 35 hours/week, paid)",
    "Homemaker",
    "Not currently employed",
    "Retired",
    "Self-employed",
    "Unpaid volunteer or intern"
  ),
  labels = c(
    "employed full time",
    "employed part time",
    rep("others", 5)
  )
)

# worker
df1719$worker <- factor(df1719$worker,
  levels = c("No jobs", "1+ job(s) (including part-time)"),
  labels = c("no", "yes")
)

# student
df1719$student <- factor(df1719$student,
  levels = c("No, not a student", "Full-time student", "Part-time student"),
  labels = c("no", "yes", "yes")
)

# education
df1719$education <- factor(df1719$education,
  levels = c(
    "Less than high school",
    "High school graduate",
    "Associates degree",
    "Some college",
    "Vocational/technical training",
    "Bachelor degree",
    "Graduate/post-graduate degree"
  ),
  labels = c(
    rep("no college degree", 5),
    "undergraducate degree",
    "graduate or higher degree"
  )
)

# smartphone
setnames(df1719, "smartphone_type", "smartphone")
df1719$smartphone <- factor(df1719$smartphone,
  levels = c(
    "No, does not have a smartphone",
    "Yes, has a blackberry",
    "Yes, has a Windows phone",
    "Yes, has an Android phone",
    "Yes, has an Apple iPhone",
    "Yes, has other type of smartphone"
  ),
  labels = c("no", rep("yes", 5))
)

# household size
df1719$hhsize <- recode(df1719$hhsize,
  "1 person" = 1, "2 people" = 2, "3 people" = 3,
  "4 people" = 4, "5 people" = 5, "6 people" = 6,
  "7 people" = 7, "8 people" = 8, "9 people" = 9
)

# income
setnames(df1719, "hhincome_broad", "hh_income")
df1719$hh_income <- factor(df1719$hh_income,
  levels = c(
    "Under $25,000", "$25,000-$49,999", "$50,000-$74,999",
    "$75,000-$99,999", "$100,000 or more"
  ),
  labels = c(
    "under $25,000", "$25,000-$49,999", "$50,000-$74,999",
    "$75,000-$99,999", "$100,000 or more"
  )
)

# residence type
df1719$res_type <- factor(df1719$res_type,
  levels = c(
    "Single-family house (detached house)",
    "Townhouse (attached house)",
    "Building with 3 or fewer apartments/condos",
    "Building with 4 or more apartments/condos",
    "Dorm or institutional housing",
    "Mobile home/trailer",
    "Other (including boat, RV, van, etc.)"
  ),
  labels = c(
    "single-family house", "townhouse", "building with 3 or fewer apartments",
    "building with 4 or more apartments", rep("others", 3)
  )
)

# res_factors
df1719 <- df1719 %>%
  mutate_at(
    c(
      "res_factors_walk", "res_factors_transit", "res_factors_30min",
      "res_factors_school", "res_factors_hwy"
    ),
    function(x) {
      (recode(x,
        "Very unimportant" = 1, "Somewhat unimportant" = 2,
        "Neither or N/A" = 3, "Somewhat important" = 4,
        "Very important" = 5
      ))
    }
  )

# vehicle_count
df1719$vehicle_count <- recode(df1719$vehicle_count,
  "0 (no vehicles)" = 0, "1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5,
  "6" = 6, "7" = 7, "8" = 8, "9" = 9, "10 or more vehicles" = 10
)

# license
df1719$license <- factor(df1719$license,
  levels = c(
    "No, does not have a license or permit",
    "Yes, has a learnerâ€™s permit",
    "Yes, has an intermediate or unrestricted license"
  ),
  labels = c("no", "yes", "yes")
)

# transit_freq, bike_freq, walk_freq, carshare_freq, rideshare_freq
setnames(df1719, "mode_freq_1", "transit_freq")
setnames(df1719, "mode_freq_2", "bike_freq")
setnames(df1719, "mode_freq_3", "walk_freq")
setnames(df1719, "mode_freq_4", "carshare_freq")
setnames(df1719, "mode_freq_5", "rideshare_freq")
var <- c(
  "transit_freq", "bike_freq", "walk_freq",
  "carshare_freq", "rideshare_freq"
)
df1719 <- df1719 %>%
  mutate_at(var, function(x) {
    as.numeric(factor(x,
      levels = c(
        "I never do this",
        "I do this, but not in the past 30 days",
        "1-3 times in the past 30 days",
        "1 day/week", "2-4 days/week",
        "5 days/week", "6-7 days/week"
      ),
      labels = c(1:7)
    ))
  })

# hours_work
df1719$hours_work <- factor(df1719$hours_work,
  levels = c(
    "More than 50 hours", "41â€“50 hours", "31-40 hours",
    "35-40 hours", "31-34 hours", "21â€“30 hours",
    "11â€“20 hours", "10 hours or fewer"
  ),
  labels = c(
    "more than 50 hours", "41-50 hours", "31-40 hours",
    "31-40 hours", "31-40 hours", "21-30 hours",
    "11-20 hours", "10 hours or fewer"
  )
)

# flexible work time
setnames(df1719, "benefits_1", "flex_worktime")
df1719$flex_worktime <- factor(df1719$flex_worktime,
  levels = c("Not offered", "Offered, but I don't use", "Offered, and I use"),
  labels = c("no", "yes", "yes")
)

# commute frequency
df1719$commute_freq <- factor(df1719$commute_freq,
  levels = c(
    "6-7 days a week", "5 days a week", "4 days a week",
    "3 days a week", "2 days a week", "1 day a week",
    "A few times per month", "Less than monthly"
  ),
  labels = c(
    "6-7 days/week", "5 days per week", "4 days per week",
    "3 days per week", "2 days per week", "1 day per week",
    "a few times per month", "less than monthly"
  )
)

# telecommute frequency
df1719$telecommute_freq <- factor(df1719$telecommute_freq,
  levels = c(
    "6-7 days a week", "5 days a week", "4 days a week",
    "3 days a week", "2 days a week", "1 day a week",
    "A few times per month", "Less than monthly",
    "Never"
  ),
  labels = c(
    "6-7 days/week", "5 days per week", "4 days per week",
    "3 days per week", "2 days per week", "1 day per week",
    "a few times per month", "less than monthly",
    "never"
  )
)

# night shift work
df1719$night_shift <- NA

# commute mode
df1719$commute_mode <- factor(df1719$commute_mode,
  levels = c(
    "Drive alone",
    "Carpool ONLY with other household members",
    "Carpool with other people not in household (may also include household members)",
    "Bicycle or e-bike",
    "Walk, jog, or wheelchair",
    "Taxi (e.g., Yellow Cab)",
    "Other hired service (Uber, Lyft, or other smartphone-app car service)",
    "Bus (public transit)",
    "Urban rail (Link light rail, monorail)",
    "Commuter rail (Sounder, Amtrak)",
    "Airplane or helicopter",
    "Ferry or water taxi",
    "Motorcycle/moped",
    "Motorcycle/moped/scooter",
    "Other (e.g. skateboard)",
    "Paratransit",
    "Private bus or shuttle",
    "Scooter or e-scooter (e.g., Lime, Bird, Razor)",
    "Streetcar", "Vanpool"
  ),
  labels = c(
    "drive alone",
    "drive/ride with other household members",
    "drive/ride with strangers",
    "bike", "walk",
    rep("taxi and Uber/Lift", 2),
    "bus",
    rep("train", 2),
    rep("others", 10)
  )
)


# av interests and concerns
setnames(df1719, "av_interest_1", "av_interest_nodriver")
setnames(df1719, "av_interest_2", "av_interest_backupdriver")
setnames(df1719, "av_interest_3", "av_interest_commutesov")
setnames(df1719, "av_interest_4", "av_interest_commutehov")
setnames(df1719, "av_interest_5", "av_interest_own")
setnames(df1719, "av_interest_6", "av_interest_carshare")
setnames(df1719, "av_interest_7", "av_interest_short")
setnames(df1719, "av_concern_1", "av_concern_safeequip")
setnames(df1719, "av_concern_2", "av_concern_legal")
setnames(df1719, "av_concern_3", "av_concern_safeveh")
setnames(df1719, "av_concern_4", "av_concern_react")
setnames(df1719, "av_concern_5", "av_concern_perform")
df1719 <- df1719 %>%
  mutate_at(av_var, function(x) {
    (recode(x,
      "Not at all interested" = 1,
      "Somewhat uninterested" = 2,
      "Neutral" = 3,
      "Somewhat interested" = 4,
      "Very interested" = 5,
      "Not at all concerned" = 1,
      "Somewhat unconcerned" = 2,
      "Somewhat concerned" = 4,
      "Very concerned" = 5
    ))
  })

# keep the required columns only
df1719 <- subset(df1719, select = col)

# remove intermediate files
rm(df1719h)
rm(df1719p)
rm(av_var)
rm(var)
rm(col)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

## Merge two data frames #######################################################
################################################################################

# merge
df <- bind_rows(df15, df1719)

# average av concern
df <- df %>%
  mutate(av_concern_average = rowMeans(cbind(
    av_concern_safeequip, av_concern_legal, av_concern_safeveh,
    av_concern_react, av_concern_perform
  )))

# survey_year
df$survey_year <- as.factor(df$survey_year)

# remove observations with NA in av interests
df <- df %>%
  filter_at(
    vars(
      av_interest_nodriver, av_interest_backupdriver,
      av_interest_own, av_interest_carshare, av_interest_short
    ),
    all_vars(!is.na(.))
  )

# remove intermediate files
rm(df15)
rm(df1719)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#

## Export data #################################################################
################################################################################

saveRDS(df, "data/intermediate/clean_data.rds")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
