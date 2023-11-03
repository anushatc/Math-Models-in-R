library(pwr)
library(dplyr)
library(effsize)
library(comprehenr)
library(readr)
library(ggplot2)
library(tidyr)
df <- read.csv('advertising_campaign_AdVert.csv')

# An advertising company (AdVert) ran an A/B test to measure the effectiveness of their advertising
# campaign. The data collected during the A/B test is given in the file advertising_campaign_AdVert.csv. It
# costs AdVert an average of $9 per 1000 impressions to run the campaign. It has estimated the worth of a
# converting user to be about $40.
# Description of the columns in the data file:
#   Each row in the CSV file dataset (advertising_campaign_AdVert.csv) represents a uniquely identified
# user in the ad campaign. For each user, the following four columns are provided:
#   user_id: Unique identifier of the user.
# test: Whether the user was exposed to advertising or was in the control group. 1 if the user was exposed
# to the read ad, 0 if the user was in the control group and was shown a Public Service Announcement
# (PSA).
# converted: Whether the user converted. 1 if the user bought the product (a handbag) during the
# campaign, 0 if not.
# tot_impr: The total number of ad impressions the user encountered. For users in the control group this
# counts the number of times they encountered the PSA. For exposed users it counts the number of times
# they were shown the real ad.

# 1) Was the advertising campaign effective? Did additional consumers convert as a result of the ad campaign? Use an appropriate statistical test to support your conclusion. (5 points)

copy<- df
copy$test <- as.factor(copy$test)
xtabs(~test + converted, data = copy)
xtabs(~test + converted, data = copy)[,2:1]
prop.test(xtabs(~test + converted, data = copy)[,2:1])
#The p-value is less than .05. We can conclude that the campaign had a significant impact on conversion rates.

# 2) What is the minimum sample size of the control group (test=0) required to have a power of 0.8? Is the statistical test performed in the above question well powered? (5 points)

prop1 = 0.01785411 # eg. test = 0
prop2 = 0.02554656 # ef. test = 1
effect_size = ES.h(p1=prop1, p2=prop2)

pwr.2p.test(h=effect_size, sig.level = 0.05, power = .80)
df %>%
  group_by(test) %>%
  summarise(sample_size = n())
#The test is well powered, as the sample sizes exceed the minimum sample size. 

# 3) Based on the total impressions (tot_impr) the customers received, the randomization done in the above A/B test is valid. Justify the statement using an appropriate test. (5 points)

test_group <- df[df$test == 1, ]
control_group <- df[df$test == 0, ]

t.test(test_group$tot_impr, control_group$tot_impr)

df%>% group_by(test)%>%summarise(mean(tot_impr))
#I used a two sample t test to check the randomization. The p-value was .8, indicating that the randomization is valid and there is not a difference in mean total impressions among the groups. Additionally, the mean tot_impr was the same for both groups


#4) How much more money would AdVert make by running the campaign (excluding advertising costs)? (5 points)

pop<- 420 + 23104 + 14423 + 550154
40*(prop2*pop - prop1*pop)
# $180957.50. This was calculated by applying both conversion rates to the entire population, subtracting those from test 0 from test 1 to account for users who would have converted regardless of the ad. Then, I multiplied by 40$. 

#5) What was the cost of the ad campaign? (5 points)

advertising_costs<- sum(df$tot_impr)*9/1000
# $131374.6. This is total impressions from the campaign * 9$ per 1000 impressions

# 6) Calculate the Return on Investment (ROI) of the campaign. Was the campaign profitable? (5 points)

total_revenue <- 40*(14423-prop1*(14423+550154))
profit = total_revenue - advertising_costs
(profit / advertising_costs)*100
# 32.23%%
# Yes, the campaign was profitable as ROI is positive. 

# 7) What was the opportunity cost of including a control group, i.e., how much more could have AdVert made by not having a control group at all? (5 point)

control<-420+23104
40*(prop2*control-420)
#This was calculated by applying the conversion rate from test 1 to the control group, then subtracting the converts and multiplying by 40.

# Given is a dataset from an observational study on the prevalence of side effects due to medication
# (medication_side_effects.csv). The study was NOT a randomized controlled trial (RCT) and it is known
# that:
#   i) Medication is more likely to be given to younger patients.
# ii) Younger patients are at higher risk of having side effects due to the medication.
# Description of the columns in the data file:
#   Each row in the CSV file dataset (medication_side_effects.csv) represents a unique patient. For each
# patient, the following three columns are provided:
#   Age<18: Flag indicating whether a patient is below 18. 1 if below 18, 0 otherwise.
# Medication: Flag indicating whether the patient was given the medication. 1 if given, 0 otherwise.
# Medication_SideEffects: Flag indicating whether the patient showed some side effects of the
# medication. 1 if yes, 0 otherwise

data<-read.csv('medication_side_effects.csv')

#8) What is the naïve Average Treatment Effect (ATE) of medication on side effects (ignore Age<18 column)? (5 points)

prop_side_effects_medication <- mean(data$Medication_SideEffects[data$Medication == 1])
prop_side_effects_no_medication <- mean(data$Medication_SideEffects[data$Medication == 0])
prop_side_effects_medication - prop_side_effects_no_medication

# 9) What is the marginal causal effect of medication on side effects, considering the Age<18
# attribute as well? Use the standardized mean (weighted average) technique or the mean
# potential outcome (E[Y] = E[Y1] – E[Y0]) framework to answer this question. (10 points)

data%>%filter(Medication_SideEffects ==0)%>% group_by(Age.18, Medication)%>%summarise(count = n())%>%pivot_wider(names_from = Medication, values_from = count)

data%>%filter(Medication_SideEffects ==1)%>% group_by(Age.18, Medication)%>%summarise(count = n())%>%pivot_wider(names_from = Medication, values_from = count, values_fill=0)

total0=223+67+125+7
total1=35+51+50+122
total=total0+total1

7/(7+124) * total0/total+122/(50+122)*total1/total
