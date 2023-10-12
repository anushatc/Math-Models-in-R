install.packages('dplyr')
library(ggplot2)
library(dplyr)
library(tidyr)
install.packages('grf')
library(reshape)
install.packages('effectsize')
install.packages('effsize')
df <- read.csv('case_study_AB_testing_at_Vungle.csv')
#Q4
eRPM_difference <- df %>% group_by(Strategy) %>% summarise(eRPM = mean(eRPM))
3.459333-	3.348667
#Q6
a_erpm <- df$eRPM[df$Strategy == 'Vungle A']
b_erpm <- df$eRPM[df$Strategy == 'Vungle B']
t.test(b_erpm, a_erpm)
t.test(b_erpm, a_erpm, paired = TRUE)
#Q7
installs <- df %>% group_by(Strategy) %>% summarize(sum_installs = sum(Installs))
installsA <- installs$sum_installs[1]
installsB <- installs$sum_installs[2]

impressions <- df %>% group_by(Strategy) %>% summarise(sum_impressions = sum(Impressions))
impressionsA <- impressions$sum_impressions[1]
impressionsB <- impressions$sum_impressions[2]

conversionA <- installsA/impressionsA
conversionB <- installsB/impressionsB

#Q8
convprop <- matrix(c(installsA, installsB, impressionsA, impressionsB), ncol = 2)
prop.test(convprop)

#Q9
impressionsdf <- df %>% summarise(total_impressions = sum(Impressions))
impressions_sum <- impressionsdf$total_impressions[1]
impressions_sum/1000 *0.04173767
impressions_sum/1000 *0.17959566