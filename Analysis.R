library(dplyr)
library(psych)

module_data <- read.csv("/Users/Jannis/Documents/EXPG_Analysis/Data.csv", sep=";", header=TRUE)
View(module_data)

mean_control <- mean(module_data$module_matching.1.player.Points[module_data$module_matching.1.group.treatment == 0])
mean_treatment <- mean(module_data$module_matching.1.player.Points[module_data$module_matching.1.group.treatment == 1])
mean_difference <- mean_treatment - mean_control

n_control <- sum(module_data$module_matching.1.group.treatment == 0)
n_treatment <- sum(module_data$module_matching.1.group.treatment == 1)

#calculation of %-share of 1st/2nd/3rd/4th preferred prio
c_1_pc <- (sum(module_data$WPM1_Prio[module_data$module_matching.1.group.treatment == 0] == 1) 
           + sum(module_data$WPM2_Prio[module_data$module_matching.1.group.treatment == 0] == 1)) / (n_control * 2) * 100
c_2_pc <- (sum(module_data$WPM1_Prio[module_data$module_matching.1.group.treatment == 0] == 2) 
           + sum(module_data$WPM2_Prio[module_data$module_matching.1.group.treatment == 0] == 2)) / (n_control * 2) * 100
c_3_pc <- (sum(module_data$WPM1_Prio[module_data$module_matching.1.group.treatment == 0] == 3) 
           + sum(module_data$WPM2_Prio[module_data$module_matching.1.group.treatment == 0] == 3)) / (n_control * 2) * 100
c_4_pc <- (sum(module_data$WPM1_Prio[module_data$module_matching.1.group.treatment == 0] == 4) 
           + sum(module_data$WPM2_Prio[module_data$module_matching.1.group.treatment == 0] == 4)) / (n_control * 2) * 100
c_5_pc <- (sum(module_data$WPM1_Prio[module_data$module_matching.1.group.treatment == 0] == 5) 
           + sum(module_data$WPM2_Prio[module_data$module_matching.1.group.treatment == 0] == 5)) / (n_control * 2) * 100
# 45.83 ; 29.17 ; 16.67 ; 4.17 ; 4.17

t_1_pc <- (sum(module_data$WPM1_Prio[module_data$module_matching.1.group.treatment == 1] == 1) 
           + sum(module_data$WPM2_Prio[module_data$module_matching.1.group.treatment == 1] == 1)) / (n_treatment * 2) * 100
t_2_pc <- (sum(module_data$WPM1_Prio[module_data$module_matching.1.group.treatment == 1] == 2) 
           + sum(module_data$WPM2_Prio[module_data$module_matching.1.group.treatment == 1] == 2)) / (n_treatment * 2) * 100
t_3_pc <- (sum(module_data$WPM1_Prio[module_data$module_matching.1.group.treatment == 1] == 3) 
           + sum(module_data$WPM2_Prio[module_data$module_matching.1.group.treatment == 1] == 3)) / (n_treatment * 2) * 100
t_4_pc <- (sum(module_data$WPM1_Prio[module_data$module_matching.1.group.treatment == 1] == 4) 
           + sum(module_data$WPM2_Prio[module_data$module_matching.1.group.treatment == 1] == 4)) / (n_treatment * 2) * 100
t_5_pc <- (sum(module_data$WPM1_Prio[module_data$module_matching.1.group.treatment == 1] == 5) 
           + sum(module_data$WPM2_Prio[module_data$module_matching.1.group.treatment == 1] == 5)) / (n_treatment * 2) * 100
# 50.00 ; 34.38 ; 3.13 ; 6.25 ; 6.25

#calculation of average ranks + distribution + significance
c_avg_rank <- sum(module_data$avg_rank[module_data$module_matching.1.group.treatment == 0]) / n_control
# 1.9167
t_avg_rank <- sum(module_data$avg_rank[module_data$module_matching.1.group.treatment == 1]) / n_treatment
# 1.8438 

describeBy(module_data$avg_rank, module_data$module_matching.1.group.treatment)
boxplot(avg_rank~module_matching.1.group.treatment, data=module_data)

wilcox.test(avg_rank~module_matching.1.group.treatment, data = module_data, exact = FALSE, correct = FALSE, alternative = "greater")
# w = 108.5, p-value = 0.2542

# variance of module choice
group_1_std <- (0.25*(sum(module_data$module_matching.1.player.prio_monetary_policy_1[module_data$group == 1])^2
                          + sum(module_data$module_matching.1.player.prio_brand_management_1[module_data$group == 1])^2
                          + sum(module_data$module_matching.1.player.prio_financial_analysis_1[module_data$group == 1])^2
                          + sum(module_data$module_matching.1.player.prio_history_of_economics_ethics_1[module_data$group == 1])^2
                          ) - 10^2)^0.5
group_2_std <- (0.25*(sum(module_data$module_matching.1.player.prio_monetary_policy_1[module_data$group == 2])^2
                          + sum(module_data$module_matching.1.player.prio_brand_management_1[module_data$group == 2])^2
                          + sum(module_data$module_matching.1.player.prio_financial_analysis_1[module_data$group == 2])^2
                          + sum(module_data$module_matching.1.player.prio_history_of_economics_ethics_1[module_data$group == 2])^2
                          ) - 10^2)^0.5
group_3_std <- (0.25*(sum(module_data$module_matching.1.player.prio_monetary_policy_1[module_data$group == 3])^2
                          + sum(module_data$module_matching.1.player.prio_brand_management_1[module_data$group == 3])^2
                          + sum(module_data$module_matching.1.player.prio_financial_analysis_1[module_data$group == 3])^2
                          + sum(module_data$module_matching.1.player.prio_history_of_economics_ethics_1[module_data$group == 3])^2
                          ) - 10^2)^0.5
group_4_std <- (0.25*(sum(module_data$module_matching.1.player.prio_monetary_policy_1[module_data$group == 4])^2
                          + sum(module_data$module_matching.1.player.prio_brand_management_1[module_data$group == 4])^2
                          + sum(module_data$module_matching.1.player.prio_financial_analysis_1[module_data$group == 4])^2
                          + sum(module_data$module_matching.1.player.prio_history_of_economics_ethics_1[module_data$group == 4])^2
                          ) - 10^2)^0.5
group_5_std <- (0.25*(sum(module_data$module_matching.1.player.prio_monetary_policy_1[module_data$group == 5])^2
                          + sum(module_data$module_matching.1.player.prio_brand_management_1[module_data$group == 5])^2
                          + sum(module_data$module_matching.1.player.prio_financial_analysis_1[module_data$group == 5])^2
                          + sum(module_data$module_matching.1.player.prio_history_of_economics_ethics_1[module_data$group == 5])^2
                          ) - 10^2)^0.5
group_6_std <- (0.25*(sum(module_data$module_matching.1.player.prio_monetary_policy_1[module_data$group == 6])^2
                          + sum(module_data$module_matching.1.player.prio_brand_management_1[module_data$group == 6])^2
                          + sum(module_data$module_matching.1.player.prio_financial_analysis_1[module_data$group == 6])^2
                          + sum(module_data$module_matching.1.player.prio_history_of_economics_ethics_1[module_data$group == 6])^2
                          ) - 10^2)^0.5
group_7_std <- (0.25*(sum(module_data$module_matching.1.player.prio_monetary_policy_1[module_data$group == 7])^2
                          + sum(module_data$module_matching.1.player.prio_brand_management_1[module_data$group == 7])^2
                          + sum(module_data$module_matching.1.player.prio_financial_analysis_1[module_data$group == 7])^2
                          + sum(module_data$module_matching.1.player.prio_history_of_economics_ethics_1[module_data$group == 7])^2
                          ) - 10^2)^0.5

c_avg_std <- (group_1_std + group_2_std + group_3_std) / 3
t_avg_std <- (group_4_std + group_5_std + group_6_std + group_7_std) / 4

group_1_rank_avg <- sum(module_data$avg_rank[module_data$group == 1]) / 4
group_2_rank_avg <- sum(module_data$avg_rank[module_data$group == 2]) / 4
group_3_rank_avg <- sum(module_data$avg_rank[module_data$group == 3]) / 4
group_4_rank_avg <- sum(module_data$avg_rank[module_data$group == 4]) / 4
group_5_rank_avg <- sum(module_data$avg_rank[module_data$group == 5]) / 4
group_6_rank_avg <- sum(module_data$avg_rank[module_data$group == 6]) / 4
group_7_rank_avg <- sum(module_data$avg_rank[module_data$group == 7]) / 4

module_variances_df <- data.frame(
                          std = c(group_1_std, group_2_std, group_3_std, group_4_std, group_5_std, group_6_std, group_7_std),
                          treatment = c(0, 0, 0, 1, 1, 1, 1))

wilcox.test(std~treatment, data = module_variances_df, exact = FALSE, correct = FALSE)
# W = 6.5 ; p-value = 0.8584

#calculation of switching individuals
c_pct_switch <- sum(module_data$delta_ges[module_data$module_matching.1.group.treatment == 0] > 0) / n_control
t_pct_switch <- sum(module_data$delta_ges[module_data$module_matching.1.group.treatment == 1] > 0) / n_treatment
wilcox.test(delta_ges~module_matching.1.group.treatment, data = module_data, exact = FALSE, correct = FALSE)
# W = 98, p-value = 0.9087
t.test(module_data$delta_ges[module_data$module_matching.1.group.treatment == 1])
# t = 2.6112, p-value = 0.0197

#calculation of questionnaire
pro_t_1better <- sum(module_data$module_matching.1.player.WhichIsBetter[module_data$module_matching.1.group.treatment == 1] == 2) / n_treatment * 100
pro_t_2control <- sum(module_data$module_matching.1.player.Control[module_data$module_matching.1.group.treatment == 1] == 2) / n_treatment * 100
pro_t_3random <- sum(module_data$module_matching.1.player.RandomChoice[module_data$module_matching.1.group.treatment == 1] == 2) / n_treatment * 100
pro_t_4comfortable <- sum(module_data$module_matching.1.player.Comfortable[module_data$module_matching.1.group.treatment == 1] == 2) / n_treatment * 100
pro_t_final <- sum(module_data$module_matching.1.player.FinalChoice[module_data$module_matching.1.group.treatment == 1] == 2) / n_treatment * 100
# 56.25 ; 75.00 ; 12.50 ; 56.25 ; 43.75

#test if better performers liked treatment more
avg_final_0 <- mean(module_data$avg_rank[module_data$module_matching.1.group.treatment == 1 & module_data$module_matching.1.player.FinalChoice == 1])
avg_final_1 <- mean(module_data$avg_rank[module_data$module_matching.1.group.treatment == 1 & module_data$module_matching.1.player.FinalChoice == 2])
avg_final_delta <- avg_final_0 - avg_final_1
wilcox.test(module_data$avg_rank[module_data$module_matching.1.group.treatment == 1]~module_data$module_matching.1.player.FinalChoice[module_data$module_matching.1.group.treatment == 1], data = module_data, exact = FALSE, correct = FALSE, alternative = "greater")
# W = 39.5, p-value = 0.1511

#regression with points
points_lm <- lm(module_matching.1.player.Points ~ module_matching.1.group.treatment, data = module_data)
summary(points_lm)