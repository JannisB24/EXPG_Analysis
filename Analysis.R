library(dplyr)

module_data <- read.csv("/Users/Jannis/Dokumente - Drive/Goethe-Uni/6. Semester/EXPG/Data.csv", sep=";", header=TRUE)
View(module_data)

mean_control <- mean(module_data$module_matching.1.player.Points[module_data$module_matching.1.group.treatment == 0])
mean_treatment <- mean(module_data$module_matching.1.player.Points[module_data$module_matching.1.group.treatment == 1])
mean_difference <- mean_treatment - mean_control

n_control <- sum(module_data$module_matching.1.group.treatment == 0)
n_treatment <- sum(module_data$module_matching.1.group.treatment == 1)

#calculation of %-share of 1st/2nd/3rd/4th preferred prio
c_1_pc <- (sum(module_data$WPM1_Prio[module_data$module_matching.1.group.treatment == 0] == 1) 
           + sum(module_data$WPM2_Prio[module_data$module_matching.1.group.treatment == 0] == 1)) / n_control * 2 * 100
c_2_pc <- (sum(module_data$WPM1_Prio[module_data$module_matching.1.group.treatment == 0] == 2) 
           + sum(module_data$WPM2_Prio[module_data$module_matching.1.group.treatment == 0] == 2)) / n_control * 2 * 100
c_3_pc <- (sum(module_data$WPM1_Prio[module_data$module_matching.1.group.treatment == 0] == 3) 
           + sum(module_data$WPM2_Prio[module_data$module_matching.1.group.treatment == 0] == 3)) / n_control * 2 * 100
c_4_pc <- (sum(module_data$WPM1_Prio[module_data$module_matching.1.group.treatment == 0] == 4) 
           + sum(module_data$WPM2_Prio[module_data$module_matching.1.group.treatment == 0] == 4)) / n_control * 2 * 100
c_5_pc <- (sum(module_data$WPM1_Prio[module_data$module_matching.1.group.treatment == 0] == 5) 
           + sum(module_data$WPM2_Prio[module_data$module_matching.1.group.treatment == 0] == 5)) / n_control * 2 * 100

t_1_pc <- (sum(module_data$WPM1_Prio[module_data$module_matching.1.group.treatment == 1] == 1) 
           + sum(module_data$WPM2_Prio[module_data$module_matching.1.group.treatment == 1] == 1)) / n_treatment * 2 * 100
t_2_pc <- (sum(module_data$WPM1_Prio[module_data$module_matching.1.group.treatment == 1] == 2) 
           + sum(module_data$WPM2_Prio[module_data$module_matching.1.group.treatment == 1] == 2)) / n_treatment * 2 * 100
t_3_pc <- (sum(module_data$WPM1_Prio[module_data$module_matching.1.group.treatment == 1] == 3) 
           + sum(module_data$WPM2_Prio[module_data$module_matching.1.group.treatment == 1] == 3)) / n_treatment * 2 * 100
t_4_pc <- (sum(module_data$WPM1_Prio[module_data$module_matching.1.group.treatment == 1] == 4) 
           + sum(module_data$WPM2_Prio[module_data$module_matching.1.group.treatment == 1] == 4)) / n_treatment * 2 * 100
t_5_pc <- (sum(module_data$WPM1_Prio[module_data$module_matching.1.group.treatment == 1] == 5) 
           + sum(module_data$WPM2_Prio[module_data$module_matching.1.group.treatment == 1] == 5)) / n_treatment * 2 * 100

#calculation of switching individuals

#calculation of questionnaire
pro_t_1better <- sum(module_data$module_matching.1.player.WhichIsBetter[module_data$module_matching.1.group.treatment == 1] == 2) / n_treatment * 100
pro_t_2control <- sum(module_data$module_matching.1.player.Control[module_data$module_matching.1.group.treatment == 1] == 2) / n_treatment * 100
pro_t_3random <- sum(module_data$module_matching.1.player.RandomChoice[module_data$module_matching.1.group.treatment == 1] == 2) / n_treatment * 100
pro_t_4comfortable <- sum(module_data$module_matching.1.player.Comfortable[module_data$module_matching.1.group.treatment == 1] == 2) / n_treatment * 100
pro_t_final <- sum(module_data$module_matching.1.player.FinalChoice[module_data$module_matching.1.group.treatment == 1] == 2) / n_treatment * 100

#regression with points
points_lm <- lm(module_matching.1.player.Points ~ module_matching.1.group.treatment, data = module_data)
summary(points_lm)