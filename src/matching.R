library(Matching)
library(ggplot2)
library(leaps)

y <- mathdumm$G3
z <- ifelse(mathdumm$higher_yes == 1, 0, 1)
x <- as.matrix(mathdumm %>% dplyr::select(-c('G3', 'higher_yes')))

matchest = Match( Y = y , Tr =  z, X = x)
summary(matchest)

matched <- data.frame(matchest$mdata)
matched$Tr <- ifelse(matched$Tr == 1, 0, 1)

model_match <- lm(Y ~ X.failures + X.absences + Tr, data = matched)
summary(model_match)
HC2 <- hccm(model_match , type = "hc2" )["Tr", "Tr"]
estimator <- summary(model_match)$coefficients["Tr", ]['Estimate']
c(estimator - HC2 * qt(0.975, df.residual(model_match)),
  estimator - HC2 * qt(0.025, df.residual(model_match)))


summary(lm(Y ~ Tr, data = matched))

diff = y[matchest$index.treated] - y[matchest$index.control]
summary(lm( diff ~ 1))$coef

c(2.1-1.65259*1.96, 2.1+1.65259*1.96)

library(ggplot2)

# create a subset of the matched data with the grade variable Y and treatment Tr
df_subset <- subset(matched, select = c(Y, Tr))

# plot the distributions of Y for the treatment and control groups using ggplot
ggplot(df_subset, aes(x = Y, fill = factor(Tr))) +
  geom_density(alpha = 0.5) +
  scale_fill_discrete(name = "Treatment",
                      labels = c("Control", "Treated")) +
  labs(title = "Distribution of Grades by Treatment Group") +
  xlab("Grade") + ylab("Density") +
  geom_vline(xintercept = mean_control, linetype = "dashed", color = "black", size = 0.5) +
  geom_vline(xintercept = mean_treated, linetype = "dashed", color = "red", size = 0.5) +
  annotate("text", x = mean_control, y = 0.07, label = sprintf("Mean: %.2f", mean_control), color = "black", vjust = -1) +
  annotate("text", x = mean_treated, y = 0.08, label = sprintf("Mean: %.2f", mean_treated), color = "red", vjust = -1) +
  ggtitle("Distribution of Grades for Treated and Control Students") + theme_bw()


 # calculate the difference in means between the treatment and control groups
mean_diff <- mean(df_subset$Y[df_subset$Tr == 1]) - mean(df_subset$Y[df_subset$Tr == 0])
cat("Difference in means:", round(mean_diff, 2), "\n")
