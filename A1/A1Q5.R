data <- read.csv("covid.2023.csv")

x = mean(data$Days)

ppois(10, lambda=x) 

recovery_days <- 0:40
probability = dpois(recovery_days, lambda=x )

RecoveryDays =data$Days 
hist(RecoveryDays, freq = FALSE, breaks = 40, 
     main = "Poission distribution predicted number of days for COVID recovery",
     ylim = c(0,.12),
     xlab = "Probability")

lines(recovery_days, probability,col="red",lwd = 3)


legend("topright", legend = c("Data", "Predicted"),
       lwd = 3, col = c("grey", "red"))


ans =ppois(80/5, lambda=x) - ppois(60/5, lambda=x)

dbinom(3, 5, 0.5900841) + dbinom(4, 5, 0.5900841) + dbinom(5, 5, 0.5900841)


ppois(14, lambda=x, lower = FALSE) 