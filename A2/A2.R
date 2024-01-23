
data <- read.csv("/Users/g/Desktop/Uni Sub/2023/Sem2/FIT2086 Data Analysis/A2/covid.19.ass2.2023.csv")
mean_recovery = mean(data$Recovery.Time)
std_dev = sqrt(var(data$Recovery.Time))
dof = length(data$Recovery.Time)

israeli_data <- read.csv("/Users/g/Desktop/Uni Sub/2023/Sem2/FIT2086 Data Analysis/A2/israeli.covid.19.ass2.2023.csv")
israeli_mean =  mean(israeli_data$Recovery.Time)
israeli_std_dev = sqrt(var(israeli_data$Recovery.Time))

