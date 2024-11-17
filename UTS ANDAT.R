#UTS ANDAT

#Read in your data
Data=read.csv2("depresi.CSV", sep=";", header = TRUE)
Data
#Check the packaging
str(Data)
nrow(Data)
ncol(Data)
summary(Data)
#Look at the top and the bottom of your data
head(Data)
head(Data$depression_diagnosis)
tail(Data$gad_score)
tail(Data$depression_diagnosis)
#Check your"n"s
head(table(Data$gad_score))
head(table(Data$depression_diagnosis))
#Validate with at least one external data source
library(dplyr)
summary(Data$gad_score)
quantile(Data$gad_score, seq(0,1, 0.1))
#Make a Plot
library(ggplot2)
ggplot(Data, aes(gad_score, depression_diagnosis)) + geom_col (colour="red")
#Comparing Model Expectations to Reality
cleaned_data <- data %>%
  filter(!is.na(gad_score),!is.na(depression_diagnosis))
# Melihat data setelah missing values dihilangkan
print(cleaned_data$depression_diagnosis)
print(cleaned_data$gad_score)

#berdasarkan diagnosis depresi dengan kurva normal overlay
hist <- ggplot(cleaned_data, aes(x = gad_score, fill = depression_diagnosis)) +
  geom_histogram(aes(y = ..density..), position = "identity", binwidth = 1, alpha = 0.5, color = "black") +
  stat_function(fun = dnorm, args = list(mean = mean(Data$gad_score, na.rm = TRUE), 
                                         sd = sd(Data$gad_score, na.rm = TRUE)), 
                color = "red", size = 1) +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "pink")) +
  labs(title = "Distribusi Tingkat kecemasan dengan Kurva Normal dan Diagnosis Depresi",
       x = "Tingkat Kecemasan",
       y = "Kepadatan",
       fill = "Diagnosis Depresi") +
  theme_minimal()
hist

