#get data
library(readr)
read_csv("C:/Users/.../Documents/training/r/imputations_regression/reg_R.csv")
reg_R = read_csv("reg_R.csv")
View(reg_R)
tail(reg_R, n = 10)

#TEST 0 - MASTER

#create dataset with missing values
library(dplyr)
setAside1 = filter(reg_R,test1 == "pick")
View(setAside1)
#remaining records
remainer1 = filter(reg_R,test1 == "dont pick")
View(remainer1)

# ACCEPTANCES - building a model based on 23 best variables: last quarter data, region and type of LA
predictiveModel1acc = lm(acc ~acc_prev + region + type, data = remainer1)
summary(predictiveModel1acc)

#predict new ACCEPTANCES data based on the above model
test1acc = predict(predictiveModel1acc, newdata = setAside1)
write_csv((select(setAside1, code)), "setAside1.csv")
write_csv((data.frame(test1acc)), "test1acc.csv")

# TA - building a model based on 23 best variables: last quarter data, region and type of LA
predictiveModel1TA = lm(tach ~tach_prev + region + type, data = remainer1)
summary(predictiveModel1TA)

#predict new TA data based on the above model
test1TA = predict(predictiveModel1TA, newdata = setAside1)
View(test1TA)
write_csv((data.frame(test1TA)), "test1TA.csv")

#get data for acceptances
library(readr)
read_csv("C:/Users/.../Documents/training/r/imputations_regression/corr_acc_R.csv")
corr_acc = read_csv("corr_acc_R.csv")
View(corr_acc)
tail(corr_acc, n = 10)

#draw plot: projected 1
library(ggplot2)
qplot(actual, projected, data = corr_acc) +
  facet_wrap(. ~test) +
  ggtitle("acceptances - projected versus actual") +
  lims(x = c(0,200), y = c(0,200)) +
  geom_smooth(method = "lm")