data <- read.csv(file.choose())
dim(data)
head(data, 10)
tail(data, 10)
str(data)
table(data$offer_dropped)
(1024/1999)*100
summary(data)
plot(data$marital_status, xlab = "marital_status")
tab_marital<- table(data$marital_status)
tab_marital
prop.table(tab_marital)*100
plot(data$education_level, xlab = "education_level")
tab_education<- table(data$education_level)
tab_education
prop.table(tab_education)*100
plot(data$gender, xlab = "gender")
tab_gender<- table(data$gender)
tab_gender
prop.table(tab_gender)*100
plot(data$ distance_from_home, xlab = "distance_from_home")
tab_dist<- table(data$distance_from_home)
tab_dist
prop.table(tab_dist)*100
plot(data$sourcing_channel, xlab = "sourcing_channel")
tab_channel<- table(data$sourcing_channel)
tab_channel
prop.table(tab_channel)*100
plot(data$career_growth, xlab = "career_growth")
tab_growth<- table(data$career_growth)
tab_growth
prop.table(tab_growth)*100
plot(data$flexi_work, xlab = "flexi_work")
tab_flexi<- table(data$flexi_work)
tab_flexi
prop.table(tab_flexi)*100
plot(data$timely_communication, xlab =  "timely_communication")
tab_communication<- table(data$timely_communication)
tab_communication
prop.table(tab_communication)*100
hist(data$age)
tab_age<- table(data$age)
tab_age
prop.table(tab_age)*100
hist(data$percent_hike)
tab_percent_hike <- table(data$ percent_hike)
tab_percent_hike 
prop.table(tab_percent_hike)*100
hist(data$satisfaction_index)
tab_satisfaction_index <- table(data$satisfaction_index)
tab_satisfaction_index
prop.table(tab_satisfaction_index)*100
hist(data$total_rounds)
tab_total_rounds <- table(data$ total_rounds)
tab_total_rounds
prop.table(tab_total_rounds)*100
sum(is.na(data))
data$jhi <- data$total_experience/data$no_companies_worked
summary(data$jhi)
hist(data$jhi)
data$days_offered <- as.Date(data$date_offered,"%m/%d/%Y") - as.Date(data$date_1st_contact,"%m/%d/%Y")
data$days_offered<-as.numeric(data$days_offered)
class(data$days_offered)
hist(data$days_offered)
tab_days_offered_num <- table(data$days_offered)
tab_days_offered_num
prop.table(tab_days_offered_num)
data2<-data[c(-9,-10,-12,-15)]
names(data2)
plot(data2$marital_status, data2$offer_dropped, xlab ="marital_status" , ylab = "Offer Dropped")
tab_marital_status<- table(data2$offer_dropped, data2$marital_status)
tab_marital_status
prop.table(tab_marital_status, 2)*100
plot(data2$education_level, data2$offer_dropped, xlab = "education_level", ylab = "Offer Dropped")
tab_education_level<- table(data2$offer_dropped, data2$education_level)
tab_education_level
prop.table(tab_education_level, 2)
plot(data2$gender, data2$offer_dropped, xlab ="gender" , ylab = "Offer Dropped")
tab_gender_lev<- table(data2$offer_dropped, data2$gender)
tab_gender_lev
prop.table(tab_gender_lev, 2)
plot(data2$distance_from_home, data2$offer_dropped,  xlab = "distance_from_home", ylab = "Offer Dropped")
tab_dfh<- table(data2$offer_dropped, data2$distance_from_home)
tab_dfh
prop.table(tab_dfh, 2)
plot(data2$sourcing_channel, data2$offer_dropped, xlab = "sourcing channel", ylab = "Offer Dropped")
tab_sc<- table(data2$offer_dropped, data2$sourcing_channel)
tab_sc
prop.table(tab_sc, 2)
plot(data2$career_growth, data2$offer_dropped,  ylab = "Offer Dropped", xlab = "career_growth")
tab_career_growth<- table(data2$offer_dropped, data2$career_growth)
tab_career_growth
prop.table(tab_career_growth, 2)
plot(data2$flexi_work, data2$offer_dropped, xlab = "flexi_work", ylab = "Offer Dropped")
tab_flexi_work<- table(data2$offer_dropped, data2$flexi_work)
tab_flexi_work
prop.table(tab_flexi_work, 2)
plot(data2$timely_communication, data2$offer_dropped, xlab = "timely_communication", ylab = "Offer Dropped")
tab_timely_communication<- table(data2$offer_dropped, data2$timely_communication)
tab_timely_communication
prop.table(tab_timely_communication, 2)
boxplot(data2$age ~ data2$offer_dropped, xlab = "Offer Dropped", ylab = "Age",range=95)
means_age <- by(data2$age, data2$offer_dropped, mean)                        
means_age
med_age <- by(data2$age, data2$offer_dropped, median) 
med_age
boxplot(data2$percent_hike ~ data2$offer_dropped, xlab = "offer_dropped", ylab = "percent_hike",range=95)
mean_percent_hike <- by(data2$percent_hike, data2$offer_dropped, mean)                        
mean_percent_hike
med_percent_hike <- by(data2$percent_hike, data2$offer_dropped, median)                        
med_percent_hike
std_percent_hike <- by(data2$percent_hike, data2$offer_dropped, sd)                        
std_percent_hike
boxplot(data2$satisfaction_index ~ data2$offer_dropped, xlab = "offer_dropped", ylab = "satisfaction_index",range=95)
mean_satisfaction_index <- by(data2$satisfaction_index, data2$offer_dropped, mean)                        
mean_satisfaction_index
med_satisfaction_index <- by(data2$satisfaction_index, data2$offer_dropped, median)                        
med_satisfaction_index
std_satisfaction_index <- by(data2$satisfaction_index, data2$offer_dropped, sd)                        
std_satisfaction_index
boxplot(data2$total_rounds ~ data2$offer_dropped, xlab = "offer_dropped", ylab = "total_rounds",range=95)
mean_total_rounds <- by(data2$total_rounds, data2$offer_dropped, mean)                        
mean_total_rounds
med_total_rounds <- by(data2$total_rounds, data2$offer_dropped, median)                        
med_total_rounds
boxplot(data2$jhi ~ data2$offer_dropped, xlab = "offer_dropped", ylab = "jhi",range=95)
mean_jhi <- by(data2$jhi, data2$offer_dropped, mean)                        
mean_jhi
med_jhi <- by(data2$jhi, data2$offer_dropped, median)                        
med_jhi
std_jhi <- by(data2$jhi, data2$offer_dropped, sd)                        
std_jhi
boxplot(data2$days_offered ~ data2$offer_dropped, xlab = "offer_dropped", ylab = "days_offered",range=95)
mean_days_offered <- by(data2$days_offered, data2$offer_dropped, mean)                        
mean_days_offered
med_days_offered <- by(data2$days_offered, data2$offer_dropped, median)                        
med_days_offered
tab_marital_status<- table(data2$marital_status, data2$offer_dropped)
tab_marital_status
chisq.test(tab_marital_status)
tab_education_level<-table(data2$education_level, data2$offer_dropped)
tab_education_level
chisq.test(tab_education_level)
tab_gender<-table(data2$gender, data2$offer_dropped)
tab_gender
chisq.test(tab_gender)
tab_dfh<-table(data2$distance_from_home, data2$offer_dropped)
tab_dfh
chisq.test(tab_dfh)
tab_sourcing_channel<-table(data2$sourcing_channel, data2$offer_dropped)
chisq.test(tab_sourcing_channel)
tab_sourcing_channel
tab_career_growth<-table(data2$career_growth, data2$offer_dropped)
tab_career_growth
chisq.test(tab_career_growth)
tab_flexi_work<-table(data2$flexi_work, data2$offer_dropped)
tab_flexi_work
chisq.test(tab_flexi_work)
tab_timely_com<-table(data2$timely_communication, data2$timely_communication)
tab_timely_com
chisq.test(tab_timely_com)
names(data2)
data3<- data2[c(-2, -3, -4)]
names(data3)
table(data3$distance_from_home) #  > 20 kms will be our reference
table(data3$sourcing_channel)  # Job Portals will be our reference
data3$dfh_15<- ifelse(data3$distance_from_home == '<15 kms', 1, 0)
data3$dfh_15to20 <- ifelse(data3$distance_from_home == '15-20 kms', 1, 0)
data3$ch_com_web<- ifelse(data3$sourcing_channel == 'Company Website', 1, 0)
data3$ch_cons <- ifelse(data3$sourcing_channel == 'Consultants', 1, 0)
data3$ch_Int_ref <- ifelse(data3$sourcing_channel == 'Internal Referrals', 1, 0)
data3$ch_soc_med <- ifelse(data3$sourcing_channel == 'Social Media', 1, 0)
data3$career_growth_lateral<-ifelse(data3$career_growth == 'Lateral', 1, 0)
data3$flexi_work_yes<-ifelse(data3$flexi_work == 'Yes', 1, 0)
data3$tc_yes<-ifelse(data3$timely_communication == 'Yes', 1, 0)
names(data3)
data4<-data3[c(-3,-4, -7, -8, -9)]
class(data4$offer_dropped)
data4$offer_drop_num<-ifelse(data4$offer_dropped == "Yes", 1, 0)
class(data4$offer_drop_num)
str(data4)
data4<-data4[c(-5)]
data_num <- data4[,c("age", "percent_hike", "total_rounds", "satisfaction_index", "jhi", "days_offered", "dfh_15", "dfh_15to20",
                     "ch_com_web","ch_cons", "ch_Int_ref","ch_soc_med","career_growth_lateral","flexi_work_yes","tc_yes","offer_drop_num")]
library(Information)
IV <- create_infotables(data = data_num, y = "offer_drop_num")
IV
data5<-data4[c(-7,-10,-12)]
names(data5)
library(car)
vif(glm(offer_drop_num ~ ., family = binomial, data = data5))
data5 <-  data5[sample(nrow(data5)),] 
set.seed(567)
index <- sample(2, nrow(data5), replace = TRUE, prob = c(0.7, 0.3))
data5_train<- data5[index == 1,]
data5_test<- data5[index == 2,]
model_0 <- glm(offer_drop_num ~ . , family = binomial, data = data5_train)
summary(model_0)
model_1 <- glm(offer_drop_num ~ . -age , family = binomial, data = data5_train)
summary(model_1)
model_2 <- glm(offer_drop_num ~ . -jhi -age, family = binomial, data = data5_train)
summary(model_2)
model_3 <- glm(offer_drop_num ~ . -ch_com_web -jhi -age, family = binomial, data = data5_train)
summary(model_3)
model_4 <- glm(offer_drop_num ~ .  -total_rounds -ch_com_web -jhi -age, family=binomial, data=data5_train)
summary(model_4)
data5_train$pred<- predict(model_4, newdata=data5_train, type = "response")
data5_train$pred
class(data5_train)
library(pscl)
pR2(model_4)
library(generalhoslem)
logitgof(obs = data5_train$offer_drop_num, exp = fitted(model_4))
library(ROCR)
pred2 <- prediction(data5_train$pred,labels = data5_train$offer_drop_num)
class(pred2)

slotNames(pred2)

roc.perf = performance(pred2, measure = "tpr", x.measure = "fpr")
plot(roc.perf)

abline(a = 0, b = 1)

auc <- performance(pred2, measure = "auc")
auc
auc <- auc@y.values[[1]]
auc

acc.perf = performance(pred2, measure = "acc")

slotNames(acc.perf)

ind = which.max(slot(acc.perf, "y.values")[[1]] )

ind

acc = slot(acc.perf, "y.values")[[1]][ind]

cutoff = slot(acc.perf, "x.values")[[1]][ind]



print(c(accuracy = acc, cutoff = cutoff))

acc.perf$y.values
predcat47<- ifelse(data5_train$pred >0.5114, 1, 0)

accuracy<- table(predcat47, data5_train[,"offer_drop_num"])
diag(accuracy)
sum(diag(accuracy)) / sum(accuracy)
table(data5_train$offer_drop_num)
data5_test$pred = predict(model_4, newdata=data5_test, type = "response")
predtest47<- ifelse(data5_test$pred >0.5114, 1, 0)
accuracy<- table(predtest47, data5_test[,"offer_drop_num"])
sum(diag(accuracy)) / sum(accuracy)

