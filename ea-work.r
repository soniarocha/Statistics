library(readr)
library(kableExtra)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(reshape2)
library(patchwork)
library(EnvStats)
library(qwraps2)
library(psych)
library(tidyverse)
library(corrplot)
library(RColorBrewer)
library(gridExtra)
library(formattable)
library("FactoMineR")
library(devtools)
library("factoextra")

dataset <- read_csv("~/Faculdade/2semestre/EA/heart_failure_clinical_records_dataset.csv")

attributes<-names(dataset)
description<-c(
  "Describes the age of the subjects in the dataset",
  "Decrease of red blood cells or hemoglobin (boolean); 0 if absent, 1 if present",
  "Level of the CPK enzyme in the blood (mcg/L)",
  "If the patient has diabetes (boolean); 0 if absent, 1 if present",
  "Percentage of blood leaving the heart at each contraction (percentage)",
  "If the patient has hypertension (boolean); 0 if absent, 1 if present",
  "Platelets in the blood (kiloplatelets/mL)",
  "Level of serum creatinine in the blood (mg/dL)",
  "Level of serum sodium in the blood (mEq/L)", 
  "Woman or man (binary); 0 if woman, 1 if man",
  "If the patient smokes or not (boolean); 0 if absent, 1 if present",
  "Follow up peroid (days)",
  "If the patient deceased during the follow-up period (boolean); 0 if no, 1 if yes.")
description_table<-as.data.frame(cbind(attributes,description))
description_table %>% kable() %>% kable_styling()

str(dataset)
dataset_t <- dataset
dataset <- dataset %>%
        mutate_at(c("anaemia", "diabetes", "high_blood_pressure", "sex", "smoking", "DEATH_EVENT"), as.factor)

#*******************************PLOTS****************************************#

#Frequency Of each variable:

anaemia_freq <- ggplot(dataset %>% group_by(anaemia) %>%summarise(counts = n()), aes(x = anaemia, y = counts)) +
  geom_bar(fill = "steelblue", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean()

diabetes_freq <- ggplot(dataset %>% group_by(diabetes) %>% summarise(counts = n()), aes(x = diabetes, y = counts)) +
  geom_bar(fill = "steelblue", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean()

highbloodp_freq <- ggplot(dataset %>% group_by(high_blood_pressure) %>% summarise(counts = n()), aes(x = high_blood_pressure, y = counts)) +
  geom_bar(fill = "steelblue", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean()

sex_freq <- ggplot(dataset %>% group_by(sex) %>% summarise(counts = n()), aes(x = sex, y = counts)) +
  geom_bar(fill = "steelblue", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean()

smoking_freq <- ggplot(dataset %>% group_by(smoking) %>% summarise(counts = n()), aes(x = smoking, y = counts)) +
  geom_bar(fill = "steelblue", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean()

death_freq <- ggplot(dataset %>% group_by(DEATH_EVENT) %>% summarise(counts = n()), aes(x = DEATH_EVENT, y = counts)) +
  geom_bar(fill = "steelblue", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) + 
  theme_pubclean()

((anaemia_freq + diabetes_freq+ highbloodp_freq) / (sex_freq + smoking_freq +death_freq)) +
  plot_annotation(title = "Frequency chart for each factor variable")

#Histograms:

age_hist <- gghistogram(dataset, x = "age", bins = 30, title ="Age",
            fill = "#0073C2FF", color = "#0073C2FF")

cpk_hist <- gghistogram(dataset, x = "creatinine_phosphokinase", bins = 30, title ="Creatinine Phosphokinase",
            fill = "#0073C2FF", color = "#0073C2FF")

ejecf_hist <- gghistogram(dataset, x = "ejection_fraction", bins = 30,  title ="Ejection Fraction",
            fill = "#0073C2FF", color = "#0073C2FF")

platelets_hist <- gghistogram(dataset, x = "platelets", bins = 30, title="Platelets",
            fill = "#0073C2FF", color = "#0073C2FF")

serumc_hist <- gghistogram(dataset, x = "serum_creatinine", bins = 30, title="Serum Creatinine",
            fill = "#0073C2FF", color = "#0073C2FF")

serums_hist <- gghistogram(dataset, x = "serum_sodium", bins = 30, title="Serum Sodium",
            fill = "#0073C2FF", color = "#0073C2FF")

time_hist <- gghistogram(dataset, x = "time", bins = 30, title="Time",
            fill = "#0073C2FF", color = "#0073C2FF")

((age_hist + cpk_hist+ ejecf_hist + platelets_hist) / (serumc_hist + serums_hist + time_hist)) +
  plot_annotation(title = "Histograms of each variable")


#Box-Plot:

age_box <- ggplot(dataset, aes(x=age)) + geom_boxplot(fill=rgb(0.1,0.1,0.7,0.5))
cpk_box <- ggplot(dataset, aes(x=creatinine_phosphokinase)) + geom_boxplot(fill=rgb(0.1,0.1,0.7,0.5))
ejecf_box <- ggplot(dataset, aes(x=ejection_fraction)) + geom_boxplot(fill=rgb(0.1,0.1,0.7,0.5))
platelets_box <- ggplot(dataset, aes(x=platelets)) + geom_boxplot(fill=rgb(0.1,0.1,0.7,0.5))
serumc_box <- ggplot(dataset, aes(x=serum_creatinine)) + geom_boxplot(fill=rgb(0.1,0.1,0.7,0.5))
serums_box <- ggplot(dataset, aes(x=serum_sodium)) + geom_boxplot(fill=rgb(0.1,0.1,0.7,0.5))
time_box <- ggplot(dataset, aes(x=time)) + geom_boxplot(fill=rgb(0.1,0.1,0.7,0.5))


((age_box + cpk_box+ ejecf_box + platelets_box) / (serumc_box + serums_box + time_box)) +
  plot_annotation(title = "Box-plots for each variable")


#checking the outliers
out_age <- boxplot.stats(dataset$age)$out # não tem outliers
out_cpk <- boxplot.stats(dataset$creatinine_phosphokinase)$out #tem muitos outliers
out_ejecf <- boxplot.stats(dataset$ejection_fraction)$out # tem poucos outilers
out_platelets <- boxplot.stats(dataset$platelets)$out #tem muitos outliers
out_creatinine <- boxplot.stats(dataset$serum_creatinine)$out #tem muitos outliers


#dataset com apenas as variáveis contínuas
cont_dataset <- dataset[,-c(2,4,6,10,11,13)]

#Position Dispersion and Shape of each Variable:
table_describe <- describe(cont_dataset, IQR=TRUE,quant=c(.1,.25,.75,.90))
coef <- cont_dataset %>% 
            gather("variable", "value") %>% 
            group_by(variable) %>% 
            summarize(
            coef_var = cv(value))

table_describe <- mutate(table_describe ,coef)
table_describe <- table_describe[,-c(1,19)]

table_describe %>% kable() %>% kable_styling()



corrplot(cor(cont_dataset), type = "upper",method = "square", order = "hclust",tl.col = "black", tl.srt = 45,addCoef.col = TRUE,number.cex = .7,
         col = brewer.pal(n = 8, name = "PuOr"))

corrplot(cor(cont_dataset), method = "number", tl.col = "black",p.mat = cor.mtest(cont_dataset)$p,sig.level = 0.05,col = brewer.pal(n = 8, name = "PuOr")) 

corrplot(cor(dataset_t), method = "number", tl.col = "black",p.mat = cor.mtest(dataset_t)$p,sig.level = 0.05,col = brewer.pal(n = 8, name = "PuOr")) 

#age has higher correlation with this factor : death
#creat_phosp has higher correlation with this factor : anaemia 
#eject_frac has higher correlation with this factors : sex and death
#platelets has low correlations with all factors
#serum_creat has higher correlation with this factor: death
#serum_sodio has higher correlation with this factor: death
#time has higher correlation with this factors : anaemia, high blood pressure, death

#Some Contingency Tables for categorical variables:
sex_diab <- as.data.frame.matrix(table(dataset$sex,dataset$diabetes))
colnames(sex_diab) <- c("absent diabetes","present diabetes")
rownames(sex_diab) <- c("woman","man")

death_smk <- as.data.frame.matrix(table(dataset$DEATH_EVENT,dataset$smoking))
colnames(death_smk) <- c("survived","dead")
rownames(death_smk) <- c("absent smoking","present smoking")
ana_hbp <- as.data.frame.matrix(table(dataset$anaemia,dataset$high_blood_pressure))
colnames(ana_hbp) <- c("absent anaemia","present anaemia")
rownames(ana_hbp) <- c("absent high blood pressure","present high blood pressure")

sex_diab %>% kable(caption = "Number of man and woman that have or not diabetes") %>% kable_styling() 
death_smk %>% kable(caption = "Death and smoking ratio") %>% kable_styling()
ana_hbp %>% kable(caption = "Anaemia and high blood pressure ratio") %>% kable_styling()

#Some Statistical tests based on the correlations of continues variables and categorical..
age_de <- ggplot(dataset, aes(x = age, fill = DEATH_EVENT)) + 
  geom_density(alpha = 0.50) +
  scale_fill_manual(values = c(rgb(0.1,0.1,0.7,0.5), rgb(204/255, 255/255, 230/255)),
                    name = "DEATH_EVENT",
                    labels = c("0 (no)", "1 (yes)")) +

  
  geom_vline(xintercept = median(filter(dataset, DEATH_EVENT == 0)$age), linetype="dotted", colour = rgb(0.1,0.1,0.7,0.5)) +
  geom_vline(xintercept = median(filter(dataset, DEATH_EVENT == 1)$age), linetype="dotted", colour = rgb(204/255, 255/255, 230/255)) +
  geom_vline(xintercept = mean(filter(dataset, DEATH_EVENT == 0)$age), linetype="twodash", colour = rgb(0.1,0.1,0.7,0.5)) +
  geom_vline(xintercept = mean(filter(dataset, DEATH_EVENT == 1)$age), linetype="twodash", colour = rgb(204/255, 255/255, 230/255)) +
  annotate(geom = "text",
           x = max(dataset$age)-10, y = 0.03,
           label = str_c("Survived median age: ", median(filter(dataset, DEATH_EVENT == 0)$age),
                         "\nDead median age: ", median(filter(dataset, DEATH_EVENT == 1)$age),
                         "\nSurvived mean age: ", formattable(mean(filter(dataset, DEATH_EVENT == 0)$age),digits=2,format="f"),
                         "\nDead mean age: ", formattable(mean(filter(dataset, DEATH_EVENT == 1)$age),digits=2,format="f")),
                          size = 3)+
                          
  
  labs(title = "Statistical test age and DEATH_EVENT") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "right", legend.direction = "vertical")
#########################################################################################################
creat_ana <- ggplot(dataset, aes(x = creatinine_phosphokinase, fill = anaemia)) + 
  geom_density(alpha = 0.50) +
  scale_fill_manual(values = c(rgb(0.1,0.1,0.7,0.5), rgb(204/255, 255/255, 230/255)),
                    name = "anaemia",
                    labels = c("0 (Absent)", "1 (Presence)")) +
  
  
  geom_vline(xintercept = median(filter(dataset, anaemia == 0)$creatinine_phosphokinase), linetype="dotted", colour = rgb(0.1,0.1,0.7,0.5)) +
  geom_vline(xintercept = median(filter(dataset, anaemia == 1)$creatinine_phosphokinase), linetype="dotted", colour = rgb(204/255, 255/255, 230/255)) +
  geom_vline(xintercept = mean(filter(dataset, anaemia == 0)$creatinine_phosphokinase), linetype="twodash", colour = rgb(0.1,0.1,0.7,0.5)) +
  geom_vline(xintercept = mean(filter(dataset, anaemia == 1)$creatinine_phosphokinase), linetype="twodash", colour = rgb(204/255, 255/255, 230/255)) +
  annotate(geom = "text",
           x = max(dataset$creatinine_phosphokinase)-1900, y = 0.0015,
           label = str_c("Absent anaemia median creat_phosp: ", median(filter(dataset, anaemia == 0)$creatinine_phosphokinase),
                         "\nPresence anaemia median creat_phosp: ", median(filter(dataset, anaemia == 1)$creatinine_phosphokinase),
                         "\nAbsent anaemia mean creat_phosp: ", formattable(mean(filter(dataset, anaemia == 0)$creatinine_phosphokinase),digits=2,format="f"),
                         "\nPresence anaemia mean creat_phosp: ", formattable(mean(filter(dataset, anaemia == 1)$creatinine_phosphokinase),digits=2,format="f")),
                          size = 3) +
  
  
  labs(title = "Statistical test creatinine phosphokinase and anaemia") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "right", legend.direction = "vertical")
#########################################################################################################
eject_de <- ggplot(dataset, aes(x = ejection_fraction, fill = DEATH_EVENT)) + 
  geom_density(alpha = 0.50) +
  scale_fill_manual(values = c(rgb(0.1,0.1,0.7,0.5), rgb(204/255, 255/255, 230/255)),
                    name = "DEATH_EVENT",
                    labels = c("0 (no)", "1 (yes)")) +

  geom_vline(xintercept = median(filter(dataset, DEATH_EVENT == 0)$ejection_fraction), linetype="dotted", colour = rgb(0.1,0.1,0.7,0.5)) +
  geom_vline(xintercept = median(filter(dataset, DEATH_EVENT == 1)$ejection_fraction), linetype="dotted", colour = rgb(204/255, 255/255, 230/255)) +
  geom_vline(xintercept = mean(filter(dataset, DEATH_EVENT == 0)$ejection_fraction), linetype="twodash", colour = rgb(0.1,0.1,0.7,0.5)) +
  geom_vline(xintercept = mean(filter(dataset, DEATH_EVENT == 1)$ejection_fraction), linetype="twodash", colour = rgb(204/255, 255/255, 230/255)) +
  annotate(geom = "text",
           x = max(dataset$ejection_fraction)-10, y = 0.03,
           label = str_c("Survived median ejectfrac: ", median(filter(dataset, DEATH_EVENT == 0)$ejection_fraction),
                         "\nDead median ejectfrac: ", median(filter(dataset, DEATH_EVENT == 1)$ejection_fraction),
                         "\nSurvived mean ejectfrac: ", formattable(mean(filter(dataset, DEATH_EVENT == 0)$ejection_fraction),digits=2,format="f"),
                         "\nDead mean ejectfrac: ", formattable(mean(filter(dataset, DEATH_EVENT == 1)$ejection_fraction),digits=2,format="f")),
           size = 3)+
  
  
  labs(title = "Statistical test ejection_fraction and DEATH_EVENT") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "right", legend.direction = "vertical")
#########################################################################################################
eject_sex <- ggplot(dataset, aes(x = ejection_fraction, fill = sex)) + 
  geom_density(alpha = 0.50) +
  scale_fill_manual(values = c(rgb(0.1,0.1,0.7,0.5), rgb(204/255, 255/255, 230/255)),
                    name = "sex",
                    labels = c("0 (woman)", "1 (man)")) +
  
  geom_vline(xintercept = median(filter(dataset, sex == 0)$ejection_fraction), linetype="dotted", colour = rgb(0.1,0.1,0.7,0.5)) +
  geom_vline(xintercept = median(filter(dataset, sex == 1)$ejection_fraction), linetype="dotted", colour = rgb(204/255, 255/255, 230/255)) +
  geom_vline(xintercept = mean(filter(dataset, sex == 0)$ejection_fraction), linetype="twodash", colour = rgb(0.1,0.1,0.7,0.5)) +
  geom_vline(xintercept = mean(filter(dataset, sex == 1)$ejection_fraction), linetype="twodash", colour = rgb(204/255, 255/255, 230/255)) +
  annotate(geom = "text",
           x = max(dataset$ejection_fraction)-15, y = 0.03,
           label = str_c("woman ejectfrac median: ", median(filter(dataset, DEATH_EVENT == 0)$ejection_fraction),
                         "\nman ejectfrac median: ", median(filter(dataset, DEATH_EVENT == 1)$ejection_fraction),
                         "\nwoman ejectfrac mean: ", formattable(mean(filter(dataset, DEATH_EVENT == 0)$ejection_fraction),digits=2,format="f"),
                         "\nman ejectfrac mean: ", formattable(mean(filter(dataset, DEATH_EVENT == 1)$ejection_fraction),digits=2,format="f")),
           size = 3)+
  
  
  labs(title = "Statistical test ejection_fraction and sex") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "right", legend.direction = "vertical")
#########################################################################################################
((age_de) / (creat_ana))
((eject_de) / (eject_sex))
#########################################################################################################
serumc_de <- ggplot(dataset, aes(x = serum_creatinine, fill = DEATH_EVENT)) + 
  geom_density(alpha = 0.50) +
  scale_fill_manual(values = c(rgb(0.1,0.1,0.7,0.5), rgb(204/255, 255/255, 230/255)),
                    name = "DEATH_EVENT",
                    labels = c("0 (no)", "1 (yes)")) +
  
  geom_vline(xintercept = median(filter(dataset, DEATH_EVENT == 0)$serum_creatinine), linetype="dotted", colour = rgb(0.1,0.1,0.7,0.5)) +
  geom_vline(xintercept = median(filter(dataset, DEATH_EVENT == 1)$serum_creatinine), linetype="dotted", colour = rgb(204/255, 255/255, 230/255)) +
  geom_vline(xintercept = mean(filter(dataset, DEATH_EVENT == 0)$serum_creatinine), linetype="twodash", colour = rgb(0.1,0.1,0.7,0.5)) +
  geom_vline(xintercept = mean(filter(dataset, DEATH_EVENT == 1)$serum_creatinine), linetype="twodash", colour = rgb(204/255, 255/255, 230/255)) +
  annotate(geom = "text",
           x = max(dataset$serum_creatinine)-1.6, y = 1.25,
           label = str_c("Survived median serum_c: ", median(filter(dataset, DEATH_EVENT == 0)$serum_creatinine),
                         "\nDead median serum_c: ", median(filter(dataset, DEATH_EVENT == 1)$serum_creatinine),
                         "\nSurvived mean serum_c: ",  formattable(mean(filter(dataset, DEATH_EVENT == 0)$serum_creatinine),digits=2,format="f"),
                         "\nDead mean serum_c: ",  formattable(mean(filter(dataset, DEATH_EVENT == 1)$serum_creatinine),digits=2,format="f")),
           size = 3)+
  
  
  labs(title = "Statistical test serum_creatinine and DEATH_EVENT") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "right", legend.direction = "vertical")
#########################################################################################################
serums_de <- ggplot(dataset, aes(x = serum_sodium, fill = DEATH_EVENT)) + 
  geom_density(alpha = 0.50) +
  scale_fill_manual(values = c(rgb(0.1,0.1,0.7,0.5), rgb(204/255, 255/255, 230/255)),
                    name = "DEATH_EVENT",
                    labels = c("0 (no)", "1 (yes)")) +
  
  geom_vline(xintercept = median(filter(dataset, DEATH_EVENT == 0)$serum_sodium), linetype="dotted", colour = rgb(0.1,0.1,0.7,0.5)) +
  geom_vline(xintercept = median(filter(dataset, DEATH_EVENT == 1)$serum_sodium), linetype="dotted", colour = rgb(204/255, 255/255, 230/255)) +
  geom_vline(xintercept = mean(filter(dataset, DEATH_EVENT == 0)$serum_sodium), linetype="twodash", colour = rgb(0.1,0.1,0.7,0.5)) +
  geom_vline(xintercept = mean(filter(dataset, DEATH_EVENT == 1)$serum_sodium), linetype="twodash", colour = rgb(204/255, 255/255, 230/255)) +
  annotate(geom = "text",
           x = max(dataset$serum_sodium)-3, y = 0.1,
           label = str_c("Survived median serum_s: ", median(filter(dataset, DEATH_EVENT == 0)$serum_sodium),
                         "\nDead median serum_s: ", median(filter(dataset, DEATH_EVENT == 1)$serum_sodium),
                         "\nSurvived mean serum_s: ", formattable(mean(filter(dataset, DEATH_EVENT == 0)$serum_sodium),digits=2,format="f"),
                         "\nDead mean serum_s: ", formattable(mean(filter(dataset, DEATH_EVENT == 1)$serum_sodium),digits=2,format="f")),
           size = 3)+
  
  
  labs(title = "Statistical test serum_sodium and DEATH_EVENT") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "right", legend.direction = "vertical")
#########################################################################################################
time_de <- ggplot(dataset, aes(x = time, fill = DEATH_EVENT)) + 
  geom_density(alpha = 0.50) +
  scale_fill_manual(values = c(rgb(0.1,0.1,0.7,0.5), rgb(204/255, 255/255, 230/255)),
                    name = "DEATH_EVENT",
                    labels = c("0 (no)", "1 (yes)")) +
  
  geom_vline(xintercept = median(filter(dataset, DEATH_EVENT == 0)$time), linetype="dotted", colour = rgb(0.1,0.1,0.7,0.5)) +
  geom_vline(xintercept = median(filter(dataset, DEATH_EVENT == 1)$time), linetype="dotted", colour = rgb(204/255, 255/255, 230/255)) +
  geom_vline(xintercept = mean(filter(dataset, DEATH_EVENT == 0)$time), linetype="twodash", colour = rgb(0.1,0.1,0.7,0.5)) +
  geom_vline(xintercept = mean(filter(dataset, DEATH_EVENT == 1)$time), linetype="twodash", colour = rgb(204/255, 255/255, 230/255)) +
  annotate(geom = "text",
           x = max(dataset$time)-50, y = 0.006,
           label = str_c("Survived median time: ", median(filter(dataset, DEATH_EVENT == 0)$time),
                         "\nDead median time:", median(filter(dataset, DEATH_EVENT == 1)$time),
                         "\nSurvived mean time: ", formattable(mean(filter(dataset, DEATH_EVENT == 0)$time),digits=2,format="f"),
                         "\nDead mean time: ", formattable(mean(filter(dataset, DEATH_EVENT == 1)$time),digits=2,format="f")),
           size = 2.5)+
  
  
  labs(title = "Statistical test time and DEATH_EVENT") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "right", legend.direction = "vertical")
#########################################################################################################
time_ana <- ggplot(dataset, aes(x = time, fill = anaemia)) + 
  geom_density(alpha = 0.50) +
  scale_fill_manual(values = c(rgb(0.1,0.1,0.7,0.5), rgb(204/255, 255/255, 230/255)),
                    name = "anaemia",
                    labels = c("0 (Absent)", "1 (Presence)")) +
  
  geom_vline(xintercept = median(filter(dataset, anaemia == 0)$time), linetype="dotted", colour = rgb(0.1,0.1,0.7,0.5)) +
  geom_vline(xintercept = median(filter(dataset, anaemia == 1)$time), linetype="dotted", colour = rgb(204/255, 255/255, 230/255)) +
  geom_vline(xintercept = mean(filter(dataset, anaemia == 0)$time), linetype="twodash", colour = rgb(0.1,0.1,0.7,0.5)) +
  geom_vline(xintercept = mean(filter(dataset, anaemia == 1)$time), linetype="twodash", colour = rgb(204/255, 255/255, 230/255)) +
  annotate(geom = "text",
           x = max(dataset$time)-50, y = 0.004,
           label = str_c("Absent anaemia median time: ", median(filter(dataset, anaemia == 0)$time),
                         "\nAbsent anaemia median time: ", median(filter(dataset, anaemia == 1)$time),
                         "\nAbsent anaemia median time: ", formattable(mean(filter(dataset, anaemia == 0)$time),digits=2,format="f"),
                         "\nAbsent anaemia median time: ", formattable(mean(filter(dataset, anaemia == 1)$time),digits=2,format="f")),
           size = 2.5)+
  
  
  labs(title = "Statistical test time and anaemia") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "right", legend.direction = "vertical")
#########################################################################################################
time_hbp <- ggplot(dataset, aes(x = time, fill = high_blood_pressure)) + 
  geom_density(alpha = 0.50) +
  scale_fill_manual(values = c(rgb(0.1,0.1,0.7,0.5), rgb(204/255, 255/255, 230/255)),
                    name = "high_blood_pressure",
                    labels = c("0 (Absent)", "1 (Presence)")) +
  
  geom_vline(xintercept = median(filter(dataset, high_blood_pressure == 0)$time), linetype="dotted", colour = rgb(0.1,0.1,0.7,0.5)) +
  geom_vline(xintercept = median(filter(dataset, high_blood_pressure == 1)$time), linetype="dotted", colour = rgb(204/255, 255/255, 230/255)) +
  geom_vline(xintercept = mean(filter(dataset, high_blood_pressure == 0)$time), linetype="twodash", colour = rgb(0.1,0.1,0.7,0.5)) +
  geom_vline(xintercept = mean(filter(dataset, high_blood_pressure == 1)$time), linetype="twodash", colour = rgb(204/255, 255/255, 230/255)) +
  annotate(geom = "text",
           x = max(dataset$time)-50, y = 0.004,
           label = str_c("Absent hbp median time: ", median(filter(dataset, high_blood_pressure == 0)$time),
                         "\nAbsent hbp median time: ", median(filter(dataset, high_blood_pressure == 1)$time),
                         "\nAbsent hbp median time: ", formattable(mean(filter(dataset, high_blood_pressure == 0)$time),digits=2,format="f"),
                         "\nAbsent hbp median time: ",  formattable(mean(filter(dataset, high_blood_pressure == 1)$time),digits=2,format="f")),
           size = 2.5)+
  
  
  labs(title = "Statistical test time and high_blood_pressure") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "right", legend.direction = "vertical")
#########################################################################################################
((serumc_de) / (serums_de))
((time_de) / (time_ana) / (time_hbp))


######################################--------PCA--------################################################


PCA_data <-PCA(cont_dataset,graph = FALSE)
summary(PCA_data)
get_eig(PCA_data)#Eigen values
PCA_data$svd$V # Eigen vectors


######################### Results for Variables
res.var <- get_pca_var(PCA_data)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 
# Results for individuals
res.ind <- get_pca_ind(PCA_data)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation 
#########################


fviz_eig(PCA_data, addlabels=TRUE) #Plot the eigenvalues/variances against the number of dimensions

#Graph of variables DIM 1 e 2 
fviz_pca_var(PCA_data, 
             col.var = "contrib", # Color by contributions to the PC
             title = "Graph of variables contribution",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#Graph of variables DIM 1 e 3 
fviz_pca_var(PCA_data, 
             axes= c(1,3),
             col.var = "contrib", # Color by contributions to the PC
             title = "Graph of variables contribution",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#Graph of variables DIM 1 e 4 
fviz_pca_var(PCA_data, 
             axes= c(1,4),
             col.var = "contrib", # Color by contributions to the PC
             title = "Graph of variables contribution",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
#Graph of variables DIM 1 e 5
fviz_pca_var(PCA_data, 
             axes= c(1,5),
             col.var = "contrib", # Color by contributions to the PC
             title = "Graph of variables contribution",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#Biplot of individuals and variables 1 2
fviz_pca_biplot(PCA_data,
             axes= c(1,2),
             label="var",
             title = "Biplot of individuals and variables contribution",
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE    # Avoid text overlapping
)



fviz_cos2(PCA_data, choice = "var", axes = 1:2)
######################################--------PCA--------################################################


