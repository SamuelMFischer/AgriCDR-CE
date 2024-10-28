# R.version #Java for 64 bit version required
# 
# install.packages("installr")
# library(installr)
# updateR()
setwd("C:/Users/fischesa/Documents")

library(apollo)
library(xlsx) 
library(readxl)
library(questionr)
library(support.CEs)
library(reshape2)
library(dplyr)
library(survival)
library(logitr)
library(ggplot2)
library(plotly)
library(stringr)
library(factoextra)
library(cluster)
library(spdep)
library(ape) 
library(here)
library(spatialreg)
library(sp)
library(modelsummary)
library(flextable)

#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####

### Loading respondent data 
database = read.csv("Y:/Home/fischesa/GONASIP/choice experimente/R/survey_final.csv",header=TRUE)

#remove non-completed surveys
database <- database[database$lastpage != 1, ] 
database <- database[!is.na(database$lastpage),] 

#remove unnecessary variables
database <- subset(database, select=-c(submitdate, startlanguage, CTEXT, Reminder, groupTime2723, groupTime2724, groupTime2718))

#make cell-content of CE numeric
database$C1 <- gsub("A", "", database$C1)
database$C2 <- gsub("A", "", database$C2)
database$C3 <- gsub("A", "", database$C3)
database$C4 <- gsub("A", "", database$C4)
database$C5 <- gsub("A", "", database$C5)
database$C6 <- gsub("A", "", database$C6)
database$C7 <- gsub("A", "", database$C7)
database$C8 <- gsub("A", "", database$C8)
database$C9 <- gsub("A", "", database$C9)
database$C10 <- gsub("A", "", database$C10)
database$C11 <- gsub("A", "", database$C11)
database$C12 <- gsub("A", "", database$C12)
database$C13 <- gsub("A", "", database$C13)
database$C14 <- gsub("A", "", database$C14)
database$C15 <- gsub("A", "", database$C15)
database$C16 <- gsub("A", "", database$C16)
database$C17 <- gsub("A", "", database$C17)
database$C18 <- gsub("A", "", database$C18)

#reshape dataset
data <- database
#write.xlsx(data, 'Y:/Home/fischesa/GONASIP/Choice experimente/R/data.xlsx')
database <- melt(database, id.vars="id", measure.vars=c("C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","C11","C12",
                                                        "C13","C14","C15","C16","C17","C18"))
colnames(database) <- c("ID","QES","choice")
database$QES <- gsub("C", "", database$QES)


#load design data
design <- read_excel("Y:/Home/fischesa/GONASIP/choice experimente/R/design_efficient_ohne.alg.xlsx", col_names = TRUE)

#add status quo alternative
design$alt3.climate <- rep(0,18)
design$alt3.waterq <- rep(0,18)
design$alt3.biodiversity <- rep(0,18)
design$alt3.flood <- rep(0,18)
design$alt3.soiler <- rep(40,18)
design$alt3.cost <- rep(0,18)

#data reshaping
design_alt1 <- design[,1:7]
design_alt1$ALT <- rep(1,18)
colnames(design_alt1) <- c("QES","climate","waterq","biodiversity","flood","soiler","cost","ALT")
design_alt2 <- design[,c(1,8:13)]
design_alt2$ALT <- rep(2,18)
colnames(design_alt2) <- c("QES","climate","waterq","biodiversity","flood","soiler","cost","ALT")
design_alt3 <- design[,c(1,14:19)]
design_alt3$ALT <- rep(3,18)
colnames(design_alt3) <- c("QES","climate","waterq","biodiversity","flood","soiler","cost","ALT")
design_long <- rbind(design_alt1,design_alt2,design_alt3)

#merge both datasets
database <- merge(database,design_long)

#rearrange data 
database <- reshape(database, v.names=c("climate","waterq","biodiversity","soiler","flood","cost"), timevar="ALT", 
                    idvar=c("QES","ID"), drop = NULL, direction="wide")
database <- arrange(.data=database,database$ID, .by_group = FALSE)
colnames(database) <- c("QES","ID","choice","alt1.climate","alt1.waterq","alt1.biodiversity","alt1.soiler","alt1.flood","alt1.cost",
                        "alt2.climate","alt2.waterq","alt2.biodiversity","alt2.soiler","alt2.flood","alt2.cost","alt3.climate",
                        "alt3.waterq","alt3.biodiversity","alt3.soiler","alt3.flood","alt3.cost")


#identify protest votes
#1. any respondent chose status quo option in all choice sets? yes, id=77 --> reason: necessity not seen and uncertainty of effects
#2. check for time needed to answer choice questions: calculate mean & median response times and identify outliers
time <- data[,c(1,74:120)]
time <- subset(time, select = -c(CYOptOutTime,groupTime2719,groupTime2720,groupTime2721,
                                groupTime2722))
time$CTEXTTimeMean <- round(mean(time$CTEXTTime, na.rm = TRUE),digits = 2)
time$CTEXTTimeDeviation <- time$CTEXTTime-time$CTEXTTimeMean
#boxplot
time_long <- melt(time, id.vars="id", measure.vars=c("CTEXTTime","C1Time","C2Time","C3Time","C4Time","C5Time","C6Time","C7Time",
                                                    "C8Time","C9Time","C10Time","C11Time","C12Time","C13Time","C14Time","C15Time",
                                                    "C16Time","C17Time","C18Time"))
boxplot(value ~ variable, time_long)
time_long %>% ggplot(aes(x=variable,y=value)) + geom_boxplot() +  xlab("Question") + ylab("Duration to answer (in seconds)") 
#=> no outliers to the downside --> check median response times as well

#calculate lowest median among response times for the CE part
threshold <- min(median(time$C2Time),
                median(time$C3Time),
                median(time$C4Time),
                median(time$C5Time),
                median(time$C6Time),
                median(time$C7Time),
                median(time$C8Time),
                median(time$C9Time),
                median(time$C10Time),
                median(time$C11Time),
                median(time$C12Time),
                median(time$C13Time),
                median(time$C14Time),
                median(time$C15Time),
                median(time$C16Time),
                median(time$C17Time),
                median(time$C18Time))
#identify respondents with below threshold response times
time$protest <- ifelse(time$C2Time<threshold &
                        time$C3Time<threshold &
                        time$C4Time<threshold &
                        time$C5Time<threshold &
                        time$C6Time<threshold &
                        time$C7Time<threshold &
                        time$C8Time<threshold &
                        time$C9Time<threshold &
                        time$C10Time<threshold &
                        time$C11Time<threshold &
                        time$C12Time<threshold &
                        time$C13Time<threshold &
                        time$C14Time<threshold &
                        time$C15Time<threshold &
                        time$C16Time<threshold &
                        time$C17Time<threshold &
                        time$C18Time<threshold,1,0)
#-> no respondents with consistently lower response times than median

#3. check comments in open questions -> id 64,74,139 possibly misunderstood something
#4. check E-PVQ questions for respondents who may have chosen same answer for all statements -> not the case
#-> how to handle 4 observations?
#exclude none

#how often did respondents go for the 500 euro-option?
chosen500.1 <- subset(database, choice==1 & alt1.cost==500)
chosen500.2 <- subset(database, choice==2 & alt2.cost==500)
#=> option was chosen in 9.1% ((136+238)/2106*2) of the situations it was shown to respondents

#create split sample variable
codes <- read_excel("Y:/Home/fischesa/GONASIP/choice experimente/codes overview.xlsx", col_names = TRUE) #load data with split sample variable
table(codes$`Split sample`) #relatively balanced
codes <- merge(data,codes,by.x="Code", by.y="Code survey")
codes <- codes[,-3:-120]
database_split <- merge(codes,database,by.x="id",by.y="ID")
data_split <- merge(database_split,data,by.x="id",by.y="id")
data_split <- data_split[,-6:-28]
data_split <- unique(data_split)
data_split.yes <- subset(data_split,`Split sample`=="yes")
data_split.no <- subset(data_split,`Split sample`=="no")
database_split <- database_split[,-6]
database_split$`Split sample` <- gsub("yes","1",database_split$`Split sample`)
database_split$`Split sample` <- gsub("no","0",database_split$`Split sample`)
database_split <- rename.variable(database_split, "Split sample", "split")
database_split <- rename.variable(database_split, "id", "ID")
database_split$split <- as.numeric(database_split$split)

#save database for later
database_simple <- database

#### DESCRIPTIVE STATISTICS                                      ####

#Age
summary(age)
ggplot(data, aes(Age) ) +  geom_histogram(binwidth = 1) + scale_x_continuous(breaks = seq(1935, 2015, by=5)) + 
  xlab("Year of birth") + ylab("Frequency")

#Education
table(education$Education)
education <- data[,c(1,71)]
ed <- ggplot(education, aes(x=Education, fill=Education) ) +  geom_bar() + xlab("Degree") + ylab("Frequency")
ed + scale_fill_discrete(name="Education groups", breaks=c("A1","A2","A3","A4","A5","A6"),
                         labels=c("No school", "Secondary school or comparable qualification without completed vocational training",
                                  "Secondary school or comparable qualification with completed vocational training", 
                                  "Secondary school without A-levels", 
                                  "General or university entrance qualification without a completed (technical) university degree", 
                                  "Completed (technical) university degree"))

#Income --> bereinigen um HH-Größe??
table(income$Income)
income <- data[,c(1,72)]
ic <- ggplot(income, aes(x=Income, fill=Income) ) +  geom_bar() + xlab("Income group") + ylab("Frequency")
ic + scale_fill_discrete(name="Income groups", breaks=c("A1","A2","A3","A4","A5","A6","A7"),
                         labels=c("Less than 1,000€ ", "1,000€ to less than 1,500€", "1,500€ to less than 2,000€", 
                                  "2,000€ to less than 2,500€", "2,500€ to less than 3,500€", "3,500€ to less than 5,000€",
                                  "5,000€ and more"))

#Gender
table(gender$Gender)
gender <- data[,c(1,65)]
gender$Gender <- gsub("A1", "Male", gender$Gender)
gender$Gender <- gsub("A2", "Female", gender$Gender)
ggplot(gender, aes(Gender)) +  geom_bar() + xlab("Gender") + ylab("Frequency")

#Household size
hh_size <- data[,c(1,67)]
table(hh_size$Household) #2 observations with value zero --> assumption they live alone --> recode to value 1
hh_size$Household <- gsub("0","1",hh_size$Household)
hh_size$Household <- as.numeric(hh_size$Household)
summary(hh_size$Household)
ggplot(hh_size, aes(Household) ) +  geom_bar() + xlab("Household size") + ylab("Frequency")

#Job (classification according to KldB 2020 Arbeitsagentur fuer Arbeit)
job <- data[,c(1,69,70)]
job$Job[job$Job.other. == "Rentner"] <- "A11"
job$Job[job$Job.other. == "Gastronomie / Öffentlicher Dienst"] <- "Others"
job$Job[job$Job.other. == "Öffentlicher Dienst"] <- "Others"
job$Job[job$Job.other. == "öffentlicher Dienst"] <- "Others" 
job$Job[job$Job.other. == "Öffentlicher Dienst - Sachbearbeiter "] <- "Others"
job$Job[job$Job.other. == "Bibliothek "] <- "A7"
job$Job[job$Job.other. == "gastro"] <- "A6"
job$Job[job$Job.other. == "Onlinebusiness"] <- "A6"
job$Job[job$Job.other. == "Handwerk anlagenbau"] <- "A3"
job$Job[job$Job.other. == "tierpflegerin"] <- "A1"
job$Job[job$Job == "-oth-"] <- "Others"
job$Job[job$Job.other. == "Sicherheitsdienst"] <- "A5"
job$Job[job$Job.other. == "Elternzeit "] <- "A11"
job$Job[job$Job.other. == "pflege- teilzeit"] <- "A8"
job$Job[job$Job.other. == "Dienstleistungen: Übersetzerin"] <- "A7"
job$Job[job$Job.other. == "Gesundheitswesen"] <- "A8"
job$Job[job$Job.other. == "öffentl. Dienst"] <- "Others"
job$Job[job$Job.other. == "Studentin"] <- "A11"
job$Job <- factor(job$Job,levels = c("A1","A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","Others"))
jo <- ggplot(job, aes(x=Job,fill=Job) ) +  geom_bar() + xlab("Job group") + ylab("Frequency") 
jo + scale_fill_discrete(name="Job groups", breaks=c("A1","A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","Others"),
                         labels=c("Agriculture, forestry, animal husbandry and horticulture", 
                                  "Raw material extraction, production and manufacturing", 
                                  "Construction, architecture, surveying and building technology", 
                                  "Natural science, geography and computer science", 
                                  "Transport, logistics, protection and security", 
                                  "Commercial services, trade in goods, distribution, hotel and tourism",
                                  "Company organization, accounting, law and administration",
                                  "Health, social affairs, teaching and education",
                                  "Linguistics, literature, humanities, social and economic sciences, media, art, culture and design",
                                  "Military","Not in employment","Others"))

#Choices in the CE
choices <- data[,c(1,8:25)]
choices_long <- melt(choices, id.vars = "id") 
colnames(choices_long) <- c("ID","Choice situation","Choice")
choices_long$`Choice situation` <- gsub("C", "", choices_long$`Choice situation`)
choices_long <- merge(choices_long,design)
choices_long <- choices_long[order(as.numeric(as.character(choices_long$`Choice situation`))), ]
choices_long$`Choice situation` <- factor(choices_long$`Choice situation`, levels=c("1","2","3","4","5","6","7","8","9","10","11",
                                                                                    "12","13","14","15","16","17","18"))
p <- ggplot(choices_long, aes(x=`Choice situation`,y=Choice)) +  geom_count() 
ggplotly(p) #interactive graph, but would be nice to have more information on the options selected and not selected
ggplot(choices_long, aes(x=`Choice situation`,y=Choice)) +  geom_count()

#E-PVQ
epvq <- data[,c(1,46:62)]
epvq_long <- melt(epvq, id.vars = "id")
epvq_long$variable <- gsub("MATRIX.SQ001.", "1", epvq_long$variable)
epvq_long$variable <- gsub("MATRIX.SQ002.", "2", epvq_long$variable)
epvq_long$variable <- gsub("MATRIX.SQ003.", "3", epvq_long$variable)
epvq_long$variable <- gsub("MATRIX.SQ004.", "4", epvq_long$variable)
epvq_long$variable <- gsub("MATRIX.SQ005.", "5", epvq_long$variable)
epvq_long$variable <- gsub("MATRIX.SQ006.", "6", epvq_long$variable)
epvq_long$variable <- gsub("MATRIX.SQ007.", "7", epvq_long$variable)
epvq_long$variable <- gsub("MATRIX.SQ008.", "8", epvq_long$variable)
epvq_long$variable <- gsub("MATRIX.SQ009.", "9", epvq_long$variable)
epvq_long$variable <- gsub("MATRIX.SQ010.", "10", epvq_long$variable)
epvq_long$variable <- gsub("MATRIX.SQ011.", "11", epvq_long$variable)
epvq_long$variable <- gsub("MATRIX.SQ012.", "12", epvq_long$variable)
epvq_long$variable <- gsub("MATRIX.SQ013.", "13", epvq_long$variable)
epvq_long$variable <- gsub("MATRIX.SQ014.", "14", epvq_long$variable)
epvq_long$variable <- gsub("MATRIX.SQ015.", "15", epvq_long$variable)
epvq_long$variable <- gsub("MATRIX.SQ016.", "16", epvq_long$variable)
epvq_long$variable <- gsub("MATRIX.SQ017.", "17", epvq_long$variable)
epvq_long$value <- gsub("A1", "1 (Totally not like me)", epvq_long$value)
epvq_long$value <- gsub("A2", "2", epvq_long$value)
epvq_long$value <- gsub("A3", "3", epvq_long$value)
epvq_long$value <- gsub("A4", "4", epvq_long$value)
epvq_long$value <- gsub("A5", "5", epvq_long$value)
epvq_long$value <- gsub("A6", "6", epvq_long$value)
epvq_long$value <- gsub("A7", "7 (Totally like me)", epvq_long$value)
epvq_long$variable <- factor(epvq_long$variable, levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17"))
a <- ggplot(epvq_long, aes(x=variable,y=value)) +  geom_count() + xlab ("Statement") + ylab("Answer")
ggplotly(a)
#look into the 4 categories of e-pvq
epvq_wide <- epvq
epvq_wide$MATRIX.SQ001. <- gsub("A","",epvq_wide$MATRIX.SQ001.)
epvq_wide$MATRIX.SQ002. <- gsub("A","",epvq_wide$MATRIX.SQ002.)
epvq_wide$MATRIX.SQ003. <- gsub("A","",epvq_wide$MATRIX.SQ003.)
epvq_wide$MATRIX.SQ004. <- gsub("A","",epvq_wide$MATRIX.SQ004.)
epvq_wide$MATRIX.SQ005. <- gsub("A","",epvq_wide$MATRIX.SQ005.)
epvq_wide$MATRIX.SQ006. <- gsub("A","",epvq_wide$MATRIX.SQ006.)
epvq_wide$MATRIX.SQ007. <- gsub("A","",epvq_wide$MATRIX.SQ007.)
epvq_wide$MATRIX.SQ008. <- gsub("A","",epvq_wide$MATRIX.SQ008.)
epvq_wide$MATRIX.SQ009. <- gsub("A","",epvq_wide$MATRIX.SQ009.)
epvq_wide$MATRIX.SQ010. <- gsub("A","",epvq_wide$MATRIX.SQ010.)
epvq_wide$MATRIX.SQ011. <- gsub("A","",epvq_wide$MATRIX.SQ011.)
epvq_wide$MATRIX.SQ012. <- gsub("A","",epvq_wide$MATRIX.SQ012.)
epvq_wide$MATRIX.SQ013. <- gsub("A","",epvq_wide$MATRIX.SQ013.)
epvq_wide$MATRIX.SQ014. <- gsub("A","",epvq_wide$MATRIX.SQ014.)
epvq_wide$MATRIX.SQ015. <- gsub("A","",epvq_wide$MATRIX.SQ015.)
epvq_wide$MATRIX.SQ016. <- gsub("A","",epvq_wide$MATRIX.SQ016.)
epvq_wide$MATRIX.SQ017. <- gsub("A","",epvq_wide$MATRIX.SQ017.)
epvq_wide$MATRIX.SQ001. <- as.numeric(epvq_wide$MATRIX.SQ001.)
epvq_wide$MATRIX.SQ002. <- as.numeric(epvq_wide$MATRIX.SQ002.)
epvq_wide$MATRIX.SQ003. <- as.numeric(epvq_wide$MATRIX.SQ003.)
epvq_wide$MATRIX.SQ004. <- as.numeric(epvq_wide$MATRIX.SQ004.)
epvq_wide$MATRIX.SQ005. <- as.numeric(epvq_wide$MATRIX.SQ005.)
epvq_wide$MATRIX.SQ006. <- as.numeric(epvq_wide$MATRIX.SQ006.)
epvq_wide$MATRIX.SQ007. <- as.numeric(epvq_wide$MATRIX.SQ007.)
epvq_wide$MATRIX.SQ008. <- as.numeric(epvq_wide$MATRIX.SQ008.)
epvq_wide$MATRIX.SQ009. <- as.numeric(epvq_wide$MATRIX.SQ009.)
epvq_wide$MATRIX.SQ010. <- as.numeric(epvq_wide$MATRIX.SQ010.)
epvq_wide$MATRIX.SQ011. <- as.numeric(epvq_wide$MATRIX.SQ011.)
epvq_wide$MATRIX.SQ012. <- as.numeric(epvq_wide$MATRIX.SQ012.)
epvq_wide$MATRIX.SQ013. <- as.numeric(epvq_wide$MATRIX.SQ013.)
epvq_wide$MATRIX.SQ014. <- as.numeric(epvq_wide$MATRIX.SQ014.)
epvq_wide$MATRIX.SQ015. <- as.numeric(epvq_wide$MATRIX.SQ015.)
epvq_wide$MATRIX.SQ016. <- as.numeric(epvq_wide$MATRIX.SQ016.)
epvq_wide$MATRIX.SQ017. <- as.numeric(epvq_wide$MATRIX.SQ017.)
epvq_wide$Biospheric_Mean <- (epvq_wide$MATRIX.SQ001. + epvq_wide$MATRIX.SQ006. + epvq_wide$MATRIX.SQ010. + epvq_wide$MATRIX.SQ017.)/4
epvq_wide$Hedonic_Mean <- (epvq_wide$MATRIX.SQ002. + epvq_wide$MATRIX.SQ014. + epvq_wide$MATRIX.SQ015.)/3
epvq_wide$Altruistic_Mean <- (epvq_wide$MATRIX.SQ003. + epvq_wide$MATRIX.SQ005. + epvq_wide$MATRIX.SQ008. + epvq_wide$MATRIX.SQ011. + 
                                epvq_wide$MATRIX.SQ013.)/5
epvq_wide$Egoistic_Mean <- (epvq_wide$MATRIX.SQ004. + epvq_wide$MATRIX.SQ007. + epvq_wide$MATRIX.SQ009. + epvq_wide$MATRIX.SQ012. + 
                              epvq_wide$MATRIX.SQ016.)/5
epvq_means <- epvq_wide[,c(1,19:22)]
epvq_means_new <- rename.variable(epvq_means,"Biospheric_Mean","Biospheric")
epvq_means_new <- rename.variable(epvq_means_new,"Hedonic_Mean","Hedonic")
epvq_means_new <- rename.variable(epvq_means_new,"Altruistic_Mean","Altruistic")
epvq_means_new <- rename.variable(epvq_means_new,"Egoistic_Mean","Egoistic")
epvq_means_new <- melt(epvq_means_new, id.vars="id")
boxplot(value ~ variable, epvq_means_new)
epvq_means_new %>% ggplot(aes(x=variable,y=value)) + geom_boxplot() +  xlab("E-PVQ category") + ylab("Average score per respondent") 

#enough information to answer choice task?
data$F1 <- gsub("A1", "Yes", data$F1)
data$F1 <- gsub("A2", "No", data$F1)
table(data$F1)/117

#which attributes were considered?
attributes_considered <- data[,c(1,30,32,34,36,38,40,42)]
colnames(attributes_considered) <- c("ID","All considered","Climate protection","Soil erosion","Water quality","Biodiversity","Flood protection",
                                     "Costs")
attributes_considered_long <- melt(attributes_considered, id.vars="ID", 
                                   measure.vars=c("All considered","Climate protection","Soil erosion","Water quality",
                                                  "Biodiversity","Flood protection","Costs"))
attributes_considered_long <- subset(attributes_considered_long, value != 0) 
ggplot(attributes_considered_long, aes(x=variable)) +  geom_bar() + xlab("Not considered attributes") + ylab("Frequency")
# check who might have chosen "all considered" AND at least one other option

#how sure when choosing an option in the CE?
data$F4 <- gsub("A1", "Very sure", data$F4)
data$F4 <- gsub("A2", "Rather sure", data$F4)
data$F4 <- gsub("A3", "Neither nor", data$F4)
data$F4 <- gsub("A4", "Rather unsure", data$F4)
data$F4 <- gsub("A5", "Very unsure", data$F4)
data$F4 <- factor(data$F4, levels=c("Very sure","Rather sure","Neither nor","Rather unsure","Very unsure"))
table(data$F4)/117

#how often in nature?
data$Nature <- gsub("A1", "Daily", data$Nature)
data$Nature <- gsub("A2", "From time to time", data$Nature)
data$Nature <- gsub("A3", "Rather rarely", data$Nature)
data$Nature <- gsub("A4", "Rarely", data$Nature)
data$Nature <- gsub("A5", "Never", data$Nature)
table(data$Nature)/117

#trust in science
data$TrustScience <- gsub("A1", "Very high", data$TrustScience)
data$TrustScience <- gsub("A2", "High", data$TrustScience)
data$TrustScience <- gsub("A3", "Medium", data$TrustScience)
data$TrustScience <- gsub("A4", "Low", data$TrustScience)
data$TrustScience <- gsub("A5", "Very low", data$TrustScience)
data$TrustScience <- factor(data$TrustScience, levels=c("Very high","High","Medium","Low","Very low"))
table(data$TrustScience)/117


#respondents' geographical distribution
#clean postal code variable 
data$PLZ <- str_pad(data$PLZ,5,pad="0")
data$PLZ <- as.factor(data$PLZ)
plz <- data[,c(1,68)]
write.csv(plz, file="Y:/Home/fischesa/GONASIP/choice experimente/R/plz.csv", row.names = FALSE) #for Sophia to work on

##SPLIT SAMPLE

#Age
ggplot(data_split, aes(x=Age, fill=`Split sample`)) +  geom_histogram(binwidth = 1, position='dodge') + 
  scale_x_continuous(breaks = seq(1935, 2015, by=5)) +  xlab("Year of birth") + ylab("Frequency")
summary(data_split.yes$Age)
summary(data_split.no$Age)
#wider range in control group, but besides pretty similar

#Education
ggplot(data_split, aes(x=Education, fill=`Split sample`)) +  geom_bar(position='dodge') + xlab("Degree") + ylab("Frequency")
table(data_split.yes$Education)/59
table(data_split.no$Education)/57
#high differences in education groups A2, A4 and A6 --> yes-group seems to have higher education

#Income
ggplot(data_split, aes(x=Income, fill=`Split sample`)) +  geom_bar(position='dodge') + xlab("Income group") + ylab("Frequency")
table(data_split.yes$Income)/59
table(data_split.no$Income)/57
#high differences in income groups A1, A2, A3 and esp. A6 --> treatment group has higher income

#Gender
data_split$Gender <- gsub("A1", "Male", data_split$Gender)
data_split$Gender <- gsub("A2", "Female", data_split$Gender)
ggplot(data_split, aes(x=Gender, fill=`Split sample`)) +  geom_bar(position='dodge') + xlab("Gender") + ylab("Frequency")
table(data_split.yes$Gender)/59
table(data_split.no$Gender)/57
#treatment group male dominated, control group female dominated

#Household size
table(data_split$Household) #2 observations with value zero --> assumption they live alone --> recode to value 1
data_split$Household <- gsub("0","1",data_split$Household)
ggplot(data_split, aes(x=Household, fill=`Split sample`)) +  geom_bar(position='dodge') + xlab("Household size") + ylab("Frequency")
table(data_split.yes$Household)
table(data_split.no$Household)
#difference in two-person household representation, rest similar

#Job (classification according to KldB 2020 Arbeitsagentur fuer Arbeit)
data_split$Job[data_split$Job.other. == "Rentner"] <- "A11"
data_split$Job[data_split$Job.other. == "Gastronomie / Öffentlicher Dienst"] <- "Others"
data_split$Job[data_split$Job.other. == "Öffentlicher Dienst"] <- "Others"
data_split$Job[data_split$Job.other. == "öffentlicher Dienst"] <- "Others" 
data_split$Job[data_split$Job.other. == "Öffentlicher Dienst - Sachbearbeiter "] <- "Others"
data_split$Job[data_split$Job.other. == "Bibliothek "] <- "A7"
data_split$Job[data_split$Job.other. == "gastro"] <- "A6"
data_split$Job[data_split$Job.other. == "Onlinebusiness"] <- "A6"
data_split$Job[data_split$Job.other. == "Handwerk anlagenbau"] <- "A3"
data_split$Job[data_split$Job.other. == "tierpflegerin"] <- "A1"
data_split$Job[data_split$Job == "-oth-"] <- "Others"
data_split$Job[data_split$Job.other. == "Sicherheitsdienst"] <- "A5"
data_split$Job[data_split$Job.other. == "Elternzeit "] <- "A11"
data_split$Job[data_split$Job.other. == "pflege- teilzeit"] <- "A8"
data_split$Job[data_split$Job.other. == "Dienstleistungen: Übersetzerin"] <- "A7"
data_split$Job[data_split$Job.other. == "Gesundheitswesen"] <- "A8"
data_split$Job[data_split$Job.other. == "öffentl. Dienst"] <- "Others"
data_split$Job[data_split$Job.other. == "Studentin"] <- "A11"
data_split$Job <- factor(data_split$Job,levels = c("A1","A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","Others"))
ggplot(data_split, aes(x=Job,fill=`Split sample`)) +  geom_bar(position='dodge') + xlab("Job group") + ylab("Frequency") 
table(data_split.yes$Job)
table(data_split.no$Job)
#high differences in the majority of job groups

#E-PVQ
epvq_long_split <- merge(epvq_long,data_split)
epvq_long_split <- epvq_long_split[,c(-4:-6,-8:-124)]
epvq_long_split.yes <- subset(epvq_long_split,`Split sample`=="yes")
epvq_long_split.no <- subset(epvq_long_split,`Split sample`=="no")
ggplotly(ggplot(epvq_long_split.yes, aes(x=variable,y=value)) +  geom_count() + xlab ("Statement") + ylab("Answer"))
ggplotly(ggplot(epvq_long_split.no, aes(x=variable,y=value)) +  geom_count() + xlab ("Statement") + ylab("Answer"))
table(epvq_long_split.yes$value)
table(epvq_long_split.no$value)
#similar subgroups

#which attributes were considered?
attributes_considered_long_split <- merge(attributes_considered_long,data_split,by.x="ID",by.y="id")
attributes_considered_long_split <- attributes_considered_long_split[,c(-3:-6,-8:-124)]
ggplot(attributes_considered_long_split, aes(x=variable,fill=`Split sample`)) +  geom_bar(position='dodge') + 
  xlab("Not considered attributes") + ylab("Frequency")
#differences exist, but don't seem to be that high

#how sure when choosing an option in the CE?
data_split$F4 <- gsub("A1", "Very sure", data_split$F4)
data_split$F4 <- gsub("A2", "Rather sure", data_split$F4)
data_split$F4 <- gsub("A3", "Neither nor", data_split$F4)
data_split$F4 <- gsub("A4", "Rather unsure", data_split$F4)
data_split$F4 <- gsub("A5", "Very unsure", data_split$F4)
data_split$F4 <- factor(data_split$F4, levels=c("Very sure","Rather sure","Neither nor","Rather unsure","Very unsure"))
data_split.yes <- subset(data_split,`Split sample`=="yes")
data_split.no <- subset(data_split,`Split sample`=="no")
table(data_split.yes$F4)
table(data_split.no$F4)
ggplot(data_split, aes(x=F4,fill=`Split sample`)) +  geom_bar(position='dodge') + 
  xlab("How sure were respondents when deciding which CE option they prefer?") + ylab("Frequency")
#differences exist, but don't seem to be that high

#how often in nature?
data_split$Nature <- gsub("A1", "Daily", data_split$Nature)
data_split$Nature <- gsub("A2", "From time to time", data_split$Nature)
data_split$Nature <- gsub("A3", "Rather rarely", data_split$Nature)
data_split$Nature <- gsub("A4", "Rarely", data_split$Nature)
data_split$Nature <- gsub("A5", "Never", data_split$Nature)
data_split.yes <- subset(data_split,`Split sample`=="yes")
data_split.no <- subset(data_split,`Split sample`=="no")
table(data_split.yes$Nature)
table(data_split.no$Nature)
ggplot(data_split, aes(x=Nature,fill=`Split sample`)) +  geom_bar(position='dodge') + 
  xlab("How often do respondents spend time in nature?") + ylab("Frequency")
#very similar

#trust in science
data_split$TrustScience <- gsub("A1", "Very high", data_split$TrustScience)
data_split$TrustScience <- gsub("A2", "High", data_split$TrustScience)
data_split$TrustScience <- gsub("A3", "Medium", data_split$TrustScience)
data_split$TrustScience <- gsub("A4", "Low", data_split$TrustScience)
data_split$TrustScience <- gsub("A5", "Very low", data_split$TrustScience)
data_split$TrustScience <- factor(data_split$TrustScience, levels=c("Very high","High","Medium","Low","Very low"))
data_split.yes <- subset(data_split,`Split sample`=="yes")
data_split.no <- subset(data_split,`Split sample`=="no")
table(data_split.yes$TrustScience)
table(data_split.no$TrustScience)
ggplot(data_split, aes(x=TrustScience,fill=`Split sample`)) +  geom_bar(position='dodge') + 
  xlab("How much do respondents trust in science?") + ylab("Frequency")
#similar, but esp. highest level differs between groups



#####################################################################################################
# 1. Multinomial Logit estimation
#####################################################################################################

#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       

### Clear memory
#rm(list = ls())

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "MNL",
  modelDescr      = "Simple MNL model on ecosystem services choice SP data",
  indivID         = "ID",
  outputDirectory = "output"
)

#### DEFINE MODEL PARAMETERS                                    

database <- database_simple
### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc              = 0, 
              b_climate.6      = 0,
              b_climate.12     = 0,
              b_waterq.1       = 0,
              b_waterq.2       = 0,
              b_biodiversity.1 = 0,
              b_biodiversity.2 = 0,
              b_flood.1        = 0,
              b_flood.2        = 0,
              b_soiler.10      = 0,
              b_soiler.20      = 0,  #only one parameter per variable??
              b_cost           = 0)

### Vector with parameters to be kept fixed at their starting value in apollo_beta
apollo_fixed = c()

#### GROUP AND VALIDATE INPUTS                                  

apollo_inputs = apollo_validateInputs()

#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities
  V = list()
  V[["alt1"]] = b_climate.6*(alt1.climate==6) + b_climate.12*(alt1.climate==12) + b_waterq.1*(alt1.waterq==1) + 
    b_waterq.2*(alt1.waterq==2) + b_biodiversity.1*(alt1.biodiversity==1) + b_biodiversity.2*(alt1.biodiversity==2) +
    b_flood.1*(alt1.flood==1) + b_flood.2*(alt1.flood==2) + b_soiler.10*(alt1.soiler==10) + b_soiler.20*(alt1.soiler==20) + 
    b_cost*alt1.cost  #include asc?
  V[["alt2"]] = b_climate.6*(alt2.climate==6) + b_climate.12*(alt2.climate==12) + b_waterq.1*(alt2.waterq==1) + 
    b_waterq.2*(alt2.waterq==2) + b_biodiversity.1*(alt2.biodiversity==1) + b_biodiversity.2*(alt2.biodiversity==2) +
    b_flood.1*(alt2.flood==1) + b_flood.2*(alt2.flood==2) + b_soiler.10*(alt2.soiler==10) + b_soiler.20*(alt2.soiler==20) + 
    b_cost*alt2.cost
  V[["alt3"]] = asc #+ b_soiler.20*(alt3.soiler==20) + b_soiler.40*(alt3.soiler==40) --> does not change anything

  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2, alt3=3), 
    choiceVar     = choice,
    utilities     = V
  )
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

### Starting values search
apollo_beta=apollo_searchStart(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)

### Use new starting values
apollo_beta=c(asc              = -0.61, 
              b_climate.6      = 0.24,
              b_climate.12     = 0.58,
              b_waterq.1       = 0.22,
              b_waterq.2       = 0.74,
              b_biodiversity.1 = 1.23,
              b_biodiversity.2 = 1.32,
              b_flood.1        = 0.41,
              b_flood.2        = 0.51,
              b_soiler.10      = -0.06,
              b_soiler.20      = -0.16,  
              b_cost           = 0)

#### MODEL ESTIMATION                                           

mnl_simple2 <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

#### MODEL OUTPUTS                                               

#### FORMATTED OUTPUT (TO SCREEN)                                

apollo_modelOutput(mnl_simple2, list(printPVal = TRUE))

#WTP
deltaMethod_settings=list(expression=c(WTP_climate.6="b_climate.6/-b_cost",
                                       WTP_climate.12="b_climate.12/-b_cost",
                                       WTP_waterq.1="b_waterq.1/-b_cost",
                                       WTP_waterq.2="b_waterq.2/-b_cost",
                                       WTP_biodiversity.1="b_biodiversity.1/-b_cost",
                                       WTP_biodiversity.2="b_biodiversity.2/-b_cost",
                                       WTP_flood.1="b_flood.1/-b_cost",
                                       WTP_flood.2="b_flood.2/-b_cost",
                                       WTP_soiler.10="b_soiler.10/-b_cost",
                                       WTP_soiler.20="b_soiler.20/-b_cost"),printPVal = TRUE)
apollo_deltaMethod(mnl_simple2, deltaMethod_settings)

#### FORMATTED OUTPUT (TO FILE, using model name)                

apollo_saveOutput(mnl_simple2, list(printPVal = TRUE))

#####################################################################################################
# 1.1. Simple MNL in WTP space
#####################################################################################################

# remove data from previous estimation
remove(apollo_beta,apollo_fixed,apollo_lcPars,apollo_probabilities,apollo_randCoeff,apollo_control,apollo_draws,apollo_inputs)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "MNL in WTP space",
  modelDescr      = "Simple MNL model on ecosystem services choice SP data in WTP space",
  indivID         = "ID",
  outputDirectory = "output"
)

#### DEFINE MODEL PARAMETERS                                    

database <- database_simple
apollo_beta=c(asc              = 0, 
              b_climate.6      = 0,
              b_climate.12     = 0,
              b_waterq.1       = 0,
              b_waterq.2       = 0,
              b_biodiversity.1 = 0,
              b_biodiversity.2 = 0,
              b_flood.1        = 0,
              b_flood.2        = 0,
              b_soiler.10      = 0,
              b_soiler.20      = 0,  
              b_cost           = 0)

### Vector with parameters to be kept fixed at their starting value in apollo_beta
apollo_fixed = c()

#### GROUP AND VALIDATE INPUTS                                  

apollo_inputs = apollo_validateInputs()

#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities
  V = list()
  V[["alt1"]] = b_cost* (b_climate.6*(alt1.climate==6) + b_climate.12*(alt1.climate==12) + b_waterq.1*(alt1.waterq==1) + 
    b_waterq.2*(alt1.waterq==2) + b_biodiversity.1*(alt1.biodiversity==1) + b_biodiversity.2*(alt1.biodiversity==2) +
    b_flood.1*(alt1.flood==1) + b_flood.2*(alt1.flood==2) + b_soiler.10*(alt1.soiler==10) + b_soiler.20*(alt1.soiler==20) + 
    alt1.cost)
  V[["alt2"]] = b_cost* (b_climate.6*(alt2.climate==6) + b_climate.12*(alt2.climate==12) + b_waterq.1*(alt2.waterq==1) + 
    b_waterq.2*(alt2.waterq==2) + b_biodiversity.1*(alt2.biodiversity==1) + b_biodiversity.2*(alt2.biodiversity==2) +
    b_flood.1*(alt2.flood==1) + b_flood.2*(alt2.flood==2) + b_soiler.10*(alt2.soiler==10) + b_soiler.20*(alt2.soiler==20) + 
    alt2.cost)
  V[["alt3"]] = asc 
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2, alt3=3), 
    choiceVar     = choice,
    utilities     = V
  )
  
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  P = apollo_panelProd(P, apollo_inputs, functionality)
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}
#### MODEL ESTIMATION                                           

mnl_simple_wtp <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
apollo_modelOutput(mnl_simple_wtp, list(printPVal = TRUE))
apollo_saveOutput(mnl_simple_wtp, list(printPVal = TRUE))

#####################################################################################################
# 1.2. MNL model incorporating split sample interactions
#####################################################################################################

# remove data from previous estimation
remove(apollo_beta,apollo_fixed,apollo_lcPars,apollo_probabilities,apollo_randCoeff,apollo_control,apollo_draws,apollo_inputs)

#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       

### Clear memory
#rm(list = ls())

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "MNL treatment interactions",
  modelDescr      = "MNL model on ecosystem services choice SP data with treatment interactions",
  indivID         = "ID",
  outputDirectory = "output"
)

#### DEFINE MODEL PARAMETERS                                    

### Data
database_epvq <- merge(database_split,epvq_means,by.x="ID",by.y="id")
database_covariates <- database_epvq[,-26:-29]
covariates <- data[,c(1,63:67)]
covariates <- merge(covariates,plz_urban)
incanded <- data[,c(1,71:72)]
covariates <- merge(covariates,incanded)
covariates <- rename.variable(covariates,"Nature","nature")
covariates$nature <- gsub("Daily","1",covariates$nature)
covariates$nature <- gsub("From time to time","2",covariates$nature)
covariates$nature <- gsub("Rather rarely","3",covariates$nature)
covariates$nature <- as.numeric(covariates$nature)
covariates <- rename.variable(covariates,"TrustScience","trustscience")
covariates$trustscience <- gsub("Very high","1",covariates$trustscience)
covariates$trustscience <- gsub("High","2",covariates$trustscience)
covariates$trustscience <- gsub("Medium","3",covariates$trustscience)
covariates$trustscience <- gsub("Low","4",covariates$trustscience)
covariates$trustscience <- gsub("Very low","5",covariates$trustscience)
covariates$trustscience <- as.numeric(covariates$trustscience)
covariates <- rename.variable(covariates,"Gender","gender")
covariates$gender <- gsub("A1", "0", covariates$gender)
covariates$gender <- gsub("A2", "1", covariates$gender)
covariates$gender <- as.numeric(covariates$gender)
covariates <- rename.variable(covariates,"Age","age")
covariates$age <- 2024-covariates$age
covariates$Education <- gsub("A","",covariates$Education)
covariates$Education <- as.numeric(covariates$Education)
covariates <- rename.variable(covariates,"Education","education")
covariates$Income <- gsub("A","",covariates$Income)
covariates$Income <- as.numeric(covariates$Income)
covariates <- rename.variable(covariates,"Income","income")
#income: take middle points of brackets as proxies for continuous income (for highest bracket, ca. double the mean net income in Germany [3813 according to Destatis, in 2021])
covariates$income_con <- c(500,1250,1750,2250,3000,4250,6907,covariates$income_con)[match(covariates$income,c("1","2","3","4","5","6","7",
                                                                                                             database_sq$income))]
covariates <- rename.variable(covariates,"Urbanisation","urbanisation")
covariates <- rename.variable(covariates,"Household","householdsize")
covariates <- covariates[,-7]
database_covariates <- merge(database_covariates,covariates,by.x="ID",by.y="id")
database <- database_covariates

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc              = -0.61, 
              b_climate.6      = 0.24,
              b_climate.12     = 0.58,
              b_waterq.1       = 0.22,
              b_waterq.2       = 0.74,
              b_biodiversity.1 = 1.23,
              b_biodiversity.2 = 1.32,
              b_flood.1        = 0.41,
              b_flood.2        = 0.51,
              b_soiler.10      = -0.06,
              b_soiler.20      = -0.16,  
              b_cost           = 0,
              b_split_asc              = 0,
              b_split_climate.6        = 0,
              b_split_climate.12       = 0,
              b_split_waterq.1         = 0,
              b_split_waterq.2         = 0,
              b_split_biodiversity.1   = 0,
              b_split_biodiversity.2   = 0,
              b_split_flood.1          = 0,
              b_split_flood.2          = 0,
              b_split_soiler.10        = 0,
              b_split_soiler.20        = 0,
              b_split_cost             = 0)

### Vector with parameters to be kept fixed at their starting value in apollo_beta
apollo_fixed = c()

#### GROUP AND VALIDATE INPUTS                                  

apollo_inputs = apollo_validateInputs()

#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  b_climate.6_value = b_climate.6 + b_split_climate.6*split 
  b_climate.12_value = b_climate.12 + b_split_climate.12*split 
  b_waterq.1_value = b_waterq.1 + b_split_waterq.1*split 
  b_waterq.2_value = b_waterq.2 + b_split_waterq.2*split 
  b_biodiversity.1_value = b_biodiversity.1 + b_split_biodiversity.1*split 
  b_biodiversity.2_value = b_biodiversity.2 + b_split_biodiversity.2*split 
  b_flood.1_value = b_flood.1 + b_split_flood.1*split 
  b_flood.2_value = b_flood.2 + b_split_flood.2*split 
  b_soiler.10_value = b_soiler.10 + b_split_soiler.10*split 
  b_soiler.20_value = b_soiler.20 + b_split_soiler.20*split 
  b_cost_value = b_cost + b_split_cost*split 
  
  ### List of utilities
  V = list()
  V[["alt1"]] = b_climate.6_value*(alt1.climate==6) + b_climate.12_value*(alt1.climate==12) + b_waterq.1_value*(alt1.waterq==1) + 
    b_waterq.2_value*(alt1.waterq==2) + b_biodiversity.1_value*(alt1.biodiversity==1) + b_biodiversity.2_value*(alt1.biodiversity==2) +
    b_flood.1_value*(alt1.flood==1) + b_flood.2_value*(alt1.flood==2) + b_soiler.10_value*(alt1.soiler==10) + 
    b_soiler.20_value*(alt1.soiler==20) + b_cost_value*alt1.cost
  V[["alt2"]] = b_climate.6_value*(alt2.climate==6) + b_climate.12_value*(alt2.climate==12) + b_waterq.1_value*(alt2.waterq==1) + 
    b_waterq.2_value*(alt2.waterq==2) + b_biodiversity.1_value*(alt2.biodiversity==1) + b_biodiversity.2_value*(alt2.biodiversity==2) +
    b_flood.1_value*(alt2.flood==1) + b_flood.2_value*(alt2.flood==2) + b_soiler.10_value*(alt2.soiler==10) + 
    b_soiler.20_value*(alt2.soiler==20) + b_cost_value*alt2.cost
  V[["alt3"]] = asc + b_split_asc*asc*split
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2, alt3=3), 
    choiceVar     = choice,
    utilities     = V
  )
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


#### MODEL ESTIMATION                                           

mnl_split_interactions <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
apollo_modelOutput(mnl_split_interactions, list(printPVal = TRUE))
apollo_saveOutput(mnl_split_interactions, list(printPVal = TRUE))


#####################################################################################################
# 1.3. MNL model incorporating SQ interactions
#####################################################################################################

# remove data from previous estimation
remove(apollo_beta,apollo_fixed,apollo_lcPars,apollo_probabilities,apollo_randCoeff,apollo_control,apollo_draws,apollo_inputs)

#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       

### Clear memory
#rm(list = ls())

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "MNL SQ interactions",
  modelDescr      = "MNL model on ecosystem services choice SP data with SQ interactions",
  indivID         = "ID",
  outputDirectory = "output"
)

#### DEFINE MODEL PARAMETERS                                    

database <- database_covariates

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc              = -0.61,
              asc_gender               = 0,
              asc_income               = 0, 
              asc_age                  = 0,
              asc_education            = 0,
              asc_householdsize        = 0,
              asc_urban                = 0,  
              asc_trustscience         = 0,
              asc_nature               = 0, 
              b_climate.6      = 0.24,
              b_climate.12     = 0.58,
              b_waterq.1       = 0.22,
              b_waterq.2       = 0.74,
              b_biodiversity.1 = 1.23,
              b_biodiversity.2 = 1.32,
              b_flood.1        = 0.41,
              b_flood.2        = 0.51,
              b_soiler.10      = -0.06,
              b_soiler.20      = -0.16,  
              b_cost           = 0)

### Vector with parameters to be kept fixed at their starting value in apollo_beta
apollo_fixed = c()

#### GROUP AND VALIDATE INPUTS                                  

apollo_inputs = apollo_validateInputs()

#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities
  V = list()
  V[["alt1"]] = b_climate.6*(alt1.climate==6) + b_climate.12*(alt1.climate==12) + b_waterq.1*(alt1.waterq==1) + 
    b_waterq.2*(alt1.waterq==2) + b_biodiversity.1*(alt1.biodiversity==1) + b_biodiversity.2*(alt1.biodiversity==2) +
    b_flood.1*(alt1.flood==1) + b_flood.2*(alt1.flood==2) + b_soiler.10*(alt1.soiler==10) + b_soiler.20*(alt1.soiler==20) + 
    b_cost*alt1.cost
  V[["alt2"]] = b_climate.6*(alt2.climate==6) + b_climate.12*(alt2.climate==12) + b_waterq.1*(alt2.waterq==1) + 
    b_waterq.2*(alt2.waterq==2) + b_biodiversity.1*(alt2.biodiversity==1) + b_biodiversity.2*(alt2.biodiversity==2) +
    b_flood.1*(alt2.flood==1) + b_flood.2*(alt2.flood==2) + b_soiler.10*(alt2.soiler==10) + b_soiler.20*(alt2.soiler==20) + 
    b_cost*alt2.cost
  V[["alt3"]] = asc + asc_gender*gender + asc_income*income + asc_age*age + asc_education*education + asc_trustscience*trustscience + 
    asc_nature*nature + asc_urban*urbanisation + asc_householdsize*householdsize

  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2, alt3=3), 
    choiceVar     = choice,
    utilities     = V
  )
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


#### MODEL ESTIMATION                                           

mnl_sq_interactions <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
apollo_modelOutput(mnl_sq_interactions, list(printPVal = TRUE))
apollo_saveOutput(mnl_sq_interactions, list(printPVal = TRUE))


#####################################################################################################
# 2. Mixed Logit estimation
#####################################################################################################

# remove data from previous estimation
remove(apollo_beta,apollo_fixed,apollo_lcPars,apollo_probabilities,apollo_randCoeff,apollo_control,apollo_draws,apollo_inputs)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "MXL2",
  modelDescr      = "Simple MXL model on ecosystem services choice SP data",
  indivID         = "ID",
  mixing          = TRUE,
  nCores          = 2,      #data splitting takes place --> descriptive statistics need to be calculated beforehand!!
  seed            = 321,
  outputDirectory = "output"
)

### Data
database <- database_simple

#### DEFINE MODEL PARAMETERS                                     

### Define starting values
apollo_beta=c(asc                      = 0, 
              mu_b_climate.6           = 0,
              sigma_b_climate.6        = 0,
              mu_b_climate.12          = 0,
              sigma_b_climate.12       = 0,
              mu_b_waterq.1            = 0,
              sigma_b_waterq.1         = 0,
              mu_b_waterq.2            = 0,
              sigma_b_waterq.2         = 0,
              mu_b_biodiversity.1      = 0,
              sigma_b_biodiversity.1   = 0,
              mu_b_biodiversity.2      = 0,
              sigma_b_biodiversity.2   = 0,
              mu_b_flood.1             = 0,
              sigma_b_flood.1          = 0,
              mu_b_flood.2             = 0,
              sigma_b_flood.2          = 0,
              mu_b_soiler.10           = 0,
              sigma_b_soiler.10        = 0,
              mu_b_soiler.20           = 0,
              sigma_b_soiler.20        = 0,
              mu_log_b_cost            = -1,
              sigma_log_b_cost         = 1)

### Vector with parameters to be kept fixed at their starting value in apollo_beta
apollo_fixed = c()

### Define parameters for the simulation
apollo_draws = list(
  interDrawsType = "sobol",
  interNDraws = 1000,
  interNormDraws = c("draws_climate_inter.6","draws_waterq_inter.1","draws_biodiversity_inter.1","draws_flood_inter.1",
                     "draws_soiler_inter.10","draws_climate_inter.12","draws_waterq_inter.2","draws_biodiversity_inter.2",
                     "draws_flood_inter.2","draws_soiler_inter.20","draws_cost_inter")
)

### Define random coefficients
apollo_randCoeff = function(apollo_beta,apollo_inputs){
  randcoeff = list()
  randcoeff[["b_climate.6"]] = mu_b_climate.6 + sigma_b_climate.6 * draws_climate_inter.6
  randcoeff[["b_climate.12"]] = mu_b_climate.12 + sigma_b_climate.12 * draws_climate_inter.12
  randcoeff[["b_waterq.1"]] = mu_b_waterq.1 + sigma_b_waterq.1 * draws_waterq_inter.1
  randcoeff[["b_waterq.2"]] = mu_b_waterq.2 + sigma_b_waterq.2 * draws_waterq_inter.2
  randcoeff[["b_biodiversity.1"]] = mu_b_biodiversity.1 + sigma_b_biodiversity.1 * draws_biodiversity_inter.1
  randcoeff[["b_biodiversity.2"]] = mu_b_biodiversity.2 + sigma_b_biodiversity.2 * draws_biodiversity_inter.2
  randcoeff[["b_flood.1"]] = mu_b_flood.1 + sigma_b_flood.1 * draws_flood_inter.1
  randcoeff[["b_flood.2"]] = mu_b_flood.2 + sigma_b_flood.2 * draws_flood_inter.2
  randcoeff[["b_soiler.10"]] = mu_b_soiler.10 + sigma_b_soiler.10 * draws_soiler_inter.10
  randcoeff[["b_soiler.20"]] = mu_b_soiler.20 + sigma_b_soiler.20 * draws_soiler_inter.20
  randcoeff[["b_cost"]] = -exp(mu_log_b_cost + sigma_log_b_cost * draws_cost_inter)
  return(randcoeff)
}

#### GROUP AND VALIDATE INPUTS                                  

apollo_inputs = apollo_validateInputs()

#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities
  V = list()
  V[["alt1"]] = b_climate.6*(alt1.climate==6) + b_climate.12*(alt1.climate==12) + b_waterq.1*(alt1.waterq==1) + 
    b_waterq.2*(alt1.waterq==2) + b_biodiversity.1*(alt1.biodiversity==1) + b_biodiversity.2*(alt1.biodiversity==2) +
    b_flood.1*(alt1.flood==1) + b_flood.2*(alt1.flood==2) + b_soiler.10*(alt1.soiler==10) + b_soiler.20*(alt1.soiler==20) + 
    b_cost*alt1.cost
  V[["alt2"]] = b_climate.6*(alt2.climate==6) + b_climate.12*(alt2.climate==12) + b_waterq.1*(alt2.waterq==1) + 
    b_waterq.2*(alt2.waterq==2) + b_biodiversity.1*(alt2.biodiversity==1) + b_biodiversity.2*(alt2.biodiversity==2) +
    b_flood.1*(alt2.flood==1) + b_flood.2*(alt2.flood==2) + b_soiler.10*(alt2.soiler==10) + b_soiler.20*(alt2.soiler==20) + 
    b_cost*alt2.cost
  V[["alt3"]] = asc
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2, alt3=3), 
    choiceVar     = choice,
    utilities     = V
  ) #model components
  P[["model"]] = apollo_mnl(mnl_settings,functionality)
  P = apollo_panelProd(P,apollo_inputs,functionality)
  P = apollo_avgInterDraws(P,apollo_inputs,functionality)
  P = apollo_prepareProb(P,apollo_inputs,functionality)
  return(P)
}

### Starting values search
apollo_beta=apollo_searchStart(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)

### Use best starting values
apollo_beta=c(asc                      = -1.22, 
              mu_b_climate.6           = 0.19,
              sigma_b_climate.6        = -0.80,
              mu_b_climate.12          = 0.58,
              sigma_b_climate.12       = 0.88,
              mu_b_waterq.1            = 0.96,
              sigma_b_waterq.1         = 1.09,
              mu_b_waterq.2            = 1.08,
              sigma_b_waterq.2         = -0.88,
              mu_b_biodiversity.1      = 1.26,
              sigma_b_biodiversity.1   = 0.72,
              mu_b_biodiversity.2      = 1.61,
              sigma_b_biodiversity.2   = 1.06,
              mu_b_flood.1             = 0.70,
              sigma_b_flood.1          = -0.90,
              mu_b_flood.2             = 0.59,
              sigma_b_flood.2          = -0.07,
              mu_b_soiler.10           = -0.08,
              sigma_b_soiler.10        = -0.59,
              mu_b_soiler.20           = 0.23,
              sigma_b_soiler.20        = 0.08,
              mu_log_b_cost            = -5.21,
              sigma_log_b_cost         = 0.96)


#### MODEL ESTIMATION                                            

mxl2 <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
apollo_modelOutput(mxl2, list(printPVal = TRUE))
apollo_saveOutput(mxl2, list(printPVal = TRUE))




#####################################################################################################
# 2.1. Mixed Logit in WTP space
#####################################################################################################

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "MXL WTP space2",
  modelDescr      = "Simple MXL model in WTP space",
  indivID         = "ID",
  mixing          = TRUE,
  nCores          = 2,      #data splitting takes place --> descriptive statistics need to be calculated beforehand!!
  seed            = 321,
  outputDirectory = "output"
)

### Data
database <- database_simple

#### DEFINE MODEL PARAMETERS                                     

### Define starting values
apollo_beta=c(asc                      = -1.22, 
              mu_b_climate.6           = 0.19,
              sigma_b_climate.6        = -0.80,
              mu_b_climate.12          = 0.58,
              sigma_b_climate.12       = 0.88,
              mu_b_waterq.1            = 0.96,
              sigma_b_waterq.1         = 1.09,
              mu_b_waterq.2            = 1.08,
              sigma_b_waterq.2         = -0.88,
              mu_b_biodiversity.1      = 1.26,
              sigma_b_biodiversity.1   = 0.72,
              mu_b_biodiversity.2      = 1.61,
              sigma_b_biodiversity.2   = 1.06,
              mu_b_flood.1             = 0.70,
              sigma_b_flood.1          = -0.90,
              mu_b_flood.2             = 0.59,
              sigma_b_flood.2          = -0.07,
              mu_b_soiler.10           = -0.08,
              sigma_b_soiler.10        = -0.59,
              mu_b_soiler.20           = 0.23,
              sigma_b_soiler.20        = 0.08,
              mu_log_b_cost            = -5.21,
              sigma_log_b_cost         = 0.96)

### Vector with parameters to be kept fixed at their starting value in apollo_beta
apollo_fixed = c()

### Define parameters for the simulation
apollo_draws = list(
  interDrawsType = "sobol",
  interNDraws = 1000,
  interNormDraws = c("draws_climate_inter.6","draws_waterq_inter.1","draws_biodiversity_inter.1","draws_flood_inter.1",
                     "draws_soiler_inter.10","draws_climate_inter.12","draws_waterq_inter.2","draws_biodiversity_inter.2",
                     "draws_flood_inter.2","draws_soiler_inter.20","draws_cost_inter")
)

### Define random coefficients
apollo_randCoeff = function(apollo_beta,apollo_inputs){
  randcoeff = list()
  randcoeff[["b_climate.6"]] = mu_b_climate.6 + sigma_b_climate.6 * draws_climate_inter.6
  randcoeff[["b_climate.12"]] = mu_b_climate.12 + sigma_b_climate.12 * draws_climate_inter.12
  randcoeff[["b_waterq.1"]] = mu_b_waterq.1 + sigma_b_waterq.1 * draws_waterq_inter.1
  randcoeff[["b_waterq.2"]] = mu_b_waterq.2 + sigma_b_waterq.2 * draws_waterq_inter.2
  randcoeff[["b_biodiversity.1"]] = mu_b_biodiversity.1 + sigma_b_biodiversity.1 * draws_biodiversity_inter.1
  randcoeff[["b_biodiversity.2"]] = mu_b_biodiversity.2 + sigma_b_biodiversity.2 * draws_biodiversity_inter.2
  randcoeff[["b_flood.1"]] = mu_b_flood.1 + sigma_b_flood.1 * draws_flood_inter.1
  randcoeff[["b_flood.2"]] = mu_b_flood.2 + sigma_b_flood.2 * draws_flood_inter.2
  randcoeff[["b_soiler.10"]] = mu_b_soiler.10 + sigma_b_soiler.10 * draws_soiler_inter.10
  randcoeff[["b_soiler.20"]] = mu_b_soiler.20 + sigma_b_soiler.20 * draws_soiler_inter.20
  randcoeff[["b_cost"]] = -exp(mu_log_b_cost + sigma_log_b_cost * draws_cost_inter)
  return(randcoeff)
}

#### GROUP AND VALIDATE INPUTS                                   

apollo_inputs = apollo_validateInputs()

#### DEFINE MODEL AND LIKELIHOOD FUNCTION                       

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities
  V = list()
  V[["alt1"]] = b_cost* (b_climate.6*(alt1.climate==6) + b_climate.12*(alt1.climate==12) + b_waterq.1*(alt1.waterq==1) + 
    b_waterq.2*(alt1.waterq==2) + b_biodiversity.1*(alt1.biodiversity==1) + b_biodiversity.2*(alt1.biodiversity==2) +
    b_flood.1*(alt1.flood==1) + b_flood.2*(alt1.flood==2) + b_soiler.10*(alt1.soiler==10) + b_soiler.20*(alt1.soiler==20) + 
    alt1.cost)
  V[["alt2"]] = b_cost* (b_climate.6*(alt2.climate==6) + b_climate.12*(alt2.climate==12) + b_waterq.1*(alt2.waterq==1) + 
    b_waterq.2*(alt2.waterq==2) + b_biodiversity.1*(alt2.biodiversity==1) + b_biodiversity.2*(alt2.biodiversity==2) +
    b_flood.1*(alt2.flood==1) + b_flood.2*(alt2.flood==2) + b_soiler.10*(alt2.soiler==10) + b_soiler.20*(alt2.soiler==20) + 
    alt2.cost)
  V[["alt3"]] = asc
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2, alt3=3), 
    choiceVar     = choice,
    utilities     = V
  ) #model components
  P[["model"]] = apollo_mnl(mnl_settings,functionality)
  P = apollo_panelProd(P,apollo_inputs,functionality)
  P = apollo_avgInterDraws(P,apollo_inputs,functionality)
  P = apollo_prepareProb(P,apollo_inputs,functionality)
  return(P)
}

#### MODEL ESTIMATION                                            

mxl_wtp2 <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
apollo_modelOutput(mxl_wtp2, list(printPVal = TRUE))
apollo_saveOutput(mxl_wtp2, list(printPVal = TRUE))

#compute conditional individual WTP (to be used later for spatial analyses)
individual_wtp2 <- apollo_conditionals(mxl_wtp2,apollo_probabilities,apollo_inputs)
#combine into dataframe
wtp2 <- cbind(individual_wtp2$b_climate.6,individual_wtp2$b_climate.12,individual_wtp2$b_waterq.1,individual_wtp2$b_waterq.2,individual_wtp2$b_biodiversity.1,
              individual_wtp2$b_biodiversity.2,individual_wtp2$b_flood.1,individual_wtp2$b_flood.2,individual_wtp2$b_soiler.10,individual_wtp2$b_soiler.20)
#remove unnecessary columns
wtp2 <- wtp2[,-c(4,7,10,13,16,19,22,25,28)]
#rename columns
colnames(wtp2) <- c("ID","climate.6_m","climate.6_sd","climate.12_m","climate.12_sd","waterq.1_m","waterq.1_sd","waterq.2_m","waterq.2_sd","biodiversity.1_m",
                    "biodiversity.1_sd","biodiversity.2_m","biodiversity.2_sd","flood.1_m","flood.1_sd","flood.2_m","flood.2_sd","soiler.10_m","soiler.10_sd",
                    "soiler.20_m","soiler.20_sd")
#compute mean WTP per ID
wtp2$overall_m <- ((wtp2$climate.6_m+wtp2$climate.12_m+wtp2$waterq.1_m+wtp2$waterq.2_m+wtp2$biodiversity.1_m+wtp2$biodiversity.2_m+wtp2$flood.1_m+wtp2$flood.2_m+
                      wtp2$soiler.10_m+wtp2$soiler.20_m)/10)*(-1)

#change sign of estimates
wtp2_means <- wtp2[,-c(3,5,7,9,11,13,15,17,19,21)]
wtp2_means$climate.6_m <- wtp2_means$climate.6_m*(-1)
wtp2_means$climate.12_m <- wtp2_means$climate.12_m*(-1)
wtp2_means$waterq.1_m <- wtp2_means$waterq.1_m*(-1)
wtp2_means$waterq.2_m <- wtp2_means$waterq.2_m*(-1)
wtp2_means$biodiversity.1_m <- wtp2_means$biodiversity.1_m*(-1)
wtp2_means$biodiversity.2_m <- wtp2_means$biodiversity.2_m*(-1)
wtp2_means$flood.1_m <- wtp2_means$flood.1_m*(-1)
wtp2_means$flood.2_m <- wtp2_means$flood.2_m*(-1)
wtp2_means$soiler.10_m <- wtp2_means$soiler.10_m*(-1)
wtp2_means$soiler.20_m <- wtp2_means$soiler.20_m*(-1)

#export for later use
write.csv(wtp2_means,"Y:/Home/fischesa/GONASIP/choice experimente/R/individual_wtp_positive.csv",row.names=F)

#####################################################################################################
# 2.2. Mixed Logit with split variable interactions
#####################################################################################################

# remove data from previous estimation
remove(apollo_beta,apollo_fixed,apollo_lcPars,apollo_probabilities,apollo_randCoeff,apollo_control,apollo_draws,apollo_inputs)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "MXL interactions split",
  modelDescr      = "MXL model with split variable interactions",
  indivID         = "ID",
  mixing          = TRUE,
  nCores          = 2,      #data splitting takes place --> descriptive statistics need to be calculated beforehand!!
  seed            = 321,
  outputDirectory = "output"
)

### Data
database <- database_covariates


#### DEFINE MODEL PARAMETERS                                     

### Define starting values
apollo_beta=c(asc                      = -1.22, 
              mu_b_climate.6           = 0.19,
              sigma_b_climate.6        = -0.80,
              mu_b_climate.12          = 0.58,
              sigma_b_climate.12       = 0.88,
              mu_b_waterq.1            = 0.96,
              sigma_b_waterq.1         = 1.09,
              mu_b_waterq.2            = 1.08,
              sigma_b_waterq.2         = -0.88,
              mu_b_biodiversity.1      = 1.26,
              sigma_b_biodiversity.1   = 0.72,
              mu_b_biodiversity.2      = 1.61,
              sigma_b_biodiversity.2   = 1.06,
              mu_b_flood.1             = 0.70,
              sigma_b_flood.1          = -0.90,
              mu_b_flood.2             = 0.59,
              sigma_b_flood.2          = -0.07,
              mu_b_soiler.10           = -0.08,
              sigma_b_soiler.10        = -0.59,
              mu_b_soiler.20           = 0.23,
              sigma_b_soiler.20        = 0.08,
              mu_log_b_cost            = -5.21,
              sigma_log_b_cost         = 0.96,
              b_split_asc              = 0,
              b_split_climate.6        = 0,
              b_split_climate.12       = 0,
              b_split_waterq.1         = 0,
              b_split_waterq.2         = 0,
              b_split_biodiversity.1   = 0,
              b_split_biodiversity.2   = 0,
              b_split_flood.1          = 0,
              b_split_flood.2          = 0,
              b_split_soiler.10        = 0,
              b_split_soiler.20        = 0,
              b_split_cost             = 0) 

### Vector with parameters to be kept fixed at their starting value in apollo_beta
apollo_fixed = c()

### Define parameters for the simulation
apollo_draws = list(
  interDrawsType = "sobol",
  interNDraws = 1000,
  interNormDraws = c("draws_climate_inter.6","draws_waterq_inter.1","draws_biodiversity_inter.1","draws_flood_inter.1",
                     "draws_soiler_inter.10","draws_climate_inter.12","draws_waterq_inter.2","draws_biodiversity_inter.2",
                     "draws_flood_inter.2","draws_soiler_inter.20","draws_cost_inter")
)

### Define random coefficients
apollo_randCoeff = function(apollo_beta,apollo_inputs){
  randcoeff = list()
  randcoeff[["b_climate.6"]] = mu_b_climate.6 + sigma_b_climate.6 * draws_climate_inter.6
  randcoeff[["b_climate.12"]] = mu_b_climate.12 + sigma_b_climate.12 * draws_climate_inter.12
  randcoeff[["b_waterq.1"]] = mu_b_waterq.1 + sigma_b_waterq.1 * draws_waterq_inter.1
  randcoeff[["b_waterq.2"]] = mu_b_waterq.2 + sigma_b_waterq.2 * draws_waterq_inter.2
  randcoeff[["b_biodiversity.1"]] = mu_b_biodiversity.1 + sigma_b_biodiversity.1 * draws_biodiversity_inter.1
  randcoeff[["b_biodiversity.2"]] = mu_b_biodiversity.2 + sigma_b_biodiversity.2 * draws_biodiversity_inter.2
  randcoeff[["b_flood.1"]] = mu_b_flood.1 + sigma_b_flood.1 * draws_flood_inter.1
  randcoeff[["b_flood.2"]] = mu_b_flood.2 + sigma_b_flood.2 * draws_flood_inter.2
  randcoeff[["b_soiler.10"]] = mu_b_soiler.10 + sigma_b_soiler.10 * draws_soiler_inter.10
  randcoeff[["b_soiler.20"]] = mu_b_soiler.20 + sigma_b_soiler.20 * draws_soiler_inter.20
  randcoeff[["b_cost"]] = -exp(mu_log_b_cost + sigma_log_b_cost * draws_cost_inter)
  return(randcoeff)
}

#### GROUP AND VALIDATE INPUTS                                   

apollo_inputs = apollo_validateInputs()

#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  b_climate.6_value = b_climate.6 + b_split_climate.6*split 
  b_climate.12_value = b_climate.12 + b_split_climate.12*split 
  b_waterq.1_value = b_waterq.1 + b_split_waterq.1*split 
  b_waterq.2_value = b_waterq.2 + b_split_waterq.2*split 
  b_biodiversity.1_value = b_biodiversity.1 + b_split_biodiversity.1*split 
  b_biodiversity.2_value = b_biodiversity.2 + b_split_biodiversity.2*split 
  b_flood.1_value = b_flood.1 + b_split_flood.1*split 
  b_flood.2_value = b_flood.2 + b_split_flood.2*split 
  b_soiler.10_value = b_soiler.10 + b_split_soiler.10*split 
  b_soiler.20_value = b_soiler.20 + b_split_soiler.20*split 
  b_cost_value = b_cost + b_split_cost*split 

  ### List of utilities
  V = list()
  V[["alt1"]] = b_climate.6_value*(alt1.climate==6) + b_climate.12_value*(alt1.climate==12) + b_waterq.1_value*(alt1.waterq==1) + 
    b_waterq.2_value*(alt1.waterq==2) + b_biodiversity.1_value*(alt1.biodiversity==1) + b_biodiversity.2_value*(alt1.biodiversity==2) +
    b_flood.1_value*(alt1.flood==1) + b_flood.2_value*(alt1.flood==2) + b_soiler.10_value*(alt1.soiler==10) + 
    b_soiler.20_value*(alt1.soiler==20) + b_cost_value*alt1.cost
  V[["alt2"]] = b_climate.6_value*(alt2.climate==6) + b_climate.12_value*(alt2.climate==12) + b_waterq.1_value*(alt2.waterq==1) + 
    b_waterq.2_value*(alt2.waterq==2) + b_biodiversity.1_value*(alt2.biodiversity==1) + b_biodiversity.2_value*(alt2.biodiversity==2) +
    b_flood.1_value*(alt2.flood==1) + b_flood.2_value*(alt2.flood==2) + b_soiler.10_value*(alt2.soiler==10) + 
    b_soiler.20_value*(alt2.soiler==20) + b_cost_value*alt2.cost
  V[["alt3"]] = asc + b_split_asc*asc*split
  
  ### Define settings for MXL model component
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2, alt3=3), 
    choiceVar     = choice,
    utilities     = V
  ) #model components
  P[["model"]] = apollo_mnl(mnl_settings,functionality)
  P = apollo_panelProd(P,apollo_inputs,functionality)
  P = apollo_avgInterDraws(P,apollo_inputs,functionality)
  P = apollo_prepareProb(P,apollo_inputs,functionality)
  return(P)
}


#### MODEL ESTIMATION                                            

mxl_interactions_asc <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
apollo_modelOutput(mxl_interactions_asc, list(printPVal = TRUE))
apollo_saveOutput(mxl_interactions_asc, list(printPVal = TRUE))


#####################################################################################################
# 2.3. Mixed Logit with SQ interaction
#####################################################################################################

# remove data from previous estimation
remove(apollo_beta,apollo_fixed,apollo_lcPars,apollo_probabilities,apollo_randCoeff,apollo_control,apollo_draws,apollo_inputs)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "MXL SQ interaction2",
  modelDescr      = "MXL model with SQ interactions",
  indivID         = "ID",
  mixing          = TRUE,
  nCores          = 100,      #data splitting takes place --> descriptive statistics need to be calculated beforehand!!
  seed            = 321,
  outputDirectory = "output"
)

database <- database_covariates

#### DEFINE MODEL PARAMETERS                                     

### Define starting values
apollo_beta=c(asc                      = -1.22, 
              asc_gender               = 0,
              asc_income               = 0, 
              asc_age                  = 0,
              asc_education            = 0,
              asc_householdsize        = 0,
              asc_urban                = 0,  
              asc_trustscience         = 0,
              asc_nature               = 0,
              mu_b_climate.6           = 0.19,
              sigma_b_climate.6        = -0.80,
              mu_b_climate.12          = 0.58,
              sigma_b_climate.12       = 0.88,
              mu_b_waterq.1            = 0.96,
              sigma_b_waterq.1         = 1.09,
              mu_b_waterq.2            = 1.08,
              sigma_b_waterq.2         = -0.88,
              mu_b_biodiversity.1      = 1.26,
              sigma_b_biodiversity.1   = 0.72,
              mu_b_biodiversity.2      = 1.61,
              sigma_b_biodiversity.2   = 1.06,
              mu_b_flood.1             = 0.70,
              sigma_b_flood.1          = -0.90,
              mu_b_flood.2             = 0.59,
              sigma_b_flood.2          = -0.07,
              mu_b_soiler.10           = -0.08,
              sigma_b_soiler.10        = -0.59,
              mu_b_soiler.20           = 0.23,
              sigma_b_soiler.20        = 0.08,
              mu_log_b_cost            = -5.21,
              sigma_log_b_cost         = 0.96)

### Vector with parameters to be kept fixed at their starting value in apollo_beta
apollo_fixed = c()

### Define parameters for the simulation
apollo_draws = list(
  interDrawsType = "sobol",
  interNDraws = 1000,
  interNormDraws = c("draws_climate_inter.6","draws_waterq_inter.1","draws_biodiversity_inter.1","draws_flood_inter.1",
                     "draws_soiler_inter.10","draws_climate_inter.12","draws_waterq_inter.2","draws_biodiversity_inter.2",
                     "draws_flood_inter.2","draws_soiler_inter.20","draws_cost_inter")
)

### Define random coefficients
apollo_randCoeff = function(apollo_beta,apollo_inputs){
  randcoeff = list()
  randcoeff[["b_climate.6"]] = mu_b_climate.6 + sigma_b_climate.6 * draws_climate_inter.6
  randcoeff[["b_climate.12"]] = mu_b_climate.12 + sigma_b_climate.12 * draws_climate_inter.12
  randcoeff[["b_waterq.1"]] = mu_b_waterq.1 + sigma_b_waterq.1 * draws_waterq_inter.1
  randcoeff[["b_waterq.2"]] = mu_b_waterq.2 + sigma_b_waterq.2 * draws_waterq_inter.2
  randcoeff[["b_biodiversity.1"]] = mu_b_biodiversity.1 + sigma_b_biodiversity.1 * draws_biodiversity_inter.1
  randcoeff[["b_biodiversity.2"]] = mu_b_biodiversity.2 + sigma_b_biodiversity.2 * draws_biodiversity_inter.2
  randcoeff[["b_flood.1"]] = mu_b_flood.1 + sigma_b_flood.1 * draws_flood_inter.1
  randcoeff[["b_flood.2"]] = mu_b_flood.2 + sigma_b_flood.2 * draws_flood_inter.2
  randcoeff[["b_soiler.10"]] = mu_b_soiler.10 + sigma_b_soiler.10 * draws_soiler_inter.10
  randcoeff[["b_soiler.20"]] = mu_b_soiler.20 + sigma_b_soiler.20 * draws_soiler_inter.20
  randcoeff[["b_cost"]] = -exp(mu_log_b_cost + sigma_log_b_cost * draws_cost_inter)
  return(randcoeff)
}

#### GROUP AND VALIDATE INPUTS                                   

#remove(gender,nature,income,age,education,trustscience)
apollo_inputs = apollo_validateInputs()

#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities
  V = list()
  V[["alt1"]] = b_climate.6*(alt1.climate==6) + b_climate.12*(alt1.climate==12) + b_waterq.1*(alt1.waterq==1) + 
    b_waterq.2*(alt1.waterq==2) + b_biodiversity.1*(alt1.biodiversity==1) + b_biodiversity.2*(alt1.biodiversity==2) +
    b_flood.1*(alt1.flood==1) + b_flood.2*(alt1.flood==2) + b_soiler.10*(alt1.soiler==10) + b_soiler.20*(alt1.soiler==20) + 
    b_cost*alt1.cost
  V[["alt2"]] = b_climate.6*(alt2.climate==6) + b_climate.12*(alt2.climate==12) + b_waterq.1*(alt2.waterq==1) + 
    b_waterq.2*(alt2.waterq==2) + b_biodiversity.1*(alt2.biodiversity==1) + b_biodiversity.2*(alt2.biodiversity==2) +
    b_flood.1*(alt2.flood==1) + b_flood.2*(alt2.flood==2) + b_soiler.10*(alt2.soiler==10) + b_soiler.20*(alt2.soiler==20) + 
    b_cost*alt2.cost
  V[["alt3"]] = asc + asc_gender*gender + asc_income*income + asc_age*age + asc_education*education + asc_trustscience*trustscience + 
    asc_nature*nature + asc_urban*urbanisation + asc_householdsize*householdsize

  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2, alt3=3), 
    choiceVar     = choice,
    utilities     = V
  ) #model components
  P[["model"]] = apollo_mnl(mnl_settings,functionality)
  P = apollo_panelProd(P,apollo_inputs,functionality)
  P = apollo_avgInterDraws(P,apollo_inputs,functionality)
  P = apollo_prepareProb(P,apollo_inputs,functionality)
  return(P)
}


#### MODEL ESTIMATION                                            

mxl_sq_f <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
apollo_modelOutput(mxl_sq_f, list(printPVal = TRUE))
apollo_saveOutput(mxl_sq_f, list(printPVal = TRUE))








  









