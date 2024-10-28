# R.version #Java for 64 bit version required
# 
# install.packages("installr")
# library(installr)
# updateR()

# install.packages("apollo")
library(apollo)

# install.packages("xlsx")
# install.packages("readxl")
# install.packages("logitr")

library(xlsx)
library(readxl)
library(questionr)
library(support.CEs)
library(reshape2)
library(dplyr)
library(survival)
library(logitr)


#Apollo R

# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "MNL_Pretest",
  modelDescr      = "Simple MNL model on ecosystem services choice SP data",
  indivID         = "ID",
  outputDirectory = "output"
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

### Loading respondent data 
database = read.csv("Y:/Home/fischesa/GONASIP/choice experimente/R/pretest.csv",header=TRUE)

#remove non-completed surveys
database <- database[database$Letzte.Seite == 41, ] 
database <- database[!is.na(database$Letzte.Seite),] 

#remove unnecessary variables
database <- subset(database, select=-c(Datum.Abgeschickt, Start.Sprache, Im.Folgenden.mö..., Zur.Erinnerung...., 
                                       Sie.haben.bei.s..., Gruppenzeit..Pe..., Gruppenzeit..Au..., Gruppenzeit..Fr..., 
                                       Gruppenzeit..Pe....1, Gruppenzeit..Fr....1, Gruppenzeit..Ende))

#rename some variables
database <- rename.variable(database, "Antwort.ID", "ID")
database <- rename.variable(database, "Schauen.Sie.sic...", "q1")
database <- rename.variable(database, "Schauen.Sie.sic....1", "q2")
database <- rename.variable(database, "Schauen.Sie.sic....2", "q3")
database <- rename.variable(database, "Schauen.Sie.sic....3", "q4")
database <- rename.variable(database, "Schauen.Sie.sic....4", "q5")
database <- rename.variable(database, "Schauen.Sie.sic....5", "q6")
database <- rename.variable(database, "Schauen.Sie.sic....6", "q7")
database <- rename.variable(database, "Schauen.Sie.sic....7", "q8")
database <- rename.variable(database, "Schauen.Sie.sic....8", "q9")
database <- rename.variable(database, "Schauen.Sie.sic....9", "q10")
database <- rename.variable(database, "Schauen.Sie.sic....10", "q11")
database <- rename.variable(database, "Schauen.Sie.sic....11", "q12")
database <- rename.variable(database, "Schauen.Sie.sic....12", "q13")
database <- rename.variable(database, "Schauen.Sie.sic....13", "q14")
database <- rename.variable(database, "Schauen.Sie.sic....14", "q15")
database <- rename.variable(database, "Schauen.Sie.sic....15", "q16")
database <- rename.variable(database, "Schauen.Sie.sic....16", "q17")
database <- rename.variable(database, "Schauen.Sie.sic....17", "q18")

#make cell-content numeric
database$q1 <- gsub("A", "", database$q1)
database$q2 <- gsub("A", "", database$q2)
database$q3 <- gsub("A", "", database$q3)
database$q4 <- gsub("A", "", database$q4)
database$q5 <- gsub("A", "", database$q5)
database$q6 <- gsub("A", "", database$q6)
database$q7 <- gsub("A", "", database$q7)
database$q8 <- gsub("A", "", database$q8)
database$q9 <- gsub("A", "", database$q9)
database$q10 <- gsub("A", "", database$q10)
database$q11 <- gsub("A", "", database$q11)
database$q12 <- gsub("A", "", database$q12)
database$q13 <- gsub("A", "", database$q13)
database$q14 <- gsub("A", "", database$q14)
database$q15 <- gsub("A", "", database$q15)
database$q16 <- gsub("A", "", database$q16)
database$q17 <- gsub("A", "", database$q17)
database$q18 <- gsub("A", "", database$q18)

#reshape dataset
database <- melt(database, id.vars="ID", measure.vars=c("q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11","q12",
                                                        "q13","q14","q15","q16","q17","q18"))
database <- rename.variable(database, "variable", "QES")
database$QES <- gsub("q", "", database$QES)
database <- rename.variable(database, "value", "choice")


#load design data
design_data_pretest <- read_excel("Y:/Home/fischesa/GONASIP/choice experimente/R/design_orthogonal.xlsx", col_names = TRUE)

#add status quo alternative
design_data_pretest$sq.climate <- rep(0,18)
design_data_pretest$sq.waterq <- rep(0,18)
design_data_pretest$sq.biodiversity <- rep(0,18)
design_data_pretest$sq.flood <- rep(0,18)
design_data_pretest$sq.soiler <- rep(40,18)
design_data_pretest$sq.cost <- rep(0,18)

#data reshaping
design_data_pretest_alt1 <- design_data_pretest[,1:7]
design_data_pretest_alt1$ALT <- rep(1,18)
colnames(design_data_pretest_alt1) <- c("QES","climate","waterq","biodiversity","flood","soiler","cost","ALT")
design_data_pretest_alt2 <- design_data_pretest[,c(1,8:13)]
design_data_pretest_alt2$ALT <- rep(2,18)
colnames(design_data_pretest_alt2) <- c("QES","climate","waterq","biodiversity","flood","soiler","cost","ALT")
design_data_pretest_alt3 <- design_data_pretest[,c(1,15:20)]
design_data_pretest_alt3$ALT <- rep(3,18)
colnames(design_data_pretest_alt3) <- c("QES","climate","waterq","biodiversity","flood","soiler","cost","ALT")
design_data_pretest_long <- rbind(design_data_pretest_alt1,design_data_pretest_alt2,design_data_pretest_alt3)

#merge both datasets
database <- merge(database,design_data_pretest_long)

#rearrange data 
database <- reshape(database, v.names=c("climate","waterq","biodiversity","soiler","flood","cost"), timevar="ALT", 
                    idvar=c("QES","ID"), drop = NULL, direction="wide")
database <- arrange(.data=database,database$ID, .by_group = FALSE)
colnames(database) <- c("QES","ID","choice","alt1.climate","alt1.waterq","alt1.biodiversity","alt1.soiler","alt1.flood","alt1.cost",
                        "alt2.climate","alt2.waterq","alt2.biodiversity","alt2.soiler","alt2.flood","alt2.cost","alt3.climate",
                        "alt3.waterq","alt3.biodiversity","alt3.soiler","alt3.flood","alt3.cost")

#create dummys
database$alt1.climate_0 <- ifelse((database$alt1.climate==0),1,0)
database$alt1.climate_6 <- ifelse((database$alt1.climate==6),1,0)
database$alt1.climate_12 <- ifelse((database$alt1.climate==12),1,0)
database$alt2.climate_0 <- ifelse((database$alt2.climate==0),1,0)
database$alt2.climate_6 <- ifelse((database$alt2.climate==6),1,0)
database$alt2.climate_12 <- ifelse((database$alt2.climate==12),1,0)
database$alt1.waterq_low <- ifelse((database$alt1.waterq==0),1,0)
database$alt1.waterq_better <- ifelse((database$alt1.waterq==1),1,0)
database$alt1.waterq_muchbetter <- ifelse((database$alt1.waterq==2),1,0)
database$alt2.waterq_low <- ifelse((database$alt2.waterq==0),1,0)
database$alt2.waterq_better <- ifelse((database$alt2.waterq==1),1,0)
database$alt2.waterq_muchbetter <- ifelse((database$alt2.waterq==2),1,0)
database$alt1.biodiversity_loss <- ifelse((database$alt1.biodiversity==0),1,0)
database$alt1.biodiversity_stable <- ifelse((database$alt1.biodiversity==1),1,0)
database$alt1.biodiversity_increase <- ifelse((database$alt1.biodiversity==2),1,0)
database$alt2.biodiversity_loss <- ifelse((database$alt2.biodiversity==0),1,0)
database$alt2.biodiversity_stable <- ifelse((database$alt2.biodiversity==1),1,0)
database$alt2.biodiversity_increase <- ifelse((database$alt2.biodiversity==2),1,0)
database$alt1.flood_low <- ifelse((database$alt1.flood==0),1,0)
database$alt1.flood_better <- ifelse((database$alt1.flood==1),1,0)
database$alt1.flood_muchbetter <- ifelse((database$alt1.flood==2),1,0)
database$alt2.flood_low <- ifelse((database$alt2.flood==0),1,0)
database$alt2.flood_better <- ifelse((database$alt2.flood==1),1,0)
database$alt2.flood_muchbetter <- ifelse((database$alt2.flood==2),1,0)
database$alt1.soiler_low <- ifelse((database$alt1.soiler==10),1,0)
database$alt1.soiler_medium <- ifelse((database$alt1.soiler==20),1,0)
database$alt1.soiler_high <- ifelse((database$alt1.soiler==40),1,0)
database$alt2.soiler_low <- ifelse((database$alt2.soiler==10),1,0)
database$alt2.soiler_medium <- ifelse((database$alt2.soiler==20),1,0)
database$alt2.soiler_high <- ifelse((database$alt2.soiler==40),1,0)


# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta=c(asc                     = 0,
              b_climate_6             = 0,
              b_climate_12            = 0,
              b_waterq_better         = 0,
              b_waterq_muchbetter     = 0,
              b_biodiversity_stable   = 0,
              b_biodiversity_increase = 0,
              b_flood_better          = 0,
              b_flood_muchbetter      = 0,
              b_soiler_medium         = 0,
              b_soiler_high           = 0,
              b_cost                  = 0)

### Vector with parameters to be kept fixed at their starting value in apollo_beta
apollo_fixed = c()

#how often did respondents go for the 200€ option?
chosen200 <- subset(database, choice==1 & alt1.cost==200)
chosen200.2 <- subset(database, choice==2 & alt2.cost==200)
#in 21% of the options 200€ options were available they have been chosen

#identify protest responses

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities
  V = list()
  V[["alt1"]] = b_climate_6*alt1.climate_6 + b_climate_12*alt1.climate_12 + 
   b_waterq_better*alt1.waterq_better + b_waterq_muchbetter*alt1.waterq_muchbetter + 
    b_biodiversity_stable*alt1.biodiversity_stable + 
    b_biodiversity_increase*alt1.biodiversity_increase + b_flood_better*alt1.flood_better + 
    b_flood_muchbetter*alt1.flood_muchbetter + b_soiler_medium*alt1.soiler_medium + 
    b_soiler_high*alt1.soiler_high + b_cost*alt1.cost
  V[["alt2"]] = b_climate_6*alt2.climate_6 + b_climate_12*alt2.climate_12 + 
    b_waterq_better*alt2.waterq_better + b_waterq_muchbetter*alt2.waterq_muchbetter + 
   b_biodiversity_stable*alt2.biodiversity_stable + 
    b_biodiversity_increase*alt2.biodiversity_increase + b_flood_better*alt2.flood_better + 
    b_flood_muchbetter*alt2.flood_muchbetter + b_soiler_medium*alt2.soiler_medium + 
    b_soiler_high*alt2.soiler_high + b_cost*alt2.cost
  V[["alt3"]] = asc
  

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

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

#model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
model = apollo_estimate(apollo_beta, apollo_fixed,
                        apollo_probabilities, apollo_inputs, 
                        estimate_settings=list(estimationRoutine = "bfgs",
                                               hessianRoutine = "analytic"))

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model, list(printPVal = TRUE))

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model, list(printPVal = TRUE))

