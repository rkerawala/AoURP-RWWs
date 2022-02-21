#All of Us Researcher Workbench Workspaces: Meta-Analysis, 4/21 1st pull
#Rhea Kerawala

#import libraries
library(readxl)
library(dplyr)

#import workspace description spreadsheet: 4/21 Version
Workspace421 <- read_excel("workspace_descriptions_421.xlsx") #317 

#exclude column: Creation Time, Modified Time, Workspace Status, Contact Email & any columns pertaining to review 
#in this copy, remove column 6,7,8. 
Workspace421 <- as.data.frame(c(Workspace421[,1:5], Workspace421[,9:31]))  

#Step 1: De-duplication
library(tidyverse)
#Removing duplicate workspaces with the same workspace ID, study approach, or study intent.  
Workspace421 <- Workspace421 %>% distinct(Workspace421[4], .keep_all = TRUE) #311
Workspace421 <- Workspace421 %>% distinct(Workspace421[5], .keep_all = TRUE) #311
Workspace421 <- Workspace421 %>% distinct(Workspace421[28], .keep_all = TRUE) #304

#Search for duplicate workspace names
Dups <- filter_all(Workspace421, any_vars(str_detect(Workspace.name, "duplicate")))
View(Dups)

#No dups
rm(Dups)

# Step 2: Filtering for Disease Focused Research (DFR) v non-DFR
#Counts (# of workspaces) is commented next to the line
DFR<- filter(Workspace421, Workspace421[6] == 'Checked' ) #145
NonDFR <- filter(Workspace421, Workspace421[6] == "Unchecked") #159

# Step 3: Counting race/ethnicity
RaceDFR <- filter(DFR, DFR[17] == 'Checked') #57
RacenonDFR <- filter(NonDFR, NonDFR[17] == 'Checked') #54

#Step 4: Counting age
AgeDFR <- filter(DFR, DFR[18] == 'Checked') #39
AgenonDFR <- filter(NonDFR, NonDFR[18] == 'Checked') #50

#Step 5: Populations of Interest (POI)

#Sex at Birth
POIDFR <- filter(DFR, DFR[19] == 'Checked') #12
POInonDFR <- filter(NonDFR, NonDFR[19] == 'Checked') #21

#Gender Identity
POI2DFR <- filter(DFR, DFR[20] == 'Checked') #10
POI2nonDFR <- filter(NonDFR, NonDFR[20] == 'Checked') #26

#Sexual Orientation
POI3DFR <- filter(DFR, DFR[21] == 'Checked') #10
POI3nonDFR <- filter(NonDFR, NonDFR[21] == 'Checked') #23

#Geography
POI4DFR <- filter(DFR, DFR[22] == 'Checked') #24
POI4nonDFR <- filter(NonDFR, NonDFR[22] == 'Checked') #43

#Disability Status
POI5DFR <- filter(DFR, DFR[23] == 'Checked') #11
POI5nonDFR <- filter(NonDFR, NonDFR[23] == 'Checked') #23

#Access to Care
POI6DFR <- filter(DFR, DFR[24] == 'Checked') #26
POI6nonDFR <- filter(NonDFR, NonDFR[24] == 'Checked') #41

#Education Level
POI7DFR <- filter(DFR, DFR[25] == 'Checked') #25
POI7nonDFR <- filter(NonDFR, NonDFR[25] == 'Checked') #35

#Fed Pov Level
POI8DFR <- filter(DFR, DFR[26] == 'Checked') #29
POI8nonDFR <- filter(NonDFR, NonDFR[26] == 'Checked') #43

#Others
POI9DFR <- filter(DFR, DFR[27] == 'Checked') #3
POI9nonDFR <- filter(NonDFR, NonDFR[27] == 'Checked') #0

#Combine all POI counts into 1 dataframe. 
POI<- data.frame(c('Race and Ethnicity','Age','Sex at Birth','Gender','Sexual Orientation','Geography', 'Disability Status', 'Access to Care','Education','Income Level','Others'))

#DFR
POI.DFR <- as.data.frame(nrow(RaceDFR))
POI.DFR <- rbind(POI.DFR,(nrow(AgeDFR)),(nrow(POIDFR)),(nrow(POI2DFR)),(nrow(POI3DFR)),(nrow(POI4DFR)),(nrow(POI5DFR)),(nrow(POI6DFR)),(nrow(POI7DFR)),
                 (nrow(POI8DFR)),(nrow(POI9DFR)))
#nonDFR
POI.nonDFR <- as.data.frame((nrow(RacenonDFR)))
POI.nonDFR <- rbind(POI.nonDFR,(nrow(AgenonDFR)),(nrow(POInonDFR)),(nrow(POI2nonDFR)),(nrow(POI3nonDFR)),(nrow(POI4nonDFR)),(nrow(POI5nonDFR)),(nrow(POI6nonDFR)),(nrow(POI7nonDFR)),
                    (nrow(POI8nonDFR)),(nrow(POI9nonDFR)))

POI<- cbind(POI, POI.DFR, POI.nonDFR)
colnames(POI) = c('Population of Interest','DFR', 'nonDFR')
View(POI)

#clean env
rm(POI.DFR, POI.nonDFR)
rm(POIDFR, POInonDFR, POI2DFR,POI3DFR, POI4DFR, POI5DFR, POI6DFR, POI7DFR, POI8DFR, POI9DFR, POI2nonDFR, POI3nonDFR, POI4nonDFR, POI5nonDFR, POI6nonDFR,POI7nonDFR,POI8nonDFR,POI9nonDFR)

# Step 6: Research Purposes, same method as step 5.
#Methods/Validation
RP10DFR <- filter(DFR, DFR[7] == 'Checked') #23
RP10nonDFR <- filter(NonDFR, NonDFR[7] == 'Checked') #42

#Research Control
RP11DFR <- filter(DFR, DFR[8] == 'Checked') #11
RP11nonDFR <- filter(NonDFR, NonDFR[8] == 'Checked') #5

#Genetic Research
RP12DFR <- filter(DFR, DFR[9] == 'Checked') #26
RP12nonDFR <- filter(NonDFR, NonDFR[9] == 'Checked') #14

#Social Behavioral Research
RP13DFR <- filter(DFR, DFR[10] == 'Checked') #18
RP13nonDFR <- filter(NonDFR, NonDFR[10] == 'Checked') #26

#Population Health/Public Health
RP14DFR <- filter(DFR, DFR[11] == 'Checked') #39
RP14nonDFR <- filter(NonDFR, NonDFR[11] == 'Checked') #61

#Drug Therapeutics/Dev Research
RP15DFR <- filter(DFR, DFR[12] == 'Checked') #9
RP15nonDFR <- filter(NonDFR, NonDFR[12] == 'Checked') #5

#Commercial
RP16DFR <- filter(DFR, DFR[13] == 'Checked') #1
RP16nonDFR <- filter(NonDFR, NonDFR[13] == 'Checked') #0

#Educational
RP17DFR <- filter(DFR, DFR[14] == 'Checked') #8
RP17nonDFR <- filter(NonDFR, NonDFR[14] == 'Checked') #49

#ELSI
RP18DFR <- filter(DFR, DFR[15] == 'Checked') #2
RP18nonDFR <- filter(NonDFR, NonDFR[15] == 'Checked') #4

#Other Purposes
RP19DFR <- filter(DFR, DFR[16] == 'Checked') #8
RP19nonDFR <- filter(NonDFR, NonDFR[16] == 'Checked') #23

#Combine into 1 dataframe. 
RP<- data.frame(c('Methods/Validation','Research Control','Genetic','Social/Behavioral', 'Population/Public Health','Drug Therapeutics/Dev Research', 
                  'Commerical','Educational','ELSI','Other RP'))

#DFR
RP.DFR <- as.data.frame((nrow(RP10DFR)))
RP.DFR <- rbind(RP.DFR,(nrow(RP11DFR)),(nrow(RP12DFR)),(nrow(RP13DFR)),(nrow(RP14DFR)),(nrow(RP15DFR)),(nrow(RP16DFR)),
                (nrow(RP17DFR)),(nrow(RP18DFR)),(nrow(RP19DFR)))
#nonDFR
RP.nonDFR <- as.data.frame((nrow(RP10nonDFR)))
RP.nonDFR <- rbind(RP.nonDFR,(nrow(RP11nonDFR)),(nrow(RP12nonDFR)),(nrow(RP13nonDFR)),(nrow(RP14nonDFR)),(nrow(RP15nonDFR)),(nrow(RP16nonDFR)),
                   (nrow(RP17nonDFR)),(nrow(RP18nonDFR)),(nrow(RP19nonDFR)))

RP<- cbind(RP, RP.DFR, RP.nonDFR)
colnames(RP) = c('Research Purpose','DFR', 'nonDFR')
View(RP)

#Step 7: Counting Institutions

#Group by Institution, count the number of entries, manually reviewed as well.
Institutions<- Workspace421 %>% group_by(Institutional.Affiliation) %>% summarise('# of Workspaces per Institution'=n())
View(Institutions)

#Step 8: Fisher's Exact test 
#DFR v. Non-DFR for Race/Ethnicity and Age

View(POI)         #[1,2] & [1,3] are our race/ethnicity focused workspaces

View(DFR) #tells us there are 145 total DFR workspaces
View(NonDFR) #159 non-DFR workspaces

#Non-Race/Ethnicity focused workspaces
DFRdiff1<- 145-POI[1,2]
DFRdiff2<- 159-POI[1,3]

#Create a data-frame to do Fisher's test on
race  <- matrix(c(POI[1,2],POI[1,3],DFRdiff1,DFRdiff2,145,159), ncol=2, byrow=TRUE) 
race  <- as.data.frame(race)
View(race)
Fisher <- fisher.test(race[1:2,])

#Comparing %s to p-values
race2<- matrix(c(race[1,1]/race[3,1]*100, race[1,2]/race[3,2]*100), ncol=2, byrow=TRUE)
race2<- cbind(race2, Fisher$p.value)
colnames(race2)<- c('% DFR', '% non-DFR', 'P-value')
View(race2)

#Repeat for age
View(POI)         #[2,2] & [2,3] are our age focused workspaces

#Non-Age focused workspaces
DFRdiff3<- 145-POI[2,2]
DFRdiff4<- 159-POI[2,3]

age <- matrix(c(POI[2,2], POI[2,3], DFRdiff3, DFRdiff4,145,159), ncol=2, byrow=TRUE)
age <- as.data.frame(age)

Fisher2 <- fisher.test(age)

age2<- matrix(c(age[1,1]/age[3,1]*100, age[1,2]/age[3,2]*100), ncol=2, byrow=TRUE)

age2<- cbind(age2, Fisher2$p.value)
colnames(age2)<- c("%DFR","%non-DFR","age p-value")
View(age2)

#save the dataframes as a csv file for export.
write.csv(Institutions, file = "Institutions_421.csv", quote = FALSE, row.names = F)
write.csv(POI, file = "Population.of.Interest_421.csv", quote = FALSE, row.names = FALSE)
write.csv(RP, file = "Research.Purpose_421.csv", quote = FALSE, row.names = FALSE)

# Age2 & Race2 were not exported.

#Step 9: Running search for substrings in R, (case-sensitive)
library(tidyr)

#clear up environment
rm(RP10DFR,RP11DFR,RP13DFR,RP14DFR,RP15DFR,RP15nonDFR,RP14nonDFR,RP13nonDFR,RP11nonDFR,RP10nonDFR, RP16DFR, RP17DFR, RP18DFR, RP19DFR,RP16nonDFR,RP17nonDFR, RP18nonDFR, RP19nonDFR)

#Disease Categories
#Search for 'hypertension'. Count:9
s <- DFR[2:5]
#loop
search_terms <- c('Hypertension','hypertension','hypertensive','Hypertensive')
for(i in search_terms) {
  i1  <- grepl(i, s$Workspace.name) 
  s$Counts[i1] <- "1" 
  i3 <- grepl(i, s$Study.Approach)
  s$Counts[i3] <- "1" 
  i4 <- grepl(i, s$Study.Intent)
  s$Counts[i4] <- "1"
}

#Counts
s[is.na(s)] <- 0
s$Counts <- as.numeric(s$Counts)
sum(s[,5])

#Table
Counts<- data.frame(sum(s[,5]))
colnames(Counts) = c('Counts')
View(Counts)

#Obesity, 7
s<- DFR[2:5]

#loop
search_terms <- c('Obesity','obesity','adiposity','fatty tissue hyperplasia','MOMO syndrome','MORM syndrome','proopiomelanocortin deficiency syndrome')
for(i in search_terms) {
  i1  <- grepl(i, s$Workspace.name)
  s$Counts[i1] <- "1"
  i3 <- grepl(i, s$Study.Approach)
  s$Counts[i3] <- "1"
  i4 <- grepl(i, s$Study.Intent)
  s$Counts[i4] <- "1"
}

#Counts
s$Counts <- as.numeric(s$Counts)
s[is.na(s)] <- 0
sum(s[,5])

Counts <- rbind(Counts, sum(s[,5])) 

#COVID-19. Count: 10
s<- DFR[2:5]
#loop
search_terms <- c('covid','sars-cov-2','coronavirus','Coronavirus','COVID', 'SARS','SARS-CoV-2','Sars-Cov-2','SARSCoV2','severe acute respiratory syndrome')
for(i in search_terms) {
  i1  <- grepl(i, s$Workspace.name)
  s$Counts[i1] <- "1"
  i3 <- grepl(i, s$Study.Approach)
  s$Counts[i3] <- "1"
  i4 <- grepl(i, s$Study.Intent)
  s$Counts[i4] <- "1"
}

#Counts
s$Counts <- as.numeric(s$Counts)
s[is.na(s)] <- 0

sum(s[,5])
Counts <- rbind(Counts, sum(s[,5])) 

#Diabetes, Count: 27
s<- DFR[2:5]
#loop
search_terms <- c('Diabetes','diabetes','diabetic','Diabetic')
for(i in search_terms) {
  i1  <- grepl(i, s$Workspace.name)
  s$Counts[i1] <- "1"
  i3 <- grepl(i, s$Study.Approach)
  s$Counts[i3] <- "1"
  i4 <- grepl(i, s$Study.Intent)
  s$Counts[i4] <- "1"
}

#Counts
s$Counts <- as.numeric(s$Counts)
s[is.na(s)] <- 0
sum(s[,5])
Counts <- rbind(Counts, sum(s[,5])) 

#Cancer & Tumors, Count: 29
#tumor(s), tumor (types), benign (tumors), oncolog(y)
s<- DFR[2:5]
#loop
search_terms<- c('Oncolog','Tumor','Cancer','oncolog', 'tumor','cancer','coma','toma','phoma','kemia','malignan','Malignan','Neoplasm','neoplasm')
for(i in search_terms) {
  i1  <- grepl(i, s$Workspace.name)
  s$Counts[i1] <- "1"
  i3 <- grepl(i, s$Study.Approach)
  s$Counts[i3] <- "1"
  i4 <- grepl(i, s$Study.Intent)
  s$Counts[i4] <- "1"
}

#Counts
s[is.na(s)] <- 0
s$Counts <- as.numeric(s$Counts)
sum(s[,5])
Counts <- rbind(Counts, sum(s[,5])) 

#Reproductive Issues: pregnancy, labor, endometriosis, menstrua, reproduc, fetal, PCOS Count: 19
s<- DFR[2:5]
#loop
search_terms<- c('Pregnan','pregnan','Labor','labor','Endometri','endometri','Menstrua','menstrua','Birth','birth','Reproduc','reproduc','Fetal','fetal','PCOS','poly cystic ovarian syndrome', 'Fertil','fertil','amnio','Amnio')
for(i in search_terms) {
  i1  <- grepl(i, s$Workspace.name)
  s$Counts[i1] <- "1"
  i3 <- grepl(i, s$Study.Approach)
  s$Counts[i3] <- "1"
  i4 <- grepl(i, s$Study.Intent)
  s$Counts[i4] <- "1"
}

#counts
s$Counts <- as.numeric(s$Counts)
s[is.na(s)] <- 0

sum(s[,5])
Counts <- rbind(Counts, sum(s[,5])) 

#Mental Health, Neurological Concerns, Cognitive Impairment: Count: 38
s<- DFR[2:5]
#loop
search_terms<- c('Psychiatric','Psychological','psychological','psychiatric', 'attention deficit','autism','Autism','Neurodev','neurodevelopmental','ADHD','alzheimer',"Alzheimer",'Depressi', 'depressi','Anxiety','anxiety','Mental','mental','Cogniti','cogniti', 'Dementia','dementia','Brain','brain','Mood','mood','cerebral',"Cerebral",
                 'psychosocial', 'psychopathic', 'intellectual disability',
                 'neuronal', 'Synder-Robinson','WAGR','Baraitser-Winter','encephalopathy','Parkinsonism',
                 'MASA', 'Partington', 'GMS', 'MORM', 'cerebellar ataxia', 'cranial', 'TBI', 'concussion',
                 'CSF','choroid plexus','CNS', 'nervous system','nerve')
for(i in search_terms) {
  i1  <- grepl(i, s$Workspace.name)
  s$Counts[i1] <- "1"
  i3 <- grepl(i, s$Study.Approach)
  s$Counts[i3] <- "1"
  i4 <- grepl(i, s$Study.Intent)
  s$Counts[i4] <- "1"
}

#counts
s$Counts <- as.numeric(s$Counts)
s[is.na(s)] <- 0

sum(s[,5])
Counts <- rbind(Counts, sum(s[,5])) 

#For Cardio: Cardiovascular Disease, CVD, Congestive Heart Failure, Cardiometabolic, ASCVD, A-fibrillation,, stroke, Count: 42
s<- DFR[2:5]
#loop
search_terms<- c('CVD','Heart','heart','Cardi','cardi','Stroke','stroke','fibrillation','coronary',"Coronary",'Carotid','carotid','arterial','brachycardia','myocardial', 'pericarditis','coronary arteriosclerosis','cardiac chamber','atrioventricular','beriberi','CVS','ectopic cordis', 'cor polmonale',
                 'resting ischaemia', 'angina','TARP','Pierre Robin','Grange syndrome','PHAVER','pterygia','protodiastolic gallop','congenital valvular insufficiency', 'congenital valve disease',
                 'aortic/MAVD','CHARGE','Lev syndrome','neurocirculatory asthenia','syncope','circulatory')
for(i in search_terms) {
  i1  <- grepl(i, s$Workspace.name)
  s$Counts[i1] <- "1"
  i3 <- grepl(i, s$Study.Approach)
  s$Counts[i3] <- "1"
  i4 <- grepl(i, s$Study.Intent)
  s$Counts[i4] <- "1"
}

#counts
s$Counts <- as.numeric(s$Counts)
s[is.na(s)] <- 0

sum(s[,5])
Counts <- rbind(Counts, sum(s[,5])) 

#Respiratory includes: asthma, respiratory failure, COPD, sinusitis, 10
s<- DFR[2:5]
#loop
search_terms<- c('asthma','respiratory','COPD','sinusitis','Asthma','Respiratory','Sinusitis','Inhal','inhal','breath','Breath','tuberculosis','Tuberculosis','atelectasis','wheez','airway','inspiratory','expiratory','dyspnea','dyspnoea')
for(i in search_terms) {
  i1  <- grepl(i, s$Workspace.name)
  s$Counts[i1] <- "1"
  i3 <- grepl(i, s$Study.Approach)
  s$Counts[i3] <- "1"
  i4 <- grepl(i, s$Study.Intent)
  s$Counts[i4] <- "1"
}

#counts
s$Counts <- as.numeric(s$Counts)
s[is.na(s)] <- 0

sum(s[,5])
Counts <- rbind(Counts, sum(s[,5])) 

#Immune: inflammation, infectious disease, SLE, lupus, autoimmune, sepsis, allergies Counts: 21
s<- DFR[2:5]
#loop
search_terms <- c('inflammat','infectio','SLE','lupus','autoimmune','sepsis','allerg','Inflammat','Infectio','Lupus','Autoimmune','Sepsis','Allerg',"Multiple Sclerosis","multiple sclerosis","MS",'AIDS','viral','Viral','B-cell','T-cell')
for(i in search_terms) {
  i1  <- grepl(i, s$Workspace.name)
  s$Counts[i1] <- "1"
  i3 <- grepl(i, s$Study.Approach)
  s$Counts[i3] <- "1"
  i4 <- grepl(i, s$Study.Intent)
  s$Counts[i4] <- "1"
}

#counts
s$Counts <- as.numeric(s$Counts)
s[is.na(s)] <- 0

sum(s[,5])
Counts <- rbind(Counts, sum(s[,5])) 

#Sleep: insomnia, narcolepsy, sleep, apnea, Counts: 12
s<- DFR[2:5]
#loop
search_terms <- c('insomnia','narcolepsy','sleep','Insomnia','Narcolepsy','Sleep','somniloquism','Somniloquism','drowsy','Drowsy','somnia','Somnia','Somnabulism','somnabulism','nocturnal epilepsy','Nocturnal epilepsy')
for(i in search_terms) {
  i1  <- grepl(i, s$Workspace.name)
  s$Counts[i1] <- "1"
  i3 <- grepl(i, s$Study.Approach)
  s$Counts[i3] <- "1"
  i4 <- grepl(i, s$Study.Intent)
  s$Counts[i4] <- "1"
}

#counts
s$Counts <- as.numeric(s$Counts)
s[is.na(s)] <- 0

sum(s[,5])
Counts <- rbind(Counts, sum(s[,5])) 

#Musculoskeletal: back pain, arthritis, postop osteoly, hernia, ACL, NAFLD, liver disease
#Count: 14
s<- DFR[2:5]
#loop
search_terms<- c('back', 'arthritis','postop','hernia','ACL','NAFLD','liver disease','Back','Arthritis','Postop','Hernia','muscle','Liver','Muscular','muscular','truncal','Truncal','trunk','Trunk','Limb','limb','Mobility','mobility','thoracic','Thoracic')
for(i in search_terms) {
  i1  <- grepl(i, s$Workspace.name)
  s$Counts[i1] <- "1"
  i3 <- grepl(i, s$Study.Approach)
  s$Counts[i3] <- "1"
  i4 <- grepl(i, s$Study.Intent)
  s$Counts[i4] <- "1"
}

#counts
s$Counts <- as.numeric(s$Counts)
s[is.na(s)] <- 0

sum(s[,5])
Counts <- rbind(Counts, sum(s[,5])) 

#GI, endocrine: CKD, GI, gastrointestinal, fecal, malnutri, kidney stones
#Count: 17
s<- DFR[2:5]
#loop
search_terms<- c('GI', 'CKD','gastrointestinal','fecal','malnutrition','kidney','endocrin','Gastrointestinal','Fecal','Malnutrition','Kidney','Endocrine','ulcer','Ulcer','Renal','renal','digest','Digest')
for(i in search_terms) {
  i1  <- grepl(i, s$Workspace.name)
  s$Counts[i1] <- "1"
  i3 <- grepl(i, s$Study.Approach)
  s$Counts[i3] <- "1"
  i4 <- grepl(i, s$Study.Intent)
  s$Counts[i4] <- "1"
}

#counts
s$Counts <- as.numeric(s$Counts)
s[is.na(s)] <- 0

sum(s[,5])
Counts <- rbind(Counts, sum(s[,5])) 

#Dermatology: neurofibromatosis, psoriasis, psoriatic disease, Count: 9
s<- DFR[2:5]
#loop
search_terms<- c('neurofibromatosis', 'psoria','skin', 'dermatolog','Neurofibromatosis','neurofibromas','Neurofibromas','Psoria','Skin','Dermatolog',
                 "rash",'syringoma','telangiectasia','sarcoidosis','pigmentation','skin trauma','scar','keloid','tumour','graft','photosensitivity','dermatitis','nevus of skin','dysplastic','skin bends','cutaneous','POEMS syndrome','chapping','eczema','spots','eccrine',
                 'serpiginosium','pruritic rash','erythema','rhytidosis facialis','pilonidal abscess','CAMOS','dermatosis','burn','Mehes syndrome','keratosis','hyperesthesia','itch','warts','alopecia','Ehler-Danlos','panronychia')
for(i in search_terms) {
  i1  <- grepl(i, s$Workspace.name)
  s$Counts[i1] <- "1"
  i3 <- grepl(i, s$Study.Approach)
  s$Counts[i3] <- "1"
  i4 <- grepl(i, s$Study.Intent)
  s$Counts[i4] <- "1"
}

#counts
s$Counts <- as.numeric(s$Counts)
s[is.na(s)] <- 0

sum(s[,5])
Counts <- rbind(Counts, sum(s[,5])) 

Diseases<- c("Hypertension", "Obesity","COVID-19","Diabetes",
             "Cancer & Tumors","Reproductive Concerns","Mental Health/Neurodev Issues",
             "Cardiovascular-related Issues","Respiratory-related Issues", "Immunology","Sleep","Musculoskeletal","GI & Endocrine","Dermatology")
Counts <- cbind(Counts, Diseases)
Counts <- Counts %>% relocate ('Diseases', .before = 'Counts')
View(Counts)

write.csv(Counts, file = "Disease Category_421.csv", quote = FALSE, row.names = F)

#Note: s below has not been reset w/ each disease category.
#Other Disease Category
s$Counts <- as.numeric(s$Counts)
s[is.na(s)] <- 0
s_other <- filter(s, Counts == 0)
#7 belong to the other disease category

library(dplyr)
View(DFR)

#######################################################################
#Genomics Highlight

#Search for 'hypertension'. 
s <- RP12DFR[2:5]

#loop
search_terms <- c('Hypertension','hypertension', 'hypertensive','Hypertensive')
for(i in search_terms) {
  i1  <- grepl(i, s$Workspace.name)
  s$Counts[i1] <- "1"
  i3 <- grepl(i, s$Study.Approach)
  s$Counts[i3] <- "1"
  i4 <- grepl(i, s$Study.Intent)
  s$Counts[i4] <- "1"
}

#Counts
s[is.na(s)] <- 0
s$Counts <- as.numeric(s$Counts)
sum(s[,5])

#Table
Counts<- data.frame(sum(s[,5]))
colnames(Counts) = c('Counts')
View(Counts)

#Obesity, 
s<- RP12DFR[2:5]

#loop
search_terms <- c('Obesity','obesity','adiposity','fatty tissue hyperplasia','MOMO syndrome','MORM syndrome','proopiomelanocortin deficiency syndrome')
for(i in search_terms) {
  i1  <- grepl(i, s$Workspace.name)
  s$Counts[i1] <- "1" 
  i3 <- grepl(i, s$Study.Approach)
  s$Counts[i3] <- "1"
  i4 <- grepl(i, s$Study.Intent)
  s$Counts[i4] <- "1"
}

#Counts
s$Counts <- as.numeric(s$Counts)
s[is.na(s)] <- 0
sum(s[,5])

Counts <- rbind(Counts, sum(s[,5])) 

#COVID-19. Count: 
s<- RP12DFR[2:5]
#loop
search_terms <- c('covid','sars-cov-2','coronavirus','Coronavirus','COVID', 'SARS','SARS-CoV-2','Sars-Cov-2','SARSCoV2','severe acute respiratory syndrome')
for(i in search_terms) {
  i1  <- grepl(i, s$Workspace.name)
  s$Counts[i1] <- "1"
  i3 <- grepl(i, s$Study.Approach)
  s$Counts[i3] <- "1"
  i4 <- grepl(i, s$Study.Intent)
  s$Counts[i4] <- "1"
}

#Counts
s$Counts <- as.numeric(s$Counts)
s[is.na(s)] <- 0

sum(s[,5])
Counts <- rbind(Counts, sum(s[,5])) 

#Diabetes,
s<- RP12DFR[2:5]
#loop
search_terms <- c('Diabetes','diabetes','diabetic','Diabetic')
for(i in search_terms) {
  i1  <- grepl(i, s$Workspace.name)
  s$Counts[i1] <- "1"
  i3 <- grepl(i, s$Study.Approach)
  s$Counts[i3] <- "1"
  i4 <- grepl(i, s$Study.Intent)
  s$Counts[i4] <- "1"
}

#Counts
s$Counts <- as.numeric(s$Counts)
s[is.na(s)] <- 0
sum(s[,5])
Counts <- rbind(Counts, sum(s[,5])) 

#Cancer & Tumors,
#tumor(s), tumor (types), benign (tumors), oncolog(y)
#Count: 
s<- RP12DFR[2:5]
#loop
search_terms<- c('Oncolog','Tumor','Cancer','oncolog', 'tumor','cancer','coma','toma','phoma','kemia','malignan','Malignan','Neoplasm','neoplasm')
for(i in search_terms) {
  i1  <- grepl(i, s$Workspace.name)
  s$Counts[i1] <- "1"
  i3 <- grepl(i, s$Study.Approach)
  s$Counts[i3] <- "1"
  i4 <- grepl(i, s$Study.Intent)
  s$Counts[i4] <- "1"
}

#counts
s$Counts <- as.numeric(s$Counts)
s[is.na(s)] <- 0

sum(s[,5])
Counts <- rbind(Counts, sum(s[,5])) 

#Reproductive Issues: pregnancy, labor, endometriosis, menstrua, reproduc, fetal, PCOS 
#Count: 
s<- RP12DFR[2:5]
#loop
search_terms<- c('Pregnan','pregnan','Labor','labor','Endometri','endometri','Menstrua','menstrua','Birth','birth','Reproduc','reproduc','Fetal','fetal','PCOS','poly cystic ovarian syndrome', 'Fertil','fertil','amnio','Amnio')
for(i in search_terms) {
  i1  <- grepl(i, s$Workspace.name)
  s$Counts[i1] <- "1"
  i3 <- grepl(i, s$Study.Approach)
  s$Counts[i3] <- "1"
  i4 <- grepl(i, s$Study.Intent)
  s$Counts[i4] <- "1"
}

#counts
s$Counts <- as.numeric(s$Counts)
s[is.na(s)] <- 0

sum(s[,5])
Counts <- rbind(Counts, sum(s[,5])) 

#Mental Health, Neurological Concerns, Cognitive Impairment: Count: 
##Includes psychiatric disorders, ADHD, neuropsychiatric study, ADRD/AD, depression/anxiety, mental health, cognitive, dementia, traumatic brain injury, MDD, brain
#Count: 
s<- RP12DFR[2:5]
#loop
search_terms<- c('Psychiatric','Psychological','psychological','psychiatric', 'attention deficit','autism','Autism','Neurodev','neurodevelopmental','ADHD','alzheimer',"Alzheimer",'Depressi', 'depressi','Anxiety','anxiety','Mental','mental','Cogniti','cogniti', 'Dementia','dementia','Brain','brain','Mood','mood','cerebral',"Cerebral",
                 'psychosocial', 'psychopathic', 'intellectual disability',
                 'neuronal', 'Synder-Robinson','WAGR','Baraitser-Winter','encephalopathy','Parkinsonism',
                 'MASA', 'Partington', 'GMS', 'MORM', 'cerebellar ataxia', 'cranial', 'TBI', 'concussion',
                 'CSF','choroid plexus','CNS', 'nervous system','nerve'
)
for(i in search_terms) {
  i1  <- grepl(i, s$Workspace.name)
  s$Counts[i1] <- "1"
  i3 <- grepl(i, s$Study.Approach)
  s$Counts[i3] <- "1"
  i4 <- grepl(i, s$Study.Intent)
  s$Counts[i4] <- "1"
}

#counts
s$Counts <- as.numeric(s$Counts)
s[is.na(s)] <- 0

sum(s[,5])
Counts <- rbind(Counts, sum(s[,5])) 

#For Cardio: Cardiovascular Disease, CVD, Congestive Heart Failure, Cardiometabolic, ASCVD, A-fibrillation,, stroke
#Count: 
s<- RP12DFR[2:5]

#loop
search_terms<- c('CVD','Heart','heart','Cardi','cardi','Stroke','stroke','fibrillation','coronary',"Coronary",'Carotid','carotid','arterial','brachycardia','myocardial', 'pericarditis','coronary arteriosclerosis','cardiac chamber','atrioventricular','beriberi','CVS','ectopic cordis', 'cor polmonale',
                 'resting ischaemia', 'angina','TARP','Pierre Robin','Grange syndrome','PHAVER','pterygia','protodiastolic gallop','congenital valvular insufficiency', 'congenital valve disease',
                 'aortic/MAVD','CHARGE','Lev syndrome','neurocirculatory asthenia','syncope','circulatory'
)                 
for(i in search_terms) {
  i1  <- grepl(i, s$Workspace.name)
  s$Counts[i1] <- "1"
  i3 <- grepl(i, s$Study.Approach)
  s$Counts[i3] <- "1"
  i4 <- grepl(i, s$Study.Intent)
  s$Counts[i4] <- "1"
}

#counts
s$Counts <- as.numeric(s$Counts)
s[is.na(s)] <- 0

sum(s[,5])
Counts <- rbind(Counts, sum(s[,5])) 

#Respiratory includes: asthma, respiratory failure, COPD, sinusitis
#Count: 
s<- RP12DFR[2:5]
#loop
search_terms<- c('asthma','respiratory','COPD','sinusitis','Asthma','Respiratory','Sinusitis','Inhal','inhal','breath','Breath','tuberculosis','Tuberculosis','atelectasis','wheez','airway','inspiratory','expiratory','dyspnea','dyspnoea')
for(i in search_terms) {
  i1  <- grepl(i, s$Workspace.name)
  s$Counts[i1] <- "1"
  i3 <- grepl(i, s$Study.Approach)
  s$Counts[i3] <- "1"
  i4 <- grepl(i, s$Study.Intent)
  s$Counts[i4] <- "1"
}

#counts
s$Counts <- as.numeric(s$Counts)
s[is.na(s)] <- 0

sum(s[,5])
Counts <- rbind(Counts, sum(s[,5])) 

#Immune: inflammation, infectious disease, SLE, lupus, autoimmune, sepsis, allergies Counts: 
s<- RP12DFR[2:5]
#loop
search_terms <- c('inflammat','infectio','SLE','lupus','autoimmune','sepsis','allerg','Inflammat','Infectio','Lupus','Autoimmune','Sepsis','Allerg',"Multiple Sclerosis","multiple sclerosis","MS",'AIDS','viral','Viral','B-cell','T-cell')
for(i in search_terms) {
  i1  <- grepl(i, s$Workspace.name)
  s$Counts[i1] <- "1"
  i3 <- grepl(i, s$Study.Approach)
  s$Counts[i3] <- "1"
  i4 <- grepl(i, s$Study.Intent)
  s$Counts[i4] <- "1"
}

#counts
s$Counts <- as.numeric(s$Counts)
s[is.na(s)] <- 0

sum(s[,5])
Counts <- rbind(Counts, sum(s[,5])) 

#Sleep: insomnia, narcolepsy, sleep, apnea, Counts: 
s<- RP12DFR[2:5]
#loop
search_terms <- c('insomnia','narcolepsy','sleep','Insomnia','Narcolepsy','Sleep','somniloquism','Somniloquism','drowsy','Drowsy','somnia','Somnia','Somnabulism','somnabulism','nocturnal epilepsy','Nocturnal epilepsy')
for(i in search_terms) {
  i1  <- grepl(i, s$Workspace.name)
  s$Counts[i1] <- "1"
  i3 <- grepl(i, s$Study.Approach)
  s$Counts[i3] <- "1"
  i4 <- grepl(i, s$Study.Intent)
  s$Counts[i4] <- "1"
}

#counts
s$Counts <- as.numeric(s$Counts)
s[is.na(s)] <- 0

sum(s[,5])
Counts <- rbind(Counts, sum(s[,5])) 

#Musculoskeletal: back pain, arthritis, postop osteoly, hernia, ACL, NAFLD, liver disease
#Count: 
s<- RP12DFR[2:5]
#loop
search_terms<- c('back', 'arthritis','postop','hernia','ACL','NAFLD','liver disease','Back','Arthritis','Postop','Hernia','muscle','Liver','Muscular','muscular','truncal','Truncal','trunk','Trunk','Limb','limb','Mobility','mobility','thoracic','Thoracic')
for(i in search_terms) {
  i1  <- grepl(i, s$Workspace.name)
  s$Counts[i1] <- "1"
  i3 <- grepl(i, s$Study.Approach)
  s$Counts[i3] <- "1"
  i4 <- grepl(i, s$Study.Intent)
  s$Counts[i4] <- "1"
}

#counts
s$Counts <- as.numeric(s$Counts)
s[is.na(s)] <- 0

sum(s[,5])
Counts <- rbind(Counts, sum(s[,5])) 

#GI, endocrine: CKD, GI (if capital), gastrointestinal, fecal, malnutri, kidney stones
#Count: 
s<- RP12DFR[2:5]
#loop
search_terms<- c('GI', 'CKD','gastrointestinal','fecal','malnutrition','kidney','endocrin','Gastrointestinal','Fecal','Malnutrition','Kidney','Endocrine','ulcer','Ulcer','Renal','renal','digest','Digest')
for(i in search_terms) {
  i1  <- grepl(i, s$Workspace.name)
  s$Counts[i1] <- "1"
  i3 <- grepl(i, s$Study.Approach)
  s$Counts[i3] <- "1"
  i4 <- grepl(i, s$Study.Intent)
  s$Counts[i4] <- "1"
}

#counts
s$Counts <- as.numeric(s$Counts)
s[is.na(s)] <- 0

sum(s[,5])
Counts <- rbind(Counts, sum(s[,5])) 

#Dermatology: neurofibromatosis, psoriasis, psoriatic disease, Count: 
s<- RP12DFR[2:5]
#loop
search_terms<- c('neurofibromatosis', 'psoria','skin', 'dermatolog','Neurofibromatosis','neurofibromas','Neurofibromas','Psoria','Skin','Dermatolog',
                 "rash",'syringoma','telangiectasia','sarcoidosis','pigmentation','skin trauma','scar','keloid','tumour','graft','photosensitivity','dermatitis','nevus of skin','dysplastic','skin bends','cutaneous','POEMS syndrome','chapping','eczema','spots','eccrine',
                 'serpiginosium','pruritic rash','erythema','rhytidosis facialis','pilonidal abscess','CAMOS','dermatosis','burn','Mehes syndrome','keratosis','hyperesthesia','itch','warts','alopecia','Ehler-Danlos','panronychia')

for(i in search_terms) {
  i1  <- grepl(i, s$Workspace.name)
  s$Counts[i1] <- "1"
  i3 <- grepl(i, s$Study.Approach)
  s$Counts[i3] <- "1"
  i4 <- grepl(i, s$Study.Intent)
  s$Counts[i4] <- "1"
}


#counts
s$Counts <- as.numeric(s$Counts)
s[is.na(s)] <- 0

sum(s[,5])
Counts <- rbind(Counts, sum(s[,5])) 


Diseases<- c("Hypertension", "Obesity","COVID-19","Diabetes",
             "Cancer & Tumors","Reproductive Concerns","Mental Health, Neurodev Issues",
             "Cardiovascular-related Issues","Respiratory-related Issues", "Immunology","Sleep","Musculoskeletal","GI & Endocrine","Dermatology")
Counts <- cbind(Counts, Diseases)
Counts <- Counts %>% relocate ('Diseases', .before = 'Counts')
View(Counts)

#save and export
write.csv(Counts, file = "Disease Category.Genomics_421.csv", quote = FALSE, row.names = F)

# #Demonstration 
# s1<- as.data.frame(DFR[2:5])
# #s1<- s1 %>% drop_na
# #loop
# search_terms<- c('Demonstration',
#                  'demonstration','tutorial','Tutorial')
# for(i in search_terms) {
#   i1  <- grepl(i, s1$Workspace.name)
#   s1$Counts[i1] <- "1"
#   i2 <- grepl(i, s1$Institutional.Affiliation)
#   s1$Counts[i2] <- "1"
#   i3 <- grepl(i, s1$Study.Approach)
#   s1$Counts[i3] <- "1"
#   i4 <- grepl(i, s1$Study.Intent)
#   s1$Counts[i4] <- "1"
# }
# 
# #counts, 3
# s1$Counts <- as.numeric(s1$Counts)
# s1[is.na(s1)] <- 0
# 
# sum(s1[,5])
# 
# #3 in s1 that fit count = 1
# DFR_demo <- filter(s1, Counts == 1 & Institutional.Affiliation != 'AouOps')
# write.csv(DFR_demo, file = "DFR_demo_421.csv", quote = FALSE, row.names = F)
# 
# #For non-DFR
# s2<- as.data.frame(NonDFR[2:5])
# 
# #loop
# search_terms<- c('Demonstration',
#                  'demonstration','tutorial',"Tutorial")
# for(i in search_terms) {
#   i1  <- grepl(i, s2$Workspace.name)
#   s2$Counts[i1] <- "1"
#   i2 <- grepl(i, s2$Institutional.Affiliation)
#   s2$Counts[i2] <- "1"
#   i3 <- grepl(i, s2$Study.Approach)
#   s2$Counts[i3] <- "1"
#   i4 <- grepl(i, s2$Study.Intent)
#   s2$Counts[i4] <- "1"
# }
# 
# #counts
# s2$Counts <- as.numeric(s2$Counts)
# s2[is.na(s2)] <- 0
# 
# sum(s2[,5])
# #3 should be demo workspaces.
# nonDFR_demo <- filter(s2, Counts == 1 & Institutional.Affiliation != 'AouOps')
# View(nonDFR_demo)
# write.csv(nonDFR_demo, file = "nonDFR_demo_421.csv", quote = FALSE, row.names = F)
