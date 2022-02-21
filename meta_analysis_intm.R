#All of Us Researcher Workbench Workspaces: Meta-Analysis, 6/7 2nd pull
#Rhea Kerawala

#import libraries
library(readxl)
library(dplyr)

#import workspace description spreadsheet: 6/7/21 Version
Workspace67 <- read_excel("workspace descriptions 6_7_2021.xlsx")

#exclude column: Creation Time, Modified Time, Workspace Status, Contact Email & any columns pertaining to review 
#Update: Remove columns 2,3, 5, 7, 49, 50 (so until 48)
Workspace67 <- as.data.frame(c(Workspace67[,1], Workspace67[,4], Workspace67[,6], Workspace67[8:48]))  

#Step 1: De-duplication
#(number of workspaces) is commented next to the line
library(tidyverse)
#Removing if same workspace ID, study approach, study intent, or anticipated findings.  
Workspace67 <- Workspace67 %>% distinct(Workspace67[17], .keep_all = TRUE) #452
Workspace67 <- Workspace67 %>% distinct(Workspace67[18], .keep_all = TRUE) #452
Workspace67 <- Workspace67 %>% distinct(Workspace67[19], .keep_all = TRUE) #449
Workspace67 <- Workspace67 %>% distinct(Workspace67[2:15], .keep_all = TRUE) #For all columns 2 to 15, remove any where two rows are the same.
View(Workspace67) #442 workspaces

#Search for workspace names with the word 'duplicate'
Dups <- filter_all(Workspace67, any_vars(str_detect(Workspace.name, "duplicate")))
View(Dups)

#Find the other duplicates particular to the results above.
Dups <- filter_all(Dups, any_vars(str_detect(Workspace.name, "QA")))
Dups <- filter_all(Dups, any_vars(str_detect(Workspace.name, "calbach")))

#drop any repeated rows w/ below code:
#Dups <- Dups[-c(row number),]
rm(Dups)

#Step 2: Filtering for Disease Focused Research (DFR) v non-DFR

#Counts (# of workspaces) is commented next to the line
DFR<- filter(Workspace67, Workspace67[4] == 'Checked' ) #203
NonDFR <- filter(Workspace67, Workspace67[4] == "Unchecked") #239

#Step 3: Counting # race/ethnicity
RaceDFR <- filter(DFR, DFR[20] == 'Checked') #85
RacenonDFR <- filter(NonDFR, NonDFR[20] == 'Checked') #71

#Step 4: Counting race/ethnicity, categories. Filtered by row and combined to one dataframe.

# Disease Focused Research
#colnames(DFR)[37]
Race.count <- as.data.frame(nrow(RaceDFR))
Race.count <- rbind(Race.count, nrow(filter(DFR, DFR[31] == 'Checked')))
Race.count <- rbind(Race.count, nrow(filter(DFR, DFR[32] == 'Checked')))
Race.count <- rbind(Race.count, nrow(filter(DFR, DFR[33] == 'Checked')))
Race.count <- rbind(Race.count, nrow(filter(DFR, DFR[34] == 'Checked')))
Race.count <- rbind(Race.count, nrow(filter(DFR, DFR[35] == 'Checked')))
Race.count <- rbind(Race.count, nrow(filter(DFR, DFR[36] == 'Checked')))
Race.count <- rbind(Race.count, nrow(filter(DFR, DFR[37] == 'Checked')))
Race.count <- rbind(Race.count, nrow(filter(DFR, DFR[38] == 'Checked')))

#Add category labels 
Race<- data.frame(c('Race Overall', 'Asian','Black','Hispanic','AIAN','MENA','NHPI','Multi','NA'))
colnames(Race) = 'Race'
colnames(Race.count)[1] = 'DFR Counts'
Race.count <-cbind(Race.count, Race)
# 
#Non-DFR, same as with DFR
Race.2.count <- as.data.frame(nrow(RacenonDFR))
Race.2.count <- rbind(Race.2.count, nrow(filter(NonDFR, NonDFR[31] == 'Checked')))
Race.2.count <- rbind(Race.2.count, nrow(filter(NonDFR, NonDFR[32] == 'Checked')))
Race.2.count <- rbind(Race.2.count, nrow(filter(NonDFR, NonDFR[33] == 'Checked')))
Race.2.count <- rbind(Race.2.count, nrow(filter(NonDFR, NonDFR[34] == 'Checked')))
Race.2.count <- rbind(Race.2.count, nrow(filter(NonDFR, NonDFR[35] == 'Checked')))
Race.2.count <- rbind(Race.2.count, nrow(filter(NonDFR, NonDFR[36] == 'Checked')))
Race.2.count <- rbind(Race.2.count, nrow(filter(NonDFR, NonDFR[37] == 'Checked')))
Race.2.count <- rbind(Race.2.count, nrow(filter(NonDFR, NonDFR[38] == 'Checked')))
# 
colnames(Race.2.count)[1] = ('Non DFR Counts')
# 
Race.count <-cbind(Race.count, Race.2.count)
Race.count <- Race.count %>% relocate ('Race', .before = 'DFR Counts')
View(Race.count)

#clear up the environment a little
rm(Race, Race.2.count)

#Step 5: Counting age. Same method as step 3.
AgeDFR <- filter(DFR, DFR[21] == 'Checked') #54
AgenonDFR <- filter(NonDFR, NonDFR[21] == 'Checked') #59

#Step 6: Counting age, categories. Same method as step 4.
Age.count <- as.data.frame(nrow(AgeDFR))
Age.count <- rbind(Age.count, nrow(filter(AgeDFR, AgeDFR[39] == 'Checked')))
Age.count <- rbind(Age.count, nrow(filter(AgeDFR, AgeDFR[40] == 'Checked')))
Age.count <- rbind(Age.count, nrow(filter(AgeDFR, AgeDFR[41] == 'Checked')))
Age.count <- rbind(Age.count, nrow(filter(AgeDFR, AgeDFR[42] == 'Checked')))
Age.count <- rbind(Age.count, nrow(filter(AgeDFR, AgeDFR[43] == 'Checked')))

# Add category labels to main dataframe.
Age<- data.frame(c('Age Overall','Children 0-11','Children 12-17','Older Adults 65-74','Older Adults 75+', 'Unknown'))
colnames(Age) = 'Age'
colnames(Age.count)[1] = ('DFR Counts')
Age.count <-cbind(Age.count, Age)
Age.count <- Age.count %>% relocate ('Age', .before = 'DFR Counts')

#nonDFRs
Age.2.count <- as.data.frame(nrow(AgenonDFR))
Age.2.count <- rbind(Age.2.count, nrow(filter(AgenonDFR, AgenonDFR[39] == 'Checked')))
Age.2.count <- rbind(Age.2.count, nrow(filter(AgenonDFR, AgenonDFR[40] == 'Checked')))
Age.2.count <- rbind(Age.2.count, nrow(filter(AgenonDFR, AgenonDFR[41] == 'Checked')))
Age.2.count <- rbind(Age.2.count, nrow(filter(AgenonDFR, AgenonDFR[42] == 'Checked')))
Age.2.count <- rbind(Age.2.count, nrow(filter(AgenonDFR, AgenonDFR[43] == 'Checked')))
colnames(Age.2.count)[1] = ('Non DFR Counts')
# 
#combine DFR and nonDFR in 1 dataframe
Age.count <- cbind(Age.count, Age.2.count)
View(Age.count)

#clean environment again
rm(Age, Age.2.count)

# Step 7: Populations of Interest (POI)
#colnames(DFR)[22] #Sex at Birth
POIDFR <- filter(DFR, DFR[22] == 'Checked') #16
POInonDFR <- filter(NonDFR, NonDFR[22] == 'Checked') #24

#colnames(DFR)[23] #Gender Identity
POI2DFR <- filter(DFR, DFR[23] == 'Checked') #18
POI2nonDFR <- filter(NonDFR, NonDFR[23] == 'Checked') #30

# colnames(DFR)[24] #Sexual Orientation
POI3DFR <- filter(DFR, DFR[24] == 'Checked') #17
POI3nonDFR <- filter(NonDFR, NonDFR[24] == 'Checked') #29

# colnames(DFR)[25] #Geography
POI4DFR <- filter(DFR, DFR[25] == 'Checked') #40
POI4nonDFR <- filter(NonDFR, NonDFR[25] == 'Checked') #50

#colnames(DFR)[26] #Disability Status
POI5DFR <- filter(DFR, DFR[26] == 'Checked') #16
POI5nonDFR <- filter(NonDFR, NonDFR[26] == 'Checked') #25

# colnames(DFR)[27] #Access to Care
POI6DFR <- filter(DFR, DFR[27] == 'Checked') #38
POI6nonDFR <- filter(NonDFR, NonDFR[27] == 'Checked') #47

# colnames(DFR)[28] #Education Level
POI7DFR <- filter(DFR, DFR[28] == 'Checked') #35
POI7nonDFR <- filter(NonDFR, NonDFR[28] == 'Checked') #43

#colnames(DFR)[29] #Income level equal/below Fed Pov Level
POI8DFR <- filter(DFR, DFR[29] == 'Checked') #39
POI8nonDFR <- filter(NonDFR, NonDFR[29] == 'Checked') #51

# colnames(DFR)[30] #Others
POI9DFR <- filter(DFR, DFR[30] == 'Checked') #5
POI9nonDFR <- filter(NonDFR, NonDFR[30] == 'Checked') #1

#Combine all POI counts into 1 dataframe. 
POI<- data.frame(c('Race and Ethnicity', 'Age','Sex at Birth','Gender','Sexual Orientation','Geography', 'Disability Status', 'Access to Care','Education','Income Level','Others'))

#DFR
POI.DFR <- as.data.frame((nrow(RaceDFR)))
POI.DFR <- rbind(POI.DFR,(nrow(AgeDFR)),(nrow(POIDFR)),(nrow(POI2DFR)),(nrow(POI3DFR)),(nrow(POI4DFR)),(nrow(POI5DFR)),(nrow(POI6DFR)),(nrow(POI7DFR)),
                 (nrow(POI8DFR)),(nrow(POI9DFR)))
#nonDFR
POI.nonDFR <- as.data.frame((nrow(RacenonDFR)))
POI.nonDFR <- rbind(POI.nonDFR, (nrow(AgenonDFR)),(nrow(POIDFR)),(nrow(POI2nonDFR)),(nrow(POI3nonDFR)),(nrow(POI4nonDFR)),(nrow(POI5nonDFR)),(nrow(POI6nonDFR)),(nrow(POI7nonDFR)),
                    (nrow(POI8nonDFR)),(nrow(POI9nonDFR)))

POI<- cbind(POI, POI.DFR, POI.nonDFR)
colnames(POI) = c('Population of Interest','DFR', 'nonDFR')
View(POI)

#clean env
rm(POI.DFR, POI.nonDFR)
rm(POIDFR, POInonDFR, POI2DFR,POI3DFR, POI4DFR, POI5DFR, POI6DFR, POI7DFR, POI8DFR, POI9DFR, POI2nonDFR, POI3nonDFR, POI4nonDFR, POI5nonDFR, POI6nonDFR,POI7nonDFR,POI8nonDFR,POI9nonDFR)

# Step 8: Research Purposes, same method as step 6.
#colnames(DFR)[5] #Methods/Validation
RP10DFR <- filter(DFR, DFR[5] == 'Checked') #32
RP10nonDFR <- filter(NonDFR, NonDFR[5] == 'Checked') #60

#colnames(DFR)[6] #Research Control
RP11DFR <- filter(DFR, DFR[6] == 'Checked') #16
RP11nonDFR <- filter(NonDFR, NonDFR[6] == 'Checked') #8

#colnames(DFR)[7] #Genetic Research
RP12DFR <- filter(DFR, DFR[7] == 'Checked') #42
RP12nonDFR <- filter(NonDFR, NonDFR[7] == 'Checked') #18

#colnames(DFR)[8] #Social Behavioral Research
RP13DFR <- filter(DFR, DFR[8] == 'Checked') #31
RP13nonDFR <- filter(NonDFR, NonDFR[8] == 'Checked') #37

# colnames(DFR)[9] #Population Health/Public Health
RP14DFR <- filter(DFR, DFR[9] == 'Checked') #57
RP14nonDFR <- filter(NonDFR, NonDFR[9] == 'Checked') #80

# colnames(DFR)[10] #Drug Therapeutics/Dev Research
RP15DFR <- filter(DFR, DFR[10] == 'Checked') #14
RP15nonDFR <- filter(NonDFR, NonDFR[10] == 'Checked') #7

#colnames(DFR)[11] #Commercial
RP16DFR <- filter(DFR, DFR[11] == 'Checked') #2
RP16nonDFR <- filter(NonDFR, NonDFR[11] == 'Checked') #2

#colnames(DFR)[12] #Educational
RP17DFR <- filter(DFR, DFR[12] == 'Checked') #10
RP17nonDFR <- filter(NonDFR, NonDFR[12] == 'Checked') #67

#colnames(DFR)[13] #ELSI
RP18DFR <- filter(DFR, DFR[13] == 'Checked') #4
RP18nonDFR <- filter(NonDFR, NonDFR[13] == 'Checked') #5

# colnames(DFR)[14] #Other Purposes
RP19DFR <- filter(DFR, DFR[14] == 'Checked') #11
RP19nonDFR <- filter(NonDFR, NonDFR[14] == 'Checked') #54

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

#Step 9: Counting Institutions

#Group by Institution, count the number of entries
#Note: Still need to manually review.
Institutions<- Workspace67 %>% group_by(Institutional.Affiliation) %>% summarise('# of Workspaces per Institution'=n())
View(Institutions)

#Step 10: Fisher's Exact test 
#DFR v. Non-DFR for Race/Ethnicity and Age

View(Race.count)         #[1,2] & [1,3] are our race/ethnicity focused workspaces

View(DFR) #tells us there are 203 total DFR workspaces
View(NonDFR) #239 non-DFR workspaces

#Non-Race/Ethnicity focused workspaces
DFRdiff1<- 203-Race.count[1,2]
DFRdiff2<- 239-Race.count[1,3]

#Create a data-frame to do Fisher's test on
race  <- matrix(c(Race.count[1,2],Race.count[1,3],DFRdiff1,DFRdiff2,203,239), ncol=2, byrow=TRUE) 
race  <- as.data.frame(race)
View(race)
Fisher <- fisher.test(race[1:2,])

#Comparing %s to p-values
race2<- matrix(c(race[1,1]/race[3,1]*100, race[1,2]/race[3,2]*100), ncol=2, byrow=TRUE)
race2<- cbind(race2, Fisher$p.value)
colnames(race2)<- c('% DFR', '% non-DFR', 'P-value')
View(race2)

#Repeat for age
View(Age.count)         #[1,2] & [1,3] are our age focused workspaces

#Non-Age focused workspaces
DFRdiff3<- 203-Age.count[1,2]
DFRdiff4<- 239-Age.count[1,3]

age <- matrix(c(Age.count[1,2], Age.count[1,3], DFRdiff3, DFRdiff4,203,239), ncol=2, byrow=TRUE)
age <- as.data.frame(age)

Fisher2 <- fisher.test(age)

age2<- matrix(c(age[1,1]/age[3,1]*100, age[1,2]/age[3,2]*100), ncol=2, byrow=TRUE)

age2<- cbind(age2, Fisher2$p.value)
colnames(age2)<- c("%DFR","%non-DFR","age p-value")
View(age2)

#how to save any of the dataframes as a csv file for export.
write.csv(Institutions, file = "Institutions_67.csv", quote = FALSE, row.names = F)
write.csv(Age.count, file = "Age_67.csv", quote = FALSE, row.names = F)
write.csv(POI, file = "Population.of.Interest_67.csv", quote = FALSE, row.names = FALSE)
write.csv(RP, file = "Research.Purpose_67.csv", quote = FALSE, row.names = FALSE)
write.csv(Race.count, file = "Race.and.Ethnicity_67.csv", quote = FALSE, row.names = F)

# Age2 & Race2 were not exported.

View(Age.count)
#Step 10: Running search for substrings in R, (case-sensitive)
library(tidyr)

#Disease Categories

#Search for 'hypertension'. Count:17
s <- DFR[15:19]
#loop
search_terms <- c('Hypertension','hypertension','hypertensive','Hypertensive')
for(i in search_terms) {
  i1  <- grepl(i, s$Disease.Focused.Research.Name)
  s$Counts[i1] <- "1"
  i2 <- grepl(i, s$Other.Purpose.Details)
  s$Counts[i2] <- "1"
  i3 <- grepl(i, s$Study.Approach)
  s$Counts[i3] <- "1"
  i4 <- grepl(i, s$Study.Intent)
  s$Counts[i4] <- "1"
  i5 <- grepl(i, s$Anticipated.Findings)
  s$Counts[i5] <- "1"
}

#Counts
s[is.na(s)] <- 0
s$Counts <- as.numeric(s$Counts)
sum(s[,6])

#Table
Counts<- data.frame(sum(s[,6]))
colnames(Counts) = c('Counts')
View(Counts)

#Obesity, 8
s<- DFR[15:19]

#loop
search_terms <- c('Obesity','obesity','adiposity','fatty tissue hyperplasia','MOMO syndrome','MORM syndrome','proopiomelanocortin deficiency syndrome')

for(i in search_terms) {
  i1  <- grepl(i, s$Disease.Focused.Research.Name)
  s$Counts[i1] <- "1"
  i2 <- grepl(i, s$Other.Purpose.Details)
  s$Counts[i2] <- "1"
  i3 <- grepl(i, s$Study.Approach)
  s$Counts[i3] <- "1"
  i4 <- grepl(i, s$Study.Intent)
  s$Counts[i4] <- "1"
  i5 <- grepl(i, s$Anticipated.Findings)
  s$Counts[i5] <- "1"
}

#Counts
s$Counts <- as.numeric(s$Counts)
s[is.na(s)] <- 0
sum(s[,6])

Counts <- rbind(Counts, sum(s[,6])) 

#COVID-19. Count: 17
s<- DFR[15:19]
#loop
search_terms <- c('covid','sars-cov-2','coronavirus','Coronavirus','COVID', 'SARS','SARS-CoV-2','Sars-Cov-2','SARSCoV2','severe acute respiratory syndrome')
for(i in search_terms) {
  i1  <- grepl(i, s$Disease.Focused.Research.Name)
  s$Counts[i1] <- "1"
  i2 <- grepl(i, s$Other.Purpose.Details)
  s$Counts[i2] <- "1"
  i3 <- grepl(i, s$Study.Approach)
  s$Counts[i3] <- "1"
  i4 <- grepl(i, s$Study.Intent)
  s$Counts[i4] <- "1"
  i5 <- grepl(i, s$Anticipated.Findings)
  s$Counts[i5] <- "1"
}

#Counts
s$Counts <- as.numeric(s$Counts)
s[is.na(s)] <- 0

sum(s[,6])
Counts <- rbind(Counts, sum(s[,6])) 

#Diabetes, 35
s<- DFR[15:19]
#loop
search_terms <- c('Diabetes','diabetes','diabetic','Diabetic')
for(i in search_terms) {
  i1  <- grepl(i, s$Disease.Focused.Research.Name)
  s$Counts[i1] <- "1"
  i2 <- grepl(i, s$Other.Purpose.Details)
  s$Counts[i2] <- "1"
  i3 <- grepl(i, s$Study.Approach)
  s$Counts[i3] <- "1"
  i4 <- grepl(i, s$Study.Intent)
  s$Counts[i4] <- "1"
  i5 <- grepl(i, s$Anticipated.Findings)
  s$Counts[i5] <- "1"
}

#Counts
s$Counts <- as.numeric(s$Counts)
s[is.na(s)] <- 0
sum(s[,6])
Counts <- rbind(Counts, sum(s[,6])) 

#Cancer & Tumors, 51
#tumor(s), tumor (types), benign (tumors), oncolog(y)
s<- DFR[15:19]
#loop
search_terms<- c('Oncolog','Tumor','Cancer','oncolog', 'tumor','cancer','coma','toma', 'noma', 'kemia','phoma','malignan','Malignan','Neoplasm','neoplasm')
for(i in search_terms) {
  i1  <- grepl(i, s$Disease.Focused.Research.Name)
  s$Counts[i1] <- "1"
  i2 <- grepl(i, s$Other.Purpose.Details)
  s$Counts[i2] <- "1"
  i3 <- grepl(i, s$Study.Approach)
  s$Counts[i3] <- "1"
  i4 <- grepl(i, s$Study.Intent)
  s$Counts[i4] <- "1"
  i5 <- grepl(i, s$Anticipated.Findings)
  s$Counts[i5] <- "1"
}

#counts
s$Counts <- as.numeric(s$Counts)
s[is.na(s)] <- 0

sum(s[,6])
Counts <- rbind(Counts, sum(s[,6])) 

#Reproductive Issues: pregnancy, labor, endometriosis, menstrua, reproduc, fetal, PCOS 
#Count: 27
s<- DFR[15:19]
#loop
search_terms<- c('Pregnan','pregnan','Labor','labor','Endometri','endometri','Menstrua','menstrua','Birth','birth','Reproduc','reproduc','Fetal','fetal','PCOS','poly cystic ovarian syndrome', 'Fertil','fertil','amnio','Amnio')
for(i in search_terms) {
  i1  <- grepl(i, s$Disease.Focused.Research.Name)
  s$Counts[i1] <- "1"
  i2 <- grepl(i, s$Other.Purpose.Details)
  s$Counts[i2] <- "1"
  i3 <- grepl(i, s$Study.Approach)
  s$Counts[i3] <- "1"
  i4 <- grepl(i, s$Study.Intent)
  s$Counts[i4] <- "1"
  i5 <- grepl(i, s$Anticipated.Findings)
  s$Counts[i5] <- "1"
}

#counts
s$Counts <- as.numeric(s$Counts)
s[is.na(s)] <- 0

sum(s[,6])
Counts <- rbind(Counts, sum(s[,6])) 

#Mental Health, Neurological Concerns, Cognitive Impairment: Count: 55
##Includes psychiatric disorders, ADHD, neuropsychiatric study, ADRD/AD, depression/anxiety, mental health, cognitive, dementia, traumatic brain injury, MDD, brain
s<- DFR[15:19]
#loop
search_terms<- c('Psychiatric','Psychological','psychological','psychiatric', 'attention deficit','autism','Autism','Neurodev','neurodevelopmental','ADHD','alzheimer',"Alzheimer",'Depressi', 'depressi','Anxiety','anxiety','Mental','mental','Cogniti','cogniti', 'Dementia','dementia','Brain','brain','Mood','mood','cerebral',"Cerebral",
                 'psychosocial', 'psychopathic', 'intellectual disability',
                 'neuronal', 'Synder-Robinson','WAGR','Baraitser-Winter','encephalopathy','Parkinsonism',
                 'MASA', 'Partington', 'GMS', 'MORM', 'cerebellar ataxia', 'cranial', 'TBI', 'concussion',
                 'CSF','choroid plexus','CNS', 'nervous system','nerve'
)
for(i in search_terms) {
  i1  <- grepl(i, s$Disease.Focused.Research.Name)
  s$Counts[i1] <- "1"
  i2 <- grepl(i, s$Other.Purpose.Details)
  s$Counts[i2] <- "1"
  i3 <- grepl(i, s$Study.Approach)
  s$Counts[i3] <- "1"
  i4 <- grepl(i, s$Study.Intent)
  s$Counts[i4] <- "1"
  i5 <- grepl(i, s$Anticipated.Findings)
  s$Counts[i5] <- "1"
}

#counts
s$Counts <- as.numeric(s$Counts)
s[is.na(s)] <- 0

sum(s[,6])
Counts <- rbind(Counts, sum(s[,6])) 

#For Cardio: Cardiovascular Disease, CVD, Congestive Heart Failure, Cardiometabolic, ASCVD, A-fibrillation,, stroke, 
#Count: 60
s<- DFR[15:19]
#loop
search_terms<- c('CVD','Heart','heart','Cardi','cardi','Stroke','stroke','fibrillation','coronary',"Coronary",'Carotid','carotid','arterial','brachycardia','myocardial', 'pericarditis','coronary arteriosclerosis','cardiac chamber','atrioventricular','beriberi','CVS','ectopic cordis', 'cor polmonale',
                 'resting ischaemia', 'angina','TARP','Pierre Robin','Grange syndrome','PHAVER','pterygia','protodiastolic gallop','congenital valvular insufficiency', 'congenital valve disease',
                 'aortic/MAVD','CHARGE','Lev syndrome','neurocirculatory asthenia','syncope','circulatory'
)
for(i in search_terms) {
  i1  <- grepl(i, s$Disease.Focused.Research.Name)
  s$Counts[i1] <- "1"
  i2 <- grepl(i, s$Other.Purpose.Details)
  s$Counts[i2] <- "1"
  i3 <- grepl(i, s$Study.Approach)
  s$Counts[i3] <- "1"
  i4 <- grepl(i, s$Study.Intent)
  s$Counts[i4] <- "1"
  i5 <- grepl(i, s$Anticipated.Findings)
  s$Counts[i5] <- "1"
}

#counts
s$Counts <- as.numeric(s$Counts)
s[is.na(s)] <- 0

sum(s[,6])
Counts <- rbind(Counts, sum(s[,6])) 

#Respiratory includes: asthma, respiratory failure, COPD, sinusitis
#Count: 13
s<- DFR[15:19]
#loop
search_terms<- c('asthma','respiratory','COPD','sinusitis','Asthma','Respiratory','Sinusitis','Inhal','inhal','breath','Breath','tuberculosis','Tuberculosis','atelectasis','wheez','airway','inspiratory','expiratory','dyspnea','dyspnoea')

for(i in search_terms) {
  i1  <- grepl(i, s$Disease.Focused.Research.Name)
  s$Counts[i1] <- "1"
  i2 <- grepl(i, s$Other.Purpose.Details)
  s$Counts[i2] <- "1"
  i3 <- grepl(i, s$Study.Approach)
  s$Counts[i3] <- "1"
  i4 <- grepl(i, s$Study.Intent)
  s$Counts[i4] <- "1"
  i5 <- grepl(i, s$Anticipated.Findings)
  s$Counts[i5] <- "1"
}

#counts
s$Counts <- as.numeric(s$Counts)
s[is.na(s)] <- 0

sum(s[,6])
Counts <- rbind(Counts, sum(s[,6])) 

#Immune: inflammation, infectious disease, SLE, lupus, autoimmune, sepsis, allergies Counts: 26
#33 
s<- DFR[15:19]
#loop
search_terms <- c('inflammat','infectio','SLE','lupus','autoimmune','sepsis','allerg','Inflammat','Infectio','Lupus','Autoimmune','Sepsis','Allerg',"Multiple Sclerosis","multiple sclerosis","MS",'AIDS','viral','Viral','B-cell','T-cell')

for(i in search_terms) {
  i1  <- grepl(i, s$Disease.Focused.Research.Name)
  s$Counts[i1] <- "1"
  i2 <- grepl(i, s$Other.Purpose.Details)
  s$Counts[i2] <- "1"
  i3 <- grepl(i, s$Study.Approach)
  s$Counts[i3] <- "1"
  i4 <- grepl(i, s$Study.Intent)
  s$Counts[i4] <- "1"
  i5 <- grepl(i, s$Anticipated.Findings)
  s$Counts[i5] <- "1"
}

#counts
s$Counts <- as.numeric(s$Counts)
s[is.na(s)] <- 0

sum(s[,6])
Counts <- rbind(Counts, sum(s[,6])) 

#Sleep: insomnia, narcolepsy, sleep, apnea, Counts: 12
s<- DFR[15:19]
#loop
search_terms <- c('insomnia','narcolepsy','sleep','Insomnia','Narcolepsy','Sleep','somniloquism','Somniloquism','drowsy','Drowsy','somnia','Somnia','Somnabulism','somnabulism','nocturnal epilepsy','Nocturnal epilepsy')

for(i in search_terms) {
  i1  <- grepl(i, s$Disease.Focused.Research.Name)
  s$Counts[i1] <- "1"
  i2 <- grepl(i, s$Other.Purpose.Details)
  s$Counts[i2] <- "1"
  i3 <- grepl(i, s$Study.Approach)
  s$Counts[i3] <- "1"
  i4 <- grepl(i, s$Study.Intent)
  s$Counts[i4] <- "1"
  i5 <- grepl(i, s$Anticipated.Findings)
  s$Counts[i5] <- "1"
}

#counts
s$Counts <- as.numeric(s$Counts)
s[is.na(s)] <- 0

sum(s[,6])
Counts <- rbind(Counts, sum(s[,6])) 

#Musculoskeletal: back pain, arthritis, postop osteoly, hernia, ACL, NAFLD, liver disease
#Count: 20
s<- DFR[15:19]
#loop
search_terms<- c('back', 'arthritis','postop','hernia','ACL','NAFLD','liver disease','Back','Arthritis','Postop','Hernia','muscle','Liver','Muscular','muscular','truncal','Truncal','trunk','Trunk','Limb','limb','Mobility','mobility','thoracic','Thoracic')

for(i in search_terms) {
  i1  <- grepl(i, s$Disease.Focused.Research.Name)
  s$Counts[i1] <- "1"
  i2 <- grepl(i, s$Other.Purpose.Details)
  s$Counts[i2] <- "1"
  i3 <- grepl(i, s$Study.Approach)
  s$Counts[i3] <- "1"
  i4 <- grepl(i, s$Study.Intent)
  s$Counts[i4] <- "1"
  i5 <- grepl(i, s$Anticipated.Findings)
  s$Counts[i5] <- "1"
}

#counts
s$Counts <- as.numeric(s$Counts)
s[is.na(s)] <- 0

sum(s[,6])
Counts <- rbind(Counts, sum(s[,6])) 

#GI, endocrine: CKD, GI (if capital), gastrointestinal, fecal, malnutri, kidney stones
#Count: 21
s<- DFR[15:19]
#loop
search_terms<- c('GI', 'CKD','gastrointestinal','fecal','malnutrition','kidney','endocrin','Gastrointestinal','Fecal','Malnutrition','Kidney','Endocrine','ulcer','Ulcer','Renal','renal','digest','Digest')

for(i in search_terms) {
  i1  <- grepl(i, s$Disease.Focused.Research.Name)
  s$Counts[i1] <- "1"
  i2 <- grepl(i, s$Other.Purpose.Details)
  s$Counts[i2] <- "1"
  i3 <- grepl(i, s$Study.Approach)
  s$Counts[i3] <- "1"
  i4 <- grepl(i, s$Study.Intent)
  s$Counts[i4] <- "1"
  i5 <- grepl(i, s$Anticipated.Findings)
  s$Counts[i5] <- "1"
}

#counts
s$Counts <- as.numeric(s$Counts)
s[is.na(s)] <- 0

sum(s[,6])
Counts <- rbind(Counts, sum(s[,6])) 

#Dermatology: neurofibromatosis, psoriasis, psoriatic disease, Count: 13
s<- DFR[15:19]
#loop
search_terms<- c('neurofibromatosis', 'psoria','skin', 'dermatolog','Neurofibromatosis','neurofibromas','Neurofibromas','Psoria','Skin','Dermatolog',
                 "rash",'syringoma','telangiectasia','sarcoidosis','pigmentation','skin trauma','scar','keloid','tumour','graft','photosensitivity','dermatitis','nevus of skin','dysplastic','skin bends','cutaneous','POEMS syndrome','chapping','eczema','spots','eccrine',
                 'serpiginosium','pruritic rash','erythema','rhytidosis facialis','pilonidal abscess','CAMOS','dermatosis','burn','Mehes syndrome','keratosis','hyperesthesia','itch','warts','alopecia','Ehler-Danlos','panronychia')
for(i in search_terms) {
  i1  <- grepl(i, s$Disease.Focused.Research.Name)
  s$Counts[i1] <- "1"
  i2 <- grepl(i, s$Other.Purpose.Details)
  s$Counts[i2] <- "1"
  i3 <- grepl(i, s$Study.Approach)
  s$Counts[i3] <- "1"
  i4 <- grepl(i, s$Study.Intent)
  s$Counts[i4] <- "1"
  i5 <- grepl(i, s$Anticipated.Findings)
  s$Counts[i5] <- "1"
}

#counts
s$Counts <- as.numeric(s$Counts)
s[is.na(s)] <- 0

sum(s[,6])
Counts <- rbind(Counts, sum(s[,6])) 

Diseases<- c("Hypertension", "Obesity","COVID-19","Diabetes",
             "Cancer & Tumors","Reproductive Concerns","Mental Health, Neurodev Issues",
             "Cardiovascular-related Issues","Respiratory-related Issues", "Immunology","Sleep","Musculoskeletal","GI & Endocrine","Dermatology")
Counts <- cbind(Counts, Diseases)
Counts <- Counts %>% relocate ('Diseases', .before = 'Counts')
View(Counts)

#save and export
write.csv(Counts, file = "Disease Category_67.csv", quote = FALSE, row.names = F)

#Running all of the searches w/out the first line resetting (s<- DFR[15:19])

#Note: s below has not been reset w/ each disease category.
#Other Disease Category
s$Counts <- as.numeric(s$Counts)
s[is.na(s)] <- 0
s_other <- filter(s, Counts == 0)
#13 belong to the other disease category

library(dplyr)
# #Demonstration 
# s1<- as.data.frame(c(DFR[2], DFR[3], DFR[15], DFR[16], DFR[17], DFR[18], DFR[19]))
# #s1<- s1 %>% drop_na
# #loop
# search_terms<- c('Demonstration',
#                  'demonstration','tutorial','Tutorial')
# for(i in search_terms) {
#   i1  <- grepl(i, s1$Disease.Focused.Research.Name)
#   s1$Disease.Focused.Research.Name[i1] <- "1"
#   i2 <- grepl(i, s1$Other.Purpose.Details)
#   s1$Counts[i2] <- "1"
#   i3 <- grepl(i, s1$Study.Approach)
#   s1$Counts[i3] <- "1"
#   i4 <- grepl(i, s1$Study.Intent)
#   s1$Counts[i4] <- "1"
#   i5 <- grepl(i, s1$Anticipated.Findings)
#   s1$Counts[i5] <- "1"
#   i6 <- grepl(i, s1$Workspace.name)
#   s1$Counts[i6] <- "1"
# }
# 
# #counts
# s1$Counts <- as.numeric(s1$Counts)
# s1[is.na(s1)] <- 0
# 
# sum(s1[,8])
# #15 in s1 that fit count = 1
# DFR_demo <- filter(s1, Counts == 1 & Institutional.Affiliation != 'AouOps')
# write.csv(DFR_demo, file = "DFR_demo_67.csv", quote = FALSE, row.names = F)
# 
# #For non-DFR
# s2<- as.data.frame(c(NonDFR[2], NonDFR[3],NonDFR[15], NonDFR[16], NonDFR[17], NonDFR[18], NonDFR[19]))
# #loop
# search_terms<- c('Demonstration',
#                  'demonstration','tutorial',"Tutorial")
# for(i in search_terms) {
#   i1  <- grepl(i, s2$Disease.Focused.Research.Name)
#   s2$Disease.Focused.Research.Name[i1] <- "1"
#   i2 <- grepl(i, s2$Other.Purpose.Details)
#   s2$Counts[i2] <- "1"
#   i3 <- grepl(i, s2$Study.Approach)
#   s2$Counts[i3] <- "1"
#   i4 <- grepl(i, s2$Study.Intent)
#   s2$Counts[i4] <- "1"
#   i5 <- grepl(i, s2$Anticipated.Findings)
#   s2$Counts[i5] <- "1"
#   i6 <- grepl(i, s2$Workspace.name)
#   s2$Counts[i6] <- "1"
# }
# 
# #counts
# s2$Counts <- as.numeric(s2$Counts)
# s2[is.na(s2)] <- 0
# 
# sum(s2[,8])
# #22 should be demo workspaces.
# nonDFR_demo <- filter(s2, Counts == 1 & Institutional.Affiliation != 'AouOps')
# View(nonDFR_demo)
# write.csv(nonDFR_demo, file = "nonDFR_demo_67.csv", quote = FALSE, row.names = F)

