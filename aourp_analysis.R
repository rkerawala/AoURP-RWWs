# All of Us Researcher Workbench Workspaces
# Analysis of workspace descriptions

#import libraries
library(readxl)
library(dplyr)

#import workspace description spreadsheet
Workspace <- read_excel("workspace description name.xlsx")

#exclude unneeded columns pertaining to review 
Workspace <- as.data.frame(c(Workspace[,1], Workspace[,4], Workspace630[,6], Workspace630[8:48]))  

#Step 1: De-duplication
library(tidyverse)
#Removing if same workspace ID, study approach, study intent, or anticipated findings.  
Workspace <- Workspace %>% distinct(Workspace[17], .keep_all = TRUE) 
Workspace <- Workspace %>% distinct(Workspace[18], .keep_all = TRUE) 
Workspace <- Workspace %>% distinct(Workspace[19], .keep_all = TRUE) 
Workspace <- Workspace %>% distinct(Workspace[2:15], .keep_all = TRUE)
View(Workspace)

#Search for workspace names with the word 'duplicate'
Dups <- filter_all(Workspace, any_vars(str_detect(Workspace.name, "duplicate")))
View(Dups)

#Find the other duplicates particular to the results above.
Dups <- filter_all(Dups, any_vars(str_detect(Workspace.name, "term")))

#drop any repeated rows w/ below code:
#Dups <- Dups[-c(row number),]
rm(Dups)

# Step 2: Filtering for Disease Focused Research (DFR) v non-DFR

#Counts 
DFR<- filter(Workspace, Workspace[4] == 'Checked' ) 
NonDFR <- filter(Workspace, Workspace[4] == "Unchecked") 

#Step 3: Counting # race/ethnicity
RaceDFR <- filter(DFR, DFR[20] == 'Checked') 
RacenonDFR <- filter(NonDFR, NonDFR[20] == 'Checked') 

#Step 4: Counting race/ethnicity, categories. Filtered by row and combined to one dataframe.

# Disease Focused Research
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

#Non-DFR, same as with DFR
Race.count.nondfr <- as.data.frame(nrow(RacenonDFR))
Race.count.nondfr <- rbind(Race.count.nondfr, nrow(filter(NonDFR, NonDFR[31] == 'Checked')))
Race.count.nondfr <- rbind(Race.count.nondfr, nrow(filter(NonDFR, NonDFR[32] == 'Checked')))
Race.count.nondfr <- rbind(Race.count.nondfr, nrow(filter(NonDFR, NonDFR[33] == 'Checked')))
Race.count.nondfr <- rbind(Race.count.nondfr, nrow(filter(NonDFR, NonDFR[34] == 'Checked')))
Race.count.nondfr <- rbind(Race.count.nondfr, nrow(filter(NonDFR, NonDFR[35] == 'Checked')))
Race.count.nondfr <- rbind(Race.count.nondfr, nrow(filter(NonDFR, NonDFR[36] == 'Checked')))
Race.count.nondfr <- rbind(Race.count.nondfr, nrow(filter(NonDFR, NonDFR[37] == 'Checked')))
Race.count.nondfr <- rbind(Race.count.nondfr, nrow(filter(NonDFR, NonDFR[38] == 'Checked')))
 
colnames(Race.count.nondfr)[1] = ('Non DFR Counts')
 
Race.count <-cbind(Race.count, Race.count.nondfr)
Race.count <- Race.count %>% relocate ('Race', .before = 'DFR Counts')
View(Race.count)

#clear up the environment 
rm(Race, Race.2.count)

#Step 5: Counting age. Same method as step 3.
AgeDFR <- filter(DFR, DFR[21] == 'Checked') 
AgenonDFR <- filter(NonDFR, NonDFR[21] == 'Checked') 

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
Age.count.nondfr <- as.data.frame(nrow(AgenonDFR))
Age.count.nondfr <- rbind(Age.count.nondfr, nrow(filter(AgenonDFR, AgenonDFR[39] == 'Checked')))
Age.count.nondfr <- rbind(Age.count.nondfr, nrow(filter(AgenonDFR, AgenonDFR[40] == 'Checked')))
Age.count.nondfr <- rbind(Age.count.nondfr, nrow(filter(AgenonDFR, AgenonDFR[41] == 'Checked')))
Age.count.nondfr <- rbind(Age.count.nondfr, nrow(filter(AgenonDFR, AgenonDFR[42] == 'Checked')))
Age.count.nondfr <- rbind(Age.count.nondfr, nrow(filter(AgenonDFR, AgenonDFR[43] == 'Checked')))
colnames(Age.count.nondfr)[1] = ('Non DFR Counts')

#combine DFR and nonDFR in 1 dataframe
Age.count <- cbind(Age.count, Age.count.nondfr)
View(Age.count)

#clean environment again
rm(Age, Age.2.count)

# Step 7: Populations of Interest (POI)
#colnames(DFR)[22] #Sex at Birth
POIDFR <- filter(DFR, DFR[22] == 'Checked') 
POInonDFR <- filter(NonDFR, NonDFR[22] == 'Checked') 

#colnames(DFR)[23] #Gender Identity
POI2DFR <- filter(DFR, DFR[23] == 'Checked') 
POI2nonDFR <- filter(NonDFR, NonDFR[23] == 'Checked') 

# colnames(DFR)[24] #Sexual Orientation
POI3DFR <- filter(DFR, DFR[24] == 'Checked') 
POI3nonDFR <- filter(NonDFR, NonDFR[24] == 'Checked') 

# colnames(DFR)[25] #Geography
POI4DFR <- filter(DFR, DFR[25] == 'Checked') 
POI4nonDFR <- filter(NonDFR, NonDFR[25] == 'Checked') 

#colnames(DFR)[26] #Disability Status
POI5DFR <- filter(DFR, DFR[26] == 'Checked') 
POI5nonDFR <- filter(NonDFR, NonDFR[26] == 'Checked') 

# colnames(DFR)[27] #Access to Care
POI6DFR <- filter(DFR, DFR[27] == 'Checked') 
POI6nonDFR <- filter(NonDFR, NonDFR[27] == 'Checked') 

# colnames(DFR)[28] #Education Level
POI7DFR <- filter(DFR, DFR[28] == 'Checked') 
POI7nonDFR <- filter(NonDFR, NonDFR[28] == 'Checked') 

#colnames(DFR)[29] #Income level equal/below Fed Pov Level
POI8DFR <- filter(DFR, DFR[29] == 'Checked') 
POI8nonDFR <- filter(NonDFR, NonDFR[29] == 'Checked') 

# colnames(DFR)[30] #Others
POI9DFR <- filter(DFR, DFR[30] == 'Checked') 
POI9nonDFR <- filter(NonDFR, NonDFR[30] == 'Checked') 

#Combine all POI counts into 1 dataframe. 
POI<- data.frame(c('Race & Ethnicity', 'Age','Sex at Birth','Gender','Sexual Orientation','Geography', 'Disability Status', 'Access to Care','Education','Income Level','Others'))

#DFR
POI.DFR <- as.data.frame((nrow(RaceDFR)))
POI.DFR <- rbind(POI.DFR,(nrow(AgeDFR)),(nrow(POIDFR)),(nrow(POI2DFR)),(nrow(POI3DFR)),(nrow(POI4DFR)),(nrow(POI5DFR)),(nrow(POI6DFR)),(nrow(POI7DFR)),
                (nrow(POI8DFR)),(nrow(POI9DFR)))
#nonDFR
POI.nonDFR <- as.data.frame((nrow(RacenonDFR)))
POI.nonDFR <- rbind(POI.nonDFR,(nrow(AgenonDFR)),(nrow(POInonDFR)),(nrow(POI2nonDFR)),(nrow(POI3nonDFR)),(nrow(POI4nonDFR)),(nrow(POI5nonDFR)),(nrow(POI6nonDFR)),(nrow(POI7nonDFR)),
                 (nrow(POI8nonDFR)),(nrow(POI9nonDFR)))

POI<- cbind(POI, POI.DFR, POI.nonDFR)
colnames(POI) = c('Population of Interest','DFR', 'nonDFR')
View(POI)

#clean environment
rm(POI.DFR, POI.nonDFR)
rm(POIDFR, POInonDFR, POI2DFR,POI3DFR, POI4DFR, POI5DFR, POI6DFR, POI7DFR, POI8DFR, POI9DFR, POI2nonDFR, POI3nonDFR, POI4nonDFR, POI5nonDFR, POI6nonDFR,POI7nonDFR,POI8nonDFR,POI9nonDFR)

# Step 8: Research Purposes, same method as step 7.
colnames(DFR)[5] #Methods/Validation
RP10DFR <- filter(DFR, DFR[5] == 'Checked') 
RP10nonDFR <- filter(NonDFR, NonDFR[5] == 'Checked') 

#colnames(DFR)[6] #Research Control
RP11DFR <- filter(DFR, DFR[6] == 'Checked') 
RP11nonDFR <- filter(NonDFR, NonDFR[6] == 'Checked')

#colnames(DFR)[7] #Genetic Research
RP12DFR <- filter(DFR, DFR[7] == 'Checked') 
RP12nonDFR <- filter(NonDFR, NonDFR[7] == 'Checked') 

# colnames(DFR)[8] #Social Behavioral Research
RP13DFR <- filter(DFR, DFR[8] == 'Checked') 
RP13nonDFR <- filter(NonDFR, NonDFR[8] == 'Checked') 

# colnames(DFR)[9] #Population Health/Public Health
RP14DFR <- filter(DFR, DFR[9] == 'Checked') 
RP14nonDFR <- filter(NonDFR, NonDFR[9] == 'Checked') 

# colnames(DFR)[10] #Drug Therapeutics/Dev Research
RP15DFR <- filter(DFR, DFR[10] == 'Checked') 
RP15nonDFR <- filter(NonDFR, NonDFR[10] == 'Checked') 

# colnames(DFR)[11] #Commercial
RP16DFR <- filter(DFR, DFR[11] == 'Checked') 
RP16nonDFR <- filter(NonDFR, NonDFR[11] == 'Checked') 

#colnames(DFR)[12] #Educational
RP17DFR <- filter(DFR, DFR[12] == 'Checked') 
RP17nonDFR <- filter(NonDFR, NonDFR[12] == 'Checked') 

#colnames(DFR)[13] #ELSI
RP18DFR <- filter(DFR, DFR[13] == 'Checked') 
RP18nonDFR <- filter(NonDFR, NonDFR[13] == 'Checked')

# colnames(DFR)[14] #Other Purposes
RP19DFR <- filter(DFR, DFR[14] == 'Checked') 
RP19nonDFR <- filter(NonDFR, NonDFR[14] == 'Checked') 

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
#Note: Manual edits are required, so the count here is inaccurate.
Institutions<- Workspace630 %>% group_by(Institutional.Affiliation) %>% summarise('# of Workspaces per Institution'=n())
View(Institutions)

#Step 10: Fisher's Exact test 
#DFR v. Non-DFR for Race/Ethnicity and Age

View(Race.count)         #[1,2] & [1,3] are our race/ethnicity focused workspaces

View(DFR) #tells us there are 'x' total DFR workspaces
View(NonDFR) #'y' non-DFR workspaces

#Non-Race/Ethnicity focused workspaces
DFRdiff1<- 'x'-Race.count[1,2]
DFRdiff2<- 'y'-Race.count[1,3]

#Create a data-frame to do Fisher's test on
race  <- matrix(c(Race.count[1,2],Race.count[1,3],DFRdiff1,DFRdiff2,'x','y'), ncol=2, byrow=TRUE) 
race  <- as.data.frame(race)
View(race)
Fisher <- fisher.test(race[1:2,])
POIstats <- print(as.data.frame(Fisher$p.value))
View(POIstats)

#Comparing %s to p-values
race2<- matrix(c(race[1,1]/race[3,1]*100, race[1,2]/race[3,2]*100), ncol=2, byrow=TRUE)
race2<- cbind(race2, Fisher$p.value)
colnames(race2)<- c('% DFR', '% non-DFR', 'P-value')
View(race2)

#Repeat for age
View(Age.count)         #[1,2] & [1,3] are our age focused workspaces

#Non-Age focused workspaces
DFRdiff3<- 'x'-Age.count[1,2]
DFRdiff4<- 'y'-Age.count[1,3]

age <- matrix(c(Age.count[1,2], Age.count[1,3], DFRdiff3, DFRdiff4,216,246), ncol=2, byrow=TRUE)
age <- as.data.frame(age)

Fisher2 <- fisher.test(age)
POIstats <- rbind(POIstats, Fisher2$p.value)
View(POIstats)

age2<- matrix(c(age[1,1]/age[3,1]*100, age[1,2]/age[3,2]*100), ncol=2, byrow=TRUE)

age2<- cbind(age2, Fisher2$p.value)
colnames(age2)<- c("%DFR","%non-DFR","age p-value")
View(age2)

#saving the dataframes as a csv file for export.
write.csv(Institutions, file = "Institutions.csv", quote = FALSE, row.names = F)
write.csv(Age.count, file = "Age.csv", quote = FALSE, row.names = F)
write.csv(POI, file = "Population.of.Interest.csv", quote = FALSE, row.names = F)
write.csv(RP, file = "Research.Purpose.csv", quote = FALSE, row.names = F)
write.csv(Race.count, file = "Race.and.Ethnicity.csv", quote = FALSE, row.names = F)

#Age2 & Race2 were not exported.

#Step 11: Running search for substrings in R, (case-sensitive)
library(tidyr)

#Disease Categories, search terms derived both manually and from Athena OHDSI

#clear up environment
rm(RP10DFR,RP11DFR,RP13DFR,RP14DFR,RP15DFR,RP15nonDFR,RP14nonDFR,RP13nonDFR,RP11nonDFR,RP10nonDFR, RP16DFR, RP17DFR, RP18DFR, RP19DFR,RP16nonDFR,RP17nonDFR, RP18nonDFR, RP19nonDFR)

#Search for 'hypertension'. 
s <- DFR[15:19]
#write.csv(s, file = "DFR.csv")

#loop
search_terms <- c('Hypertension','hypertension', 'hypertensive','Hypertensive')
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

#Obesity
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

#COVID-19. 
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

#Diabetes
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

#Cancer & Tumors
#Count
s<- DFR[15:19]

#loop
search_terms<- c('Oncolog','Tumor','Cancer','oncolog', 'tumor','cancer','coma','toma', 'noma', 'kemia','phoma', 'malignan','Malignan','Neoplasm','neoplasm')
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

#creating a spreadsheet for further analysis of cancer-specific Workspaces.
s2<- filter(s, s$Counts == '1' ) #64
write.csv(s2, file = "Cancer.csv", quote = FALSE, row.names = F)

#Reproductive Issues: pregnancy, labor, endometriosis, menstrua, reproduc, fetal, PCOS 
#Count
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


#Mental Health, Neurological Concerns, Cognitive Impairment: Count
#Includes psychiatric disorders, ADHD, neuropsychiatric study, ADRD/AD, depression/anxiety, mental health, cognitive, dementia, traumatic brain injury, MDD, etc.
#Count
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

#For Cardio: Cardiovascular Disease, CVD, Congestive Heart Failure, Cardiometabolic, ASCVD, A-fibrillation,, stroke
#Count
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

#saving for further analysis
s2<- filter(s, s$Counts == '1' ) #64
write.csv(s2, file = "CVD.csv", quote = FALSE, row.names = F)

#Respiratory includes: asthma, respiratory failure, COPD, sinusitis
#Count
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

#Immune: inflammation, infectious disease, SLE, lupus, autoimmune, sepsis, allergies Counts
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

#Sleep: insomnia, narcolepsy, sleep, apnea, Counts
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
#Count
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
#Count
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

#Dermatology: neurofibromatosis, psoriasis, psoriatic disease, Count
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

write.csv(Counts, file = "Disease Category.csv", quote = FALSE, row.names = F)

#Running all of the searches w/out the first line resetting (s<- DFR[15:19])

#Other Disease Category
s$Counts <- as.numeric(s$Counts)
s[is.na(s)] <- 0
s_other <- filter(s, Counts == 0)
#18 belong to the other disease category


######################################
#More Statistics
#Step 9 cont. Fisher's exact test:

#For Research Purposes, comparing DFR to nonDFR
#Methods
DFRdiff1<- 'x'-RP[1,2]
DFRdiff2<- 'y'-RP[1,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(RP[1,2],RP[1,3],DFRdiff1,DFRdiff2,'x','y'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
View(Research)
Fisher <- fisher.test(Research[1:2,])
RPstats <- print(as.data.frame(Fisher$p.value))
View(RPstats)
#value

#Research Control
DFRdiff1<- 'x'-RP[2,2]
DFRdiff2<- 'y'-RP[2,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(RP[2,2],RP[2,3],DFRdiff1,DFRdiff2,'x','y'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
#View(Research)
Fisher <- fisher.test(Research[1:2,])
#print(Fisher$p.value)
RPstats <- rbind(RPstats, print(as.data.frame(Fisher$p.value)))
View(RPstats)
#value

#Genetic
DFRdiff1<- 'x'-RP[3,2]
DFRdiff2<- 'y'-RP[3,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(RP[3,2],RP[3,3],DFRdiff1,DFRdiff2,'x','y'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
#View(Research)
Fisher <- fisher.test(Research[1:2,])
RPstats <- rbind(RPstats, print(as.data.frame(Fisher$p.value)))
#View(RPstats)

#Social Behavioral
DFRdiff1<- 'x'-RP[4,2]
DFRdiff2<- 'y'-RP[4,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(RP[4,2],RP[4,3],DFRdiff1,DFRdiff2,'x','y'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
Fisher <- fisher.test(Research[1:2,])
RPstats <- rbind(RPstats, print(as.data.frame(Fisher$p.value)))

#Population/Public Health
DFRdiff1<- 'x'-RP[5,2]
DFRdiff2<- 'y'-RP[5,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(RP[5,2],RP[5,3],DFRdiff1,DFRdiff2,'x','y'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
Fisher <- fisher.test(Research[1:2,])
RPstats <- rbind(RPstats, print(as.data.frame(Fisher$p.value)))

#Drug Therapeutics/Dev Research
DFRdiff1<- 'x'-RP[6,2]
DFRdiff2<- 'y'-RP[6,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(RP[6,2],RP[6,3],DFRdiff1,DFRdiff2,'x','y'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
Fisher <- fisher.test(Research[1:2,])
RPstats <- rbind(RPstats, print(as.data.frame(Fisher$p.value)))

#Commercial
DFRdiff1<- 'x'-RP[7,2]
DFRdiff2<- 'y'-RP[7,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(RP[7,2],RP[7,3],DFRdiff1,DFRdiff2,'x','y'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
Fisher <- fisher.test(Research[1:2,])
RPstats <- rbind(RPstats, print(as.data.frame(Fisher$p.value)))

#Educational
DFRdiff1<- 'x'-RP[8,2]
DFRdiff2<- 'y'-RP[8,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(RP[8,2],RP[8,3],DFRdiff1,DFRdiff2,'x','y'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
Fisher <- fisher.test(Research[1:2,])
RPstats <- rbind(RPstats, print(as.data.frame(Fisher$p.value)))

#ELSI
DFRdiff1<- 'x'-RP[9,2]
DFRdiff2<- 'y'-RP[9,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(RP[9,2],RP[9,3],DFRdiff1,DFRdiff2,'x','y'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
Fisher <- fisher.test(Research[1:2,])
RPstats <- rbind(RPstats, print(as.data.frame(Fisher$p.value)))

#Other RP
DFRdiff1<- 'x'-RP[10,2]
DFRdiff2<- 'y'-RP[10,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(RP[10,2],RP[10,3],DFRdiff1,DFRdiff2,'x','y'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
Fisher <- fisher.test(Research[1:2,])
RPstats <- rbind(RPstats, print(as.data.frame(Fisher$p.value)))

RPstats <- cbind(RPstats, c("Methods","Research Control","Genetic","Social Behavioral","Pop/Public Health","Drug Therapeutics","Commercial","Educational","ELSI","Other RP"))
colnames(RPstats) = c("Fisher.pval","RP")
RPstats <- RPstats %>% relocate("RP", .before = "Fisher.pval")
write.csv(RPstats, file = "RPstats.csv", quote = FALSE, row.names = F)

#POIs
#Sex at Birth
DFRdiff1<- 'x'-POI[3,2]
DFRdiff2<- 'y'-POI[3,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(POI[3,2], POI[3,3],DFRdiff1,DFRdiff2,'x','y'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
Fisher <- fisher.test(Research[1:2,])
POIstats <- rbind(POIstats, Fisher$p.value)
View(POIstats)

#Gender
DFRdiff1<- 'x'-POI[4,2]
DFRdiff2<- 'y'-POI[4,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(POI[4,2], POI[4,3],DFRdiff1,DFRdiff2,'x','y'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
Fisher <- fisher.test(Research[1:2,])
POIstats <- rbind(POIstats, Fisher$p.value)

#Sexual Orientation
DFRdiff1<- 'x'-POI[5,2]
DFRdiff2<- 'y'-POI[5,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(POI[5,2], POI[5,3],DFRdiff1,DFRdiff2,'x','y'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
Fisher <- fisher.test(Research[1:2,])
POIstats <- rbind(POIstats, Fisher$p.value)

#Geography
DFRdiff1<- 'x'-POI[6,2]
DFRdiff2<- 'y'-POI[6,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(POI[6,2], POI[6,3],DFRdiff1,DFRdiff2,'x','y'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
View(Research)
Fisher <- fisher.test(Research[1:2,])
POIstats <- rbind(POIstats, Fisher$p.value)

#Disability Status
DFRdiff1<- 'x'-POI[7,2]
DFRdiff2<- 'y'-POI[7,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(POI[7,2], POI[7,3],DFRdiff1,DFRdiff2,'x','y'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
View(Research)
Fisher <- fisher.test(Research[1:2,])
POIstats <- rbind(POIstats, Fisher$p.value)

#Access to Care
DFRdiff1<- 'x'-POI[8,2]
DFRdiff2<- 'y'-POI[8,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(POI[8,2], POI[8,3],DFRdiff1,DFRdiff2,'x','y'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
View(Research)
Fisher <- fisher.test(Research[1:2,])
POIstats <- rbind(POIstats, Fisher$p.value)

#Education
DFRdiff1<- 'x'-POI[9,2]
DFRdiff2<- 'y'-POI[9,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(POI[9,2], POI[9,3],DFRdiff1,DFRdiff2,'x','y'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
View(Research)
Fisher <- fisher.test(Research[1:2,])
POIstats <- rbind(POIstats, Fisher$p.value)

#Income Level
DFRdiff1<- 'x'-POI[10,2]
DFRdiff2<- 'y'-POI[10,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(POI[10,2], POI[10,3],DFRdiff1,DFRdiff2,'x','y'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
View(Research)
Fisher <- fisher.test(Research[1:2,])
POIstats <- rbind(POIstats, Fisher$p.value)

POIstats <- cbind(POIstats, c("Race/Ethnicity","Age","Sex at Birth","Gender","Sexual Orientation","Geography","Disability Status","Access to Care","Education","Income Level"))
colnames(POIstats) = c("Fisher.pval","POI")
POIstats <- POIstats %>% relocate("POI", .before = "Fisher.pval")

#save and export
write.csv(POIstats, file = "POIstats.csv", quote = FALSE, row.names = F)

#Race/Ethnicity Highlight
#Fisher's Test

#Asian
DFRdiff1<- 'x'-Race.count[2,2]
DFRdiff2<- 'y'-Race.count[2,3]

#Create a data-frame to do Fisher's test on
race  <- matrix(c(Race.count[2,2],Race.count[2,3],DFRdiff1,DFRdiff2,'x','y'), ncol=2, byrow=TRUE) 
race  <- as.data.frame(race)
View(race)
Fisher <- fisher.test(race[1:2,])
print(Fisher$p.value)
Racestats <- print(as.data.frame(Fisher$p.value))

#Black
DFRdiff1<- 'x'-Race.count[3,2]
DFRdiff2<- 'y'-Race.count[3,3]

#Create a data-frame to do Fisher's test on
race  <- matrix(c(Race.count[3,2],Race.count[3,3],DFRdiff1,DFRdiff2,'x','y'), ncol=2, byrow=TRUE) 
race  <- as.data.frame(race)
View(race)
Fisher <- fisher.test(race[1:2,])
Racestats <- rbind(Racestats, Fisher$p.value)

#Hispanic
DFRdiff1<- 'x'-Race.count[4,2]
DFRdiff2<- 'y'-Race.count[4,3]

#Create a data-frame to do Fisher's test on
race  <- matrix(c(Race.count[4,2],Race.count[4,3],DFRdiff1,DFRdiff2,'x','y'), ncol=2, byrow=TRUE) 
race  <- as.data.frame(race)
View(race)
Fisher <- fisher.test(race[1:2,])
Racestats <- rbind(Racestats, Fisher$p.value)

#AIAN
DFRdiff1<- 'x'-Race.count[5,2]
DFRdiff2<- 'y'-Race.count[5,3]

#Create a data-frame to do Fisher's test on
race  <- matrix(c(Race.count[5,2],Race.count[5,3],DFRdiff1,DFRdiff2,'x','y'), ncol=2, byrow=TRUE) 
race  <- as.data.frame(race)
#View(race)
Fisher <- fisher.test(race[1:2,])
Racestats <- rbind(Racestats, Fisher$p.value)

#MENA
DFRdiff1<- 'x'-Race.count[6,2]
DFRdiff2<- 'y'-Race.count[6,3]

#Create a data-frame to do Fisher's test on
race  <- matrix(c(Race.count[6,2],Race.count[6,3],DFRdiff1,DFRdiff2,'x','y'), ncol=2, byrow=TRUE) 
race  <- as.data.frame(race)
#View(race)
Fisher <- fisher.test(race[1:2,])
Racestats <- rbind(Racestats, Fisher$p.value)

#NHP
DFRdiff1<- 'x'-Race.count[7,2]
DFRdiff2<- 'y'-Race.count[7,3]

#Create a data-frame to do Fisher's test on
race  <- matrix(c(Race.count[7,2],Race.count[7,3],DFRdiff1,DFRdiff2,'x','y'), ncol=2, byrow=TRUE) 
race  <- as.data.frame(race)
Fisher <- fisher.test(race[1:2,])
Racestats <- rbind(Racestats, Fisher$p.value)

#Multi

DFRdiff1<- 'x'-Race.count[8,2]
DFRdiff2<- 'y'-Race.count[8,3]

#Create a data-frame to do Fisher's test on
race  <- matrix(c(Race.count[8,2],Race.count[8,3],DFRdiff1,DFRdiff2,'x','y'), ncol=2, byrow=TRUE) 
race  <- as.data.frame(race)
Fisher <- fisher.test(race[1:2,])
Racestats <- rbind(Racestats, Fisher$p.value)

Racestats <- cbind(Racestats, c("Asian","Black","Hispanic","AIAN","MENA","NHPI","Multi"))
colnames(Racestats) = c("Fisher.pval","Race/Ethnic Categories")
Racestats <- Racestats %>% relocate("Race/Ethnic Categories", .before = "Fisher.pval")

#save and export
write.csv(Racestats, file = "RaceCategstats.csv", quote = FALSE, row.names = F)

#Age Highlight

#Kids 0-11
DFRdiff1<- 'x'-Age.count[2,2]
DFRdiff2<- 'y'-Age.count[2,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(Age.count[2,2], Age.count[2,3],DFRdiff1,DFRdiff2,'x','y'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
#View(Research)
Fisher <- fisher.test(Research[1:2,])
Agestats <- print(as.data.frame(Fisher$p.value))

#Kids 12-17
DFRdiff1<- 'x'-Age.count[3,2]
DFRdiff2<- 'y'-Age.count[3,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(Age.count[3,2], Age.count[3,3],DFRdiff1,DFRdiff2,'x','y'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
Fisher <- fisher.test(Research[1:2,])
Agestats <- rbind(Agestats, print(as.data.frame(Fisher$p.value)))

#Older 65-74

DFRdiff1<- 'x'-Age.count[4,2]
DFRdiff2<- 'y'-Age.count[4,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(Age.count[4,2], Age.count[4,3],DFRdiff1,DFRdiff2,'x','y'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
#View(Research)
Fisher <- fisher.test(Research[1:2,])
Agestats <- rbind(Agestats, print(as.data.frame(Fisher$p.value)))

#Older 75+

DFRdiff1<- 'x'-Age.count[5,2]
DFRdiff2<- 'y'-Age.count[5,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(Age.count[5,2], Age.count[5,3],DFRdiff1,DFRdiff2,'x','y'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
#View(Research)
Fisher <- fisher.test(Research[1:2,])
Agestats <- rbind(Agestats, print(as.data.frame(Fisher$p.value)))

Agestats <- cbind(Agestats, c("Children 0-11","Children 12-17","Older Adults 65-74","Older Adults 75+"))
colnames(Agestats) = c("Fisher.pval","Age Categories")
Agestats <- Agestats %>% relocate("Age Categories", .before = "Fisher.pval")

#save and export
write.csv(Agestats, file = "AgeCategstats.csv", quote = FALSE, row.names = F)

#Genomics Highlight
#. Step 1: Filter out the genomic workspaces (genetic research)
View(RP12DFR) #All genetic ones are checkmarked

#Search for 'hypertension'. Count
s <- RP12DFR[15:19]
#write.csv(s, file = "RP12DFR.csv")

#loop
search_terms <- c('Hypertension','hypertension', 'hypertensive','Hypertensive')
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

#Obesity
s<- RP12DFR[15:19]

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

#COVID-19.
s<- RP12DFR[15:19]
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

#Diabetes
s<- RP12DFR[15:19]
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

#Cancer & Tumors
s<- RP12DFR[15:19]
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
s<- RP12DFR[15:19]
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

#Mental Health, Neurological Concerns, Cognitive Impairment:
##Includes psychiatric disorders, ADHD, neuropsychiatric study, ADRD/AD, depression/anxiety, mental health, cognitive, dementia, traumatic brain injury, MDD, brain

s<- RP12DFR[15:19]
#loop
search_terms<- c('Psychiatric','Psychological','psychological','psychiatric', 'attention deficit','autism','Autism','Neurodev','neurodevelopmental','ADHD','alzheimer',"Alzheimer",'Depressi', 'depressi','Anxiety','anxiety','Mental','mental','Cogniti','cogniti', 'Dementia','dementia','Brain','brain','Mood','mood','cerebral',"Cerebral",
                 'psychosocial', 'psychopathic', 'intellectual disability',
                 'neuronal', 'Synder-Robinson','WAGR','Baraitser-Winter','encephalopathy','Parkinsonism',
                 'MASA', 'Partington', 'GMS', 'MORM', 'cerebellar ataxia', 'cranial', 'TBI', 'concussion',
                 'CSF','choroid plexus','CNS', 'nervous system','nerve'
)
for(i in search_terms) {
  i1<- grepl(i, s$Disease.Focused.Research.Name)
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

#For Cardio: Cardiovascular Disease, CVD, Congestive Heart Failure, Cardiometabolic, ASCVD, A-fibrillation,, stroke

s<- RP12DFR[15:19]

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

s<- RP12DFR[15:19]
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

#Immune: inflammation, infectious disease, SLE, lupus, autoimmune, sepsis, allergies
s<- RP12DFR[15:19]
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

#Sleep: insomnia, narcolepsy, sleep, apnea
s<- RP12DFR[15:19]
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

s<- RP12DFR[15:19]
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
s<- RP12DFR[15:19]
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

#Dermatology: neurofibromatosis, psoriasis, psoriatic disease
s<- R[15:19]
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
write.csv(Counts, file = "Disease Category.Genomic.csv", quote = FALSE, row.names = F)

##################Fisher for 4/21 and 6/30

#Import in the 2 counts files
#Make sure to manually add in Other before beginning.
DC630 <- read.csv(file="Disease Category.csv")
colnames(DC630)[2] = ("Counts.630")
#View(DC630)
DC421 <- read.csv(file="Disease Category_421.csv")
colnames(DC421)[2] = ("Counts.421")
#View(DC421)
mydata <- cbind(DC630, DC421[2])
View(mydata)

#'z' refers to # of DFR workspaces in dataset A (4/21) and 'x' refers to the same value for dataset C (06/30)

#Hypertension
DFRdiff1<- 'x'-mydata[1,2]
DFRdiff2<- 'z'-mydata[1,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(mydata[1,2], mydata[1,3],DFRdiff1,DFRdiff2,'x','z'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
#View(Research)
Fisher <- fisher.test(Research[1:2,])
Timestats <- print(as.data.frame(Fisher$p.value))
View(Timestats)

#Obesity
DFRdiff1<- 'x'-mydata[2,2]
DFRdiff2<- 'z'-mydata[2,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(mydata[2,2], mydata[2,3],DFRdiff1,DFRdiff2,'x','z'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
#View(Research)
Fisher <- fisher.test(Research[1:2,])
Timestats <- rbind(Timestats, Fisher$p.value)
View(Timestats)

#COVID-19
DFRdiff1<- 'x'-mydata[3,2]
DFRdiff2<- 'z'-mydata[3,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(mydata[3,2], mydata[3,3],DFRdiff1,DFRdiff2,'x','z'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
#View(Research)
Fisher <- fisher.test(Research[1:2,])
Timestats <- rbind(Timestats, Fisher$p.value)
View(Timestats)

#Diabetes
DFRdiff1<- 'x'-mydata[4,2]
DFRdiff2<- 'z'-mydata[4,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(mydata[4,2], mydata[4,3],DFRdiff1,DFRdiff2,'x','z'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
#View(Research)
Fisher <- fisher.test(Research[1:2,])
Timestats <- rbind(Timestats, Fisher$p.value)
View(Timestats)

#Cancer & Tumors
DFRdiff1<- 'x'-mydata[5,2]
DFRdiff2<- 'z'-mydata[5,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(mydata[5,2], mydata[5,3],DFRdiff1,DFRdiff2,'x','z'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
#View(Research)
Fisher <- fisher.test(Research[1:2,])
Timestats <- rbind(Timestats, Fisher$p.value)
View(Timestats)

#Reproduction
DFRdiff1<- 'x'-mydata[6,2]
DFRdiff2<- 'z'-mydata[6,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(mydata[6,2], mydata[6,3],DFRdiff1,DFRdiff2,'x','z'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
#View(Research)
Fisher <- fisher.test(Research[1:2,])
Timestats <- rbind(Timestats, Fisher$p.value)
View(Timestats)

#Mental Health/Neurodev Issues
DFRdiff1<- 'x'-mydata[7,2]
DFRdiff2<- 'z'-mydata[7,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(mydata[7,2], mydata[7,3],DFRdiff1,DFRdiff2,'x','z'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
#View(Research)
Fisher <- fisher.test(Research[1:2,])
Timestats <- rbind(Timestats, Fisher$p.value)
View(Timestats)

#CVD
DFRdiff1<- 'x'-mydata[8,2]
DFRdiff2<- 'z'-mydata[8,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(mydata[8,2], mydata[8,3],DFRdiff1,DFRdiff2,'x','z'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
#View(Research)
Fisher <- fisher.test(Research[1:2,])
Timestats <- rbind(Timestats, Fisher$p.value)
View(Timestats)

#Respiratory
DFRdiff1<- 'x'-mydata[9,2]
DFRdiff2<- 'z'-mydata[9,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(mydata[9,2], mydata[9,3],DFRdiff1,DFRdiff2,'x','z'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
#View(Research)
Fisher <- fisher.test(Research[1:2,])
Timestats <- rbind(Timestats, Fisher$p.value)
View(Timestats)

#Immunology
DFRdiff1<- 'x'-mydata[10,2]
DFRdiff2<- 'z'-mydata[10,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(mydata[10,2], mydata[10,3],DFRdiff1,DFRdiff2,'x','z'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
#View(Research)
Fisher <- fisher.test(Research[1:2,])
Timestats <- rbind(Timestats, Fisher$p.value)
View(Timestats)

#Sleep
DFRdiff1<- 'x'-mydata[11,2]
DFRdiff2<- 'z'-mydata[11,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(mydata[11,2], mydata[11,3],DFRdiff1,DFRdiff2,'x','z'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
#View(Research)
Fisher <- fisher.test(Research[1:2,])
Timestats <- rbind(Timestats, Fisher$p.value)
View(Timestats)

#Musculoskeletal
DFRdiff1<- 'x'-mydata[12,2]
DFRdiff2<- 'z'-mydata[12,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(mydata[12,2], mydata[12,3],DFRdiff1,DFRdiff2,'x','z'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
#View(Research)
Fisher <- fisher.test(Research[1:2,])
Timestats <- rbind(Timestats, Fisher$p.value)
View(Timestats)

#GI & Endocrine
DFRdiff1<- 'x'-mydata[13,2]
DFRdiff2<- 'z'-mydata[13,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(mydata[13,2], mydata[13,3],DFRdiff1,DFRdiff2,'x','z'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
#View(Research)
Fisher <- fisher.test(Research[1:2,])
Timestats <- rbind(Timestats, Fisher$p.value)
View(Timestats)

#Dermatology
DFRdiff1<- 'x'-mydata[14,2]
DFRdiff2<- 'z'-mydata[14,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(mydata[14,2], mydata[14,3],DFRdiff1,DFRdiff2,'x','z'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
#View(Research)
Fisher <- fisher.test(Research[1:2,])
Timestats <- rbind(Timestats, Fisher$p.value)

#Other
DFRdiff1<- 'x'-mydata[15,2]
DFRdiff2<- 'z'-mydata[15,3]
Research <- matrix(c(mydata[15,2], mydata[15,3], DFRdiff1, DFRdiff2,'x','z'), ncol=2, byrow=TRUE)
Research <- as.data.frame(Research)
View(Research)
Fisher <- fisher.test(Research[1:2,])
Timestats <- rbind(Timestats, Fisher$p.value)

library(dplyr)
Timestats <- cbind(Timestats, mydata[1])
View(Timestats)

#save and export
write.csv(Timestats,file="DCovertime.csv")

#Genomics workspaces over time
#Import in the 2 counts files
DC630 <- read.csv(file="Disease Category.Genomic.csv")
colnames(DC630)[2] = ("Counts.630")
#View(DC630)
DC421 <- read.csv(file="Disease Category.Genomics_421.csv")
colnames(DC421)[2] = ("Counts.421")
#View(DC421)
mydata <- cbind(DC630, DC421[2])
View(mydata)

#'a' counts for DFR in workspaces with a genomic focus in 06/30 dataset
#'b' counrs for DFR  in workspaces with a genomic focus in 4/21 dataset

#Hypertension
DFRdiff1<- 'a'-mydata[1,2]
DFRdiff2<- 'b'-mydata[1,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(mydata[1,2], mydata[1,3],DFRdiff1,DFRdiff2,'a','b'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
#View(Research)
Fisher <- fisher.test(Research[1:2,])
Genomstats <- print(as.data.frame(Fisher$p.value))
View(Genomstats)

#Obesity
DFRdiff1<- 'a'-mydata[2,2]
DFRdiff2<- 'b'-mydata[2,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(mydata[2,2], mydata[2,3],DFRdiff1,DFRdiff2,'a','b'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
#View(Research)
Fisher <- fisher.test(Research[1:2,])
Genomstats <- rbind(Genomstats, Fisher$p.value)
View(Genomstats)

#COVID-19
DFRdiff1<- 'a'-mydata[3,2]
DFRdiff2<- 'b'-mydata[3,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(mydata[3,2], mydata[3,3],DFRdiff1,DFRdiff2,'a','b'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
#View(Research)
Fisher <- fisher.test(Research[1:2,])
Genomstats <- rbind(Genomstats, Fisher$p.value)
View(Genomstats)

#Diabetes
DFRdiff1<- 'a'-mydata[4,2]
DFRdiff2<- 'b'-mydata[4,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(mydata[4,2], mydata[4,3],DFRdiff1,DFRdiff2,'a','b'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
#View(Research)
Fisher <- fisher.test(Research[1:2,])
Genomstats <- rbind(Genomstats, Fisher$p.value)
View(Genomstats)

#Cancer & Tumors
DFRdiff1<- 'a'-mydata[5,2]
DFRdiff2<- 'b'-mydata[5,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(mydata[5,2], mydata[5,3],DFRdiff1,DFRdiff2,'a','b'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
#View(Research)
Fisher <- fisher.test(Research[1:2,])
Genomstats <- rbind(Genomstats, Fisher$p.value)
View(Genomstats)

#Reproduction
DFRdiff1<- 'a'-mydata[6,2]
DFRdiff2<- 'b'-mydata[6,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(mydata[6,2], mydata[6,3],DFRdiff1,DFRdiff2,'a','b'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
#View(Research)
Fisher <- fisher.test(Research[1:2,])
Genomstats <- rbind(Genomstats, Fisher$p.value)
View(Genomstats)

#Mental Health/Neurodev Issues
DFRdiff1<- 'a'-mydata[7,2]
DFRdiff2<- 'b'-mydata[7,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(mydata[7,2], mydata[7,3],DFRdiff1,DFRdiff2,'a','b'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
#View(Research)
Fisher <- fisher.test(Research[1:2,])
Genomstats <- rbind(Genomstats, Fisher$p.value)
View(Genomstats)

#CVD
DFRdiff1<- 'a'-mydata[8,2]
DFRdiff2<- 'b'-mydata[8,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(mydata[8,2], mydata[8,3],DFRdiff1,DFRdiff2,'a','b'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
#View(Research)
Fisher <- fisher.test(Research[1:2,])
Genomstats <- rbind(Genomstats, Fisher$p.value)
View(Genomstats)

#Respiratory
DFRdiff1<- 'a'-mydata[9,2]
DFRdiff2<- 'b'-mydata[9,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(mydata[9,2], mydata[9,3],DFRdiff1,DFRdiff2,'a','b'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
#View(Research)
Fisher <- fisher.test(Research[1:2,])
Genomstats <- rbind(Genomstats, Fisher$p.value)
View(Genomstats)

#Immunology
DFRdiff1<- 'a'-mydata[10,2]
DFRdiff2<- 'b'-mydata[10,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(mydata[10,2], mydata[10,3],DFRdiff1,DFRdiff2,'a','b'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
#View(Research)
Fisher <- fisher.test(Research[1:2,])
Genomstats <- rbind(Genomstats, Fisher$p.value)
View(Genomstats)

#Sleep
DFRdiff1<- 'a'-mydata[11,2]
DFRdiff2<- 'b'-mydata[11,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(mydata[11,2], mydata[11,3],DFRdiff1,DFRdiff2,'a','b'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
#View(Research)
Fisher <- fisher.test(Research[1:2,])
Genomstats <- rbind(Genomstats, Fisher$p.value)
View(Genomstats)

#Musculoskeletal
DFRdiff1<- 'a'-mydata[12,2]
DFRdiff2<- 'b'-mydata[12,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(mydata[12,2], mydata[12,3],DFRdiff1,DFRdiff2,'a','b'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
#View(Research)
Fisher <- fisher.test(Research[1:2,])
Genomstats <- rbind(Genomstats, Fisher$p.value)
View(Genomstats)

#GI & Endocrine
DFRdiff1<- 'a'-mydata[13,2]
DFRdiff2<- 'b'-mydata[13,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(mydata[13,2], mydata[13,3],DFRdiff1,DFRdiff2,'a','b'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
#View(Research)
Fisher <- fisher.test(Research[1:2,])
Genomstats <- rbind(Genomstats, Fisher$p.value)
View(Genomstats)

#Dermatology
DFRdiff1<- 'a'-mydata[14,2]
DFRdiff2<- 'b'-mydata[14,3]
#Create a data-frame to do Fisher's test on
Research  <- matrix(c(mydata[14,2], mydata[14,3],DFRdiff1,DFRdiff2,'a','b'), ncol=2, byrow=TRUE) 
Research <- as.data.frame(Research)
#View(Research)
Fisher <- fisher.test(Research[1:2,])
Genomstats <- rbind(Genomstats, Fisher$p.value)
library(dplyr)
Genomstats <- cbind(Genomstats, mydata[1])
View(Genomstats)

#save and export
write.csv(Genomstats,file="DC.Genomics.overtime.csv")

#Demonstration
#Searching for demonstration workspaces, did not end up in final analysis.

# s1<- DFR[3]
# s1 <- cbind(s1, DFR[15:19])

#s1<- s1 %>% drop_na
#loop
# search_terms<- c('Demonstration','demonstrate','demo project', 'copy of demo', 'demonstration','Demonstrate', 'Demo project','tutorial',"Tutorial")
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
# }
# 
# #counts
# s1$Counts <- as.numeric(s1$Counts)
# s1[is.na(s1)] <- 0
# 
# sum(s1[,7])
# DFR_demo <- filter(s1, Counts == 1 & Institutional.Affiliation != 'AouOps')
# View(DFR_demo)
# write.csv(DFR_demo, file = "DFR_demo.csv", quote = FALSE, row.names = F)
# 
# #For non-DFR
# s2<- NonDFR[3]
# s2 <- cbind(s2, NonDFR[15:19])
# 
# #loop
# search_terms<- c('Demonstration','demonstrate','demo project', 'copy of demo', 'demonstration','Demonstrate', 'Demo project','tutorial',"Tutorial")
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
# }
# 
# #counts
# s2$Counts <- as.numeric(s2$Counts)
# s2[is.na(s2)] <- 0
# 
# sum(s2[,7])
# nonDFR_demo <- filter(s2, Counts == 1 & Institutional.Affiliation != 'AouOps')
# View(nonDFR_demo)
# write.csv(DFR_demo, file = "nonDFR_demo.csv", quote = FALSE, row.names = F)

