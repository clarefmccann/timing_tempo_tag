###LOAD PACKAGES AND SET DIRECTORIES ######

pacman::p_load("psych", "xlsx", "dplyr", "tidyr", install = TRUE)

#cas_dir <- '/Volumes/psych-cog/dsnlab/TAG/'
#age_dir <- paste0(cas_dir,'behavior/Demographics/')
#quest_dir <- paste0(cas_dir,'behavior/Questionnaires/')
#puberty_dir <- paste0(cas_dir,'behavior/Questionnaires/Puberty/')

cas_dir <- '/Volumes/psych-cog/dsnlab/TAG/'
quest_dir <- paste0(cas_dir, "behavior/Questionnaires/Long/")
age_dir <- paste0(cas_dir, "behavior/Demographics/")
proj_dir <- paste0(cas_dir, "projects/timing_tempo_tag/")

options(digits=3)

###IMPORT TAG OVERVIEW DOC FROM CAS ######

overview <- read.xlsx(paste0(cas_dir,'behavior/Overview/Overview_Withdrawn_Completed/TAG_Overview_Doc.xlsx'),1)

overview <- overview[,c("TAG_ID","W1S2_Completed","Withdrawn_W1","Exclusionary_Withdrawl")]

#removing everyone who withdrew at W1 (exclusionary withdrawals) -- has anyone withdrawn their data recently?? 
overview <- overview %>%
  mutate(Exclusionary_Withdrawl = replace_na(Exclusionary_Withdrawl, "0")) %>% 
  mutate(Withdrawn_W1 = replace_na(Withdrawn_W1, "0")) %>% 
  arrange(Exclusionary_Withdrawl) %>%
  filter(Exclusionary_Withdrawl=="0") %>%
  filter(Withdrawn_W1=="0") %>% 
  mutate(tagid=paste0("TAG",
                      substring(TAG_ID,first=5,last=length(TAG_ID))))


###### LOAD PDS, PBIP AND AGE  ######
#file_list_pds = Sys.glob(paste0(quest_dir, 'Wave*/PDS_Wave*.csv'))
#file_list_pbip = Sys.glob(paste0(quest_dir,'Wave*/PBIP_Wave*.csv'))
file_list_pds = Sys.glob(paste0(quest_dir, 'PDS_AllObs*.csv'))
file_list_pbip = Sys.glob(paste0(quest_dir,'PBIP_AllObs*.csv'))


PDS <- read.csv(paste0(quest_dir, "PDS_AllObs_Chronological.csv"))
PBIP <- read.csv(paste0(quest_dir, "PBIP_AllObs_Chronological.csv"))

Age <- read.table(paste0(age_dir, "allwaves_age_long_06.13.2025.csv"), header = TRUE, sep = ",") 

Allwaves_Puberty_Q <- left_join(PDS, PBIP,  by=c("tagid","wave"))

#If there are duplicates for TAG077 W2 and TAG124 W2, this is because these pp completed the W2 version at Wave 2 and 3
# same for TAG001 and TAG152 (completed wave 1 questionnaire at w5)

Allwaves_Puberty_Q <- Allwaves_Puberty_Q[!(Allwaves_Puberty_Q$tagid=="TAG077" & Allwaves_Puberty_Q$pdss==3.5 & Allwaves_Puberty_Q$stage==2),]

Allwaves_Puberty_Q <- Allwaves_Puberty_Q[!(Allwaves_Puberty_Q$tagid=="TAG077" & Allwaves_Puberty_Q$pdss==2 & Allwaves_Puberty_Q$stage==3),]

Allwaves_Puberty_Q$wave[Allwaves_Puberty_Q$tagid=="TAG077" & Allwaves_Puberty_Q$pdss==3.5 & Allwaves_Puberty_Q$stage==3] = 3

Allwaves_Puberty_Q <- Allwaves_Puberty_Q[!(Allwaves_Puberty_Q$tagid=="TAG124" & Allwaves_Puberty_Q$pdss==5 & Allwaves_Puberty_Q$stage==2.5),]

Allwaves_Puberty_Q <- Allwaves_Puberty_Q[!(Allwaves_Puberty_Q$tagid=="TAG124" & Allwaves_Puberty_Q$pdss==3 & Allwaves_Puberty_Q$stage==4),]

Allwaves_Puberty_Q$wave[Allwaves_Puberty_Q$tagid=="TAG124" & Allwaves_Puberty_Q$pdss==5 & Allwaves_Puberty_Q$stage==4] = 3

Allwaves_Puberty_Q$wave[Allwaves_Puberty_Q$tagid == "TAG001" & 
                          Allwaves_Puberty_Q$pdss == 5 & 
                          Allwaves_Puberty_Q$stage == 4.5] <- 5

Allwaves_Puberty_Q$wave[Allwaves_Puberty_Q$tagid == "TAG152" & 
                          Allwaves_Puberty_Q$pdss == 5 & 
                          Allwaves_Puberty_Q$stage == 5] <- 5

## age at w5 is also age at w1 so correcting based on w5, s2 date and dob

Allwaves_Puberty_Q$age[Allwaves_Puberty_Q$tagid == "TAG152" & 
                          Allwaves_Puberty_Q$wave == 5] <- 17.1

## this calculation is wrong so removing this duplicate
Allwaves_Puberty_Q <- Allwaves_Puberty_Q[!(Allwaves_Puberty_Q$tagid=="TAG001" & Allwaves_Puberty_Q$pdss==5 & Allwaves_Puberty_Q$stage==2.5),]
  
Allwaves_Puberty_Q <- merge(Allwaves_Puberty_Q, Age, by=c("tagid","wave"), all.x=T) %>%
  filter(tagid %in% overview$tagid) %>% 
  select(tagid, wave, adrenf2, PBIP_1A, PBIP_2A, gonadf2, pdss, stage, survey_date.x) 


###CREATE COMPOSITE ADRENAL AND GONADAL AND PUBERTAL SCORES FROM PDS & PBIP ######
#calculates mean of adrenal and gonadal composites that are mean of PBIP and PDS items,
PubertyComposite <- Allwaves_Puberty_Q %>%
  mutate(ADRENdiff = adrenf2-PBIP_2A,
         GONADdiff = gonadf2-PBIP_1A) %>%
  rowwise %>%
  mutate(ADRENcomp = mean(c(adrenf2,PBIP_2A),na.rm=T), #if only one measure (i.e. PDS or PBIP) is available, will use that instead of NA.
         GONADcomp = mean(c(gonadf2,PBIP_1A),na.rm=T),
         PUBcomp = mean(c(pdss, stage),na.rm=T)) 

## DOES 15 HAVE TWO W5 / LOOK AT TAG022 W5 / TAG050 / TAG051, TAG079, TAG084, TAG085, TAG106, TAG119, TAG122, TAG179, TAG190, TAG202, TAG203, TAG207, TAG218, TAG225, TAG238, TAG250??
## W4 -- TAG027, TAG074, TAG077, TAG087, TAG106, TAG110, TAG116, TAG119, TAG122, TAG127, TAG155, TAG175, TAG179, TAG188, TAG190, TAG202, TAG203, TAG206, TAG215, TAG218, TAG220, TAG223, TAG225, TAG238, TAG253, TAG261
## W3 -- TAG124
## W5 at W1 -- TAG152


PubertyMissing <- PubertyComposite %>% filter(is.na(PUBcomp))

PubertyComposite <- left_join(PubertyComposite, Age, by = c("tagid", "wave"))

###SAVE COMPOSITE SCORES ######
write.csv(PubertyComposite,paste0(proj_dir,'Allwaves_PubertyComposite_updated.csv'),row.names=F)

