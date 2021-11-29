# tabula rasa
rm(list = ls()) #clears environment
cat("\014") #clears console

#loading libraries
library(tidyverse)
library(tidycensus)
library(dplyr)
library(readr)
library(sf)
library(ggplot2)
library(ggmap)
library(pastecs)
library(janitor)
library(tidyr)
library(rgdal)
library(RColorBrewer)

options(tigris_use_cache = TRUE)

#set working directory to the location where data is stored
setwd("D:/Folder")

#run below hashed out code with FALSE to install an existing API (in place of API_HERE) on a new device
#census_api_key("API_HERE", overwrite=TRUE, install = TRUE) 
Sys.getenv("CENSUS_API_KEY")
#==============================
#2014-18 Census API for Median Household Incnome by zip code
mhhinc = get_acs(
  geography = "zip code tabulation area",
  variables = c("B19013_001"),
  year = 2018,
  output = "wide",
  geometry = FALSE
) %>% rename(zip_code = GEOID, zcta = NAME, median_income = B19013_001E, mhhinc_moe = B19013_001M)


#==============================
#2014-18 Census API for Computer and Internet by zip code
computer = get_acs(
  geography = "zip code tabulation area",
  variables = c("B28003_001", "B28003_002", "B28003_003", "B28003_004", "B28003_005", "B28003_006"),
  year = 2018,
  output = "wide",
  geometry = FALSE
) %>% rename(zip_code = GEOID, zcta = NAME, total = B28003_001E, total_moe = B28003_001M, has_comp_total = B28003_002E, has_comp_total_moe = B28003_002M,
             dialup = B28003_003E, dialup_moe = B28003_003M, bband = B28003_004E, bband_moe = B28003_004M,
             no_int = B28003_005E, no_int_moe = B28003_005M, no_comp = B28003_006E, no_comp_moe = B28003_006M)

#calculate percent of people with computer by zip code
computer$perc_comp<-(computer$has_comp_total/computer$total)*100
#calculate percent of people without computer by zip code
computer$perc_no_comp<-(computer$no_comp/computer$total)*100
#calculate percent of people without home internet by zip code
computer$perc_no_int<-(computer$no_int/computer$total)*100

#read in csv file of registered student data and drop unneeded columns
students <- read.csv("reg_student_0FIPS.csv")
students = subset(students, select = -c(Term.Code,Name...Full,Name...Prefix, Preferred.Last.Name, Preferred.First.Name, Name...Suffix, Student.Type.Code, First.Semester.Attended, Last.Semester.Attended, College,Education.Goal.Code, Education.Goal.Description, Registration.Status.Code,Registration.Status.Description, Financial.Aid.Application.Received.Date, Bill.Date, Hold.Indicator, Associated.Campus.Center.City.Indicator, Associated.Campus.Fort.Washington.Indicator, Associated.Campus.Harrisburg.Indicator, Associated.Campus.HSC.Indicator, Associated.Campus.Main.Indicator, Co.Op.Code.1,Co.Op.Code.2,Co.Op.Code.3, Email.Type.Code,Email.Address,Confidentiality.Indicator, Deceased.Indicator, 
                                        Phone.Number,Phone.Extension, Faculty.Advisor.Indicator, Advisor.ID, Advisor.First.Name, Advisor.Last.Name,Financial.Aid.Year, Religion.Code, Marital.Status.Code, Count.ID., Associated.Campus.Ambler.Indicator, Associated.Campus.Online.Indicator, Bill.Due.Date, Veteran.Indicator, Graduate.Level.Code, Hours.Attempted.G, Hours.Passed.G, GPA.G, GPA.Hours.G, Overall.Hours.Passed.G, Transfer.Hours.G, Grade.Points.G, Cumulative.Credits.G, Cumulative.Quality.Points.G, Professional.Level.Code, Hours.Attempted.P, Hours.Passed.P, GPA.Hours.P, Grade.Points.P, GPA.P, Overall.Hours.Passed.P, Transfer.Hours.P, Cumulative.Credits.P, Cumulative.Quality.Points.P, Continuing.Education.Level.Code, Hours.Attempted.CE,
                                        Hours.Passed.CE, GPA.Hours.CE, Grade.Points.CE, GPA.CE, Overall.Hours.Passed.CE, Transfer.Hours.CE, Cumulative.Credits.CE, Cumulative.Quality.Points.CE) )

#snakecase
students = students%>% clean_names()

#identify main campus students only
students_main <- subset(students, calculated_campus_code=="MN" | calculated_campus_code=="CC")

#add leading 0's for join to census data
students_main$address_county_code <- as.character(students_main$address_county_code)

students_elsewhere <-subset(students, calculated_campus_code!="MN" & calculated_campus_code!="CC")

#join students_main with computer data from Census 
students_main <- left_join(students_main, computer, by="zip_code")

#join students_main with income data from Census 
students_main <- left_join(students_main, mhhinc, by="zip_code")


#save as csv to path in quotation marks
write.csv(students_main,"D:/Capstone/students_main.csv", row.names=TRUE)

#=====================================



#convert zip code to 5-digits
students$zip_code<-substr(students$address_zip,1, 5)

#count number of students in each unique county_code
cc_count <- students %>% count(address_county_code, sort=TRUE, name= "student_count")
#add leading 0's for joining
cc_count$address_county_code<-sprintf("%05d", cc_count$address_county_code)
#save as csv to path in quotation marks
write.csv(cc_count,"D:/Capstone/cc_count.csv", row.names=TRUE)
#===============================================
#categorize domestic (in-state, out-of-state, mismatched) and international residency
res_cat <- students %>%
  mutate(residency = case_when(
    residency_code == "R" & address_state_code == "PA"~ "pa",
    residency_code == "R" & address_state_code != "PA"~ "mismatch",
    address_state_code == "PA" ~ "mismatch",
    address_nation_code == 157 ~ "non_pa",
    TRUE ~ "international"
  ))

#selecting 16 variables
student_address = subset(res_cat, select = c(name_last, name_first, id, residency_code,address_street_line_1, address_county_code, residency_description, address_city, address_county_description, address_county_code, address_state_code, address_nation_code, address_nation_description, citizenship_description, residency, campus_of_record_description))

#select only those with mismatched residency (permanent address vs. financial aid)
mismatch <-subset(student_address, residency=="mismatch")

#get unique types of citizenship description
unique(mismatch$citizenship_description)

#subsetting students by citizenship
nra <- subset(mismatch, citizenship_description=="Nonresident Alien")
pr <- subset(mismatch, citizenship_description=="Permanent Resident")
citizen <-subset(mismatch, citizenship_description=="U.S. Citizen")

#subsetting students by standing
standing_gs <- subset(students_main, academic_standing_description_in_term_selected=="Academic Good Standing")
standing_aw <- subset(students_main, academic_standing_description_in_term_selected=="Academic Warning")
standing_ap <-subset(students_main, academic_standing_description_in_term_selected=="Academic Probation")

#save as csv to path in quotation marks
write.csv(mismatch,"D:/Capstone/mismatch.csv", row.names=TRUE)

#================================
#read in midterm data
midterm <- read.csv("midterm.csv")
midterm = subset(midterm, select = -c(Name...Initial,Student.Campus.of.Record,Student.Campus.of.Record.Description,Instructor.ID,Instructor.Last.Name,Instructor.First.Name,Instructor.Middle.Intial, Confidentiality.Indicator) )
#convert to snakecase
midterm = midterm %>% clean_names()

#forward fill to bring TUid, semester hours into every row
midterm <- midterm%>%fill(total_semester_hours_registered, id, program_1)

#count number of records
#count(midterm, vars="ID", sort=TRUE)

#identify nulls
midterm$mid_term_grade_code[midterm$mid_term_grade_code==""] <- NA

#add column for satisfactory, unsatisfactory, or never attended to simplify midterm ratings
midterm <- midterm %>%
  mutate(sun_recode = case_when(
    mid_term_grade_code == "N" ~ "NA",
    mid_term_grade_code == "S" ~ "satisfactory",
    mid_term_grade_code == "WNR" ~ "NA",
    TRUE ~ "unsatisfactory"
  ))

#convert to numeric and add leading 0 to Gen Eds
midterm$course_number<-as.numeric(midterm$course_number)

sprintf("%04d", midterm$course_number)

#add course levels
midterm<- midterm %>%
  mutate(course_level = case_when(
    course_number < 1000 ~ "Gen Ed",
    course_number >= 1000 & course_number < 2000 ~ "1000",
    course_number >= 2000 & course_number < 3000 ~ "2000",
    course_number >= 3000 & course_number < 4000 ~ "3000",
    course_number >= 4000 & course_number < 5000 ~ "4000", 
    TRUE ~ "Graduate"
  ))


#check unique values for midterm grade codes
unique(midterm$mid_term_grade_code)

#======================================

#read in credit no credit grade mode report
crnc <- read.csv("grade_mode.csv")
crnc<-subset(crnc, select= -c(PREFERRED_LAST_NAME, PREFERRED_FIRST_NAME, PRIMARY_PROGRAM, PROGRAM_DESCRIPTION, TERM, TERM_DESCRIPTION, COURSE_LEVEL, EMAIL, TO_CHAR.SFRSTCR_ACTIVITY_DATE..MMDDYYYYHH24.MI.., SFRSTCR_USER))

#convert to snakecase
crnc = crnc%>%clean_names()

#subsetting to include only Klein primary majors (1,660 records)
crnc<-subset(crnc, student_college_desc=="Media & Comm, Klein College")
#get complete list of Klein majors in report
unique(crnc["primary_major"])
#dropping graduate programs (1,652 records remaining)
crnc<- crnc[-grep("MDCM", crnc$primary_major),]
crnc<- crnc[-grep("CDSC", crnc$primary_major),]

crnc$course_number<-as.numeric(crnc$course_number)

sprintf("%04d", crnc$course_number)

#add course levels
crnc<- crnc %>%
  mutate(course_level = case_when(
    course_number < 1000 ~ "Gen Ed",
    course_number >= 1000 & course_number < 2000 ~ "1000",
    course_number >= 2000 & course_number < 3000 ~ "2000",
    course_number >= 3000 & course_number < 4000 ~ "3000",
    course_number >= 4000 & course_number < 5000 ~ "4000", 
    TRUE ~ "Graduate"
  ))

#group by TUid
crnc%>%group_by(tuid)

unique(crnc$tuid)

#====================================
##CREDIT/NO CREDIT STUDENTS, ANY CAMPUS 
#join midterm and crnc keeping all crnc records & group by tuid
mid_crnc <-left_join(crnc, midterm, by=c("tuid"="id", "crn"="crn"))%>%group_by(tuid)

#determine maximum number of midterm grades for a student (7)
#max(table(mid_crnc$tuid))

#return average credits for all students in semester (15.5)
#mean(mid_crnc$total_semester_hours_registered)

#join reg_student and midterm 
mid_crnc_students<-left_join(mid_crnc, students, by=c("tuid"="id"))

#join reg_midterm with computer data from Census 
census_mid_crnc <- left_join(mid_crnc_students, computer, by="zip_code")

#join census_mid_Crnc with median household income data from Census
census_mid_crnc <- left_join(census_mid_crnc, mhhinc, by="zip_code")

#returns number of unique TUids
group_keys(census_mid_crnc)

unique(census_mid_crnc$tuid)
unique(census_mid_crnc$pot_description)

#subset by each unique part of term
tuj<-subset(census_mid_crnc, pot_description=="Temple Japan")%>%group_by(tuid)
unique(tuj$tuid)

ft<-subset(census_mid_crnc, pot_description=="Full Term Courses")%>%group_by(tuid)
unique(ft$tuid)

sevenb<-subset(census_mid_crnc, pot_description=="7-week Courses (7B)")%>%group_by(tuid)
unique(sevenb$tuid)

sdc<-subset(census_mid_crnc, pot_description=="Short Duration Course")%>%group_by(tuid)
unique(sdc$tuid)

rome<-subset(census_mid_crnc, pot_description=="Study Abroad: Rome")%>%group_by(tuid)
unique(rome$tuid)

#identify main campus students
crnc_main_only<-census_mid_crnc[!is.na(census_mid_crnc$gender),]%>%group_by(tuid)
crnc_main_only<-subset(crnc_main_only, part_of_term!="S01")
crnc_main_only<-subset(crnc_main_only, part_of_term!="TUJ")
crnc_main_only<-subset(crnc_main_only, calculated_campus_code!="ROM")
crnc_main_only<-subset(crnc_main_only, calculated_campus_code!="DUB")
crnc_main_only<-subset(crnc_main_only, calculated_campus_code!="OFF")
#confirm only main campus remains as calculated campus for semester
unique(crnc_main_only$calculated_campus_code)

#write CSV file
write.csv(crnc_main_only,"D:/Capstone/crnc_main_only.csv", row.names=TRUE)

#=========================
#CR/NC Analysis
#get each unique race in the international reported race column
unique(census_mid_crnc$international_reported_race)

#subset crnc dataset by each unique race
afa<-subset(census_mid_crnc, international_reported_race=="AFA")%>%group_by(tuid)
unique(afa$tuid)

asn<-subset(census_mid_crnc, international_reported_race=="ASN")%>%group_by(tuid)
unique(asn$tuid)

hsp<-subset(census_mid_crnc, international_reported_race=="HSP")%>%group_by(tuid)
unique(hsp$tuid)

int<-subset(census_mid_crnc, international_reported_race=="INT")%>%group_by(tuid)
unique(int$tuid)

mul<-subset(census_mid_crnc, international_reported_race=="MUL")%>%group_by(tuid)
unique(mul$tuid)

unk<-subset(census_mid_crnc, international_reported_race=="UNK")%>%group_by(tuid)
unique(unk$tuid)

wh<-subset(census_mid_crnc, international_reported_race=="WH")%>%group_by(tuid)
unique(wh$tuid)

tuj_na <- census_mid_crnc[is.na(census_mid_crnc$international_reported_race),]%>%group_by(tuid)
unique(tuj$tuid)