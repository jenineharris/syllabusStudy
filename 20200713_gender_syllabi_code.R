# PROLOG   ################################################################'

# PROJECT: DIVERSIFY THE SYLLABI
# PURPOSE: DATA ANALYSIS FOR MANUSCRIPT EXAMINING GENDER & AUTHORSHIP
# DATA:    20200713_deidentified_reading_level_data.csv
#          20200427_NSF_2017_college_graduates.csv 
#          20200427_earned_doctorates_2018_discipline.csv 
#          20200713_gender_by_department_wustl_2019.csv
# AUTHOR:  Jenine Harris, Merriah Croston, Ellen Hutti, Amy Eyler 
# LATEST:  2020-07-17
# NOTES:   Goes with 20200713_gender_syllabi_text.Rmd file to create  
#          manuscript for submission to PloS One. Data is from syllabi
#          for Washington University in St. Louis courses in 2018-2019
#          academic year. Workforce data from NSF publicly available
#          online data sources. Data on faculty gender by department is
#          from university archival sources.

# PROLOG   ###############################################################


# open packages 
library(package = "tidyverse")
library(package = "DiagrammeR")
library(package = "gt")

# bring in the data 
readingData <- read.csv("Data/20200713_deidentified_reading_level_data.csv")
length(unique(readingData$course_id))

# remove readings with unknown and nonbinary authors
# remove courses with no readings
readingDataNoUnk <- readingData %>% 
  distinct() %>% 
  filter(hasUnkAuth == "No") %>% 
  filter(hasNonBiAuth == "No") %>% 
  drop_na(gender.first.author) %>% 
  mutate(course_id = as.character(course_id)) 
length(unique(readingDataNoUnk$course_id))

# Sampling flow chart
flow_chart <- grViz("digraph flowchart {
      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle]        
      tab1 [label = '@@1']
      tab2 [label = '@@2']
      tab3 [label = '@@3']
      tab4 [label = '@@4']
      tab5 [label = '@@5']
      tab6 [label = '@@6']

      # edge definitions with the node IDs
      tab1 -> tab2 -> tab3 -> tab4 -> tab5 -> tab6;
      }

      [1]: 'Population of n = 4,856 total courses in 2018'
      [2]: 'Removed duplicates for n = 2,463 distinct courses'
      [3]: 'Sampled 10% by discipline for n = 240 courses'
      [4]: 'Collected 80 archived syllabi and 68 via email for n = 148 courses with 2,777 readings'
      [5]: 'Removed 19 syllabi without readings or with readings not having clear author name or gender'
      [6]: 'Analytic sample n = 129 courses with n = 2,435 readings'
      ")

# percent women in each dept at WU by category

percWuF <- read.csv("Data/20200713_gender_by_department_wustl_2019.csv")
percWuFclean <- percWuF %>% 
  mutate_if(is.integer, ~replace(., is.na(.), 0)) 

sum(percWuFclean$F)/sum(percWuFclean$F, percWuFclean$M)
# 41.5% of faculty at WU are Female
sum(percWuFclean$F[percWuFclean$Category == "Humanities"])/sum(percWuFclean$F[percWuFclean$Category == "Humanities"], 
                                                               percWuFclean$M[percWuFclean$Category == "Humanities"])
# 53.8% of Humanities faculty are Female 
sum(percWuFclean$F[percWuFclean$Category == "Social Sciences"])/sum(percWuFclean$F[percWuFclean$Category == "Social Sciences"], 
                                                                    percWuFclean$M[percWuFclean$Category == "Social Sciences"])
# 52.7% of social sciences faculty are Female 
sum(percWuFclean$F[percWuFclean$Category == "Other"])/sum(percWuFclean$F[percWuFclean$Category == "Other"], 
                                                          percWuFclean$M[percWuFclean$Category == "Other"])
# 40.8% of other disciplines faculty are Female 
sum(percWuFclean$F[percWuFclean$Category == "STEM"])/sum(percWuFclean$F[percWuFclean$Category == "STEM"], 
                                                         percWuFclean$M[percWuFclean$Category == "STEM"])
# 25% of STEM faculty are Female

# get course level info 
courseDataNoUnk <- readingDataNoUnk %>% 
  distinct(course_id, .keep_all = TRUE)

# readings with a female 
# or nonbinary sole author or first author 
femNonbiFirst <- readingDataNoUnk %>% 
  filter(gender.first.author == "female") %>% 
  select(gender.first.author)

# Social science readings
socSciRead <- readingDataNoUnk %>% 
  filter(category == "Social Sciences") 

# other readings
otherRead <- readingDataNoUnk %>% 
  filter(category == "Other") 

# humanities readings 
humanRead <- readingDataNoUnk %>% 
  filter(category == "Humanities") 

# STEM 
stemRead <- readingDataNoUnk %>% 
  filter(category == "STEM") 

# Run the code and copy/paste the table into the final word document
# create a tibble for table 1
allDisc <- c(nrow(courseDataNoUnk),
             nrow(readingDataNoUnk),
             sum(courseDataNoUnk$se_cur_enr),
             sum(readingDataNoUnk$femSumAuth, readingDataNoUnk$maleSumAuth, na.rm = TRUE),
             sum(readingDataNoUnk$femSumAuth, na.rm = TRUE),
             100*sum(readingDataNoUnk$femSumAuth, na.rm = TRUE)/sum(readingDataNoUnk$femSumAuth, 
                                                                readingDataNoUnk$maleSumAuth, na.rm = TRUE),
             round(mean(table(readingDataNoUnk$course_id)), 0),
             round(mean(courseDataNoUnk$se_cur_enr), 0),
             round(100*sum(courseDataNoUnk$gender.instructor == "female")/nrow(courseDataNoUnk), 0),
             round(100*mean(readingDataNoUnk$femPercAuth), 0),
             round(100*median(readingDataNoUnk$femPercAuth), 0),
             nrow(femNonbiFirst),
             round(100*nrow(femNonbiFirst)/nrow(readingDataNoUnk), 0),
             round(100*mean(readingDataNoUnk$femPercAuth[readingDataNoUnk$gender.instructor == "female"]), 0),
             round(100*median(readingDataNoUnk$femPercAuth[readingDataNoUnk$gender.instructor == "female"]), 0),
             sum(readingDataNoUnk$gender.instructor[readingDataNoUnk$gender.first.author != "male"] == "female"),
             round(100*sum(readingDataNoUnk$gender.first.author[readingDataNoUnk$gender.instructor == "female"] != "male")/sum(readingDataNoUnk$gender.instructor == "female"),0),
             round(100*mean(readingDataNoUnk$femPercAuth[readingDataNoUnk$gender.instructor == "male"]), 0),
             round(100*median(readingDataNoUnk$femPercAuth[readingDataNoUnk$gender.instructor == "male"]), 0),
             sum(readingDataNoUnk$gender.instructor[readingDataNoUnk$gender.first.author != "male"] == "male"),
             round(100*sum(readingDataNoUnk$gender.first.author[readingDataNoUnk$gender.instructor == "male"] != "male")/sum(readingDataNoUnk$gender.instructor == "male"),0)
)
humDisc <- c(sum(courseDataNoUnk$category == "Humanities"),
             sum(readingDataNoUnk$category == "Humanities"),
             sum(courseDataNoUnk$se_cur_enr[courseDataNoUnk$category == "Humanities"]),
             sum(readingDataNoUnk$femSumAuth[courseDataNoUnk$category == "Humanities"], 
                 readingDataNoUnk$maleSumAuth[courseDataNoUnk$category == "Humanities"], na.rm = TRUE),
             sum(readingDataNoUnk$femSumAuth[courseDataNoUnk$category == "Humanities"], na.rm = TRUE),
             100*sum(readingDataNoUnk$femSumAuth[courseDataNoUnk$category == "Humanities"], 
                 na.rm = TRUE)/sum(readingDataNoUnk$femSumAuth[courseDataNoUnk$category == "Humanities"], 
                                   readingDataNoUnk$maleSumAuth[courseDataNoUnk$category == "Humanities"], na.rm = TRUE),
             round(mean(table(readingDataNoUnk$course_id[readingDataNoUnk$category == "Humanities"])), 0),
             round(mean(courseDataNoUnk$se_cur_enr[courseDataNoUnk$category == "Humanities"]), 0),
             round(100*sum(courseDataNoUnk$gender.instructor[courseDataNoUnk$category == "Humanities"] == "female")/sum(courseDataNoUnk$category == "Humanities"), 0),
             round(100*mean(readingDataNoUnk$femPercAuth[readingDataNoUnk$category == "Humanities"]), 0),
             round(100*median(readingDataNoUnk$femPercAuth[readingDataNoUnk$category == "Humanities"]), 0),
             sum(readingDataNoUnk$category[readingDataNoUnk$gender.first.author != "male"] == "Humanities"),
             round(100*sum(readingDataNoUnk$category[readingDataNoUnk$gender.first.author != "male"] == "Humanities")/sum(readingDataNoUnk$category == "Humanities"), 0),
             round(100*mean(readingDataNoUnk$femPercAuth[readingDataNoUnk$category == "Humanities" & readingDataNoUnk$gender.instructor == "female"]), 0),
             round(100*median(readingDataNoUnk$femPercAuth[readingDataNoUnk$category == "Humanities" & readingDataNoUnk$gender.instructor == "female"]), 0),
             sum(readingDataNoUnk$gender.first.author[readingDataNoUnk$category == "Humanities" & readingDataNoUnk$gender.instructor == "female"] != "male"),
             round(100*sum(readingDataNoUnk$gender.first.author[readingDataNoUnk$category == "Humanities" & readingDataNoUnk$gender.instructor == "female"] != "male")/sum(readingDataNoUnk$category[readingDataNoUnk$gender.instructor == "female"] == "Humanities"),0),
             round(100*mean(readingDataNoUnk$femPercAuth[readingDataNoUnk$category == "Humanities" & readingDataNoUnk$gender.instructor == "male"]), 0),
             round(100*median(readingDataNoUnk$femPercAuth[readingDataNoUnk$category == "Humanities" & readingDataNoUnk$gender.instructor == "male"]), 0),
             sum(readingDataNoUnk$gender.first.author[readingDataNoUnk$category == "Humanities" & readingDataNoUnk$gender.instructor == "male"] != "male"),
             round(100*sum(readingDataNoUnk$gender.first.author[readingDataNoUnk$category == "Humanities" & readingDataNoUnk$gender.instructor == "male"] != "male")/sum(readingDataNoUnk$category[readingDataNoUnk$gender.instructor == "male"] == "Humanities"),0)
)
othDisc <- c(sum(courseDataNoUnk$category == "Other"),
             sum(readingDataNoUnk$category == "Other"),
             sum(courseDataNoUnk$se_cur_enr[courseDataNoUnk$category == "Other"]),
             sum(readingDataNoUnk$femSumAuth[courseDataNoUnk$category == "Other"], 
                 readingDataNoUnk$maleSumAuth[courseDataNoUnk$category == "Other"], na.rm = TRUE),
             sum(readingDataNoUnk$femSumAuth[courseDataNoUnk$category == "Other"], na.rm = TRUE),
             100*sum(readingDataNoUnk$femSumAuth[courseDataNoUnk$category == "Other"], 
                 na.rm = TRUE)/sum(readingDataNoUnk$femSumAuth[courseDataNoUnk$category == "Other"], 
                                   readingDataNoUnk$maleSumAuth[courseDataNoUnk$category == "Other"], na.rm = TRUE),
             round(mean(table(readingDataNoUnk$course_id[readingDataNoUnk$category == "Other"])), 0),
             round(mean(courseDataNoUnk$se_cur_enr[courseDataNoUnk$category == "Other"]), 0),
             round(100*sum(courseDataNoUnk$gender.instructor[courseDataNoUnk$category == "Other"] == "female")/sum(courseDataNoUnk$category == "Other"), 0),
             round(100*mean(readingDataNoUnk$femPercAuth[readingDataNoUnk$category == "Other"]), 0),
             round(100*median(readingDataNoUnk$femPercAuth[readingDataNoUnk$category == "Other"]), 0),
             sum(readingDataNoUnk$category[readingDataNoUnk$gender.first.author != "male"] == "Other"),
             round(100*sum(readingDataNoUnk$category[readingDataNoUnk$gender.first.author != "male"] == "Other")/sum(readingDataNoUnk$category == "Other"), 0),
             round(100*mean(readingDataNoUnk$femPercAuth[readingDataNoUnk$category == "Other" & readingDataNoUnk$gender.instructor == "female"]), 0),
             round(100*median(readingDataNoUnk$femPercAuth[readingDataNoUnk$category == "Other" & readingDataNoUnk$gender.instructor == "female"]), 0),
             sum(readingDataNoUnk$gender.first.author[readingDataNoUnk$category == "Other" & readingDataNoUnk$gender.instructor == "female"] != "male"),
             round(100*sum(readingDataNoUnk$gender.first.author[readingDataNoUnk$category == "Other" & readingDataNoUnk$gender.instructor == "female"] != "male")/sum(readingDataNoUnk$category[readingDataNoUnk$gender.instructor == "female"] == "Other"),0),
             round(100*mean(readingDataNoUnk$femPercAuth[readingDataNoUnk$category == "Other" & readingDataNoUnk$gender.instructor == "male"]), 0),
             round(100*median(readingDataNoUnk$femPercAuth[readingDataNoUnk$category == "Other" & readingDataNoUnk$gender.instructor == "male"]), 0),
             sum(readingDataNoUnk$gender.first.author[readingDataNoUnk$category == "Other" & readingDataNoUnk$gender.instructor == "male"] != "male"),
             round(100*sum(readingDataNoUnk$gender.first.author[readingDataNoUnk$category == "Other" & readingDataNoUnk$gender.instructor == "male"] != "male")/sum(readingDataNoUnk$category[readingDataNoUnk$gender.instructor == "male"] == "Other"),0)
)
socSciDisc <- c(sum(courseDataNoUnk$category == "Social Sciences"),
                sum(readingDataNoUnk$category == "Social Sciences"),
                sum(courseDataNoUnk$se_cur_enr[courseDataNoUnk$category == "Social Sciences"]),
                sum(readingDataNoUnk$femSumAuth[courseDataNoUnk$category == "Social Sciences"], 
                    readingDataNoUnk$maleSumAuth[courseDataNoUnk$category == "Social Sciences"], na.rm = TRUE),
                sum(readingDataNoUnk$femSumAuth[courseDataNoUnk$category == "Social Sciences"], na.rm = TRUE),
                100*sum(readingDataNoUnk$femSumAuth[courseDataNoUnk$category == "Social Sciences"], 
                    na.rm = TRUE)/sum(readingDataNoUnk$femSumAuth[courseDataNoUnk$category == "Social Sciences"], 
                                      readingDataNoUnk$maleSumAuth[courseDataNoUnk$category == "Social Sciences"], na.rm = TRUE),
                round(mean(table(readingDataNoUnk$course_id[readingDataNoUnk$category == "Social Sciences"])), 0),
                round(mean(courseDataNoUnk$se_cur_enr[courseDataNoUnk$category == "Social Sciences"]), 0),
                round(100*sum(courseDataNoUnk$gender.instructor[courseDataNoUnk$category == "Social Sciences"] == "female")/sum(courseDataNoUnk$category == "Social Sciences"), 0),
                round(100*mean(readingDataNoUnk$femPercAuth[readingDataNoUnk$category == "Social Sciences"]), 0),
                round(100*median(readingDataNoUnk$femPercAuth[readingDataNoUnk$category == "Social Sciences"]), 0),
                sum(readingDataNoUnk$category[readingDataNoUnk$gender.first.author != "male"] == "Social Sciences"),
                round(100*sum(readingDataNoUnk$category[readingDataNoUnk$gender.first.author != "male"] == "Social Sciences")/sum(readingDataNoUnk$category == "Social Sciences"), 0),
                round(100*mean(readingDataNoUnk$femPercAuth[readingDataNoUnk$category == "Social Sciences" & readingDataNoUnk$gender.instructor == "female"]), 0),
                round(100*median(readingDataNoUnk$femPercAuth[readingDataNoUnk$category == "Social Sciences" & readingDataNoUnk$gender.instructor == "female"]), 0),
                sum(readingDataNoUnk$gender.first.author[readingDataNoUnk$category == "Social Sciences" & readingDataNoUnk$gender.instructor == "female"] != "male"),
                round(100*sum(readingDataNoUnk$gender.first.author[readingDataNoUnk$category == "Social Sciences" & readingDataNoUnk$gender.instructor == "female"] != "male")/sum(readingDataNoUnk$category[readingDataNoUnk$gender.instructor == "female"] == "Social Sciences"),0),
                round(100*mean(readingDataNoUnk$femPercAuth[readingDataNoUnk$category == "Social Sciences" & readingDataNoUnk$gender.instructor == "male"]), 0),
                round(100*median(readingDataNoUnk$femPercAuth[readingDataNoUnk$category == "Social Sciences" & readingDataNoUnk$gender.instructor == "male"]), 0),
                sum(readingDataNoUnk$gender.first.author[readingDataNoUnk$category == "Social Sciences"& readingDataNoUnk$gender.instructor == "male"] != "male"),
                round(100*sum(readingDataNoUnk$gender.first.author[readingDataNoUnk$category == "Social Sciences" & readingDataNoUnk$gender.instructor == "male"] != "male")/sum(readingDataNoUnk$category[readingDataNoUnk$gender.instructor == "male"] == "Social Sciences"),0)
)
stemDisc <- c(sum(courseDataNoUnk$category == "STEM"),
              sum(readingDataNoUnk$category == "STEM"),
              sum(courseDataNoUnk$se_cur_enr[courseDataNoUnk$category == "STEM"]),
              sum(readingDataNoUnk$femSumAuth[courseDataNoUnk$category == "STEM"], 
                  readingDataNoUnk$maleSumAuth[courseDataNoUnk$category == "STEM"], na.rm = TRUE),
              sum(readingDataNoUnk$femSumAuth[courseDataNoUnk$category == "STEM"], na.rm = TRUE),
              100*sum(readingDataNoUnk$femSumAuth[courseDataNoUnk$category == "STEM"], 
                  na.rm = TRUE)/sum(readingDataNoUnk$femSumAuth[courseDataNoUnk$category == "STEM"], 
                                    readingDataNoUnk$maleSumAuth[courseDataNoUnk$category == "STEM"], na.rm = TRUE),
              round(mean(table(readingDataNoUnk$course_id[readingDataNoUnk$category == "STEM"])), 0),
              round(mean(courseDataNoUnk$se_cur_enr[courseDataNoUnk$category == "STEM"]), 0),
              round(100*sum(courseDataNoUnk$gender.instructor[courseDataNoUnk$category == "STEM"] == "female")/sum(courseDataNoUnk$category == "STEM"), 0),
              round(100*mean(readingDataNoUnk$femPercAuth[readingDataNoUnk$category == "STEM"]), 0),
              round(100*median(readingDataNoUnk$femPercAuth[readingDataNoUnk$category == "STEM"]), 0),
              sum(readingDataNoUnk$category[readingDataNoUnk$gender.first.author != "male"] == "STEM"),
              round(100*sum(readingDataNoUnk$category[readingDataNoUnk$gender.first.author != "male"] == "STEM")/sum(readingDataNoUnk$category == "STEM"), 0),
              round(100*mean(readingDataNoUnk$femPercAuth[readingDataNoUnk$category == "STEM" & readingDataNoUnk$gender.instructor == "female"]), 0),
              round(100*median(readingDataNoUnk$femPercAuth[readingDataNoUnk$category == "STEM" & readingDataNoUnk$gender.instructor == "female"]), 0),
              sum(readingDataNoUnk$gender.first.author[readingDataNoUnk$category == "STEM"& readingDataNoUnk$gender.instructor == "female"] != "male"),
              round(100*sum(readingDataNoUnk$gender.first.author[readingDataNoUnk$category == "STEM" & readingDataNoUnk$gender.instructor == "female"] != "male")/sum(readingDataNoUnk$category[readingDataNoUnk$gender.instructor == "female"] == "STEM"),0),
              round(100*mean(readingDataNoUnk$femPercAuth[readingDataNoUnk$category == "STEM" & readingDataNoUnk$gender.instructor == "male"]), 0),
              round(100*median(readingDataNoUnk$femPercAuth[readingDataNoUnk$category == "STEM" & readingDataNoUnk$gender.instructor == "male"]), 0),
              sum(readingDataNoUnk$gender.first.author[readingDataNoUnk$category == "STEM"& readingDataNoUnk$gender.instructor == "male"] != "male"),
              round(100*sum(readingDataNoUnk$gender.first.author[readingDataNoUnk$category == "STEM" & readingDataNoUnk$gender.instructor == "male"] != "male")/sum(readingDataNoUnk$category[readingDataNoUnk$gender.instructor == "male"] == "STEM"),0)
)

row <- c("Number of courses", "Number of readings", "Number of students", 
         "Number of authors", "Number of female authors", "Percent of female authors",
         "Mean number of readings", "Mean number of students", "Percent with female instructors",
         "Mean percent of female authors", "Median percent of female authors", "Number with female first/sole author", "Percent with female/sole first author",   
         "Mean percent of female authors", "Median percent of female authors", "Number with female first/sole author", "Percent with female first/sole author", 
         "Mean percent of female authors", "Median percent of female authors", "Number with female first/sole author", "Percent with female first/sole author")
group <- c("Total", "Total", "Total", "Total", "Total", "Total",
           "Course characteristics", "Course characteristics", "Course characteristics",
           "Reading characteristics", "Reading characteristics", "Reading characteristics", "Reading characteristics",
           "Readings in classes with female instructors", "Readings in classes with female instructors", "Readings in classes with female instructors", "Readings in classes with female instructors", 
           "Readings in classes with male instructors", "Readings in classes with male instructors", "Readings in classes with male instructors", "Readings in classes with male instructors")

table1 <- data.frame(allDisc, humDisc, socSciDisc, stemDisc, othDisc, row, group)

table1 <- table1 %>% 
  gt(rowname_col = "row", groupname_col = "group") %>% 
  cols_label(
    allDisc = "All disciplines",
    humDisc = "Humanities",
    othDisc = "Other",
    socSciDisc = "Social sciences",
    stemDisc = "STEM"
  ) %>% 
  fmt_number(columns = 1:5, decimals = 0)

# make new variable in data frame for female-nonbinary
# first authors and male only first authors
readingDataNoUnkFem <- readingDataNoUnk %>%
  mutate(femFirst = recode_factor(gender.first.author, 
                                  female = "female",
                                  #nonbinary = NA_character_,
                                  male = "male")) 

# order the bars
readingDataNoUnkFem$categoryOrd <- factor(readingDataNoUnkFem$category,
                                          levels = c("Other", "STEM", "Humanities", "Social Sciences"),
                                          ordered = TRUE)

# graph first author gender by discipline
Fig2 <- readingDataNoUnkFem %>% 
  filter(femFirst != "unknown") %>%
  group_by(categoryOrd, femFirst) %>% 
  count() %>% 
  group_by(categoryOrd) %>% 
  mutate(perc = 100 * n / sum(n)) %>% 
  ggplot(aes(x = categoryOrd, y = perc, fill = femFirst)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_minimal(base_size = 15) +
  scale_fill_manual(values = c("#1f78b4", "#a6cee3"),
                    name = "First/sole author gender") +
  labs(x = "Discipline", y = "Percent within discipline group") +
  coord_flip() +
  theme(legend.position = "top")

# male lead instructor  
maleInst <- readingDataNoUnk %>% 
  filter(gender.instructor == "male") 

# female or nonbinary lead instructor 
femaleInst <- readingDataNoUnk %>% 
  filter(gender.instructor == "female" | gender.instructor == "nonbinary") 

# mean and median percent of female and nonbinary authors per reading
Fig3 <- readingDataNoUnkFem %>% 
  mutate(gender.instructor = recode_factor(gender.instructor,
                                           female = "Female instructor",
                                           male = "Male instructor")) %>% 
  group_by(categoryOrd, gender.instructor) %>% 
  summarize(medPercFem = median(femPercAuth),
            meanPercFem = mean(femPercAuth)) %>% 
  pivot_longer(cols = medPercFem:meanPercFem, names_to = "measure", values_to = "value") %>% 
  mutate(measure = recode(measure, 
                          medPercFem = "Median",
                          meanPercFem = "Mean")) %>% 
  ggplot(aes(x = categoryOrd, y = 100*value, fill = measure)) +
  geom_col(position = "dodge") +
  theme_minimal(base_size = 10) +
  scale_fill_manual(values = c("#1f78b4", "#a6cee3"),
                    name = "") +
  labs(x = "Discipline", y = "Percent female authors per reading") +
  theme(legend.position = "top") +
  coord_flip() +
  facet_wrap(facets = "gender.instructor")
Fig3

# graph first author gender by discipline
Fig4 <- readingDataNoUnkFem %>% 
  
  mutate(gender.instructor = recode_factor(gender.instructor,
                                           female = "Female instructor",
                                           male = "Male instructor")) %>% 
  group_by(categoryOrd, femFirst, gender.instructor) %>% 
  count() %>% 
  group_by(categoryOrd, gender.instructor) %>% 
  mutate(perc = 100 * n / sum(n)) %>% 
  drop_na(categoryOrd) %>% 
  ggplot(aes(x = categoryOrd, y = perc, fill = femFirst)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_minimal(base_size = 15) +
  scale_fill_manual(values = c("#1f78b4", "#a6cee3"),
                    name = "First/sole author gender") +
  labs(x = "Discipline", y = "Percent within discipline group") +
  coord_flip() +
  theme(legend.position = "top") +
  facet_wrap(facets = "gender.instructor")

# 2013 paper from West et. al. data table
# used discipline categories for topic areas 
# looked up in NSF if topic was not in our original data
fieldW <- c('Mathematics', 'Philosophy', 'Economics', 'Probability and Statistics', 'Political science - international',
           'Political science-US domestic', 'Ecology and evolution', 'Law', 'Organizational and marketing',
           'Physical anthropology', 'Radiation damage', 'Classical studies', 'Molecular & Cell biology',
           'History', 'Veterinary medicine', 'Cognitive science', 'Anthropolgy', 'Pollution and occupational health',
           'Sociology', 'Demography', 'Education')
discW <- c('STEM', 'Humanities', 'Social Sciences', 'STEM', 'Social Sciences',
           'Social Sciences', 'STEM', 'Other', 'Other',
           'Social Sciences', 'STEM', 'Humanities', 'STEM',
           'Humanities', 'STEM', 'Social Sciences', 'Social Sciences', 'Social Sciences',
           'Social Sciences', 'Social Sciences', 'Social Sciences')
fieldW.percF <- c(10.64, 12.04, 13.68, 18.11, 19.07, 
                  19.09, 22.76, 24.21, 25.44, 
                  27.05, 27.69, 28.88, 29.25,
                 30.47, 31.81, 32.12, 36.46, 37.57, 
                 41.41, 41.90, 46.35)
authorshipsW <- c(6134, 12190, 69142, 28324, 14908, 
                  15705, 279012, 18503, 32119, 
                  16296, 7825, 6372, 277032,
                 15585, 10960, 12786, 19900, 32108, 
                 44895, 7600, 28635)
westFemAuth <- data.frame(fieldW, discW, fieldW.percF, authorshipsW)

# number of women authors
westFemAuthClean <- westFemAuth %>% 
  mutate(numByFem = fieldW.percF*authorshipsW/100) 

westFemAuthClean %>% 
  group_by(discW) %>% 
  dplyr::summarize(percByF = sum(numByFem)/sum(authorshipsW))

# total percent of women authors by discipline our data
sum(readingDataNoUnkFem$femSumAuth, na.rm = TRUE)/(sum(readingDataNoUnkFem$femSumAuth, na.rm = TRUE) + 
                                                     sum(readingDataNoUnkFem$maleSumAuth, na.rm = TRUE))
# total percent of women authors in sample is 36.8%

# percent of women authors by discipline totals
readingDataNoUnkFem %>% 
  group_by(category) %>% 
  dplyr::summarize(percF = sum(femSumAuth, na.rm = TRUE)/(sum(femSumAuth, na.rm = TRUE) + sum(maleSumAuth, na.rm = TRUE)),
                   numF = sum(femSumAuth, na.rm = TRUE),
                   numM = sum(maleSumAuth, na.rm = TRUE),
                   totAuth = sum(maleSumAuth, na.rm = TRUE) + sum(femSumAuth, na.rm = TRUE))

# 2017 survey of college graduates from NSF
colGrads <- read.csv("Data/20200427_NSF_2017_college_graduates.csv")
prop.table(table(colGrads$sex, colGrads$discipline), margin = 2)

# 2018 survey of earned doctorates from NSF
earnDoc <- read.csv("Data/20200427_earned_doctorates_2018_discipline.csv")

# Compare reading types APPENDIX 1
# number of each reading type by discipline
tabApp1 <- readingDataNoUnk %>% 
  group_by(category, reading_type_clean) %>% 
  drop_na(reading_type_clean) %>% 
  dplyr::summarize(count = n()) 

# percent first author/sole author female by reading type and discipline
figApp1 <- readingDataNoUnk %>% 
  #filter(gender.first.author != "nonbinary") %>% 
  filter(gender.first.author != "unknown") %>% 
  drop_na(reading_type_clean) %>% 
  group_by(category, reading_type_clean, gender.first.author) %>% 
  dplyr::summarize(count = n()) %>% 
  group_by(category, reading_type_clean) %>% 
  mutate(percentage = count/sum(count)*100) %>% 
  #filter(gender.first.author == "female") %>% 
  ggplot(aes(x = reading_type_clean, y = percentage, fill = gender.first.author)) +
  geom_col(position = 'dodge') +
  labs(x = "Reading Type", 
       y = "Percentage First or Sole Authors\nof Readings in Discipline") +
  theme_minimal(base_size = 10) +
  scale_fill_manual(values = c("#1f78b4", "#a6cee3"), name = "Gender") +
  facet_wrap(facets = vars(category), nrow = 2) +
  #theme(legend.title = element_text(name = "Reading\nType")) + 
  coord_flip()
  
# APPENDIX 2 SENSITIVITY ANALYSIS by syllabus source
# compare percent female first/sole authors in readings 
# obtained on syllabi central vs. through email to course
# instructors 
tableApp2 <- readingDataNoUnk %>% 
  #filter(gender.first.author != "nonbinary") %>% 
  filter(gender.first.author != "unknown") %>% 
  rename(syllabi_source = syllabi_central) %>% 
  mutate(syllabi_source = recode(syllabi_source,
                                 "archive" = "yes",
                                 "email" = "no")) %>% 
  group_by(category, syllabi_source, gender.first.author) %>% 
  dplyr::summarize(count = n()) %>% 
  group_by(category, syllabi_source) %>% 
  mutate(percentage = count/sum(count)*100)
# for humanities archival 22.2% female, email 44.3% female
# for other archival 7.69% female, email 10.9% female
# for social sciences archival 38.0% female, email 46.1% female
# for STEM archival 12.1% female, email 18.2% female

# Appendix 2 Figure
figApp2 <- readingDataNoUnk %>% 
  filter(gender.first.author != "nonbinary") %>% 
  filter(gender.first.author != "unknown") %>% 
  rename(archival = syllabi_central) %>% 
  group_by(category, archival, gender.first.author) %>% 
  dplyr::summarize(count = n()) %>% 
  group_by(category, archival) %>% 
  mutate(percentage = count/sum(count)*100) %>% 
  filter(gender.first.author == "female") %>% 
  ggplot(aes(x = category, y = percentage, fill = archival)) +
  geom_col(position = 'dodge') +
  labs(x = "Discipline", y = "Percentage Female First or Sole Authors") +
  theme_minimal(base_size = 12) +
  scale_fill_manual(values = c("#1f78b4", "#a6cee3"),
                    name = "", 
                    labels = c("Readings from\nSyllabi Emailed\nby Faculty", "Readings from\nSyllabi Retrieved\nfrom Archive")) +
  theme(legend.position = "top")

# Review response figure comparing lower division with upper and grad school
readingDataNoUnk %>% 
  group_by(category, division) %>% 
  dplyr::summarize(count = n()) 

unique(readingDataNoUnk$course_id)

readingDataNoUnk %>% 
  #filter(gender.first.author != "nonbinary") %>% 
  filter(gender.first.author != "unknown") %>% 
  group_by(category, division, gender.first.author) %>% 
  dplyr::summarize(count = n()) %>% 
  group_by(category, division) %>% 
  mutate(percentage = count/sum(count)*100) %>% 
  filter(gender.first.author == "female") %>% 
  ggplot(aes(x = category, y = percentage, fill = division)) +
  geom_col(position = 'dodge') +
  labs(x = "Discipline", y = "Percentage Female First or Sole Authors") +
  theme_minimal(base_size = 12) +
  scale_fill_manual(values = c("#1f78b4", "#a6cee3"))+
  theme(legend.position = "top")

