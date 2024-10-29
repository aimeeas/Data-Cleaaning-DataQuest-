# Data Cleaning With R
# Data Analyst in R - Data Quest - Part 3
# Aimêe ANDRADE SILVA


# DOWNLOADING DATABASE - New York Schools ---------

install.packages("readr")
library(readr)
library(tidyverse)

ap_2010 <- read_csv("ap_2010.csv")
class_size <- read_csv("class_size.csv")
demographics <- read_csv("demographics.csv")
graduation <- read_csv("graduation.csv")
hs_directory <- read_csv("hs_directory.csv")
sat_results <- read_csv("sat_results.csv")


# IMPORTANT INFORMATION -------
# 1 - We will only be working with the data from high school students, so sometimes
# the we will have to remove data regarding different school years;

# WORKING WITH SAT_RESULTS DATASET ---------

head(sat_results) # To take a look and understand the data

# Create a new column with the avegare sat_results

install.packages("purrr")
library(purrr) # So we can you the map function

map(sat_results, class) # To check if the informations are all numerics (head also gives me this information)

install.packages("tidyverse")
library(dplyr)

# To converte the columns in numeric
sat_results <- sat_results %>%
  mutate(across(contains("SAT"), as.numeric))

# Creating a new column with the sum of the average SAT scores
sat_results <- sat_results %>%
  mutate('avg_sat_score' = `SAT Critical Reading Avg. Score` + `SAT Math Avg. Score` + `SAT Writing Avg. Score`)
       

# WORKING WITH AP_2010 DATASET ------------

head(ap_2010)

map(ap_2010, class)

ap_2010 <- ap_2010 %>%
  mutate(across(3:5, as.numeric))

# Creating 2 new columns
ap_2010 <- ap_2010 %>%
  mutate("exams_per_student" = `Total Exams Taken`/`AP Test Takers`) %>%
  mutate("high_score_percent" = 
           (`Number of Exams with scores 3 4 or 5`/`Total Exams Taken`)*100)


# WORKING WITH CLASS_SIZE DATASET --------

head(class_size)

# We have multiples rows for the same school in this dataset.
# We will work only with those that have '9-12' in the GRADE column, which corresponds to High School
# We will keep also only rows with 'GEN ED' in the 'PROGRAM TYPE4 column,
# because it corresponds to "general education"

class_size <- filter(class_size, GRADE == "09-12", `PROGRAM TYPE` == "GEN ED")

# Creating a new dataset with the avegares scores from each school
class_size <- class_size %>%
  group_by(CSD, `SCHOOL CODE`, `SCHOOL NAME`) %>%
  summarize("avg_class_size" = mean(`AVERAGE CLASS SIZE`),
            "avg_largest_class" = mean(`SIZE OF LARGEST CLASS`),
            "avg_smallest_class" = mean(`SIZE OF SMALLEST CLASS`)  )
          # In this case there is no need to use the mutate function

# Joining CSD and SCHOOL CODE columns to create the DBN column

class_size <- class_size %>%
  mutate(DBN = str_c(`CSD`,`SCHOOL CODE`, sep = "")) %>% # str_c combines the columns together 
  mutate(DBN = str_pad(DBN, width = 6, side = "left", pad = "0")) #str_pad makes all the values
                                                                  # in a column have the same size


# WORKING WITH GRADUATION DATASET -------

head(graduation)

# Filtering the desired rows and selecting columns to clean the dataset
graduation <- graduation %>% filter( 
                      Demographic == "Total Cohort", 
                      Cohort == "2006") %>%
  select(DBN, `School Name`, `Total Grads - % of cohort`, `Dropped Out - % of cohort`)

# Transforming columns into numeric data by using the function parse_number;
# This function takes a vector as input and drops any characters before or after the first number in strings;
# In this example, it will take the % symbol out and transform the vectors/columns
# into numeric data

graduation <- graduation %>%
  mutate(across(3:4, parse_number))

# WORKING WITH DEMOGRAPHICS DATASET ---------

head(demographics)

# Filtering the desired rows and selecting columns to clean the dataset
demographics <- demographics %>%
  filter(schoolyear == "20112012",
         !is.na(grade9)) %>%
  select(DBN, Name, contains("_per"), total_enrollment)

demographics_clean <- demographics %>%
  select(-c(female_per, Name))
         
# 
# WORKING WITH HS_DIRECTORY DATASET -------

head(hs_directory)

# Choosing columns and renaming
hs_directory <- hs_directory %>%
  select(dbn, school_name, `Location 1`) %>%
  rename(DBN = dbn)

# Tidying the data

library(tidyr) # So we can use the 'separate' function

hs_directory <- hs_directory %>%
  separate(col = `Location 1`,
           into = c("string_1", "string_2", "string_3"), # The name of the news columns
           sep = "\n") %>% # the separator of string 
  select(-c("string_1", "string_2")) %>% # Excluing unecessary columns
  rename("lat_long" = "string_3") # Renaming


# Separating columns in lat and long

hs_directory <- hs_directory %>%
  separate(col = "lat_long",
           into = c("lat", "long"),
           sep = ",")

# Removing non numerical caracteres and transforming in numeric data
hs_directory <- hs_directory %>%
  mutate(across(3:4, parse_number)) %>%
  mutate(across(c(lat,long), as.numeric))



# JOINING DATA FRAMES -------------

# Veryfing if there is no duplicates in the dataframes

sum(duplicated(sat_results$DBN))

sum(duplicated(ap_2010$DBN)) # Has one value duplicated

sum(duplicated(class_size$DBN))

sum(duplicated(demographics$DBN))

sum(duplicated(graduation$DBN))

sum(duplicated(hs_directory$DBN))


# Removing the duplicated row from the dataframe

ap_2010 <- ap_2010 %>%
  distinct(DBN, .keep_all = TRUE) # The .keep_all = TRUE parameter allows 
                                  # keeping all the columns in the dataframe.



# Joing dataframes by INNER JOIN (when we're only interested in getting results 
# that exist in both tables we're bringing together.)

sat_class_size <- sat_results %>%
  inner_join(class_size, by = "DBN")

# Generating a plot to see if there is any relation between the average class size
# and the average sat score 

sat_class_size %>%
  ggplot(aes(x = avg_class_size, y = avg_sat_score)) +
  geom_point()



# ANALYSIS GOAL: goal for this analysis: we aim to explore how various 
# demographic aspects of NYC high schools, such as race, gender, and income, 
# influence students' academic achievements.

# sat_results and ap_2010 have insights about the students' academice performance;
# We will preservate all the data from both dataframes (FULL_JOIN)

# For the other dataframes, only the data that matches sat_results and ap_2010
# will be preserveted (left_join)

combined <- sat_results %>%
  full_join(ap_2010, by = "DBN") %>%
  left_join(class_size, by = "DBN") %>%
  left_join(demographics, by = "DBN") %>%
  left_join(graduation, by = "DBN") %>%
  left_join(hs_directory, by = "DBN")

# Removing duplicated columns and renaming it

combined <- combined %>%
  select(-c(SchoolName, `SCHOOL NAME.y`, Name, `School Name`, school_name)) %>%
  rename("school_name" = `SCHOOL NAME.x`)



# CREATING PLOTS ---------------

# PLOT 1 
# Relation between the percentage of students at a school who qualify for 
# discounted lunch based on household income and the average SAT scores

combined %>%
  ggplot(aes(x = frl_percent, y = avg_sat_score, color = avg_sat_score)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  scale_color_gradient(low = "red", high = "green") +
  labs(
    title = "Relationship between Reduced Lunch % and Average SAT Score",
    x = "Reduced Lunch Percentage (%)",
    y = "Average SAT Score",
    color = "SAT Score"
  ) +
  theme_minimal()

# INTERPRETATION: This plot shows a possible inverse relationship between the
# highers SAT scores and the classes with a big percentage of students that need
# financial add to eat. We could imagine that students that come from households
# with financial issues tend to not have results in the SAT exam as good and the others. 


# PLOT 2
# Relation between the percentage of students in a school who are learning
# English and the average SAT scores

combined %>%
  ggplot(aes(x = ell_percent, y = avg_sat_score, color = ell_percent)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_color_gradient(low = "yellow", high = "red") +
  labs(
    title = "Relationship between ELL % and Average SAT Score",
    x = "English Language Learner Percentage (%)",
    y = "Average SAT Score",
    color = "ELL %"
  ) +
  theme_minimal()

# INTERPRETATION: From my analysis, this plot has 2 interesting points. First we can
# see that classes that have more than 50% of students enrolled in supplementary
# english classes, have the lowests SAT Scores. This is probably due to the fact
# that this exam is taken in English and those students don't have english as
# first language. The other interesting thing we can visualize with this plot is
# that classes that have a low ELL % or don't have any student in ELL at all, have
# a large diversity of SAT scores. That means that some of those classes we have
# a very high score and other could have scores as low as those with a high ELL %.
# This probably indicates that when analyzing classes with no or low ELL students,
# the factor that will determinate the value of the avg SAT score would be something
# else. Having a low ELL students % doesn't guarantee good SAT scores. 



# PLOT 3
# Relation between the percentage of students in a scholl who get specialized
# instruction for things like learning disabilities and the average SAT scores

combined %>%
  ggplot(aes(x = sped_percent, y = avg_sat_score, color = sped_percent)) +
  geom_point(alpha = 0.8, size = 3) +
  scale_color_gradient(low = "beige", high = "purple") +
  geom_smooth(method = "lm", color = "darkblue", se = FALSE) +
  labs(
    title = "Relationship between Special Education % and Average SAT Score",
    x = "Special Education Percentage (%)",
    y = "Average SAT Score",
   color = "Special Education %") +
  theme_minimal()


# INTERPRETATION: This plot analysis is similiar to the second one concerning
# the SAT score's values for the classes that have low percentages of Special
# Education Students, however, it's possible to interprate that there is an
# inverse relationship between the Avg SAT Score and the Special Education %.
# Classes with higher % of Special Education Students seems to have alower
# average SAT Score.