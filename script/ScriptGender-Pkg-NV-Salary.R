#----------------------------------------------------------------#
#                      Gender-Pkg-NV-Salary                      #
#----------------------------------------------------------------#
# Script purpose                                                 #
#   Prepares dataset on Nevada salary data (all-nevada-2016.csv) #
#   Adds a gender column using Gender, an R package              #
# See README for details                                         #
#----------------------------------------------------------------#

# Exploring what's in the gender package
##################################################################

# install.packages("gender")
library(gender)
ls("package:gender")
help(package = "gender")

# Bringing in other packages & the data
##################################################################
#install.packages(c("stringr", "dplyr", "RCurl"))
library(stringr)
library(dplyr)
library(RCurl)

# Import & explore data
urlfile <- 'https://raw.githubusercontent.com/mguideng/Gender-Pkg-NV-Salary/master/data/all-nevada-2016.csv'
salary <- read.csv(urlfile)
salary$X <- NULL

str(salary)
head(salary,5)

#Preparing the salary data
##################################################################

# Task 1. Remove non-characters
salary$Employee.Name <- str_replace_all(salary$Employee.Name, "\\d |\\d", "")     # Remove digits
salary$Employee.Name <- gsub("\\.", " ", salary$Employee.Name)                    # Replace '.' with a space
salary$Employee.Name <- gsub("-", " ", salary$Employee.Name)                      # Replace '-' with a space
salary$Employee.Name <- gsub("   ", " ", salary$Employee.Name)                    # Remove triple spaces
salary$Employee.Name <- gsub("  ", " ", salary$Employee.Name)                     # Remove double spaces

# Task 2. Isolate first names
salary$First.Name <- sub(".*\\, ", "", salary$Employee.Name)     # If comma, take element following ','
salary$First.Name <- sub(" .*", "", salary$First.Name)           # Otherwise, just take first element.

# Task3. Lower case the first names
salary$First.Name <- str_to_lower(salary$First.Name)

# Ready for the gender function
##################################################################
salary_gender <- gender(salary$First.Name)
head(salary_gender,5)

# Join to main salary dataframe
salary_gender <- salary_gender[c("name", "gender")]
colnames(salary_gender) <- c("First.Name", "Gender")
salary_gender <- subset(salary_gender, !duplicated(salary_gender$First.Name))
salary <- left_join(salary, salary_gender)

# Gender distribution?
salary %>%
  group_by(Gender) %>%
  summarize(employees=n()) %>%
  mutate(percent=round((employees/sum(employees)*100))) %>%
  arrange(desc(percent))

salary$Gender[is.na(salary$Gender)] <- "undetm"

# Can undetm be improved?
undetm <- salary %>%
  filter(Gender=="undetm") %>%
  group_by(First.Name, Employee.Name, Agency) %>%
  summarize (n = n()) %>%
  arrange(-n)

print(undetm)

# By agency?
undetm %>%
  group_by(Agency) %>%
  summarize (n = n()) %>%
  arrange(-n)

# Fixing undetermined genders
##################################################################
# Issues:
# 1. Some names are "redacted" / "undercover" and some ethnic names. Keep as undetm.
# 2. University Medical Center reports last name first without a comma preface
# 3. First names sometimes appear as suffixes ("sr"|"iii"|"iv")
# 4. Instances where first and last strings of Employee.Name are the same
# 5. Single-initial letters are being picked up as first names

# Solution for issues 2 - 5: adopt second element as the first name
##################################################################
# First, split strings for later pattern matching functions
salary$stringcount <- str_count(salary$Employee.Name," ")+1                         # Count of all elements in string
salary$string1 <- sapply(strsplit(salary$Employee.Name, " "), function(x) x[1])     # 1st elem
salary$string1len <- nchar(salary$string1)                                          # Length of 1st elem
salary$string2 <- sapply(strsplit(salary$Employee.Name, " "), function(x) x[2])     # 2nd elem
salary$stringN <- sapply(strsplit(salary$Employee.Name, " "), tail, 1)              # Last elem

# Fix issue 2. UMC
salary$First.Name <- ifelse(salary$Agency %in% c("University Medical Center"), salary$string2, salary$First.Name)

# Fix issue 3. Suffix
salary$First.Name <- ifelse(salary$First.Name %in% c("jr","sr", "ii", "iii", "iv"), salary$string2, salary$First.Name)

# Fix issue 4. First string = Last string
salary$First.Name <- ifelse(salary$string1 == salary$stringN & salary$stringcount>1, salary$string2, salary$First.Name)

# Fix issue 5. Initials
salary$First.Name <- ifelse(salary$string1len == 1 & salary$stringcount>=3, salary$string2, salary$First.Name)

# Gender function, Round 2
##################################################################
salary_gender <- gender(salary$First.Name)

salary_gender <- salary_gender[c("name", "gender")]
colnames(salary_gender) <- c("First.Name", "Gender")
salary_gender <- subset(salary_gender, !duplicated(salary_gender$First.Name))

salary$Gender <- NULL
salary <- left_join(salary, salary_gender)
salary$Gender[is.na(salary$Gender)] <- "undetm"

# Any better by % overall?
table(salary$Gender)
prop.table(table(salary$Gender))

# Any better by agency?
undetm <- salary %>%
  filter(Gender=="undetm") %>%
  group_by(First.Name, Employee.Name, Agency) %>%
  summarize (n = n()) %>%
  arrange(-n)

undetm %>%
  group_by(Agency) %>%
  summarize (n = n()) %>%
  arrange(-n)

# Last step: revert to the original dimensions + Gender column
names(salary)
salary <- salary[, -c(13:18)] # delete columns 13 through 18

# Output
write.csv(salary, "all-nevada-2016-with-gender.csv")