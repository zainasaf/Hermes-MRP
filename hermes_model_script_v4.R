
wd <- getwd()
if (wd != "/root") {
  setwd("C:/Users/Zain/More In Common/More In Common Team Site - MRP")
}; rm(wd)



library(cmdstanr)
library(brms)
library(dplyr)
library(stringr)
library(readr)
library(readxl)
library(data.table)
library(rstan)
library(future)
library(future.apply)
library(posterior)
library(parallel)
library(nnet)
# library(tidyverse)
library(dplyr)
library(purrr)

# brms settings
chains <- 4
iter <- 2500



## load polls ## 

options(timeout = 300)

library(dplyr)


poll1 <- read.csv("https://mrp-psf.ams3.digitaloceanspaces.com/polls/26-01-hermes-4k.csv")
poll2 <- read.csv("https://mrp-psf.ams3.digitaloceanspaces.com/polls/25-12-winter-10-for-hermes-n9k.csv")


poll_list <- list(poll1, poll2)
polldata <- rbindlist(poll_list, fill = TRUE, idcol = TRUE)


# Poll 1: ONLY "I have voted for them before in a general election"
poll1 <- poll1 %>%
  mutate(
    cons_ever_ge = evervotedfor_grid_2 ==
      "I have voted for them before in a general election"
  )


# (EXCLUDE "I have voted not voted for them in a general Election, but have in a local election")
poll2 <- poll2 %>%
  mutate(
    cons_ever_ge = evervoted_2 ==
      "I have voted for them before"
  )


polldata <- bind_rows(poll1, poll2)



## crate DV 
# cons_left and cons_reform, using poll-specific cons_ever_ge

polldata$cons_left <- ifelse(
  polldata$cons_ever_ge &
    polldata$votingintention %in% c("Labour", "Liberal Democrat", "The Green Party"),
  "Yes", "No"
)

polldata$cons_reform <- ifelse(
  polldata$cons_ever_ge &
    polldata$votingintention == "Reform UK",
  "Yes", "No"
)



# Manual check for poll 1
cons_left_1 <- poll1 %>%
  filter(
    evervotedfor_grid_2 ==
      "I have voted for them before in a general election",
    votingintention %in% c("Labour", "Liberal Democrat", "The Green Party")
  )

# Manual check for poll 2
cons_left_2 <- poll2 %>%
  filter(
    evervoted_2 ==
      "I have voted for them before",
    votingintention %in% c("Labour", "Liberal Democrat", "The Green Party")
  )

nrow(cons_left_1)
nrow(cons_left_2)
nrow(cons_left_1) + nrow(cons_left_2)   


cons_reform_1 <-  poll1 %>%
  filter(
    evervotedfor_grid_2 ==
      "I have voted for them before in a general election",
    votingintention %in% c("Reform UK")
  )


cons_reform_2 <- poll2 %>%
  filter(
    evervoted_2 ==
      "I have voted for them before",
    votingintention %in% c("Reform UK")
  )


nrow(cons_reform_1)
nrow(cons_reform_2)
nrow(cons_reform_1) + nrow(cons_reform_2)   



if (detectCores() == 16) {
  threads <- 3
  workers <- 4
}
if (detectCores() == 8) {
  threads <- 2
  workers <- 4
}
if (detectCores() == 4) {
  threads <- 1
  workers <- 4
}
if (detectCores() == 32) {
  threads <- 4
  workers <- 4
}

if (detectCores() == 48) {
  threads <- 11
  workers <- 4
}


## check all the variables and values are the same across the poll 



vars_to_check <- c("ethnicity", "leaveremain", "ge2019", "education", 
                   "workstatus", "ownrent", "gender", "welshlang", "ge2024", "votingintention")

# Function to get unique values for each variable
get_unique_values <- function(poll, var) {
  if(var %in% names(poll)) {
    unique(poll[[var]]) %>% sort() %>% as.character()
  } else {
    NA
  }
}


results <- list()

for(var in vars_to_check) {
  
  values_by_poll <- map(poll_list, ~get_unique_values(.x, var))
  
  
  has_var <- map_lgl(values_by_poll, ~!all(is.na(.x)))
  
  
  all_values <- values_by_poll %>% 
    discard(~all(is.na(.x))) %>% 
    unlist() %>% 
    unique() %>% 
    sort()
  
  # Check if values are consistent
  values_consistent <- values_by_poll %>% 
    discard(~all(is.na(.x))) %>% 
    map_lgl(~identical(sort(.x), sort(all_values))) %>% 
    all()
  
  results[[var]] <- list(
    present_in_all = all(has_var),
    polls_with_var = names(poll_list)[has_var],
    polls_missing_var = names(poll_list)[!has_var],
    all_unique_values = all_values,
    values_consistent = values_consistent,
    values_by_poll = values_by_poll
  )
}

for(var in vars_to_check) {
  cat("\n====================\n")
  cat("Variable:", var, "\n")
  cat("Present in all polls:", results[[var]]$present_in_all, "\n")
  
  if(!results[[var]]$present_in_all) {
    cat("Missing in:", paste(results[[var]]$polls_missing_var, collapse = ", "), "\n")
  }
  
  cat("Values consistent across polls:", results[[var]]$values_consistent, "\n")
  cat("All unique values:\n")
  print(results[[var]]$all_unique_values)
  
  if(!results[[var]]$values_consistent) {
    cat("\nValues by poll:\n")
    print(results[[var]]$values_by_poll)
  }
}



#### Constituency level data ####
aux <- read.csv("https://mrp-psf.ams3.digitaloceanspaces.com/other-files/const_data_full_new.csv")
aux <- aux %>% mutate(const_name = gsub("Ynys Mon", "Ynys Môn", const_name))

elex_results <- read.csv("https://mrp-psf.ams3.digitaloceanspaces.com/other-files/ge2024_results.csv")%>%
  mutate(const_name = gsub("Ynys M\xf4n", "Ynys Môn", const_name, useBytes = TRUE)) %>% select(const_name,turnout,ge_con,ge_lab,ge_libdem,ge_ref,ge_green,ge_snp,ge_pc,ge_oth)

vote_cols <- c("ge_con", "ge_lab", "ge_libdem", "ge_ref", 
               "ge_green", "ge_snp", "ge_pc", "ge_oth")

# Then multiply by turnout so they sum to the remaining proportion
elex_results[vote_cols] <- elex_results[vote_cols] * (elex_results$turnout)
elex_results <- elex_results %>% mutate(turnout = 1-turnout)

aux <- aux %>% left_join(elex_results,by='const_name')



## survey data ## 

col_list <- c('cid',
              'gender','region','age_1','education','ownrent', 'gender_age',
              'religion', 'ge2019',
              'ethnicity','workstatus', 'postcode_1','const_name',
              'leaveremain','likelihoodtovote','votingintention','votingforced',
              'indyref', 'welshlang',
              'START_DATE', 'start_date',
              'ge2024', 'cons_ever_ge', 'cons_left', 'cons_reform') 




## download postcode lookup file

options(timeout =1200)
# download.file("https://mrp-psf.ams3.digitaloceanspaces.com/other-files/postcodelookup.csv", 
#               destfile = "postcode_lookup.csv")
postcode_lookup <- read.csv("postcode_lookup.csv")


standardize_postcode <- function(postcode) {
  postcode <- as.character(postcode)
  postcode <- toupper(postcode)       
  postcode <- gsub("[[:space:]]", "", postcode) 
  postcode <- gsub("[[:punct:]]", "", postcode) 
  return(postcode)
}

# Standardize postcodes in both datasets
postcode_lookup$postcode_clean <- standardize_postcode(postcode_lookup$postcode)
postcode_lookup <- subset(postcode_lookup , !is.na(postcode_clean))

## this makes it so every name-clean postcode combination only has one entry in the data
postcode_lookup <- postcode_lookup %>%
  dplyr::select(name, postcode_clean) %>%
  dplyr::group_by(name, postcode_clean) %>%
  filter(row_number() == 1)

## this makes it so that the lookup table only has observations where a postcode matches a single constituency
postcode_lookup <- postcode_lookup %>% 
  group_by(postcode_clean) %>% 
  filter(n() == 1)


polldata$postcode_clean <- standardize_postcode(polldata$postcode_1)


# Join to match constituency names
polldata <- left_join(polldata, 
                      postcode_lookup[, c("postcode_clean", "name")], 
                      by = "postcode_clean")

polldata$constituency <- polldata$name
polldata$name <- NULL  

cat("Total rows in polldata:", nrow(polldata), "\n")
cat("Rows with matched constituency:", sum(!is.na(polldata$constituency)), "\n")
cat("Match rate:", round(100 * sum(!is.na(polldata$constituency)) / nrow(polldata), 1), "%\n")


polldata <- polldata %>% filter(constituency != "")
polldata$constituency <- gsub("Ynys Mon", "Ynys Môn", polldata$constituency, useBytes = TRUE)
polldata$constituency <- gsub("Ynys M\xf4n", "Ynys Môn", polldata$constituency, useBytes = TRUE)
polldata$constituency <- gsub("Ynys MÌ«n", "Ynys Môn", polldata$constituency, useBytes = TRUE)
polldata$constituency <- gsub("Ynys Mon", "Ynys Môn", polldata$constituency, useBytes = TRUE)
polldata$constituency <- gsub("Ynys MÃ´n", "Ynys Môn", polldata$constituency, useBytes = TRUE)
polldata$constituency <- gsub("Montgomeryshire and Glynd  r", "Montgomeryshire and Glyndwr", polldata$constituency, useBytes = TRUE)
polldata$constituency <- gsub("Montgomeryshire and Glynd_r", "Montgomeryshire and Glyndwr", polldata$constituency, useBytes = TRUE)
polldata$constituency <- gsub("Montgomeryshire and Glynd?r", "Montgomeryshire and Glyndwr", polldata$constituency, useBytes = TRUE)
polldata$constituency <- gsub("Montgomeryshire and Glynd_r", "Montgomeryshire and Glyndwr", polldata$constituency, useBytes = TRUE)
polldata$constituency <- gsub("Montgomeryshire$", "Montgomeryshire and Glyndwr", polldata$constituency, useBytes = TRUE)
polldata$constituency <- gsub("Montgomeryshire and GlyndÅµr", "Montgomeryshire and Glyndwr", polldata$constituency, useBytes = TRUE)
polldata$constituency <- ifelse(grepl("Montgomeryshire", polldata$constituency),"Montgomeryshire and Glyndwr", polldata$constituency)
polldata$constituency <- ifelse(grepl("Ynys", polldata$constituency),"Ynys Môn", polldata$constituency)

const_name_short <- read.csv("https://mrp-psf.ams3.digitaloceanspaces.com/other-files/const_name_short.csv")
const_name_short <- const_name_short %>% mutate(const_long = ifelse(grepl("Ynys", const_name_short$const_long),"Ynys MÃ´n", const_name_short$const_long))
const_name_short <- const_name_short %>% mutate(const_short = ifelse(grepl("Ynys", const_name_short$const_short),"Ynys MÃ´n", const_name_short$const_short))
polldata <- left_join(polldata, const_name_short, join_by(constituency==const_short))
polldata$constituency <- ifelse(is.na(polldata$const_long), polldata$constituency, polldata$const_long)
rm(const_name_short)

polldata$const_name <- polldata$constituency

polldata <- left_join(polldata,aux,by='const_name')
polldata$region <- polldata$region_name

polldata <- polldata %>% filter(region != "Northern Ireland")

## recode variables 






polldata$votingintention <- ifelse(polldata$votingintention == "Another party", "Another party/Independent candidate",
                                   ifelse(polldata$votingintention == "Another party/Independent Candidate", "Another party/Independent candidate", 
                                          ifelse(polldata$votingintention == "Another party / Independent candidate", "Another party/Independent candidate", 
                                                 ifelse(polldata$votingintention == "Another party / An independent candidate", "Another party/Independent candidate",
                                                        polldata$votingintention))))


polldata$votingintention <- ifelse(polldata$votingintention == "Liberal Democrats","Liberal Democrat",polldata$votingintention)
polldata$votingintention <- ifelse(polldata$votingintention == "Conservatives","Conservative",polldata$votingintention)

polldata$votingforced <- ifelse(polldata$votingforced == "Liberal Democrats","Liberal Democrat",polldata$votingforced)
polldata$votingforced <- ifelse(polldata$votingforced == "Conservatives","Conservative",polldata$votingforced)

polldata$votingforced <- ifelse(polldata$votingforced == "Another party", "Another party/Independent candidate",
                                ifelse(polldata$votingforced == "Another party / Independent candidate", "Another party/Independent candidate",
                                       ifelse(polldata$votingforced == "Another party/Independent Candidate", "Another party/Independent candidate", polldata$votingforced)))

polldata$ge2019 <- ifelse(polldata$ge2019 == "Another party", "Another party/Independent candidate",
                          ifelse(polldata$ge2019 == "Another Party/Independent candidate", "Another party/Independent candidate",
                                 ifelse(polldata$ge2019 == "Don't know/Can't remember", "DK_DNV", 
                                        ifelse(polldata$ge2019 == "Can't remember", "DK_DNV", 
                                               ifelse(polldata$ge2019 == "Don't know/prefer not to say", "DK_DNV",
                                                      ifelse(polldata$ge2019 == "I did not/could not vote", "DK_DNV",
                                                             ifelse(polldata$ge2019 == "I did not / could not vote", "DK_DNV",
                                                                    ifelse(grepl("Labour", polldata$ge2019, ignore.case = TRUE), "Labour",
                                                                           ifelse(grepl("Conservative", polldata$ge2019, ignore.case = TRUE), "Conservative",
                                                                                  ifelse(grepl("Liberal Democrat", polldata$ge2019, ignore.case = TRUE), "Liberal Democrats",
                                                                                         ifelse(grepl("Plaid Cymru", polldata$ge2019, ignore.case = TRUE), "Plaid Cymru",
                                                                                                ifelse(grepl("SNP", polldata$ge2019, ignore.case = TRUE), "Scottish National Party (SNP)",
                                                                                                       ifelse(grepl("Brexit", polldata$ge2019, ignore.case = TRUE), "The Brexit Party",
                                                                                                              ifelse(grepl("Green Party", polldata$ge2019, ignore.case = TRUE), "The Green Party",
                                                                                                                     polldata$ge2019
                                                                                                              ))))))))))))))


polldata$age_level <- ifelse(polldata$age_1 < 25,'18-24',
                             ifelse(polldata$age_1 < 35,'25-34',
                                    ifelse(polldata$age_1 < 45,'35-44',
                                           ifelse(polldata$age_1 < 55,'45-54',
                                                  ifelse(polldata$age_1 < 65,'55-64',
                                                         ifelse(polldata$age_1 < 75,'65-74','75+'))))))
polldata$age_level <- ifelse(is.na(polldata$age_1), polldata$age_weight, polldata$age_level)

# exclude pc if not in wales
exc_pc <- polldata %>% filter((region_name != "Wales") &(votingintention == "Plaid Cymru"))
polldata <- subset(polldata,!(polldata$cid%in%exc_pc$cid))

# exclude snp if not in scotland

exc_snp <- polldata %>% filter((region_name != "Scotland") & (votingintention == "Scottish National Party (SNP)"))
polldata <- subset(polldata,!(polldata$cid%in%exc_snp$cid))

#adj voting intention and turnout
# polldata$adjvotingintention<- ifelse(polldata$votingintention=="Don't know",polldata$votingforced,polldata$votingintention) ##squeeze don't knows
# polldata <- subset(polldata,polldata$adjvotingintention != "Don't know") ## exclude double dont knows
# polldata <- polldata %>% mutate(likelihoodtovote=recode(likelihoodtovote,'10 - Certain to vote'='10','0 - Certain not to vote'='0',"Don't know"='0'))
# polldata$likelihoodtovote <- as.numeric(polldata$likelihoodtovote)
# 
# polldata$adjvotingintention <- ifelse((polldata$ge2019%in%c("Don't know",'I did not vote','I was too young to vote'))&(polldata$age_level=="18-24")&(polldata$likelihoodtovote<10),'WNV',
#                                       ifelse(!(polldata$ge2019%in%c("Don't know",'I did not vote','I was too young to vote'))&(polldata$age_level=="18-24")&(polldata$likelihoodtovote<8),'WNV',
#                                              ifelse((polldata$age_level=="18-24")&(polldata$likelihoodtovote<9),"WNV",polldata$adjvotingintention)))
# polldata$adjvotingintention<- ifelse(polldata$adjvotingintention=='Another party/Independent candidate',"Other",polldata$adjvotingintention)
# polldata$adjvotingintention<- ifelse(polldata$adjvotingintention%in%c('I would not vote'),"WNV",polldata$adjvotingintention)
# polldata$adjvotingintention<- ifelse(polldata$adjvotingintention%in%c('I will not vote'),"WNV",polldata$adjvotingintention)

polldata <- polldata %>%
  mutate(vote5 = case_when(
    votingintention %in% c("Don't know", "I would not vote") ~ "WNV",
    votingintention %in% c("Another party/Independent candidate", "Scottish National Party (SNP)", "Plaid Cymru") ~ "Other",
    votingintention %in% c("Labour", "Liberal Democrat", "The Green Party") ~ "Left", 
    TRUE ~ votingintention), 
    vote5 = as.factor(vote5))


polldata <- polldata %>%
  mutate(ethn_level = case_when(
    grepl("^Asian", ethnicity) ~ "Asian",
    grepl("^Black", ethnicity) ~ "Black", 
    grepl("^White", ethnicity) ~ "White",
    grepl("^Mixed", ethnicity) ~ "Mixed",
    grepl("^Other", ethnicity) ~ "Other",
    TRUE ~ ethnicity  # keeps original value if none match
  ))

polldata <- polldata %>% filter(ethn_level != "Prefer not to say")
polldata$ge2019 <- recode(polldata$ge2019,"The Brexit Party"="Brexit Party",
                          "The Green Party"="Green Party",
                          "Another party/Independent candidate"="Other",
                          "Another party"="Other",
                          "Scottish National Party (SNP)"="Scottish National Party",
                          "Don't know"="DK_DNV",
                          "Don’t know"="DK_DNV",
                          "I did not vote"="DK_DNV",
                          "I was too young to vote"="NA",
                          "Change UK" = "Other",
                          "Conservative Party"="Conservative",
                          "I could not vote"="NA",
                          "Labour Party"="Labour",
                          "Liberal Democrats"="Liberal Democrat",
                          "Another party / An independent candidate" = "Other")
polldata$euref <- recode(polldata$leaveremain,"Don't know"="DK_DNV",
                         "Don't know/prefer not to say"="DK_DNV",
                         "I did not/could not vote"="DK_DNV",
                         "I did not vote"="DK_DNV",
                         "I was too young to vote"="DK_DNV",
                         "Can't remember"="DK_DNV",
                         "Did not vote"="DK_DNV")
polldata$edu_level <- recode(polldata$education,
                             "Vocational or Technical Qualifications Completed (e.g. HND, NVQ)"='No degree',
                             "Secondary Education Completed (GCSE / O Level / CSE or equivalent)"='No degree',
                             "Doctorate, Post-doctorate or equivalent  (Higher Degree)"='Degree',
                             "Secondary Education Completed (A Level or equivalent)"='No degree',
                             "University Education Completed (First Degree e.g. BA, BSc)"='Degree',
                             "Postgraduate Education Completed  (e.g. Masters)"='Degree',
                             "Incomplete Secondary Education  (Below GCSE / O Level)"='No degree',
                             "Some Vocational or Technical Qualifications"='No degree',
                             "Incomplete Secondary Education  (Below GCSE /O Level)"='No degree',
                             "Secondary Education Completed (GCSE /O Level /CSE or equivalent)"='No degree',
                             "Prefer not to answer"='No degree',
                             "Lower Secondary School"="No degree",
                             "Post-graduate"="Degree",
                             "Some Vocational or Technical Qualifications"="No degree",
                             "University"="Degree",
                             "Upper Secondary School"="No degree",
                             "Vocational/Technical college"="No degree",
                             "Doctorate, Post-doctorate or equivalent (Higher Degree)"="Degree",
                             "Incomplete Secondary Education (Below GCSE/O Level)"="No degree",
                             "Postgraduate Education Completed (e.g. Masters)"= "Degree",
                             "Secondary Education Completed (GCSE/O Level/CSE or equivalent)"="Degree")
polldata$econ_level <- recode(polldata$workstatus,
                              "Homemaker/Househusband/Housewife etc"="Inactive",
                              "Homemaker/Househusband/Housewife etc."="Inactive",
                              "Not working and not seeking work"="Inactive",
                              "Not working/temporarily unemployed/sick but seeking work"="Unemployed",
                              "Prefer not to say"="Inactive",
                              "Retired"="Inactive",
                              "Retired on a state pension only"="Inactive",
                              "Retired with a private pension"="Inactive",
                              "Student"="Inactive",
                              "Working full time - working 30 hours per week or more"="Employed",
                              "Working part time - working less than 30 hours per week"="Employed",
                              "Full-time employed"="Employed",
                              "Homemaker"="Inactive",
                              "Military"="Employed",
                              "Other"="Inactive",
                              "Out of work but looking"="Unemployed",
                              "Part-time employed"="Employed",
                              "Self-employed"="Employed",
                              "Unable to work"="Inactive",
                              "Unemployed and not looking for work (and not retired)"="Inactive")
polldata$housing <- recode(polldata$ownrent,
                           "Own outright"='Own',
                           "Owned outright"='Own',
                           "Own with a mortgage or loan"='Own',
                           "Owned with a mortgage or loan"='Own',
                           "Rent from the housing association"='Rent',
                           "Rented from the housing association"='Rent',
                           "Privately rent"='Rent',
                           "Privately rented"="Rent",
                           "Rent free"='Other',
                           "Prefer not to say"='Other',
                           "Rent from the council"='Rent',
                           "Rented from the council"='Rent',
                           "Home owner with mortgage"="Own",
                           "Home owner without mortgage"="Own",
                           "Living with parents or family"="Other",
                           "Rent from council or local authority"="Rent")

polldata <- polldata %>% filter(gender != "Other (please specify)")
polldata <- rename(polldata,'sex_level'='gender')
polldata$sex_level <- recode(polldata$sex_level,
                             "Male"="Male",
                             "Female"="Female",
                             "Man"="Male",
                             "Woman"="Female")
polldata<-polldata %>%filter(polldata$sex_level%in%c("Male","Female"))
polldata$rel_level <- recode(polldata$religion,
                             "Prefer not to say"='Not answered',
                             "Christian (including Church of England, Catholic, Protestant and all other Christian denominations)"="Christian",
                             "Any other religion, write in"="Other religion",
                             "Other"='Other religion')
polldata$welsh_level <- recode(polldata$welshlang,
                               "I can speak Welsh"='Fluent',
                               "I cannot speak Welsh but I can understand, read or write Welsh"='Limited',
                               "I can't speak, understand, read or write Welsh"="None",
                               "NA"="Not Available")
polldata <- polldata %>% filter(ge2024 != "")
polldata$ge2024 <- recode(polldata$ge2024,
                          "Another party"="Other",
                          "Another party / An independent candidate"="Other",
                          "Another party/Independent candidate"="Other",
                          "Another party / Independent candidate"="Other",
                          "Conservatives"="Conservative",
                          "Don't know"="DK_DNV",
                          "Can't remember"="DK_DNV",
                          "Don't know/prefer not to say"="DK_DNV",
                          "I did not vote"="DK_DNV",
                          "I did not/could not vote"="DK_DNV",
                          "I did not / could not vote"="DK_DNV",
                          "I was too young to vote"="DK_DNV",
                          "Scottish National Party (SNP)"="Scottish National Party",
                          "The Green Party"="Green Party")

polldata$welsh_level <- ifelse(polldata$region != "Wales","Not Available",polldata$welsh_level)
polldata$welsh_level <- ifelse(polldata$welsh_level == "", "Not Available", polldata$welsh_level)

polldata$indyref <- ifelse(polldata$indyref=="","Not Available",polldata$indyref)
polldata$indyref <- recode(polldata$indyref,
                           'I did not vote'='DK_DNV',
                           'I did not /could not vote'="DK_DNV",
                           'I was too young to vote'='DK_DNV',
                           "Don't know"='DK_DNV',
                           'AGAINST Scottish independence'='No',
                           'FOR Scottish independence'='Yes')
polldata$region <- recode(polldata$region, "South East England"="South East",
                          "North East England"="North East",
                          "South West England"="South West",
                          "North West England"="North West",
                          "Yorkshire and the Humber"="Yorkshire & Humber",
                          "Greater London"="London", 
                          "East of England"="Eastern")
polldata <- polldata %>% select(,  'cid', 'votingintention', 'votingforced',
                                'likelihoodtovote', 
                                'const_name','age_level','sex_level','edu_level','econ_level',
                                'ethn_level','housing',
                                'rel_level','region',
                                'euref','ge2019', 'indyref', 'welsh_level',
                                'ge2024', 'cons_ever_ge', 'cons_left', 'cons_reform', 'vote5')
## 'date', 'day', 'log_day',

## convert the dependent variable to binary 
 
  polldata <- polldata  %>% 
  mutate(
    cons_left_num = case_when(
      cons_left == "Yes" ~ 1,
      cons_left == "No" ~ 0,
      TRUE ~ NA_real_
    ))

polldata <- polldata %>% 
  mutate(
    cons_reform_num = case_when(
      cons_reform == "Yes" ~ 1, 
      cons_reform == "No" ~ 0,
      TRUE ~ NA_real_
    )) 
    
  


polldata <- polldata %>% left_join(aux,by='const_name')

polldata <- polldata %>%
  filter(ge2019 != "",
         age_level != "")

### check categories
polldata %>% group_by(ge2019) %>% count()
polldata %>% group_by(ge2024) %>% count()
polldata %>% group_by(age_level) %>% count()
polldata %>% group_by(sex_level) %>% count()
polldata %>% group_by(edu_level) %>% count()
polldata %>% group_by(euref) %>% count()
polldata %>% group_by(ethn_level) %>% count()
polldata %>% group_by(housing) %>% count()
polldata %>% group_by(indyref) %>% count()
polldata %>% group_by(rel_level) %>% count()
polldata %>% group_by(welsh_level) %>% count()
polldata %>% group_by(votingintention) %>% count()
polldata %>% group_by(votingforced) %>% count()
polldata %>% group_by(cons_left_num) %>% count()
polldata %>% group_by(cons_reform_num) %>% count() 



polldata <- polldata %>% 
  mutate(across(c(age_level, sex_level), as.factor))

## subset data to england, scotland, wales 

polldata_eng <- subset(polldata, !(polldata$region_name %in% c('Scotland','Wales'))) # england data only
polldata_eng <- subset(polldata_eng,!(polldata_eng$ge2019%in%c("Scottish National Party (SNP)", 'Plaid Cymru')))

polldata_wales <- subset(polldata, polldata$region_name=='Wales') # wales data only
polldata_wales <- subset(polldata_wales,!(polldata_wales$ge2019=='Scottish National Party (SNP)'))

polldata_scot <- subset(polldata, polldata$region_name=='Scotland') # scotland data only
polldata_scot <- subset(polldata_scot,!(polldata_scot$ge2019=='Plaid Cymru'))




## models

# Define model parameters
iter <- 5000        
chains <- 4         
cores <- 4          
threads <- 1        
treedepth <- 15     


formula_scot_cons_ever <-  cons_ever_ge ~
  (1 | ge2019) + (1 | ge2024) + (1|age_level:sex_level) +
  (1 | edu_level) + (1 | euref) + (1 | econ_level) + (1 | ethn_level) + (1 | housing) + (1 | indyref) + (1 | rel_level) +
  scale(depriv_index) + scale(avg_ruc) + scale(euref_leave) + scale(house_prices_perc) +
  scale(pct_self_emp) + scale(indyref_const) + scale(veterans) + scale(ukip_2015) + scale(students) +
  ge_lab + ge_libdem + ge_snp + ge_oth + ge_ref + turnout +
  council_lab + council_libdem + council_green + council_other + council_snp

a <- mean(polldata_scot$cons_ever_ge)
log(a/(1-a))

# Priors for model soctland conservative left 
prior_scot_cons_ever <- c(
  prior(normal(-.2, 1), class = "Intercept"),          
  prior(normal(0, .5), class = "b"),                     
  #prior(student_t(3, 0, 1), class = "sd")               
  prior(exponential(1), class = "sd")               
)

scot_cons_ever_mod <- brm(
  formula = formula_scot_cons_ever,
  data = polldata_scot,
  family = bernoulli(link = "logit"),
  seed = 320,
  silent = 0,
  iter = iter,
  prior = prior_scot_cons_ever,
  chains = chains,
  threads = threading(threads),
  refresh = 50,
  cores = cores,
  backend = "cmdstanr", 
  control = list(adapt_delta = 0.99, max_treedepth = treedepth)
)


formula_scot_vote <-  vote5 ~
  (1 | ge2019) + (1 | ge2024) + (1|age_level:sex_level) +
  (1 | edu_level) + (1 | euref) + (1 | econ_level) + (1 | ethn_level) + (1 | housing) + (1 | indyref) + (1 | rel_level) +
  cons_ever_ge + scale(depriv_index) + scale(avg_ruc) + scale(euref_leave) + scale(house_prices_perc) +
  scale(pct_self_emp) + scale(indyref_const) + scale(veterans) + scale(ukip_2015) + scale(students) +
  ge_lab + ge_libdem + ge_snp + ge_oth + ge_ref + turnout +
  council_lab + council_libdem + council_green + council_other + council_snp

pr <- get_prior(formula_scot_vote, polldata_scot, family = categorical())

dpars <- unique(pr$dpar[pr$class %in% c("b","Intercept")])
dpars <- dpars[dpars != ""]  # drop blanks

coef_nms <- pr$coef[2:22]
eg1 <- expand.grid(coef = coef_nms, dpar = dpars)
priors <- "c("

## priors for the fixed effect intercepts
for(i in 1:length(dpars)){
  priors <- paste0(priors, "prior(normal(0, 1.5), class= 'Intercept', dpar= '", dpars[i], "'),", "\n")
}

## priors for the fixed effect coefficients
for(i in 1:nrow(eg1)){
  priors <- paste0(priors, "prior(normal(0, .5), class= 'b', coef='", eg1$coef[i], "', dpar= '", eg1$dpar[i], "'),", "\n")
}

## priors on the random effect standard deviations
## use exponential(2) to produce a bit more regularization relative to exponential(1) or half student's t. 
for(i in 1:(length(dpars)-1)){
  priors <- paste0(priors, "prior(exponential(2), class= 'sd', dpar= '", dpars[i], "'),", "\n")
}
priors <- paste0(priors, "prior(exponential(2), class= 'sd', dpar= '", dpars[length(dpars)], "'))", "\n")

## priors from above is one long character string.  The function below evaluates those and turns 
## them into a brms prior object. 
prior_scot_vote <- eval(parse(text=priors))

scot_vote_mod <- brm(
  formula = formula_scot_vote,
  data = polldata_scot,
  family = categorical(),
  seed = 320,
  silent = 0,
  iter = iter,
  prior = prior_scot_vote,
  chains = chains,
  threads = threading(threads),
  refresh = 50,
  cores = cores,
  backend = "cmdstanr", 
  control = list(adapt_delta = 0.99, max_treedepth = treedepth)
)

summary(scot_cons_left)

saveRDS(scot_vote_mod, "scot_vote_mod.RDS")
saveRDS(scot_cons_ever_mod, "scot_cons_ever_mod.RDS")


a <- mean(polldata_wales$cons_left_num)
log(a/(1-a))
prior_wales_cons_left <- c(
  prior(normal(-3.2, .75), class = "Intercept"),          
  prior(normal(0, .25), class = "b"),                     
  prior(exponential(1), class = "sd")               
)



formula_wales_cons_ever <- cons_ever_ge ~
  (1 | ge2019) + (1 | ge2024) + (1|age_level:sex_level) +
  (1 | edu_level) + (1 | euref) + (1 | econ_level) + (1 | ethn_level) + (1 | housing) + (1 | welsh_level) + (1 | rel_level) +
  scale(depriv_index) + scale(avg_ruc) + scale(euref_leave) + scale(house_prices_perc) +
  scale(military) + scale(students) + scale(pct_welshlang) + scale(pct_self_emp) + scale(ukip_2015) +
  ge_lab + ge_libdem + ge_green + ge_pc + ge_ref + ge_oth + turnout +
  council_lab + council_libdem + council_green +  council_other + council_pc

prior_wales_cons_ever <- c(
  prior(normal(-.2, 1), class = "Intercept"),          
  prior(normal(0, .5), class = "b"),                     
  prior(exponential(2), class = "sd")               
)




wales_cons_ever_mod <- brm(
  formula = formula_wales_cons_ever,
  data = polldata_wales,
  family = bernoulli(link = "logit"),
  seed = 320,
  silent = 0,
  iter = iter,
  prior = prior_wales_cons_ever,
  chains = chains,
  threads = threading(threads),
  refresh = 50,
  cores = cores,
  backend = "cmdstanr", 
  control = list(adapt_delta = 0.99, max_treedepth = treedepth)
)


formula_wales_vote <- vote5 ~
  (1 | ge2019) + (1 | ge2024) + (1|age_level:sex_level) +
  (1 | edu_level) + (1 | euref) + (1 | econ_level) + (1 | ethn_level) + (1 | housing) + (1 | welsh_level) + (1 | rel_level) +
  cons_ever_ge + scale(depriv_index) + scale(avg_ruc) + scale(euref_leave) + scale(house_prices_perc) +
  scale(military) + scale(students) + scale(pct_welshlang) + scale(pct_self_emp) + scale(ukip_2015) +
  ge_lab + ge_libdem + ge_green + ge_pc + ge_ref + ge_oth + turnout +
  council_lab + council_libdem + council_green +  council_other + council_pc

pr <- get_prior(formula_wales_vote, polldata_wales, family = categorical())

dpars <- unique(pr$dpar[pr$class %in% c("b","Intercept")])
dpars <- dpars[dpars != ""]  # drop blanks

coef_nms <- pr$coef[2:23]
eg1 <- expand.grid(coef = coef_nms, dpar = dpars)
priors <- "c("
for(i in 1:length(dpars)){
  priors <- paste0(priors, "prior(normal(0, 1.5), class= 'Intercept', dpar= '", dpars[i], "'),", "\n")
}

for(i in 1:nrow(eg1)){
  priors <- paste0(priors, "prior(normal(0, .5), class= 'b', coef='", eg1$coef[i], "', dpar= '", eg1$dpar[i], "'),", "\n")
}

for(i in 1:(length(dpars)-1)){
  priors <- paste0(priors, "prior(exponential(2), class= 'sd', dpar= '", dpars[i], "'),", "\n")
}
priors <- paste0(priors, "prior(exponential(2), class= 'sd', dpar= '", dpars[length(dpars)], "'))", "\n")

prior_wales_vote <- eval(parse(text=priors))

wales_vote_mod <- brm(
  formula = formula_wales_vote,
  data = polldata_wales,
  family = categorical(),
  seed = 320,
  silent = 0,
  iter = iter,
  prior = prior_wales_vote,
  chains = chains,
  threads = threading(threads),
  refresh = 50,
  cores = cores,
  backend = "cmdstanr", 
  control = list(adapt_delta = 0.99, max_treedepth = treedepth)
)

saveRDS(wales_cons_ever_mod, "wales_cons_evel_mod.RDS")
saveRDS(wales_vote_mod, "wales_vote_mod.RDS")

## englland cons _left 

prior_eng_cons_ever <- c(
  prior(normal(0, 1.5), class = "Intercept"),          
  prior(normal(0, 1), class = "b"),                     
  prior(exponential(1), class = "sd")               
)


formula_eng_cons_ever <-  cons_ever_ge ~ 
  (1 | ge2019) + (1 | ge2024) + (1|age_level:sex_level) +
  (1 | edu_level) + (1 | euref) +  (1 | econ_level) +  (1 | ethn_level) + 
  (1 | housing) + (1 | rel_level) + (1 | region_name) +
  scale(depriv_index) +  scale(avg_ruc) + scale(euref_leave) + scale(house_prices_perc) +
  scale(pct_self_emp) + scale(students) + scale(military) + scale(ukip_2015) +
  ge_lab +  ge_libdem +  ge_green + ge_ref + ge_oth + turnout +
  council_lab + council_libdem + council_green + council_ref + council_other



eng_cons_ever_mod <- brm(
  formula = formula_eng_cons_ever,
  data = polldata_eng,
  family = bernoulli(link = "logit"),
  seed = 320,
  silent = 0,
  iter = iter,
  prior = prior_eng_cons_ever,
  chains = chains,
  threads = threading(threads),
  refresh = 50,
  cores = cores,
  backend = "cmdstanr", 
  control = list(adapt_delta = 0.99, max_treedepth = treedepth)
)

threads <- 1
treedepth <- 15
cores <- 4
formula_eng_vote <-  vote5 ~ 
  (1 | ge2019) + (1 | ge2024) + (1|age_level:sex_level) +
  (1 | edu_level) + (1 | euref) +  (1 | econ_level) +  (1 | ethn_level) + 
  (1 | housing) + (1 | rel_level) + (1 | region_name) +
  cons_ever_ge + scale(depriv_index) +  scale(avg_ruc) + scale(euref_leave) + scale(house_prices_perc) +
  scale(pct_self_emp) + scale(students) + scale(military) + scale(ukip_2015) +
  ge_lab +  ge_libdem +  ge_green + ge_ref + ge_oth + turnout +
  council_lab + council_libdem + council_green + council_ref + council_other

pr <- get_prior(formula_eng_vote, polldata_eng, family = categorical())

dpars <- unique(pr$dpar[pr$class %in% c("b","Intercept")])
dpars <- dpars[dpars != ""]  # drop blanks

coef_nms <- pr$coef[2:21]
eg1 <- expand.grid(coef = coef_nms, dpar = dpars)
priors <- "c("
for(i in 1:length(dpars)){
  priors <- paste0(priors, "prior(normal(0, 1.5), class= 'Intercept', dpar= '", dpars[i], "'),", "\n")
}

for(i in 1:nrow(eg1)){
  priors <- paste0(priors, "prior(normal(0, .5), class= 'b', coef='", eg1$coef[i], "', dpar= '", eg1$dpar[i], "'),", "\n")
}

for(i in 1:(length(dpars)-1)){
  priors <- paste0(priors, "prior(exponential(1), class= 'sd', dpar= '", dpars[i], "'),", "\n")
}
priors <- paste0(priors, "prior(exponential(1), class= 'sd', dpar= '", dpars[length(dpars)], "'))", "\n")

prior_eng_vote <- eval(parse(text=priors))

eng_vote_mod <- brm(
  formula = formula_eng_vote,
  data = polldata_eng,
  family = categorical(),
  seed = 320,
  silent = 0,
  iter = iter,
  prior = prior_eng_vote,
  chains = chains,
  threads = threading(threads),
  refresh = 50,
  cores = cores,
  backend = "cmdstanr", 
  control = list(adapt_delta = 0.99, max_treedepth = treedepth)
)

saveRDS(eng_cons_ever_mod, "eng_cons_ever_mod.RDS")