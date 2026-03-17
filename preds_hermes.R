all_objects <- ls(envir = .GlobalEnv)
if (exists("pred_master")) {objects_to_remove <- all_objects[all_objects != "pred_master"]}
if (!exists("pred_master")) {objects_to_remove <- all_objects}
rm(list = objects_to_remove)

listOfPackages <- c("tictoc","tibble","tidyr","brms","dplyr",
                    "readr","data.table","rstan","future",
                    "future.apply","stringr")

for (i in 1:length(listOfPackages)) {
  package <- listOfPackages[i]
  if(!package %in% installed.packages()){install.packages(listOfPackages[i], dependencies = TRUE)} 
}

library(tictoc)
library(tibble)
library(tidyr)
library(brms)
library(dplyr)
library(readr)
library(data.table)
library(rstan)
library(future)
library(future.apply)
library(stringr)



     
eng_vote <- "eng_vote_mod.RDS"
scot_vote <- "scot_vote_mod.RDS"
wales_vote <- "wales_vote_mod.RDS"

eng_cons <- "eng_cons_ever_mod.RDS"
scot_cons <- "scot_cons_ever_mod.RDS"
wales_cons <- "wales_cons_ever_mod.RDS"






# url to digital ocean space where psf is stored
psf_location <- "https://mrp-psf.ams3.digitaloceanspaces.com/psf-with-ge2024"

ndraws <- 500

#### loading and prepping ####

aux <- read.csv("https://mrp-psf.ams3.digitaloceanspaces.com/other-files/const_data_full_new.csv")
aux <- aux %>% mutate(const_name = gsub("Ynys Mon", "Ynys Môn", const_name))

elex_results <- read.csv("https://mrp-psf.ams3.digitaloceanspaces.com/other-files/ge2024_results.csv")%>%
  mutate(const_name = gsub("Ynys M\xf4n", "Ynys Môn", const_name, useBytes = TRUE),
         const_name = gsub("Ynys Mon", "Ynys Môn", const_name, useBytes = TRUE)) %>% 
  select(const_name,turnout,ge_con,ge_lab,ge_libdem,ge_ref,ge_green,ge_snp,ge_pc,ge_oth)

vote_cols <- c("ge_con", "ge_lab", "ge_libdem", "ge_ref", 
               "ge_green", "ge_snp", "ge_pc", "ge_oth")

# Then multiply by (1 - turnout) so they sum to the remaining proportion
elex_results[vote_cols] <- elex_results[vote_cols] * (elex_results$turnout)
elex_results$turnout <- 1-elex_results$turnout

aux <- aux %>% left_join(elex_results,by='const_name')

const_names <- read.csv("https://mrp-psf.ams3.digitaloceanspaces.com/other-files/order_for_preds.csv") %>% select(const_name)

const_to_region <- read.csv("https://mrp-psf.ams3.digitaloceanspaces.com/other-files/Const%20to%20region.csv")
const_names <- left_join(const_names, const_to_region)
if (!exists("scot_cons")) {const_names <- const_names %>% filter(region != "Scotland")}
if (!exists("wales_cons")) {const_names <- const_names %>% filter(region != "Wales")}
if (!exists("eng_cons")) {const_names <- const_names %>% filter(region != "East Midlands")}
if (!exists("eng_cons")) {const_names <- const_names %>% filter(region != "East of England")}
if (!exists("eng_cons")) {const_names <- const_names %>% filter(region != "Greater London")}
if (!exists("eng_cons")) {const_names <- const_names %>% filter(region != "North East England")}
if (!exists("eng_cons")) {const_names <- const_names %>% filter(region != "North West England")}
if (!exists("eng_cons")) {const_names <- const_names %>% filter(region != "South East England")}
if (!exists("eng_cons")) {const_names <- const_names %>% filter(region != "South West England")}
if (!exists("eng_cons")) {const_names <- const_names %>% filter(region != "West Midlands")}
if (!exists("eng_cons")) {const_names <- const_names %>% filter(region != "Yorkshire and the Humber")}

const_names <- data.frame(const_name = const_names$const_name)

## load electorate size data and make sure constituency names are consistent
# esize <- read_csv("/Users/david/Dropbox/zain_asaf/September MRP Vote/electorate_size_const.csv") %>%
#   setNames(c("const_name", "electorate", "n_voters"))
# 
# esize$const_name <- ifelse(grepl("Montgomeryshire",esize$const_name),"Montgomeryshire and Glyndwr", esize$const_name)
# esize$const_name <- ifelse(grepl("Ynys",esize$const_name),"Ynys Môn", esize$const_name)

# const_names <- const_names %>% slice(1:25)

library(doFuture)
options('doFuture.rng.onMisuse' = "ignore")
registerDoFuture()
# wrk <- parallel::detectCores()-2
wrk <- 6
plan(multisession, workers=wrk)


## Look at Islington 
const_names %>% mutate(row = row_number()) %>%  filter(grepl("Aldridge", const_name))
## assign row number of constituency
n <- 374
## block and run from const_name_i <- const_names$const_name[n] to just before the write.csv()

try(dir.create("overall"), silent=TRUE)
try(dir.create("age"), silent=TRUE)
try(dir.create("ethnicity"), silent=TRUE)
try(dir.create("sex"), silent=TRUE)
try(dir.create("degree"), silent=TRUE)
try(dir.create("age_sex"), silent=TRUE)

for(n in 1:nrow(const_names)){
     const_name_i <- const_names$const_name[n]
     const_formatted <- str_replace_all(const_name_i, "ô", "%C3%B4")
     const_formatted <- str_replace_all(const_formatted, " ", "%20")
     psf_file <- paste0("https://mrp-psf.ams3.digitaloceanspaces.com/psf-with-ge2024/psf-", const_formatted,".csv")
     data <- read.csv(psf_file)
     names(data)[names(data) == 'welsh_level'] <- 'welshlang'
     ## join in electorate size and number of voters
     # data <- data %>% left_join(esize) %>% 
     #   ## calculate expected number of voters in each cell assuming equal turnout rates across groups. 
     #   mutate(voters_per_cell = n_voters*perc, 
     #          electorate_per_cell = electorate*perc)
     region <- data$region[1]
     if (region == "Scotland") {cons_fit <- readRDS(scot_cons); vote_fit <- readRDS(scot_vote)}
     if (region == "Wales") {cons_fit <- readRDS(wales_cons); vote_fit <- readRDS(wales_vote)}
     if (region != "Wales" & region != "Scotland") {cons_fit <- readRDS(eng_cons); vote_fit <- readRDS(eng_vote)}
     if (const_name_i == "South Holland and The Deepings") {data$region <- "East Midlands"}
     if (const_name_i == "Queen's Park and Maida Vale") {data$region <- "Greater London"}
     data <- data %>% select(-c(X))
     ## data is post stratfication  for ever voted conservative 
     ## data 1 is post strat for vote intention model - includes variable set to ever voted conservative before in the vote model
     data <- data %>% left_join(aux, by = 'const_name')
     data1 <- data %>% mutate(cons_ever_ge = TRUE)
     options(timeout = 1200)
     ## # Draw posterior expected probabilities from both models at poststrat cell
     
     ## Gives a matrix  of posterior expected probabilities that each poststrat cell  has ever voted Conservative, for each posterior draw s
     cons_pred <- posterior_epred(cons_fit, newdata = data, allow_new_levels = TRUE, ndraws = ndraws) 
    # Gives expected probabilities for the second-stage outcome categories (Left, Reform, etc.) conditional on cons_ever_ge = TRUE in data (data1)
     vote_pred <- posterior_epred(vote_fit, newdata = data1, allow_new_levels = TRUE, ndraws = ndraws) 
    
      # Convert predictions draws to data frames and attach cell IDs
     
     cons_pred <- as.data.frame(cons_pred)
     vote_pred <- as.data.frame(vote_pred)
     data <- data %>% dplyr::mutate(id = as.character(row_number()))
     data1 <- data1 %>% dplyr::mutate(id = as.character(row_number()))
    
    # Transpose so that each row corresponds to a posterior draw and each column corresponds to a poststrat cell (or cell × category). Is easier for later operations

     cons_pred <- t(cons_pred)
     vote_pred <- t(vote_pred)
     cons_pred <- rownames_to_column(data.frame(cons_pred), var = "names")
     vote_pred <- rownames_to_column(data.frame(vote_pred), var = "names")
    # tmp <- cons_pred
     cons_pred <- cons_pred %>%
       rename_with(~str_replace(., "X", "pred"), everything()) %>%
       dplyr::mutate(id = str_replace(names, "V", ""), .before = "pred1") %>% 
       select(-names) %>% 
     #  attach the poststrat cell info to each row of predictions.
       full_join(data, pred, by = "id")
     
    # for every posterior draw and every category (Left, Reform UK, etc.), get predicted probabilities for each cell, plus the cell’s covariates and an outcome label level.
     vote_pred <- vote_pred %>%
       rename_with(~str_replace(., "X", "pred"), everything()) %>%
       separate(names,
                into = c("num", "level"),
                extra = "merge") %>%
       dplyr::mutate(level = str_replace_all(level, "[.]", " ")) %>%
       dplyr::mutate(id = str_replace(num, "X", "")) %>% 
       full_join(data1, vote_pred, by = "id")
     
     
     # Build matrices of posterior expectations by cell and draw
     ## first stage predictions - proability of having voted conservative before 
     
     cons_pred_mat <- cons_pred %>% 
       
       select(starts_with("pred")) %>% 
       as.matrix()
     ## probaility of voting for each of the groups - each post strat cell x posterior disitbrution 
     ## 1 matrix for having voted Cons and 1 for current VI -(preidctions for second stage model)
     ## probaility of voting for each group having voted for Cons ever 
     vote_pred_left <- vote_pred %>% 
       filter(level == "Left") %>% 
       select(starts_with("pred")) %>%
       as.matrix()
     vote_pred_reform <- vote_pred %>% 
       filter(level == "Reform UK") %>% 
       select(starts_with("pred")) %>%
       as.matrix()
     vote_pred_wnv <- vote_pred %>% 
       filter(level == "WNV") %>% 
       select(starts_with("pred")) %>%
       as.matrix()
     vote_pred_other <- vote_pred %>% 
       filter(grepl("Other", level)) %>% 
       select(starts_with("pred")) %>%
       as.matrix()
     vote_pred_cons <- vote_pred %>% 
       filter(level == "Conservative") %>% 
       select(starts_with("pred")) %>%
       as.matrix()
     
     
     # Combine stages: joint probabilities = P(ever-Con) * P(vote | ever-Con)
     
     pr_cons_left <- cons_pred_mat * vote_pred_left                 
     pr_cons_reform <- cons_pred_mat * vote_pred_reform                 
     pr_cons_wnv <- cons_pred_mat * vote_pred_wnv                 
     pr_cons_other <- cons_pred_mat * vote_pred_other                 
     pr_cons_cons <- cons_pred_mat * vote_pred_cons   
     
     ## takes postrat frame slect releveant democgprhic variable and cell percentage and constiteuncy name  
     cons_left <- cons_pred %>% 
      select(const_name, perc, ethn_level, age_level, sex_level, edu_level) %>% 
      mutate(level = "Left", .after="const_name") %>% 
      bind_cols(pr_cons_left) 
    cons_reform <- cons_pred %>% 
      select(const_name, perc, ethn_level, age_level, sex_level, edu_level) %>% 
      mutate(level = "Reform UK", .after="const_name") %>% 
      bind_cols(pr_cons_reform) 
    cons_wnv <- cons_pred %>% 
      select(const_name, perc, ethn_level, age_level, sex_level, edu_level) %>% 
      mutate(level = "WNV", .after="const_name") %>% 
      bind_cols(pr_cons_wnv) 
    cons_other <- cons_pred %>% 
      select(const_name, perc, ethn_level, age_level, sex_level, edu_level) %>% 
      mutate(level = "Other", .after="const_name") %>% 
      bind_cols(pr_cons_other)
    cons_cons <- cons_pred %>% 
      select(const_name, perc, ethn_level, age_level, sex_level, edu_level) %>% 
      mutate(level = "Conservative", .after="const_name") %>% 
      bind_cols(pr_cons_cons) 
    
    cons_delta <- bind_rows(cons_left, cons_reform, cons_wnv, cons_other, cons_cons)
    
  overall_pred <- cons_delta %>%
       select(c(level, const_name, perc, starts_with("pred"))) %>% 
       dplyr::mutate(across(starts_with("pred"), function(z) as.double(z))) %>%
       dplyr::mutate(across(starts_with("pred"), function(z) z*perc)) %>%
       group_by(const_name,level) %>% 
       dplyr::summarise(across(starts_with("pred"), sum)) %>% 
       rowwise() %>%
       dplyr::mutate(mean = mean(c_across(c(starts_with('pred'))), na.rm=TRUE)) %>%
       select(const_name,level,mean)
  eth_pred <- cons_delta %>%
    select(c(level, const_name, perc, ethn_level, starts_with("pred"))) %>% 
    dplyr::mutate(across(starts_with("pred"), function(z) as.double(z))) %>%
    dplyr::mutate(across(starts_with("pred"), function(z) z*perc)) %>%
    group_by(const_name,level, ethn_level) %>% 
    dplyr::summarise(across(starts_with("pred"), sum)) %>% 
    rowwise() %>%
    dplyr::mutate(mean = mean(c_across(c(starts_with('pred'))), na.rm=TRUE)) %>%
    select(const_name,level, ethn_level, mean) 
  age_pred <- cons_delta %>%
    select(c(level, const_name, perc, age_level, starts_with("pred"))) %>% 
    dplyr::mutate(across(starts_with("pred"), function(z) as.double(z))) %>%
    dplyr::mutate(across(starts_with("pred"), function(z) z*perc)) %>%
    group_by(const_name,level, age_level) %>% 
    dplyr::summarise(across(starts_with("pred"), sum)) %>% 
    rowwise() %>%
    dplyr::mutate(mean = mean(c_across(c(starts_with('pred'))), na.rm=TRUE)) %>%
    select(const_name,level, age_level, mean) 
  sex_pred <- cons_delta %>%
    select(c(level, const_name, perc, sex_level, starts_with("pred"))) %>% 
    dplyr::mutate(across(starts_with("pred"), function(z) as.double(z))) %>%
    dplyr::mutate(across(starts_with("pred"), function(z) z*perc)) %>%
    group_by(const_name,level, sex_level) %>% 
    dplyr::summarise(across(starts_with("pred"), sum)) %>% 
    rowwise() %>%
    dplyr::mutate(mean = mean(c_across(c(starts_with('pred'))), na.rm=TRUE)) %>%
    select(const_name,level, sex_level, mean) 
  edu_pred <- cons_delta %>%
    select(c(level, const_name, perc, edu_level, starts_with("pred"))) %>% 
    dplyr::mutate(across(starts_with("pred"), function(z) as.double(z))) %>%
    dplyr::mutate(across(starts_with("pred"), function(z) z*perc)) %>%
    group_by(const_name,level, edu_level) %>% 
    dplyr::summarise(across(starts_with("pred"), sum)) %>% 
    rowwise() %>%
    dplyr::mutate(mean = mean(c_across(c(starts_with('pred'))), na.rm=TRUE)) %>%
    select(const_name,level, edu_level, mean) 
  age_sex_pred <- cons_delta %>%
    select(c(level, const_name, perc, age_level, sex_level, starts_with("pred"))) %>% 
    dplyr::mutate(across(starts_with("pred"), function(z) as.double(z))) %>%
    dplyr::mutate(across(starts_with("pred"), function(z) z*perc)) %>%
    group_by(const_name,level, age_level, sex_level) %>% 
    dplyr::summarise(across(starts_with("pred"), sum)) %>% 
    rowwise() %>%
    dplyr::mutate(mean = mean(c_across(c(starts_with('pred'))), na.rm=TRUE)) %>%
    select(const_name,level, age_level, sex_level, mean) 
  write.csv(overall_pred,paste0("overall/preds_",const_name_i,".csv"))
  write.csv(age_pred,paste0("age/preds_",const_name_i,".csv"))
  write.csv(eth_pred,paste0("ethnicity/preds_",const_name_i,".csv"))
  write.csv(sex_pred,paste0("sex/preds_",const_name_i,".csv"))
  write.csv(edu_pred,paste0("degree/preds_",const_name_i,".csv"))
  write.csv(age_sex_pred,paste0("age_sex/preds_",const_name_i,".csv"))
}


pred_files <- list.files(path="overall/", pattern="preds_.*\\.csv", full.names = TRUE)
all_preds <- lapply(pred_files, \(x)read_csv(x)) %>%
  bind_rows()

all_preds %>%
  select(const_name, level, mean) %>%
  tidyr::pivot_wider(names_from = "level", values_from = "mean") -> pred_master

write.csv(pred_master, "overall/switch_mrp_preds.csv")

pred_files <- list.files(path="age/", pattern="preds_.*\\.csv", full.names = TRUE)
all_preds <- lapply(pred_files, \(x)read_csv(x)) %>%
  bind_rows()

all_preds %>%
  select(const_name, level, age_level,  mean) %>%
  tidyr::pivot_wider(names_from = "level", values_from = "mean") -> pred_master
write.csv(pred_master, "age/switch_mrp_preds.csv")

pred_files <- list.files(path="sex/", pattern="preds_.*\\.csv", full.names = TRUE)
all_preds <- lapply(pred_files, \(x)read_csv(x)) %>%
  bind_rows()

all_preds %>%
  select(const_name, level, sex_level,  mean) %>%
  tidyr::pivot_wider(names_from = "level", values_from = "mean") -> pred_master
write.csv(pred_master, "sex/switch_mrp_preds.csv")

pred_files <- list.files(path="ethnicity/", pattern="preds_.*\\.csv", full.names = TRUE)
all_preds <- lapply(pred_files, \(x)read_csv(x)) %>%
  bind_rows()

all_preds %>%
  select(const_name, level, ethn_level,  mean) %>%
  tidyr::pivot_wider(names_from = "level", values_from = "mean") -> pred_master

write.csv(pred_master, "ethnicity/switch_mrp_preds.csv")

pred_files <- list.files(path="degree/", pattern="preds_.*\\.csv", full.names = TRUE)
all_preds <- lapply(pred_files, \(x)read_csv(x)) %>%
  bind_rows()

all_preds %>%
  select(const_name, level, edu_level,  mean) %>%
  tidyr::pivot_wider(names_from = "level", values_from = "mean") -> pred_master

write.csv(pred_master, "degree/switch_mrp_preds.csv")

pred_files <- list.files(path="age_sex/", pattern="preds_.*\\.csv", full.names = TRUE)
all_preds <- lapply(pred_files, \(x)read_csv(x)) %>%
  bind_rows()

all_preds %>%
  select(const_name, level, age_level, sex_level,  mean) %>%
  tidyr::pivot_wider(names_from = "level", values_from = "mean") -> pred_master
write.csv(pred_master, "age_sex/switch_mrp_preds.csv")


