
# The aim of this code is to calculate the % of former smokers now who have quit within the last 10 years
# based on the Health Survey for England 2019

# Point to the location of the X drive
root_dir <- "X:/"

# The HSE 2019 data is already downloaded
# dataset available from the UK data service https://ukdataservice.ac.uk/help/

# Package names
# packages <- c("data.table",
#               "stringr",
#               "here",
#               "magrittr",
#               "devtools",
#               "readr",
#               "Rfast",
#               "dplyr",
#               "writexl",
#               "mice",
#               "Hmisc",
#               "magrittr")
# 
# # Install packages not yet installed
# installed_packages <- packages %in% rownames(installed.packages())
# if (any(installed_packages == FALSE)) {
#   install.packages(packages[!installed_packages])
# }
# 
# # Install hseclean
# devtools::install_git("https://github.com/stapm/hseclean.git", ref = "1.11.3", build_vignettes = FALSE)

# Load packages
library(data.table)
library(hseclean)
library(magrittr)


# apply functions to create the variables for analysis and to retain only the required variables

# The variables to retain
keep_vars = c("hse_id", "wt_int", "psu", "cluster", "year", "quarter",
              "age", "age_cat", "sex", "imd_quintile",
              "employ2cat", "social_grade",
              "nssec3_lab", "man_nonman", "activity_lstweek",
              
              "cig_smoker_status", "years_since_quit",
              "smk_start_age", "smk_stop_age")

# The variables that must have complete cases
complete_vars <- c("age", "sex", "year", "psu", "cluster", "imd_quintile", "cig_smoker_status")

# Read and clean the HSE tobacco data

cleandata <- function(data) {
  
  data %<>%
    clean_age %>%
    clean_demographic %>% 
    clean_economic_status %>%
    smk_status %>%
    smk_former %>%
    smk_life_history %>%
    smk_amount %>%
    
    select_data(
      ages = 13:89,
      years = 2019,
      
      # variables to retain
      keep_vars = keep_vars,
      
      # The variables that must have complete cases
      complete_vars = complete_vars)
  
  return(data)
}

# Read and clean each year of data
data <- cleandata(read_2019(root = root_dir))

# remake age categories
data[, age_cat := c("13-15",
                    "16-17",
                    "18-24",
                    "25-34",
                    "35-44",
                    "45-54",
                    "55-64",
                    "65-74",
                    "75-89")[findInterval(age, c(-1, 16, 18, 25, 35, 45, 55, 65, 75, 1000))]]

# select individuals age 18+
data <- data[age >= 18]

# Filter to retain only former smokers with data on the number of years since quitting
data <- data[cig_smoker_status == "former" & !is.na(years_since_quit)]
# 2605 data points

# Make a variable where
# y = quit < 10 years
# n = quit >= 10 years

data[years_since_quit <= 10, quit10 := "y"]
data[years_since_quit > 10, quit10 := "n"]

# calculate the survey weighted proportion

# overall
prop_quit10 <- prop_summary(data = data,
                            var_name = "quit10",
                            levels_1 = "y",
                            levels_0 = "n",
                            strat_vars = "year")

write.table(prop_quit10, "prop_quit10.csv", row.names = F, sep = ",")

# by age and sex
prop_quit10_age <- prop_summary(data = data,
                                var_name = "quit10",
                                levels_1 = "y",
                                levels_0 = "n",
                                strat_vars = c("age_cat", "sex"))

write.table(prop_quit10_age, "prop_quit10_age_sex.csv", row.names = F, sep = ",")


# by imdq
prop_quit10_imdq <- prop_summary(data = data,
                                 var_name = "quit10",
                                 levels_1 = "y",
                                 levels_0 = "n",
                                 strat_vars = "imd_quintile")

write.table(prop_quit10_imdq, "prop_quit10_imdq.csv", row.names = F, sep = ",")






