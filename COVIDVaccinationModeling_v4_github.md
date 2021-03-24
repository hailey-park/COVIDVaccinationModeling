COVIDVaccinationModeling\_v4\_github
================
Hailey Park
3/23/2021

``` r
library(tidyverse)
library(tibble)
library(ggplot2)
library(tidyr)
library(readr)
library(dplyr)
library(binom)

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)
```

``` r
#Reading in datasets
tracts <- readRDS("tracts_with_cdph_mapping.RDS")

county_cases <- readRDS("cdph_cases_by_county_age.RDS")

essential_worker_ages <- read_csv("essential_worker_pop_by_age.csv")
```

``` r
#Cleaning the datasets
county_cleaned <- county_cases %>% filter(county_res != "UNASSIGNED", age_cat != "NA")

tract_pop_all_ages <- tracts %>%
  filter(age_text == "all_ages",
         pop_2019 != 0,
         pop_2019 >= total_tract_cases)

tracts_age_cleaned <- tracts %>%
  filter(age_text != "all_ages",
         pop_2019 != 0, 
         geoid %in% tract_pop_all_ages$geoid) %>%
  select(geoid, geo_name, pop_2019, age_text, county_name, total_tract_cases)

essential_workers_cleaned <- essential_worker_ages %>%
  rename(frontline = "6", non_frontline = "7") %>%
  mutate(combined = frontline + non_frontline)
```

``` r
#Cleaning the tracts_age_cleaned dataset to convert the county names of each tract to ones defined by the county_cleaned dataset

#Source: https://census.ca.gov/regions/
#Source: https://www.apacalifornia-conference.org/sacramento_valley.php 

central_sierra_counties <- c("Alpine", "Amador","Calaveras", "El Dorado", "Mariposa", "Mono", "Nevada", "Tuolumne")
north_ca_counties <- c("Del Norte", "Glenn", "Humboldt", "Lake", "Lassen", "Mendocino", "Modoc", "Shasta", "Siskiyou", "Tehama", "Trinity")
north_sac_valley_counties <- c("Colusa", "Sierra")
greater_sac_counties <- c("Butte", "Placer", "Plumas", "Sutter", "Yolo", "Yuba")
san_joaquin_val_counties <- c( "Inyo", "Kings", "Madera")
santa_crz_san_benito <- c("Santa Cruz", "San Benito")
napa_sonoma <- c("Napa", "Sonoma")

tracts_names <- tracts_age_cleaned$county_name

for (i in 1:length(tracts_names)){
  if (tracts_names[i] %in% central_sierra_counties) {
      tracts_names[i] <- "Central Sierra"
  }
  else if (tracts_names[i] %in% north_ca_counties) {
      tracts_names[i] <- "Northern California"
  }
  else if (tracts_names[i] %in% north_sac_valley_counties) {
      tracts_names[i] <- "Northern Sacramento Valley"
  }
  else if (tracts_names[i] %in% greater_sac_counties) {
      tracts_names[i] <- "Greater Sacramento"
  }
  else if (tracts_names[i] %in% san_joaquin_val_counties) {
      tracts_names[i] <- "San Joaquin Valley"
  }
  else if (tracts_names[i] %in% santa_crz_san_benito) {
      tracts_names[i] <- "SantaCruz_SanBenito"
  }
  else if (tracts_names[i] %in% napa_sonoma) {
      tracts_names[i] <- "Napa_Sonoma"
  }
}

tracts_age_cleaned$county_name <- tracts_names
```

``` r
#Estimation of total population using tract-level data
tract_pop_all_ages <- tracts %>%
  filter(age_text == "all_ages",
         pop_2019 != 0,
         pop_2019 >= total_tract_cases)

total_pop <- sum(tract_pop_all_ages$pop_2019)
```

``` r
#Plot Number of People By Age Category

age_category_ordering <- c('under_5_years', 
                           '5_to_9_years', 
                           '10_to_14_years',
                           '15_to_19_years',
                           '20_to_24_years',
                           '25_to_34_years',
                           '35_to_44_years',
                           '45_to_54_years',
                           '55_to_59_years',
                           '60_to_64_years',
                           '65_to_74_years',
                           '75_to_84_years',
                           '85_years_and_over')

pop_by_age <- tracts %>% 
  filter(age_text != "all_ages") %>% 
  group_by(age_text) %>% 
  summarise(total_pop = sum(pop_2019)) %>% 
  mutate(pop_million = total_pop/1000000) %>% 
  mutate(age_text = factor(age_text, levels = age_category_ordering, ordered = TRUE))
```

``` r
############################################
#MERGING COUNTY DATASET AND TRACT DATASETS
############################################

#The county-level dataset provides age-specific COVID-19 cumulative incidence rates for each county. So these age-specific rates (by county) are mapped onto each census tract to estimate the number of clinical COVID-19 cases in age group within each tract prior to the simulation.


#Creating df of total cases by county
total_cases_by_county <- county_cases %>% 
  filter(!is.na(age_cat), county_res != "UNASSIGNED") %>%
  group_by(county_res) %>% 
  summarise(total_cases_by_county=sum(county_cases))

#Creating a Column for # Cases in 5-YR Intervals by dividing the county dataset's 10-YR age categories in half
county_data_five_yr_intervals <- county_cases  %>%
  filter(!is.na(age_cat), county_res != "UNASSIGNED") %>% 
  mutate (cases_5yr_age_interval = county_cases/2) %>%
  rename(cases_10yr_age_interval = county_cases) 

#Merging the tables above to calculate age-specific incidence estimates in each tract
merged_county <- left_join(county_data_five_yr_intervals, total_cases_by_county, by = "county_res") %>%
  mutate(case_proportion_by_age = cases_5yr_age_interval/total_cases_by_county)

merged_county_tract <- left_join(tracts_age_cleaned, merged_county, by=c("county_name" = "county_res")) %>%
  filter((age_text == "under_5_years" & age_cat == "<10") |
           (age_text == "5_to_9_years" & age_cat == "<10") |
           (age_text == "10_to_14_years" & age_cat == "10-19") |
           (age_text == "15_to_19_years" & age_cat == "10-19") |
           (age_text == "20_to_24_years" & age_cat == "20-29") |
           (age_text == "25_to_34_years" & age_cat == "20-29") |
           (age_text == "25_to_34_years" & age_cat == "30-39") |
           (age_text == "35_to_44_years" & age_cat == "30-39") |
           (age_text == "35_to_44_years" & age_cat == "40-49") |
           (age_text == "45_to_54_years" & age_cat == "40-49") |
           (age_text == "45_to_54_years" & age_cat == "50-59") |
           (age_text == "55_to_59_years" & age_cat == "50-59") |
           (age_text == "60_to_64_years" & age_cat == "60-69") |
           (age_text == "65_to_74_years" & age_cat == "60-69") |
           (age_text == "65_to_74_years" & age_cat == "70-79") |
           (age_text == "75_to_84_years" & age_cat == "70-79") |
           (age_text == "75_to_84_years" & age_cat == "80+") |
           (age_text == "85_years_and_over" & age_cat == "80+")) %>%
  mutate (num_cases_by_tract_age = total_tract_cases*case_proportion_by_age)%>%
  group_by(geoid, geo_name, pop_2019, age_text, county_name, total_tract_cases) %>%
  summarize(cases_by_age = round(sum(num_cases_by_tract_age))) %>%
  mutate(total_cases_by_age = ifelse(cases_by_age < pop_2019, cases_by_age, pop_2019))
```

``` r
######################################
#ESSENTIAL WORKER AGE DISTRIBUTION
######################################

#Creating an Essential Worker Age Distribution using tract-defined age categories
age_categories_df <- as.data.frame(age_category_ordering) %>%
  rename(age_text = age_category_ordering)

essential_workers_cleaned <- essential_workers_cleaned %>%
  mutate(pop_in_5_yr_age_category = combined/2)

#NOTE: I'm ignoring age groups 0-14yrs because it would be illegal for children to work as essential workers. There are essential workers in the 10-19yr age category, so when converting to the tract-defined age categories, I am assuming that they all fall into the 15_to_19_years age category. That is why I have to multiply by 2 the values in the 15_to_19_years section. I'm also ignoring 65+ yrs in the essential pop. because otherwise this group is double-counted later on.

essential_excluding_65 <- left_join(age_categories_df, essential_workers_cleaned, by = character()) %>%
  filter(  (age_text == "15_to_19_years" & age_cat == "10-19") |
           (age_text == "20_to_24_years" & age_cat == "20-29") |
           (age_text == "25_to_34_years" & age_cat == "20-29") |
           (age_text == "25_to_34_years" & age_cat == "30-39") |
           (age_text == "35_to_44_years" & age_cat == "30-39") |
           (age_text == "35_to_44_years" & age_cat == "40-49") |
           (age_text == "45_to_54_years" & age_cat == "40-49") |
           (age_text == "45_to_54_years" & age_cat == "50-59") |
           (age_text == "55_to_59_years" & age_cat == "50-59") |
           (age_text == "60_to_64_years" & age_cat == "60-69") ) %>%
  group_by(age_text) %>%
  summarize(essential_pop_by_age = round(sum(pop_in_5_yr_age_category))) %>%
  add_row(age_text = "under_5_years", essential_pop_by_age = 0) %>%
  add_row(age_text = "5_to_9_years", essential_pop_by_age = 0) %>%
  add_row(age_text = "10_to_14_years", essential_pop_by_age = 0) %>%
  add_row(age_text = "65_to_74_years", essential_pop_by_age = 0) %>%
  add_row(age_text = "75_to_84_years", essential_pop_by_age = 0) %>%
  add_row(age_text = "85_years_and_over", essential_pop_by_age = 0) 

essential_excluding_65$essential_pop_by_age[essential_excluding_65$age_text == "15_to_19_years"] <- essential_excluding_65$essential_pop_by_age*2
```

    ## Warning in
    ## essential_excluding_65$essential_pop_by_age[essential_excluding_65$age_text == :
    ## number of items to replace is not a multiple of replacement length

``` r
######################################################################################
#ELIGIBLE POPULATION AGE DISTRIBUTION 
######################################################################################

#Creating an age distribution of the eligible population using tract-defined age categories. Here 
# I am combining the <65 yr essential worker pop and the +65 yr pop

#Getting the +65 population from tract-level data
over_65_pop <- merged_county_tract %>%
  group_by(age_text) %>%
  summarise(total_pop = sum(pop_2019))

over_65_pop$total_pop[!(over_65_pop$age_text %in% c("65_to_74_years", "75_to_84_years", "85_years_and_over"))] <- 0

#Adding the +65 age group and essential worker pop to get combined age distribution using the 
#tract-defined age categories.
eligible_pop_age_dist <-  left_join(over_65_pop, essential_excluding_65, by = "age_text") %>%
  mutate(total_eligible_pop = ceiling(essential_pop_by_age + total_pop)) %>%
  select(age_text, total_eligible_pop)

#Creating a column to get the proportion of each age category is fraction_eligible
eligible_pop_age_dist <-  left_join(eligible_pop_age_dist, pop_by_age, by = "age_text") %>%
  mutate(fraction_eligible = total_eligible_pop/total_pop) %>%
  select(age_text, total_eligible_pop, total_pop, fraction_eligible)

#Using the state-wide eligible population age distribution to map out tract-level eligible pop age distributions in each Census tract. Then I will add all the eligible population from each tract, and scale up or down as necessary

tracts_with_eligible_dist <- left_join(merged_county_tract, eligible_pop_age_dist, by = "age_text") %>%
  select(geoid, geo_name, pop_2019, age_text, total_tract_cases, total_cases_by_age, fraction_eligible) %>%
  mutate(total_eligible_pop = ceiling(pop_2019*fraction_eligible),
         eligible_cases_by_age = round(fraction_eligible*total_cases_by_age))
```

    ## Adding missing grouping variables: `county_name`

``` r
#Scaling Up or Down the eligible pop in each tract depending on how it compares to the original total_eligible_pop
print("Total eligible population after mapping out into each tract:")
```

    ## [1] "Total eligible population after mapping out into each tract:"

``` r
print(sum(tracts_with_eligible_dist$total_eligible_pop))
```

    ## [1] 14327519

``` r
print("Original total eligible population: ")
```

    ## [1] "Original total eligible population: "

``` r
print(sum(eligible_pop_age_dist$total_eligible_pop))
```

    ## [1] 14300692

``` r
#NOTE: The totals are fairly similar, so I won't scale up.

#Adding a column "total pop" of each tract 
tracts_total_pop_by_age <- tract_pop_all_ages %>% 
                                  select(geoid, pop_2019) %>%
                                  rename(total_pop = pop_2019)

tracts_with_eligible_dist <- left_join(tracts_with_eligible_dist, tracts_total_pop_by_age, by = "geoid")
```

``` r
######################################################################################
#MODIFIED ELIGIBLE POPULATION AGE DISTRIBUTION 
######################################################################################

#Creating an age distribution of the eligible population using tract-defined age categories. This modification includes essential workers under 50 years and everyone over 50 years

#Here, I am getting the <50 essential worker population 
essential_excluding_50 <- left_join(age_categories_df, essential_workers_cleaned, by = character()) %>%
  filter(  (age_text == "15_to_19_years" & age_cat == "10-19") |
           (age_text == "20_to_24_years" & age_cat == "20-29") |
           (age_text == "25_to_34_years" & age_cat == "20-29") |
           (age_text == "25_to_34_years" & age_cat == "30-39") |
           (age_text == "35_to_44_years" & age_cat == "30-39") |
           (age_text == "35_to_44_years" & age_cat == "40-49") |
           (age_text == "45_to_54_years" & age_cat == "40-49")) %>%
  group_by(age_text) %>%
  summarize(essential_pop_by_age = round(sum(pop_in_5_yr_age_category))) %>%
  add_row(age_text = "under_5_years", essential_pop_by_age = 0) %>%
  add_row(age_text = "5_to_9_years", essential_pop_by_age = 0) %>%
  add_row(age_text = "10_to_14_years", essential_pop_by_age = 0) %>%
  add_row(age_text = "55_to_59_years", essential_pop_by_age = 0) %>%
  add_row(age_text = "60_to_64_years", essential_pop_by_age = 0) %>%
  add_row(age_text = "65_to_74_years", essential_pop_by_age = 0) %>%
  add_row(age_text = "75_to_84_years", essential_pop_by_age = 0) %>%
  add_row(age_text = "85_years_and_over", essential_pop_by_age = 0) 

essential_excluding_50$essential_pop_by_age[essential_excluding_50$age_text == "15_to_19_years"] <- essential_excluding_50$essential_pop_by_age[essential_excluding_50$age_text == "15_to_19_years"]*2

#NOTE: I'm ignoring age groups 0-14yrs because it would be illegal for children to work as essential workers. There are essential workers in the 10-19yr age category, so when converting to the tract-defined age categories, I am assuming that they all fall into the 15_to_19_years age category. That is why I have to multiply by 2 the values in the 15_to_19_years section. I'm also ignoring 65+ yrs in the essential pop. because otherwise this group is double-counted later on.

#Here I am combining the <50 yr essential worker pop and the +50 yr pop

#Getting the +50 population from tract-level data
over_50_pop <- merged_county_tract %>%
  group_by(age_text) %>%
  summarise(total_pop = sum(pop_2019))

over_50_pop$total_pop[!(over_50_pop$age_text %in% c("45_to_54_years", "55_to_59_years", "60_to_64_years", "65_to_74_years", "75_to_84_years", "85_years_and_over"))] <- 0

#I am dividing the "45_to_54_years" age group in half because I only want to count those who are 50+
over_50_pop$total_pop[over_50_pop$age_text == "45_to_54_years"]  <- over_50_pop$total_pop[over_50_pop$age_text == "45_to_54_years"] * 0.5

#Adding the +50 age group and essential worker pop to get combined age distribution using the 
#tract-defined age categories.
eligible_pop_age_dist_modified <-  left_join(over_50_pop, essential_excluding_50, by = "age_text") %>%
  mutate(total_eligible_pop = ceiling(essential_pop_by_age + total_pop)) %>%
  select(age_text, total_eligible_pop)

#Creating a column to get the proportion of each age category is fraction_eligible
eligible_pop_age_dist_modified <-  left_join(eligible_pop_age_dist_modified, pop_by_age, by = "age_text") %>%
  mutate(fraction_eligible = total_eligible_pop/total_pop) %>%
  select(age_text, total_eligible_pop, total_pop, fraction_eligible)

#Using the state-wide eligible population age distribution to map out tract-level eligible pop age distributions in each Census tract. Then I will add all the eligible population from each tract, and scale up or down as necessary

tracts_with_eligible_dist_modified <- left_join(merged_county_tract, eligible_pop_age_dist_modified, by = "age_text") %>%
  select(geoid, geo_name, pop_2019, age_text, total_tract_cases, total_cases_by_age, fraction_eligible) %>%
  mutate(total_eligible_pop = ceiling(pop_2019*fraction_eligible),
         eligible_cases_by_age = round(fraction_eligible*total_cases_by_age))
```

    ## Adding missing grouping variables: `county_name`

``` r
#Adding a "total pop" column
tracts_with_eligible_dist_modified <- left_join(tracts_with_eligible_dist_modified, tracts_total_pop_by_age, by = "geoid")

#Scaling Up or Down the eligible pop in each tract depending on how it compares to the original total_eligible_pop
print("Total eligible population after mapping out into each tract:")
```

    ## [1] "Total eligible population after mapping out into each tract:"

``` r
print(sum(tracts_with_eligible_dist_modified$total_eligible_pop))
```

    ## [1] 19287869

``` r
print("Original total eligible population: ")
```

    ## [1] "Original total eligible population: "

``` r
print(sum(eligible_pop_age_dist_modified$total_eligible_pop))
```

    ## [1] 19269071

``` r
#NOTE: The totals are fairly similar, so I won't scale up.
```

``` r
#MODIFICATION: Accounting for asymptomatic cases for the immune population

#I will first creating a vector of age-specific probabilities for symptoms given infections, borrowing from "https://github.com/LloydChapman/COVIDVaccineModelling/blob/master/Code/calc_hosp_ICU_death_risk.R", adopted from Davies Nature Medicine paper (Extended Data Fig. 4 p14). The multiplier is just (1/probability of symptoms). Then I will apply another 1.5x multiplier for under-reporting or access to testing. I will convert this vector into tract-defined age categories. Then I will make a column called "total_immune" using these age-specific multipliers of total number of cumulative cases. I will make sure to cap the total_immune tot the total population in the tract

county_defined_age_categories <- county_cleaned %>%
  group_by(age_cat) %>%
  summarise(n()) %>%
  select(age_cat)

age_specific_multipliers <- county_defined_age_categories %>%
  mutate(probability_symptom = c(0.29,0.21,0.27,0.33,0.4,0.49,0.63,0.69,0.69),
         multiplier = (1/probability_symptom)*1.5)

age_specific_multipliers <- merge(age_categories_df, age_specific_multipliers, by = NULL) %>%
  filter((age_text == "under_5_years" & age_cat == "<10") |
           (age_text == "5_to_9_years" & age_cat == "<10") |
           (age_text == "10_to_14_years" & age_cat == "10-19") |
           (age_text == "15_to_19_years" & age_cat == "10-19") |
           (age_text == "20_to_24_years" & age_cat == "20-29") |
           (age_text == "25_to_34_years" & age_cat == "20-29") |
           (age_text == "25_to_34_years" & age_cat == "30-39") |
           (age_text == "35_to_44_years" & age_cat == "30-39") |
           (age_text == "35_to_44_years" & age_cat == "40-49") |
           (age_text == "45_to_54_years" & age_cat == "40-49") |
           (age_text == "45_to_54_years" & age_cat == "50-59") |
           (age_text == "55_to_59_years" & age_cat == "50-59") |
           (age_text == "60_to_64_years" & age_cat == "60-69") |
           (age_text == "65_to_74_years" & age_cat == "60-69") |
           (age_text == "65_to_74_years" & age_cat == "70-79") |
           (age_text == "75_to_84_years" & age_cat == "70-79") |
           (age_text == "75_to_84_years" & age_cat == "80+") |
           (age_text == "85_years_and_over" & age_cat == "80+")) %>%
  group_by(age_text) %>%
  summarise(age_multiplier = mean(multiplier))


tracts_with_eligible_dist <- left_join(tracts_with_eligible_dist, age_specific_multipliers, by = "age_text") %>%
  mutate(total_immune = ifelse(round(total_cases_by_age*age_multiplier) <= pop_2019, round(total_cases_by_age * age_multiplier), pop_2019),
         eligible_immune = round(fraction_eligible*total_immune))


tracts_with_eligible_dist_modified <- left_join(tracts_with_eligible_dist_modified, age_specific_multipliers, by = "age_text") %>%
  mutate(total_immune = ifelse(round(total_cases_by_age*age_multiplier) <= pop_2019, round(total_cases_by_age * age_multiplier), pop_2019),
         eligible_immune = round(fraction_eligible*total_immune))
```

``` r
#Creating A Vector that Stores the Number of Vaccinations. 
#(Each element corresponds with a percent coverage, incremented by 10%)

#Creating the "vector" age distribution of the fraction of population that is eligible for vaccine
fraction_eligible <- eligible_pop_age_dist %>%
  select(age_text, fraction_eligible)

#Creating population + cases count df, grouped by age, from tract-level data
pop_and_cases_by_age <- left_join(merged_county_tract, tracts_total_pop_by_age, by = "geoid")

pop_and_cases_by_age <- pop_and_cases_by_age %>%
  mutate(cases_by_age = round((pop_2019/total_pop) * total_tract_cases)) %>%
  group_by(age_text) %>% 
  summarise(total_pop = sum(pop_2019))

pop_and_cases_by_age <- left_join(pop_and_cases_by_age, fraction_eligible, by = "age_text") 

number_vaccines <- pop_and_cases_by_age %>% 
        select(age_text, total_pop, fraction_eligible) %>%
        mutate("0% coverage" = fraction_eligible*0*total_pop,
               "10% coverage" = fraction_eligible*0.1*total_pop,
               "20% coverage" = fraction_eligible*0.2*total_pop,
               "30% coverage" = fraction_eligible*0.3*total_pop,
               "40% coverage" = fraction_eligible*0.4*total_pop,
               "50% coverage" = fraction_eligible*0.5*total_pop,
               "60% coverage" = fraction_eligible*0.6*total_pop,
               "70% coverage" = fraction_eligible*0.7*total_pop) %>%
        select("0% coverage",
               "10% coverage", 
               "20% coverage", 
               "30% coverage", 
               "40% coverage", 
               "50% coverage", 
               "60% coverage", 
               "70% coverage") %>%
        summarize_if(is.numeric, sum, na.rm=TRUE)

vaccine_increments <- as.numeric(number_vaccines[1,])
```

``` r
#################################################################################
#Modeling the Homogeneous Distribution of Vaccines Across Census Tracts
#################################################################################

#STEPS: 1. Using the tract dataset with the eligible pop distributions, calculate a fraction of the 
#          eligible pop (in each age group in each tract) out of total state eligible population. 
#          Call this
#          fraction "state_fraction_eligible"
#       2. Create a function that iterates through the different quantities of vaccinations (in 10%
#          increments from 0% to 70%)
#       3. Multiply the total number of vaccines * state_fraction_eligible to distribute the vaccines
#          homogeneously across Census tracts. Call this column "vacc_distributed"
#       4. From here, carry out normal incidence and estimated cases over 6 mo calculations

percent_coverage <- c("0%", "10%", "20%", "30%", "40%", "50%", "60%", "70%")

total_state_eligible_pop <- sum(tracts_with_eligible_dist$total_eligible_pop)

homogeneous_approach_df <- tracts_with_eligible_dist%>%
  mutate(state_fraction_eligible = total_eligible_pop/total_state_eligible_pop)

homogeneous_cases_estimate <- data.frame(percent_coverage) %>%
  mutate(homogeneous = 0)

for (i in 1:length(percent_coverage)){
  num_vacc <- vaccine_increments[i]
  
  homogeneous_approach_df <- homogeneous_approach_df %>%
    mutate(vacc_distributed = state_fraction_eligible*num_vacc,
           vacc_received_with_efficacy = vacc_distributed * 0.95,
           incidence_rate_per_mo = total_cases_by_age/pop_2019/11,
           at_risk_pop = pop_2019 - total_immune - vacc_received_with_efficacy,
           estimated_cases_3_mo = round(incidence_rate_per_mo*at_risk_pop*3))
  
  homogeneous_cases_estimate$homogeneous[i] <- sum(homogeneous_approach_df$estimated_cases_3_mo)
}
```

``` r
#Barplot of Estimated COVID Cases in 3 Months with Different Incremented Coverage Using the Homogeneous Vaccine Distribution Approach

 homogeneous_cases_estimate <- homogeneous_cases_estimate %>%
   mutate(estimated_cases_thousands = homogeneous/1000,
          percent_coverage = factor(percent_coverage, levels = percent_coverage, ordered = TRUE)) 
 
ggplot(data = homogeneous_cases_estimate, aes(x= percent_coverage, y = estimated_cases_thousands))+
  geom_bar(stat = "identity", fill = "sienna2") +
  xlab("Percent Vaccine Coverage (Increments of 10%)") +
  ylab("Estimated COVID Cases in 3 MO (in Thousands)") +
  ggtitle("Distribution of Estimated COVID Cases by Incremented Vaccine Coverage") +
  theme(axis.text.x=element_text(angle=0,hjust=0.5))+
  scale_y_continuous(breaks=seq(0,400,50))
```

![](COVIDVaccinationModeling_v4_github_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
########################################################################
#Modeling Targeted Distribution of Vaccines Across Census Tracts
########################################################################

#STEPS: 1. Using the combined eligible age distribution "eligible_pop_age_dist" to find the age 
#          distributions of eligible populations within each Census tract
#       2. Sort the Census tracts in order of descending # absolute COVID cases
#       3. Using the "targeted_approach" function, distribute vaccines in a stepwise fashion 
#          through the Census tracts until the vaccines run out.
#       4. In each tract, calculate the estimated # of COVID cases over the age distribution, making
#          sure to exclude the vaccinated individuals from the "at-risk" population.
#       5. Plot the number of estimated COVID cases by the number of vaccinated individuals


#I created a function called "targeted_approach" which takes in a input of number of vaccines. Before using the function, I first created an empty column in the "targeted_vaccine_df" dataframe called "vacc_distributed", which represents how many vaccines are distributed. This function will populate values for this empty "vacc_distributed" column. How this function works is that it iterates through each row in the dataframe, where each row represents an age category within each Census tract. I've already calculated how many individuals in each age group in each tract are needed to vaccine to reach 70% coverage, and this number is stored in the column "70%_coverage". As I iterate through each row, I set "vacc_distributed" to the number of vaccines needed to reach 70% coverage, and subtract that same number from num_vaccines. The function stops populating once num_vaccines <= 0, meaning that there are no more vaccines left to distribute. 

#With this newly populated dataframe, I calculate efficacy, incidence, at-risk-pop, and estimated cases in 3 mo. This function returns the total estimated COVID cases in 3 mo.

targeted_approach <- function(num_vaccines, sorted_df){
  targeted_vaccine_df <- sorted_df %>%
    mutate(vacc_distributed = 0,
         "70%_coverage" = round(total_eligible_pop*0.7))
  
  vacc_distr_vector <- targeted_vaccine_df$vacc_distributed
  seventy_percent_cov <- targeted_vaccine_df$'70%_coverage'
  
  for (i in 1:nrow(targeted_vaccine_df)){
    if (num_vaccines <= 0) {
      break
    }
    else if (seventy_percent_cov[i] > num_vaccines) {
      vacc_distr_vector[i] <- num_vaccines
      num_vaccines <- 0
    } 
    else if (seventy_percent_cov[i] <= num_vaccines){
      vacc_distr_vector[i] <- seventy_percent_cov[i]
      num_vaccines <- num_vaccines - seventy_percent_cov[i] 
    }
  }
  targeted_vaccine_df$vacc_distributed <- vacc_distr_vector
  
  targeted_vaccine_df <- targeted_vaccine_df %>%
    mutate(vacc_received_with_efficacy = vacc_distributed * 0.95,
           incidence_rate_per_mo = total_cases_by_age/pop_2019/11,
           at_risk_pop = pop_2019 - total_immune - vacc_received_with_efficacy,
           estimated_cases_3_mo = round(incidence_rate_per_mo*at_risk_pop*3))
      
  return(sum(targeted_vaccine_df$estimated_cases_3_mo))
}
```

``` r
#################################################################
#Testing Out the 2 Measures of Risk for Each Eligibility Scenario
#################################################################

#Creating a dataframe that will store estimated COVID cases for both the homogeneous and targeted approaches.
homogeneous_and_targeted_estimated <- homogeneous_cases_estimate %>%
  select(percent_coverage, homogeneous) %>%
  mutate(targeted_absolute = 0,
         targeted_absolute_expanded_age = 0,
         targeted_incidence = 0,
         targeted_incidence_expanded_age = 0)
```

``` r
#Risk by Absolute Total COVID Cases for All-Pop in Unexpanded Eligibility (65+)

total_immune_by_tract <- tracts_with_eligible_dist %>%
  group_by(geoid) %>%
  summarise(total_tract_immune = sum(total_immune))

absolute_df <- left_join(tracts_with_eligible_dist, total_immune_by_tract, by = 'geoid')

absolute_df <- absolute_df[order(-absolute_df$total_tract_immune),]

for (i in 1:length(homogeneous_and_targeted_estimated$percent_coverage)) {
  homogeneous_and_targeted_estimated$targeted_absolute[i] <- targeted_approach(vaccine_increments[i], absolute_df)
}
```

``` r
#Risk by Absolute Total COVID Cases for All-Pop in Expanded Eligibility (50+)

total_immune_by_tract_modified <- tracts_with_eligible_dist_modified %>%
  group_by(geoid) %>%
  summarise(total_tract_immune = sum(total_immune))

absolute_expanded_df <- left_join(tracts_with_eligible_dist_modified, total_immune_by_tract_modified, by = 'geoid')

absolute_expanded_df <- absolute_expanded_df[order(-absolute_expanded_df$total_tract_immune),]

for (i in 1:length(homogeneous_and_targeted_estimated$percent_coverage)) {
  homogeneous_and_targeted_estimated$targeted_absolute_expanded_age[i] <- targeted_approach(vaccine_increments[i], absolute_expanded_df)
}
```

``` r
#Risk by Incidence of COVID in All Pop in Unexpanded Eligibility (65+)
incidence_df <- tracts_with_eligible_dist %>%
  mutate(fraction_age = pop_2019/total_pop,
         incidence = total_immune/pop_2019,
         product = fraction_age*incidence) 
  
dot_product_incidence <- incidence_df %>%
  group_by(geoid) %>%
  summarise(weighted_sum = sum(product))

incidence_df <- left_join(incidence_df, dot_product_incidence, by = 'geoid')

incidence_df <- incidence_df[order(-incidence_df$weighted_sum),]

for (i in 1:length(homogeneous_and_targeted_estimated$percent_coverage)) {
  homogeneous_and_targeted_estimated$targeted_incidence[i] <- targeted_approach(vaccine_increments[i], incidence_df)
}
```

``` r
#Risk by Incidence of COVID in All Pop in Expanded Eligibility (65+)
incidence_expanded_df <- tracts_with_eligible_dist_modified %>%
  mutate(fraction_age = pop_2019/total_pop,
         incidence = total_immune/pop_2019,
         product = fraction_age*incidence) 
  
dot_product_incidence_expanded <- incidence_expanded_df %>%
  group_by(geoid) %>%
  summarise(weighted_sum = sum(product))

incidence_expanded_df <- left_join(incidence_expanded_df, dot_product_incidence_expanded, by = 'geoid')

incidence_expanded_df <- incidence_expanded_df[order(-incidence_expanded_df$weighted_sum),]

for (i in 1:length(homogeneous_and_targeted_estimated$percent_coverage)) {
  homogeneous_and_targeted_estimated$targeted_incidence_expanded_age[i] <- targeted_approach(vaccine_increments[i], incidence_expanded_df)
}
```

``` r
#Plotting Incremented Vaccine Coverage by Estimated Number of Cases for Both the Homogoneous and Target Vaccine Distribution Approaches

ggplot(data = homogeneous_and_targeted_estimated, aes(x = percent_coverage, group = 1))+
  geom_line(aes(y = homogeneous/1000, colour = "Homogeneous Approach")) +
  geom_line(aes(y = targeted_absolute/1000, colour="Targeted Approach (Absolute Cases)")) +
  geom_line(aes(y = targeted_absolute_expanded_age/1000, colour="Targeted Approach (Absolute Cases, Expanded Eligibility)")) +
  geom_line(aes(y = targeted_incidence/1000, colour="Targeted Approach (Incidence)")) +
  geom_line(aes(y = targeted_incidence_expanded_age/1000, colour="Targeted Approach (Incidence, Expanded Eligibility)")) +
  xlab("Vaccine coverage in eligible population (%)") +
  ylab("Total COVID-19 cases over 3 months (in Thousands)") +
  ylim(0, NA) +
  scale_y_continuous(breaks = seq(0, 400, 50), limits = c(0, 400)) +
  scale_colour_manual("", 
                      breaks = c("Homogeneous Approach", 
                                 "Targeted Approach (Absolute Cases)", 
                                 "Targeted Approach (Absolute Cases, Expanded Eligibility)",
                                 "Targeted Approach (Incidence)",
                                  "Targeted Approach (Incidence, Expanded Eligibility)"),
                      values = c("deeppink", "steelblue", "darkmagenta", "darkgoldenrod1", "limegreen"))
```

    ## Scale for 'y' is already present. Adding another scale for 'y', which will
    ## replace the existing scale.

    ## Warning: Removed 1 row(s) containing missing values (geom_path).
    
    ## Warning: Removed 1 row(s) containing missing values (geom_path).
    
    ## Warning: Removed 1 row(s) containing missing values (geom_path).
    
    ## Warning: Removed 1 row(s) containing missing values (geom_path).
    
    ## Warning: Removed 1 row(s) containing missing values (geom_path).

![](COVIDVaccinationModeling_v4_github_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
#SANITY CHECK: Creating an overlaid histogram of the proportion of immune (symptomatic + asymptomatic) and the proportion of symptomatic cases for the census tracts prior to simulation

census_seroprevalance_unexpanded <- tracts_with_eligible_dist %>%
  group_by(geoid) %>%
  summarise(total_tract_immune = sum(total_immune),
            total_tract_pop = mean(total_pop),
            total_cases = mean(total_tract_cases)) %>%
  mutate(immune_prop = total_tract_immune/total_tract_pop,
         symptomatic_prop = total_cases/total_tract_pop)

ggplot(data = census_seroprevalance_unexpanded, aes(fill=group)) +
  geom_histogram(aes(x = immune_prop, fill = "Proportion.Immune"), alpha = 0.5) +
  geom_histogram(aes(x = symptomatic_prop, fill = "Proportion.Symptomatic"), alpha = 0.5) +
  scale_fill_manual(values=c(Proportion.Immune = "blue",
                             Proportion.Symptomatic = "sienna2")) +
  xlab("Proportion") +
  ylab("Number of Census Tracts") +
  ggtitle("Histogram of Baseline COVID-19 Seroprevalance for Census Tracts in CA")+
  theme_minimal() 
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](COVIDVaccinationModeling_v4_github_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
#SANITY CHECK: Total number of cases with no vaccination for the homogeneous approach == targeted approaches

#print(c("From estimated_cases_by_age", sum(estimated_cases_by_age$estimated_cases)))

print(c("From homogeneous approach, no vaccination:", (homogeneous_and_targeted_estimated %>%
        filter(percent_coverage == "0%"))$homogeneous))
```

    ## [1] "From homogeneous approach, no vaccination:"
    ## [2] "400492"

``` r
print(c("From targeted approach, no vaccination:", (homogeneous_and_targeted_estimated %>%
        filter(percent_coverage == "0%"))$targeted_absolute))
```

    ## [1] "From targeted approach, no vaccination:"
    ## [2] "400492"

``` r
#Sanity Check: Total Essential Worker Pop == 20% of Total CA Pop

essential_worker_total_pop <- sum(essential_workers_cleaned$combined)  

print(c("Essential Worker Population: ", essential_worker_total_pop))
```

    ## [1] "Essential Worker Population: " "9515342"

``` r
print(c("20% Total Population from Tract Data: ", 0.2*total_pop))
```

    ## [1] "20% Total Population from Tract Data: "
    ## [2] "7856057.8"

``` r
#SANITY CHECK: The total cases from tract-level dataset == total cases from county-level dataset
total_tract_cases <- sum(tract_pop_all_ages$total_tract_cases)
total_county_cases <- sum(county_cleaned$county_cases)

print(c("Total Cases from Tract-Level Data: ", total_tract_cases))
```

    ## [1] "Total Cases from Tract-Level Data: " "3033843"

``` r
print(c("Total Cases from County-Level Data: ", total_county_cases))
```

    ## [1] "Total Cases from County-Level Data: "
    ## [2] "3251531"

``` r
print(c("Total Cases after distributing eligible pop within tracts (THIS IS WHAT IS USED FOR THE HOMOGENEOUS/TARGETED APPROACHES", sum(tracts_with_eligible_dist$total_cases_by_age)))
```

    ## [1] "Total Cases after distributing eligible pop within tracts (THIS IS WHAT IS USED FOR THE HOMOGENEOUS/TARGETED APPROACHES"
    ## [2] "3028291"

``` r
#SANITY CHECK: The age distributions for the eligible population is the same in the homogeneous and targeted approach

#Homogeneous approach: 
print("Total # of Individuals in Eligible Population using Homogeneous Approach")
```

    ## [1] "Total # of Individuals in Eligible Population using Homogeneous Approach"

``` r
print(sum(eligible_pop_age_dist$total_eligible_pop))
```

    ## [1] 14300692

``` r
#Targeted approach: 
print("Total # of Individuals in Eligible Population using Targeted Approach")
```

    ## [1] "Total # of Individuals in Eligible Population using Targeted Approach"

``` r
print(sum((tracts_with_eligible_dist %>%
        group_by(age_text) %>%
        summarise(targeted_eligible_pop = sum(total_eligible_pop)))$targeted_eligible_pop))
```

    ## [1] 14327519

``` r
#Creating a dataframe comparing the age distributions from both approaches.
homogeneous_eligible <- eligible_pop_age_dist %>% 
  select(age_text, total_eligible_pop)%>% 
  rename(homogeneous_eligible_pop = total_eligible_pop)

targeted_eligible <- tracts_with_eligible_dist %>%
        group_by(age_text) %>%
        summarise(targeted_eligible_pop = sum(total_eligible_pop))

print(left_join(homogeneous_eligible, targeted_eligible, by = "age_text") %>%
        mutate(difference_between_targeted_hom = targeted_eligible_pop - homogeneous_eligible_pop))
```

    ## # A tibble: 13 x 4
    ##    age_text      homogeneous_eligibl… targeted_eligible… difference_between_tar…
    ##    <chr>                        <dbl>              <dbl>                   <dbl>
    ##  1 10_to_14_yea…                    0                  0                       0
    ##  2 15_to_19_yea…               398116             402024                    3908
    ##  3 20_to_24_yea…              1042219            1045982                    3763
    ##  4 25_to_34_yea…              2081710            2085646                    3936
    ##  5 35_to_44_yea…              2021674            2025429                    3755
    ##  6 45_to_54_yea…              1879674            1883391                    3717
    ##  7 5_to_9_years                     0                  0                       0
    ##  8 55_to_59_yea…               897492             901330                    3838
    ##  9 60_to_64_yea…               495667             499577                    3910
    ## 10 65_to_74_yea…              3171213            3171213                       0
    ## 11 75_to_84_yea…              1599696            1599696                       0
    ## 12 85_years_and…               713231             713231                       0
    ## 13 under_5_years                    0                  0                       0

``` r
#NOTE: There is a difference in the total priority populations by around 13 individuals. And the age distributions show little difference.
```

``` r
#SANITY CHECK: Is same number of vaccines given out in both strategies?

#NOTE: YES! Using the vaccination totals from the homogeneous approach for the targeted approach. 
# These numbers are stored in the variable "homogeneous_number_vaccines"
```

``` r
#SANITY CHECK: Are Census tracts with highest # of cases also low incidence (bc large pop)?

tract_pop_with_incidence <- tract_pop_all_ages %>%
  mutate(incidence = total_tract_cases/pop_2019)
#  filter(incidence > 1)

ggplot(tract_pop_with_incidence, aes(x=total_tract_cases, y=incidence)) + 
  geom_point() +
  xlab("Total COVID Cases") +
  ylab("Incidence") +
  ggtitle("Mapping Total COVID Cases and Incidence for each Census Tract")
```

![](COVIDVaccinationModeling_v4_github_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

``` r
#NOTE: It appears that tracts with highest case counts are in fact low-incidence.
```

``` r
#SANITY CHECK: Calculating the proportion of CA pop that is essential workers after subtracting +65 population (I'm trying to confirm that this will still be around 20%)

essential_worker_pop_excluding_65 <- sum(essential_excluding_65$essential_pop_by_age)

print(c("Proportion of essential workers excluding +65 in CA population: ", essential_worker_pop_excluding_65/total_pop))
```

    ## [1] "Proportion of essential workers excluding +65 in CA population: "
    ## [2] "0.224452320093673"
