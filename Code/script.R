# Weighted ranking of counties by diversity index and # of jobs for:
# IndustryClassification:
#    Professional, scientific, and technical services
#    Educational services
#    Health care and social assistance
#    Information

# Reading in diversity data
# Data Dictionary:
# https://docs.google.com/spreadsheets/d/1OiTD9Xt0Fmjd79P3Vi2szT67WI_x9MsY/edit?usp=sharing&ouid=105901885807046626254&rtpof=true&sd=true

# Job data comes from:
# https://apps.bea.gov/regional/downloadzip.cfm

library(readxl)
library(dplyr)
library(fastDummies)
library(RankAggreg)

diversity_data = read_xlsx('data-indicator-147-display-2-018443d2270f9cd90f83c3a586056833.xlsx') %>% filter(year == max(year))

diversity_data = diversity_data %>% 
  mutate(GEO_NAME = toupper(trimws(GEO_NAME)),
         ent_score_acsglyt = as.numeric(ent_score_acsglyt)) %>% 
  select(GEO_NAME, ent_score_acsglyt) %>% 
  rename("diversity_score" = ent_score_acsglyt) %>% 
  janitor::clean_names()

employment = read.csv("CAEMP25N__ALL_AREAS_2001_2020.csv")

education = employment %>% 
  mutate(Description = trimws(Description),
         GeoName = toupper(trimws(GeoName))) %>% 
  filter(GeoName %in% diversity_data$geo_name) %>% 
  filter(Description %in% c("Educational services",
                            "Health care and social assistance",
                            "Information",
                            "Professional, scientific, and technical services",
                            "Management of companies and enterprises")) %>% 
  select(GeoFIPS:Description, X2020) %>% 
  fastDummies::dummy_cols(select_columns = 'Description') %>% 
  janitor::clean_names() %>% 
  filter(description_educational_services == 1) %>% 
  filter(x2020 != "(D)") %>% 
  select(-contains("description_")) %>% 
  inner_join(., diversity_data) %>% 
  select(geo_name, x2020, diversity_score) %>% 
  mutate(x2020 = as.numeric(x2020),
         diversity_score = as.numeric(diversity_score))

health = employment %>% 
  mutate(Description = trimws(Description),
         GeoName = toupper(trimws(GeoName))) %>% 
  filter(GeoName %in% diversity_data$geo_name) %>% 
  filter(Description %in% c("Educational services",
                            "Health care and social assistance",
                            "Information",
                            "Professional, scientific, and technical services",
                            "Management of companies and enterprises")) %>% 
  select(GeoFIPS:Description, X2020) %>% 
  fastDummies::dummy_cols(select_columns = 'Description') %>% 
  janitor::clean_names() %>% 
  filter(description_health_care_and_social_assistance == 1) %>% 
  filter(x2020 != "(D)") %>% 
  select(-contains("description_")) %>% 
  inner_join(., diversity_data) %>% 
  select(geo_name, x2020, diversity_score) %>% 
  mutate(x2020 = as.numeric(x2020),
         diversity_score = as.numeric(diversity_score))

mgmt = employment %>% 
  mutate(Description = trimws(Description),
         GeoName = toupper(trimws(GeoName))) %>% 
  filter(GeoName %in% diversity_data$geo_name) %>% 
  filter(Description %in% c("Educational services",
                            "Health care and social assistance",
                            "Information",
                            "Professional, scientific, and technical services",
                            "Management of companies and enterprises")) %>% 
  select(GeoFIPS:Description, X2020) %>% 
  fastDummies::dummy_cols(select_columns = 'Description') %>% 
  janitor::clean_names() %>% 
  filter(description_management_of_companies_and_enterprises == 1) %>% 
  filter(x2020 != "(D)") %>% 
  select(-contains("description_")) %>% 
  inner_join(., diversity_data) %>% 
  select(geo_name, x2020, diversity_score) %>% 
  mutate(x2020 = as.numeric(x2020),
         diversity_score = as.numeric(diversity_score))

info = employment %>% 
  mutate(Description = trimws(Description),
         GeoName = toupper(trimws(GeoName))) %>% 
  filter(GeoName %in% diversity_data$geo_name) %>% 
  filter(Description %in% c("Educational services",
                            "Health care and social assistance",
                            "Information",
                            "Professional, scientific, and technical services",
                            "Management of companies and enterprises")) %>% 
  select(GeoFIPS:Description, X2020) %>% 
  fastDummies::dummy_cols(select_columns = 'Description') %>% 
  janitor::clean_names() %>% 
  filter(description_information == 1) %>% 
  filter(x2020 != "(D)") %>% 
  select(-contains("description_")) %>% 
  inner_join(., diversity_data) %>% 
  select(geo_name, x2020, diversity_score) %>% 
  mutate(x2020 = as.numeric(x2020),
         diversity_score = as.numeric(diversity_score))


# Top Employment Regions --------------------------------------------------

out = head(select(education,-diversity_score) %>% arrange(-x2020),20)
out = out %>% rename("County" = geo_name, "Number of Openings" = x2020)

out = head(select(health,-diversity_score) %>% arrange(-x2020),20)
out = out %>% rename("County" = geo_name, "Number of Openings" = x2020)

out = head(select(mgmt,-diversity_score) %>% arrange(-x2020),20)
out = out %>% rename("County" = geo_name, "Number of Openings" = x2020)

out = head(select(info,-diversity_score) %>% arrange(-x2020),20)
out = out %>% rename("County" = geo_name, "Number of Openings" = x2020)

# Top Diversity Regions ---------------------------------------------------

out = diversity_data %>% arrange(-diversity_score)
out = out %>% rename("County" = geo_name, "Diversity Score" = diversity_score)

# Computing Average Ranks -------------------------------------------------

education %>% 
  arrange(-x2020) %>% 
  mutate(emp_rank = 1:nrow(.)) %>% 
  arrange(-diversity_score) %>% 
  mutate(diversity_rank = 1:nrow(.)) %>% 
  mutate(avg_rank = rowMeans(select(., emp_rank, diversity_rank))) %>% 
  arrange(avg_rank) %>% 
  mutate(avg_rank = 1:nrow(.)) %>% 
  select(geo_name, avg_rank, emp_rank, diversity_rank) %>% 
  rename("County" = geo_name, "Average Ranking" = avg_rank, "Employment Ranking" = emp_rank, "Diversity Ranking" = diversity_rank) %>% 
  head(20)

health %>% 
  arrange(-x2020) %>% 
  mutate(emp_rank = 1:nrow(.)) %>% 
  arrange(-diversity_score) %>% 
  mutate(diversity_rank = 1:nrow(.)) %>% 
  mutate(avg_rank = rowMeans(select(., emp_rank, diversity_rank))) %>% 
  arrange(avg_rank) %>% 
  mutate(avg_rank = 1:nrow(.)) %>% 
  select(geo_name, avg_rank, emp_rank, diversity_rank) %>% 
  rename("County" = geo_name, "Average Ranking" = avg_rank, "Employment Ranking" = emp_rank, "Diversity Ranking" = diversity_rank) %>% 
  head(20)

mgmt %>% 
  arrange(-x2020) %>% 
  mutate(emp_rank = 1:nrow(.)) %>% 
  arrange(-diversity_score) %>% 
  mutate(diversity_rank = 1:nrow(.)) %>% 
  mutate(avg_rank = rowMeans(select(., emp_rank, diversity_rank))) %>% 
  arrange(avg_rank) %>% 
  mutate(avg_rank = 1:nrow(.)) %>% 
  select(geo_name, avg_rank, emp_rank, diversity_rank) %>% 
  rename("County" = geo_name, "Average Ranking" = avg_rank, "Employment Ranking" = emp_rank, "Diversity Ranking" = diversity_rank) %>% 
  head(20)

info %>% 
  arrange(-x2020) %>% 
  mutate(emp_rank = 1:nrow(.)) %>% 
  arrange(-diversity_score) %>% 
  mutate(diversity_rank = 1:nrow(.)) %>% 
  mutate(avg_rank = rowMeans(select(., emp_rank, diversity_rank))) %>% 
  arrange(avg_rank) %>% 
  mutate(avg_rank = 1:nrow(.)) %>% 
  select(geo_name, avg_rank, emp_rank, diversity_rank) %>% 
  rename("County" = geo_name, "Average Ranking" = avg_rank, "Employment Ranking" = emp_rank, "Diversity Ranking" = diversity_rank) %>% 
  head(20)

# Experimental Portion for Rank Aggregation below -------------------------

x = rbind(education %>% 
        arrange(-x2020) %>% 
        .$geo_name,
      education %>% 
        arrange(-diversity_score) %>% 
        .$geo_name)

(CESnoweights <- RankAggreg(x, ncol(x), 
                            method="CE", 
                            distance="Spearman", 
                            N=100, 
                            convIn=5, 
                            rho=.1))

