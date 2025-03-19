#------------------------------------------- CMS-SVI (2020-2022) --------------------------------------------------------------------------------


#purpose: 

#1.To have a CMS dataset with SVI scores & visualize PA counties with Antibiotic prescribing patterns & SVI scores
#2.Merging : CMS- by provider (2020-2022) (zipcode) --- PA Zipcode file (zipcode, county)--- SVI- CDC/ATSDR 2020-2022 (county, SVI scores for each county)
#3.classify counties into SVI categories (low, high, low-medium, medium-high)
#4.Calculate AB prescribing rates & analyze rates  among counties & SVI categories

#detach("package:MASS", unload = T)

# load libraries

library(lubridate)
library(scales)
library(summarytools)
library(expss)
library(tidyverse)
library(readxl)
library(readr)
library(janitor)
library(dplyr)
library(stringr)
library(flextable)
library(haven)
library(tidyverse)
library(janitor)
library(stringr)
library(english)
library(knitr)
library(lubridate)
library(DT)
library(haven)
library(htmlwidgets)
library(gtools)
library(writexl)
library(zipcodeR)
library(RColorBrewer) 


# Read files

CMS_2020 <- read.csv("C:/Users/c-qjahan/Downloads/Antimicrobial Stewardship CMS analysis/REPORTS/stewardship/SVI analysis/Medicare_Part_D_Prescribers_by_Provider_2020.csv")
CMS_2020 <- rename (CMS_2020, zipcode = Prscrbr_zip5)

zipcode <- search_state('PA')

SVI_2020<- read.csv("C:/Users/c-qjahan/Downloads/Antimicrobial Stewardship CMS analysis/REPORTS/stewardship/SVI analysis/svi_interactive_map 2020.csv") 


# Data cleaning & validation 

# select(COUNTY_NAME,RPL_THEMES)
# range(SVI_2020$RPL_THEMES) # 0-1
# colnames(SVI_2020)
# distinct(zipcode)
# missing_zipcodes <- anti_join(CMS_2020,zipcode, by = "zipcode")


# Main analysis

#1. Merging CMS 2020 with PA zipcode file to add county column

CMS_with_county_2020 <- merge(CMS_2020, zipcode, by = "zipcode") %>%
  rename(COUNTY_NAME = county) %>% 
  mutate(COUNTY_NAME = case_when(COUNTY_NAME== "McKean County" ~ "Mckean County", T ~ COUNTY_NAME))

# Mckean County -- SVI
# McKean County -- Zipcode
# select(PRSCRBR_NPI,Prscrbr_Type, zipcode,COUNTY_NAME, Prscrbr_St1, Antbtc_Tot_Clms, Tot_Benes)
# distinct(county, .keep_all = T) # n= 67 counties

#2. Merging CMS_county 2020 with SVI 2020 to add SVI data to CMS

CMS_SVI_2020_rev <- left_join(CMS_with_county_2020,SVI_2020, by = "COUNTY_NAME")

# Validation  -- ignore running below code ----------------------------------------------

# CMS_SVI_2020_rev <-  CMS_SVI_2020_rev %>% #(n= 60207)
#   select(PRSCRBR_NPI,Prscrbr_Type, zipcode, Prscrbr_St1,COUNTY_NAME,lat,lng,Antbtc_Tot_Clms,Tot_Benes,RPL_THEME1,RPL_THEME2,RPL_THEME3,
#          RPL_THEME4,RPL_THEMES) 
# 
# 
# colnames(CMS_SVI_2020_rev)
# range(CMS_SVI_2020_rev$RPL_THEMES)
# 
# # Exporting as XLSX
# 
# write_xlsx(CMS_SVI_2020_rev,"N:/CMS 2020-2022/CMS_SVI_2020.xlsx")

#----------------------------------------------------------------------------------

# Calculating AB presctiption rate
# Parent dataset

CMS_SVI_2020_rev <- CMS_SVI_2020_rev%>%
  mutate(Tot_Benes = case_when(Tot_Benes == ""~ NA_character_, T~Tot_Benes), #blanks refer to suppressed values <11 and converted to missing and later to numeric 5
         Tot_Benes = as.numeric(str_remove_all(Tot_Benes, ",")),
         Tot_Benes = case_when(is.na(Tot_Benes)~5, T~Tot_Benes),
         Antbtc_Tot_Clms = case_when(Antbtc_Tot_Clms == ""~ NA_character_, T~Antbtc_Tot_Clms),
         Antbtc_Tot_Clms = as.numeric(str_remove_all(Antbtc_Tot_Clms, ",")),
         Antbtc_Tot_Clms = case_when(is.na(Antbtc_Tot_Clms) ~ 1,T~Antbtc_Tot_Clms)) %>% 
  filter(!(Antbtc_Tot_Clms %in% c(0,1))) %>% # excluded values < 11 which are assumed to be suppressed values
  filter(!(Tot_Benes %in% 5)) %>%
  mutate(AB_Prescp_rate = (Antbtc_Tot_Clms )/(Tot_Benes) * 1000) %>% 
  dplyr::select(PRSCRBR_NPI,Prscrbr_Type, zipcode, Prscrbr_St1,COUNTY_NAME,Antbtc_Tot_Clms,Tot_Benes,AB_Prescp_rate,RPL_THEME1,RPL_THEME2,RPL_THEME3,
                RPL_THEME4,RPL_THEMES) 

# collapsing specialties
CMS_SVI_2020_rev <- CMS_SVI_2020_rev %>% 
  mutate(Prscrbr_Type_new = case_when(Prscrbr_Type == "Dentist"~ "Dentistry",
                                      
                                      Prscrbr_Type %in% c("Colon & Rectal Surgery",
                                                          "Colorectal Surgery (Proctology)") ~ "Colorectal Surgery",
                                      Prscrbr_Type %in% c("Maxillofacial Surgery",
                                                          "Oral & Maxillofacial Surgery",
                                                          "Oral Surgery (Dentist only)") ~ "Oral and Maxillofacial Surgery",
                                      Prscrbr_Type %in% c("Neurological Surgery",
                                                          "Neurosurgery") ~  "Neurosurgery",
                                      Prscrbr_Type %in% c("Thoracic Surgery",
                                                          "Thoracic Surgery (Cardiothoracic Vascular Surgery)") ~ "Thoracic Surgery",
                                      Prscrbr_Type %in% c("Orthopaedic Surgery",
                                                          "Orthopedic Surgery") ~ "Orthopaedic Surgery",
                                      Prscrbr_Type %in% c("Pediatric Medicine",
                                                          "Pediatrics")~ "Pediatrics",
                                      Prscrbr_Type %in% c("Physical Medicine & Rehabilitation",
                                                          "Physical Medicine and Rehabilitation",
                                                          "Rehabilitation Practitioner")~ "Physical Medicine & Rehabilitation",
                                      Prscrbr_Type %in% c("Plastic and Reconstructive Surgery",
                                                          "Plastic Surgery") ~ "Plastic and Reconstructive Surgery",
                                      Prscrbr_Type %in% c("Neuropsychiatry",
                                                          "Psychiatry & Neurology") ~ "Neuropsychiatry",
                                      Prscrbr_Type %in% c("Medical Genetics and Genomics",
                                                          "Medical Genetics, Ph.D. Medical Genetics") ~ "Medical Genetics and Genomics",
                                      Prscrbr_Type %in% c("Family Medicine",
                                                          "Family Practice")~ "Family Medicine",
                                      Prscrbr_Type %in% c("Radiology",
                                                          "Diagnostic Radiology",
                                                          "Interventional Radiology") ~ "Radiology",
                                      Prscrbr_Type %in% c("Gynecological Oncology",
                                                          "Hematology-Oncology",
                                                          "Medical Oncology",
                                                          "Radiation Oncology")~ "Oncology",
                                      Prscrbr_Type %in% c("Pain Management",
                                                          " Pain Medicine")~ "Pain Medicine",
                                      Prscrbr_Type %in% c("Adult Congenital Heart Disease",
                                                          "Advanced Heart Failure and Transplant Cardiology",
                                                          "Cardiology",
                                                          "Clinical Cardiac Electrophysiology",
                                                          "Interventional Cardiology")~ "Cardiology", T~ Prscrbr_Type))


tot_specialties_2__SVI_2020 <- CMS_SVI_2020_rev %>% 
  summarize(Specialties = n_distinct(Prscrbr_Type_new)) #(n=67) # after collapsing specialites and without excluding nurse practioners and Physician Assistant

specialt_name_SVI_2020 <- CMS_SVI_2020_rev %>% #(n=23952)
  select(PRSCRBR_NPI,Prscrbr_Type_new,Prscrbr_Type, zipcode, Prscrbr_St1,COUNTY_NAME,Antbtc_Tot_Clms,Tot_Benes,AB_Prescp_rate,RPL_THEME1,RPL_THEME2,RPL_THEME3,
         RPL_THEME4,RPL_THEMES) %>% 
  filter(!(Prscrbr_Type_new %in% c("Nurse Practitioner","Physician Assistant")))


# Creating SV categories

# CMS_SVI_2020_rev_2 <- specialt_name_SVI_2020 %>%
#   mutate(Vul_Cat = case_when(
#     RPL_THEMES >= 0 & RPL_THEMES <= 0.2500 ~ "low",
#     RPL_THEMES > 0.2500 & RPL_THEMES <= 0.5000 ~ "low-medium",
#     RPL_THEMES > 0.5000 & RPL_THEMES <= 0.7500 ~ "medium-high",
#     RPL_THEMES > 0.7500 & RPL_THEMES <= 1.0 ~ "high",
#     TRUE ~ NA_character_))
# 
# # 2 levels only 
# CMS_SVI_2020_rev_3 <- specialt_name_SVI_2020 %>%
#   mutate(Vul_Cat = case_when(
#     RPL_THEMES >= 0 & RPL_THEMES <= 0.5500 ~ "low",
#     RPL_THEMES > 0.5500 & RPL_THEMES <= 1.0 ~"high",
#     TRUE ~ NA_character_))



# 3 levels 
CMS_SVI_2020_rev_4 <- specialt_name_SVI_2020 %>%
  mutate(Vul_Cat = case_when(
    RPL_THEMES <= 0.33 ~ "Low",
    RPL_THEMES > 0.33 & RPL_THEMES <= 0.67 ~ "Moderate",
    RPL_THEMES > 0.67 ~ "High",
    TRUE ~ NA_character_))

# # check
# moderate <- CMS_SVI_2020_rev_4 %>% 
#   filter(Vul_Cat=="moderate")
# range(moderate$RPL_THEMES)


# Calculating Mean Antibiotic prescribing rate for each SV category

mean_prescribing_rate_by_vul_cat_rev <- CMS_SVI_2020_rev_4 %>%
  group_by(Vul_Cat) %>%
  summarise(mean_AB_Prescp_rate = mean(AB_Prescp_rate, na.rm = TRUE))


# County by SV categories

county_vul_category_rev <- CMS_SVI_2020_rev_4 %>%
  group_by(COUNTY_NAME) %>%
  summarise(Vul_Cat = first(Vul_Cat)) 

# Calculate the mean prescribing rate by county and get vulnerability category
prescribing_rate_by_county_rev <- CMS_SVI_2020_rev_4 %>%
  group_by(COUNTY_NAME) %>%
  summarise(mean_AB_Prescp_rate = mean(AB_Prescp_rate, na.rm = TRUE),
            num_prescribers = n_distinct(PRSCRBR_NPI),
            Vul_Cat = first(Vul_Cat))

#Mapping
library(sf)
library(tigris)

pa_counties <- counties(state = "PA", year = 2020, class = "sf") %>% 
  mutate(NAMELSAD = case_when(NAMELSAD == "McKean County" ~ "Mckean County", T ~ NAMELSAD))


#Merge the prescribing rate data with the Pennsylvania county 

pa_county_data <- pa_counties %>%
  left_join(prescribing_rate_by_county_rev, by = c("NAMELSAD" = "COUNTY_NAME")) %>% 
  mutate(NAME = str_replace(NAMELSAD, "County", ""))


#Color palette for the mean prescribing rate
palette <- colorRampPalette(brewer.pal(9, "Blues"))(100)   #not color blind

# palette <- c(brewer.pal(n = 9, name = "Blues"))
# 
# palette <- c("darkgrey", colorRampPalette(brewer.pal(9, "Blues"))(100))
# 

#Symbols for vulnerability categories
vul_cat_symbols <- c("High" = 24,        
                     "Moderate" = 22,  
                     "Low" = 21)          
# coordinates
pa_county_data <- pa_county_data %>%
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(lon = map_dbl(centroid, ~st_coordinates(.x)[1]),
         lat = map_dbl(centroid, ~st_coordinates(.x)[2]))

symbol_offset <- 0.075
pa_county_data <- pa_county_data %>%
  mutate(lon_offset = lon + symbol_offset,
         lat_offset = lat - symbol_offset) 



pa_county_data <- pa_county_data %>%
  mutate(
    Vul_Cat = factor(Vul_Cat, levels = c("High", "Moderate", "Low")))

pa_county_data <- pa_county_data %>% 
  mutate(Vul_Cat = case_when(is.na(Vul_Cat)~ "",
                             T ~Vul_Cat))


# shape_color_palette <- brewer.pal(n = 4, name = "Set1")
#library(RColorBrewer)

library(viridis)

# CMS County- SVI Map 2020
svi_map_20 <- 
  ggplot(data = pa_county_data) +
  geom_sf(aes(fill = mean_AB_Prescp_rate)) + 
  geom_sf_text(aes(label = NAME), size = 3, fontface = "bold",color = "black") +
  geom_point(aes(x = lon_offset, y = lat_offset, shape = Vul_Cat), size = 2, color = "red") + 
  scale_color_viridis(discrete = TRUE, option = "D", name = "Vulnerability Category")  +
  scale_fill_gradientn(colors = palette,  name = "Prescribing Rate",
                       guide = guide_colorbar(barwidth = 0.5, barheight = 10)) +  
  scale_shape_manual(values = vul_cat_symbols, name = "Vulnerability Category") +
  labs(title = "")+
  theme_minimal() +
  theme(legend.position = "right",
        legend.text = element_text(size = 8),  
        legend.title = element_text(size = 9)) 


ggsave("2020 SVI map rev.png",svi_map_20,width =10, height = 8, dpi = 300)
