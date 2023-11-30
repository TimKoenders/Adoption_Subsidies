######### SET-UP #######################
#### Set-up / Loading packages --------------------------------------------
rm(list=ls())

pacman::p_load(
  tidyverse,
  urca,
  readxl,
  ggplot2,
  lubridate,
  vars,
  dplyr,
  MCMCpack,
  magic,
  coda,
  readr,
  tidyr,
  haven,
  MASS
)

#### Data loading and pre-processing and merging------------------------------------------------------------

## World Economic Outlook database: April 2022 from IMF (https://www.imf.org/en/Publications/WEO/weo-database/2022/April/download-entire-database)
WEOApr2022all_raw <- read_delim("data/Raw/WEOApr2022all.csv", 
                                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)
# Pivot the data
reshaping_1<- WEOApr2022all_raw %>%
  pivot_longer(
    cols = starts_with(c("19", "20")),  # Assuming your year columns start with "19" or "20"
    names_to = "Year",
    values_to = "Value"
  ) %>%
  pivot_wider(
    id_cols = c("WEO Country Code", "ISO", "Country", "Year","Subject Notes", "Year", "Units" ),
    names_from = "WEO Subject Code",
    values_from = "Value"
  )
reshaping_2 <- reshaping_1[, !(colnames(reshaping_1) %in% c("Subject Notes", "Units"))]
reshaping_3 <- reshaping_2 %>%
  group_by(`WEO Country Code`, ISO, Country, Year) %>%
  fill(everything(), .direction = "downup")
# Identify and remove duplicates
reshaping_4 <- reshaping_3 %>%
  distinct()
df_macro <- reshaping_4

## Grants and Subsidies (https://www.iea.org/policie)
Subsidies_Policies_Industry_2 <- read_excel("data/Subsidies_Policies_Industry_2.xlsx")

df_policies <- Subsidies_Policies_Industry_2 %>%
  group_by(Year, Country) %>%
  summarise(Number_of_Policies = n())%>%
  mutate(Number_of_Policies = ifelse(is.na(Number_of_Policies), 0, Number_of_Policies))

df_macro_policies <- merge(df_macro, df_policies, by = c("Year", "Country"), all.x = TRUE, all.y = TRUE) %>%
  arrange(Country, Year)


## Political and election data (https://manifesto-project.wzb.eu/datasets/MPDS2023a)
df_political <- read_dta("data/Raw/MPDataset_MPDS2023a_stata14.dta")
df_political_2000_2022 <- df_political %>%
  filter(Year >= 2000, Year < 2023)

df_political_2000_2022 <- df_political_2000_2022 %>%
  mutate(
    shareofECO = NA_real_,
    shareofLEF = NA_real_,
    shareofSOC = NA_real_,
    shareofLIB = NA_real_,
    shareofCHR = NA_real_,
    shareofCON = NA_real_,
    shareofNAT = NA_real_,
    shareofAGR = NA_real_,
    shareofETH = NA_real_,
    shareofSIP = NA_real_,
    shareofDIV = NA_real_,
    shareofMI = NA_real_
  )

df_political_2000_2022  <- df_political_2000_2022  %>%
  mutate(
    shareofECO = ifelse(parfam == 10, pervote, shareofECO),
    shareofLEF = ifelse(parfam == 20, pervote, shareofLEF),
    shareofSOC = ifelse(parfam == 30, pervote, shareofSOC),
    shareofLIB = ifelse(parfam == 40, pervote, shareofLIB),
    shareofCHR = ifelse(parfam == 50, pervote, shareofCHR),
    shareofCON = ifelse(parfam == 60, pervote, shareofCON),
    shareofNAT = ifelse(parfam == 70, pervote, shareofNAT),
    shareofAGR = ifelse(parfam == 80, pervote, shareofAGR),
    shareofETH = ifelse(parfam == 90, pervote, shareofETH),
    shareofSIP = ifelse(parfam == 95, pervote, shareofSIP),
    shareofDIV = ifelse(parfam == 98, pervote, shareofDIV),
    shareofMI = ifelse(parfam == 999, pervote, shareofMI)
  )

## Creating general measure per party about likely progresiveness in terms of market interventions to green economy

# Creating the progressiveness variable
df_political_2000_2022$party_progressiveness <- -1 * df_political_2000_2022$per401 +
  -1 * df_political_2000_2022$per702 +
  -1 * df_political_2000_2022$per4012 +
  1 * df_political_2000_2022$per402 +
  1 * df_political_2000_2022$per403 +
  1 * df_political_2000_2022$per411 +
  1 * df_political_2000_2022$per412 +
  1 * df_political_2000_2022$per416 +
  1 * df_political_2000_2022$per501 +
  1 * df_political_2000_2022$per701 +
  1 * df_political_2000_2022$per416_2

df_political_2000_2022 <- df_political_2000_2022 %>%
  group_by(Country, Year) %>%
  reframe(
    shareofECO = coalesce(shareofECO),
    shareofLEF = coalesce(shareofLEF),
    shareofSOC = coalesce(shareofSOC),
    shareofLIB = coalesce(shareofLIB),
    shareofCHR = coalesce(shareofCHR),
    shareofCON = coalesce(shareofCON),
    shareofNAT = coalesce(shareofNAT),
    shareofAGR = coalesce(shareofAGR),
    shareofETH = coalesce(shareofETH),
    shareofSIP = coalesce(shareofSIP),
    shareofDIV = coalesce(shareofDIV),
    shareofMI = coalesce(shareofMI),
    party_progressiveness = coalesce(party_progressiveness)
  )


# Divide each value in the specified columns by 100
columns_to_divide <- 3:14
df_political_2000_2022[, columns_to_divide] <- df_political_2000_2022[, columns_to_divide] / 100

# Create a new variable by multiplying and summing the specified columns
df_political_2000_2022 <- df_political_2000_2022 %>%
  group_by(Country, Year) %>%
  mutate(progress_index = sum(na_if(party_progressiveness, NA) * na_if(c_across(shareofECO:shareofMI), NA), na.rm = TRUE))
df_political_2000_2022 <- df_political_2000_2022 %>%
  mutate(progress_index = ifelse(progress_index == 0, NA, progress_index))

df_political_2000_2022 <- df_political_2000_2022 %>%
  group_by(Country, Year) %>%
  summarise(across(everything(), ~first(.[!is.na(.)])), .groups = 'drop')


df_total <- merge(df_macro_policies, df_political_2000_2022, by = c("Year", "Country"), all.x = TRUE, all.y = TRUE) %>%
  arrange(Country, Year)
write.csv(df_total, "df_total.csv", row.names = FALSE)


#### Final data processing --------------------------------------------------------
rm(list=ls())
df_total <- read_csv("df_total.csv")
# Create subset observations for year between 2000 and 2022, remove duplicates, and sort
df_total_2000_2022 <- df_total %>%
  filter(Year >= 2000, Year < 2023) %>%
  distinct(Country, Year, .keep_all = TRUE) %>%
  arrange(Country, Year)
# Replace missing values in Number_of_Policies with 0
df_total_2000_2022 <- df_total_2000_2022 %>%
  mutate(Number_of_Policies = ifelse(is.na(Number_of_Policies), 0, Number_of_Policies))
df_total_2000_2022[df_total_2000_2022 == "n/a"] <- NA
df_total_2000_2022 <- subset(df_total_2000_2022, !is.na(Country))

# manually imputed data with STATA (Only use for illustrations!!)
Data_with_imputed_data <- read_dta("data/Data_with_imputed_data_2.dta")
Data_with_imputed_data$progress_index <- as.numeric(as.character(Data_with_imputed_data$progress_index))
Data_with_imputed_data$number_of_policies <- factor(Data_with_imputed_data$number_of_policies, ordered = TRUE)
df_total_2000_2022_imp <- read_dta("Data_with_imputed_data.dta")
write.csv(df_total_2000_2022_imp, "df_total_2000_2022_imp.csv", row.names = FALSE)



#### Analysis ----------------------------------------------------------------
# Set-up data
rm(list=ls())
df_total_2000_2022_imp <- read_csv("df_total_2000_2022_imp.csv")
df_total_2000_2022_imp[df_total_2000_2022_imp == "NA"] <- NA
df_total_2000_2022_imp$progress_index <- as.numeric(as.character(df_total_2000_2022_imp$progress_index))
df_total_2000_2022_imp$number_of_policies <- factor(df_total_2000_2022_imp$number_of_policies, ordered = TRUE)
df_total_2000_2022_imp$ngdpd <- as.numeric(as.character(df_total_2000_2022_imp$ngdpd))
df_total_2000_2022_imp$ngdpd_log <- log(df_total_2000_2022_imp$ngdpd + 1)

# Ordered probit model
model_polr <- polr(number_of_policies ~ progress_index, data = df_total_2000_2022_imp, method = "probit")
summary(model_polr) 

# Calculation of p-values
coef_progress_index <- 0.03
t_progress_index <- 3.953
df <- 404
p_value_progress_index <- 2 * (1 - pt(abs(t_progress_index), df))
cat("p-value for progress_index:", p_value_progress_index, "\n")


# Ordered probit model controlling for GDP
model_polr_2 <- polr(number_of_policies ~ progress_index + ngdpd_log, data = df_total_2000_2022_imp, method = "probit")
summary(model_polr_2) 

# Calculation of p-values
coef_progress_index_1 <- 0.01602
coef_ngdpd_log <- 0.28251
t_progress_index_1 <- 2.000
t_ngdpd_log <- 5.956
p_value_progress_index_1 <- 2 * (1 - pt(abs(t_progress_index_1), df))
p_value_ngdpd_log <- 2 * (1 - pt(abs(t_ngdpd_log), df))
cat("p-value for progress_index:", p_value_progress_index_1, "\n")
cat("p-value for ngdpd_log:", p_value_ngdpd_log, "\n")






