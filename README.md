# MultidimensionalPovertyCoding_CA
library(dplyr)

acs_data <- acs_data[acs_data$agep >= 18 & acs_data$agep <= 64, ]
# Assuming 'acs_data' is your dataframe containing the ACS PUMS data
{
  # Define the indicators as binary variables (1 for deprived, 0 for not deprived)
  acs_data$mhispend <- ifelse(acs_data$grpip > 30 | acs_data$ocpip > 30, 1, 0)
  acs_data$lesshs <- ifelse(acs_data$agep >= 19 & acs_data$schl < 12, 1, 0)
  acs_data$disa <- ifelse(acs_data$dis == 1, 1, 0)
  acs_data$nohcov <- ifelse(acs_data$hicov == 2, 1, 0)
  acs_data$lesseng <- ifelse(acs_data$lanx == 1 & acs_data$eng >= 2, 1, 0)
  acs_data$morehu <- ifelse(acs_data$np / acs_data$bdsp >= 1.01, 1, 0)

  # Calculate the percentage of each binary variable
  percentage_mhispend <- mean(acs_data$mhispend) * 100
  percentage_lesshs <- mean(acs_data$lesshs) * 100
  percentage_disa <- mean(acs_data$disa) * 100
  percentage_nohcov <- mean(acs_data$nohcov) * 100
  percentage_lesseng <- mean(acs_data$lesseng) * 100
  percentage_morehu <- mean(acs_data$morehu) * 100
  # Print the results
  print(paste("Percentage of mhispend:", percentage_mhispend))
  print(paste("Percentage of lesshs:", percentage_lesshs))
  print(paste("Percentage of disa:", percentage_disa))
  print(paste("Percentage of nohcov:", percentage_nohcov))
  print(paste("Percentage of lesseng:", percentage_lesseng))
  print(paste("Percentage of morehu:", percentage_morehu))
  
# Define the weights for each indicator
  num_indicators <- 6
  weights <- rep(1 / num_indicators, num_indicators)
  
  # Calculate the weighted sum of deprivations for each individual
  acs_data$weighted_deprivations <- rowSums(acs_data[, c("mhispend", "lesshs", "disa", "nohcov", "lesseng", "morehu")] * weights)
  
  # Define the deprivation cutoff (k) for being considered multidimensionally deprived
  # Assuming k = 2 deprivations for the adjusted analysis
  deprivation_cutoff <- 2 / num_indicators
  
  # Identify individuals as multidimensionally deprived
  acs_data$multidim_deprived <- ifelse(acs_data$weighted_deprivations >= deprivation_cutoff, 1, 0)
  

#weight MPI
  hcr <- weighted.mean(acs_data$multidim_deprived, acs_data$pwgtp)
  deprived_indices <- which(acs_data$multidim_deprived == 1)
  a <- sum(acs_data$weighted_deprivations[deprived_indices] * acs_data$pwgtp[deprived_indices]) / sum(acs_data$pwgtp[deprived_indices])
  m <- hcr * a
  print(paste("Headcount ratio", hcr))
  print(paste("Alternative Indices",a))
  print(paste("Multidimensional Poverty Index (MPI):", m))
  # To calculate the percentage of MPI, you can simply multiply m by 100
  percentage_mpi <- m * 100
  # Print the percentage of MPI
  print(paste("Percentage of MPI:", percentage_mpi, "%"))
}

  
#RACEMPI
{
  # Assuming acs_data is your dataframe and it has been properly loaded with the necessary columns
  
  # Create a new column 'raceethnicity' based on conditions
  acs_data$raceethnicity <- ifelse(acs_data$hisp > 1, "Hispanic",  # Hispanic alone
                                   ifelse(acs_data$rac1p == 1, "White",  # White alone
                                          ifelse(acs_data$rac1p == 2, "Black",  # Black or African American alone
                                                 ifelse(acs_data$rac1p == 6, "Asian",  # Asian alone
                                                        "Others"))))  # Other races
  
  # Define a function to calculate the Multidimensional Deprivation Index (MDI) for each race/ethnicity group
  mdi_formula_weighted <- function(data) {
    # Calculate the headcount ratio
    hcr <- weighted.mean(data$multidim_deprived, data$pwgtp, na.rm = TRUE)
    
    # Calculate the average deprivation score among the deprived
    a <- sum(data$weighted_deprivations * data$pwgtp[data$multidim_deprived == 1], na.rm = TRUE) / 
      sum(data$pwgtp[data$multidim_deprived == 1], na.rm = TRUE)
    
    # Calculate the MDI
    m <- hcr * a
    return(m)
  }
  
  # Apply MDI calculation for each race/ethnicity group considering weights
  mdi_results_weighted <- by(acs_data, acs_data$raceethnicity, mdi_formula_weighted)
  
  # Print the results
  print("Multidimensional Deprivation Index (MDI) by Race/Ethnicity Group :")
  print(mdi_results_weighted)
  mdi_results_df <- data.frame(
    RaceEthnicity = names(mdi_results_weighted),
    MDI = as.numeric(mdi_results_weighted)
  )
  
  # Use stargazer to create a summary table
  stargazer(mdi_results_df, type = "text", title = "Multidimensional Deprivation Index (MDI) by Race/Ethnicity Group (Weighted)", summary = FALSE)
}
  

  
  
{
  ###Demographic groups
  # Education Levels
  acs_data$education_lessthanhs <- ifelse(acs_data$schl <= 11, 1, 0)
  acs_data$education_hsdegree <- ifelse(acs_data$schl == 12, 1, 0)
  acs_data$education_somecollege <- ifelse(acs_data$schl > 12 & acs_data$schl <= 16, 1, 0)
  acs_data$education_bachelors <- ifelse(acs_data$schl == 21, 1, 0)
  acs_data$education_graduatedegree <- ifelse(acs_data$schl > 21, 1, 0)
  
  # Marital Status
  acs_data$maritalstatus_married <- ifelse(acs_data$mar == 1, 1, 0)
  acs_data$maritalstatus_livingwithpartner <- ifelse(acs_data$mar == 5, 1, 0)
  acs_data$maritalstatus_allothers <- ifelse(!(acs_data$mar == 1 | acs_data$mar == 5), 1, 0)
  
  # Household Size
  acs_data$householdsize_1to2 <- ifelse(acs_data$np >= 1 & acs_data$np <= 2, 1, 0)
  acs_data$householdsize_3to5 <- ifelse(acs_data$np > 2 & acs_data$np <= 5, 1, 0)
  acs_data$householdsize_6plus <- ifelse(acs_data$np > 5, 1, 0)
  
  # Household Income
  acs_data$householdincome_lessthan40k <- ifelse(acs_data$hincp <= 40000, 1, 0)
  acs_data$householdincome_40kto99k <- ifelse(acs_data$hincp > 40000 & acs_data$hincp <= 99999, 1, 0)
  acs_data$householdincome_100kplus <- ifelse(acs_data$hincp > 99999, 1, 0)
  
  # Sex Variables
  acs_data$sex_male <- acs_data$sex == 1
  acs_data$sex_female <- acs_data$sex == 2
  
  # Calculate MPI for each demographic group
  calculate_mpi_for_group <- function(data, group_name) {
    mpi <- data %>%
      filter(!!rlang::sym(group_name) == 1) %>%
      summarise(mpi = mdi_formula_weighted(.), .groups = 'drop')
    return(mpi)
  }
  
  # Print MPI for each demographic group
  demographic_groups <- c("education_lessthanhs", "education_hsdegree", "education_somecollege", 
                          "education_bachelors", "education_graduatedegree", "maritalstatus_married", 
                          "maritalstatus_livingwithpartner", "maritalstatus_allothers", "householdsize_1to2", 
                          "householdsize_3to5", "householdsize_6plus", "householdincome_lessthan40k", 
                          "householdincome_40kto99k", "householdincome_100kplus", "sex_male", "sex_female")
  for(group in demographic_groups) {
    mpi_result <- calculate_mpi_for_group(acs_data, group)
    print(paste("MPI for", gsub("_", " ", group, fixed = TRUE), ":", mpi_result$mpi))
  }

  # Create binary variables for each age group
  acs_data$age_18_29 <- ifelse(acs_data$agep > 17 & acs_data$agep <= 29, 1, 0)
  acs_data$age_30_44 <- ifelse(acs_data$agep > 29 & acs_data$agep <= 44, 1, 0)
  acs_data$age_45_59 <- ifelse(acs_data$agep > 44 & acs_data$agep <= 59, 1, 0)
  acs_data$age_60_plus <- ifelse(acs_data$agep > 59, 1, 0)
  
  # Calculate MPI for age_18_29 group
  mpi_age_18_29 <- acs_data %>%
    filter(age_18_29 == 1) %>%
    summarise(mpi = mdi_formula_weighted(.), .groups = 'drop')
  
  # Calculate MPI for age_30_44 group
  mpi_age_30_44 <- acs_data %>%
    filter(age_30_44 == 1) %>%
    summarise(mpi = mdi_formula_weighted(.), .groups = 'drop')
  
  # Calculate MPI for age_45_59 group
  mpi_age_45_59 <- acs_data %>%
    filter(age_45_59 == 1) %>%
    summarise(mpi = mdi_formula_weighted(.), .groups = 'drop')
  
  # Calculate MPI for age_60_plus group
  mpi_age_60_plus <- acs_data %>%
    filter(age_60_plus == 1) %>%
    summarise(mpi = mdi_formula_weighted(.), .groups = 'drop')
  
  # Print the MPI results for each age group
  print("MPI for Age 18-29:")
  print(mpi_age_18_29)
  
  print("MPI for Age 30-44:")
  print(mpi_age_30_44)
  
  print("MPI for Age 45-59:")
  print(mpi_age_45_59)
  
  print("MPI for Age 60+:")
  print(mpi_age_60_plus)
}
