# Here are two different functions to perform multicollinearity test. 


#-------------------------------------------------------------------------------------------------=
# Function 1. -------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------=

# in this function, the threshold of correlation is set. 
# the function identifies the predictor variable with correlation greater than the given threshold. 
# along with the correlation, the variance inflation factor (VIF) is also calculated. 
# between the two predictor variables, the predictor variable with greater VIF is removed. 
# then the process is repeated until none of the predictor variables has correlation greater than the given threshold. 
# keep in mind that the threshold is absolute.
# pearson correlation coefficient is considered the thershold. 

# loading the necessary packages. 
library(dplyr) # you can instead load tidivyerse package which contains dply package, along with other. 
library(car) # car package is for correlation calculation. 
# you have to install aforementioned packages if they're not available in your machine. 

multicollinearity_function <- function(data, th) {
  library(dplyr)
  
  vif_function <- function(data) {
    vif_values <- data.frame(Variable = character(), VIF = numeric())
    for (var in names(data)) {
      formula <- as.formula(paste(var, "~ ."))
      model <- lm(formula, data = data)
      r_squared <- summary(model)$r.squared
      vif_value <- 1 / (1 - r_squared)
      vif_values <- rbind(vif_values, data.frame(Variable = var, VIF = vif_value))
    }
    return(vif_values)
  }
  
  repeat {
    correlation_matrix <- cor(data)
    correlation_df <- as.data.frame(as.table(correlation_matrix))
    colnames(correlation_df) <- c("variable1", "variable2", "correlation")
    
    correlation_df <- correlation_df %>%
      filter(variable1 != variable2) %>%
      mutate(abs_correlation = abs(correlation)) %>%
      arrange(desc(abs_correlation))
    
    high_corr <- correlation_df %>% filter(abs_correlation > th)
    
    if (nrow(high_corr) == 0) {
      break
    }
    
    pair <- high_corr[1, c("variable1", "variable2")]
    
    vif_values <- vif_function(data)
    vif_pair <- vif_values %>% filter(Variable %in% c(pair$variable1, pair$variable2))
    
    variable_to_remove <- vif_pair$Variable[which.max(vif_pair$VIF)]
    
    data <- data %>% select(-all_of(variable_to_remove))
  }
  
  return(data)
}


# Example. 
final_variables <- multicollinearity_function( data = your_data_frame_with_only the predictor_variables, th =  # the_threshold which you wish (e.g. 0.7, or 0.8 (numeric) )



#-------------------------------------------------------------------------------------------------=
# Function 2. -------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------=

# in this function, however, we perform multicollinearity assessment solely based on variance inflation factor (VIF). 
# the function calculates VIF of all the variables, removes the one with the highest VIF, and then iterates, until the VIF of all the variables is not less than the threshold given. 
# the threshold is given for the VIF value (e.g., 5, 10). 


library(dplyr) # you can load tidyverse package as well. 
library(car)

stepwise_multicollinearity <- function(data, th) {
  vif_function <- function(data) {
    vif_values <- data.frame(Variable = character(), VIF = numeric())
    for (var in names(data)) {
      formula <- as.formula(paste(var, "~ ."))
      model <- lm(formula, data = data)
      r_squared <- summary(model)$r.squared
      vif_value <- 1 / (1 - r_squared)
      vif_values <- rbind(vif_values, data.frame(Variable = var, VIF = vif_value))
    }
    return(vif_values)
  }
  
  data_filtered <- data
  
  repeat {
    vif_values <- vif_function(data_filtered)
    max_vif <- max(vif_values$VIF)
    
    if (max_vif < th) {
      break
    }
    
    variable_to_remove <- vif_values$Variable[which.max(vif_values$VIF)]
    data_filtered <- data_filtered %>% select(-all_of(variable_to_remove))
  }
  
  return(vif_function(data_filtered))
}

# example 
final_variables <- stepwise_multicollinearity(data = the_data_frame_with_only_the_predictor_variables , th = # the threshold limti of VIF you want)





