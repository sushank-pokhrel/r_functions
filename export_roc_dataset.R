# This function is used in extracting the dataset for developing Reciever Operator Curve. 
# This curve is widely used in demonstrating how well the model has performed. 
# Primarily used in binary prediction. 

roc_curve <- function(result_data, step, original_column, predicted_probability) {
  # Compute Sensitivity and Specificity for different cutoff values to generate ROC Curve.
  #
  # Parameters:
  # result_data (data.frame): A dataframe containing 'Original Presence' (actual values) and 'Predicted Probability'.
  # step (numeric): The step size for cutoff values (default is 0.05).
  #
  # Returns:
  # data.frame: A dataframe with Cutoff, Sensitivity, and Specificity values.
  
  cutoffs <- seq(0, 1, by = step)  # Generate cutoffs from 0 to 1 with step
  roc_data <- data.frame(Cutoff = numeric(), Sensitivity = numeric(), Specificity = numeric())  # Dataframe to store results
  
  y_true <- result_data[[original_column]]
  y_prob <- result_data[[predicted_probability]]
  
  for (cutoff in cutoffs) {
    y_pred <- as.integer(y_prob >= cutoff)  # Convert probability to binary class based on cutoff
    
    # Calculate TP, FN, FP, TN
    TP <- sum((y_true == 1) & (y_pred == 1))
    FN <- sum((y_true == 1) & (y_pred == 0))
    FP <- sum((y_true == 0) & (y_pred == 1))
    TN <- sum((y_true == 0) & (y_pred == 0))
    
    # Compute Sensitivity (Recall) and Specificity
    sensitivity <- ifelse((TP + FN) > 0, TP / (TP + FN), 0)
    specificity <- ifelse((TN + FP) > 0, TN / (TN + FP), 0)
    
    # Store results
    roc_data <- rbind(roc_data, data.frame(Cutoff = cutoff, Sensitivity = sensitivity, Specificity = specificity))
  }
  
  # Add True Positive Rate and False Positive Rate columns
  roc_data$True_Positive_Rate <- roc_data$Sensitivity
  roc_data$False_Positive_Rate <- 1 - roc_data$Specificity
  
  return(roc_data)
}


# Example

# Set seed for reproducibility
set.seed(123)

# Number of samples
n <- 50

# Generate actual binary class (0 or 1)
Original_Presence <- sample(c(0, 1), n, replace = TRUE, prob = c(0.4, 0.6))

# Generate predicted probabilities (random values between 0 and 1)
Predicted_Probability <- runif(n, min = 0, max = 1)

# Adjust predicted probabilities to make them somewhat aligned with the actual class
Predicted_Probability <- ifelse(Original_Presence == 1, 
                                Predicted_Probability + 0.2, 
                                Predicted_Probability - 0.2)
Predicted_Probability <- pmax(0, pmin(1, Predicted_Probability))  # Ensure values stay between 0 and 1

# Create the data frame
roc_dataset <- data.frame(
  Original_Presence = Original_Presence,
  Predicted_Probability = Predicted_Probability
)

new_data <- roc_curve(
  result_data = roc_dataset,
  step = 0.05, 
  original_column = "Original_Presence", 
  predicted_probability = "Predicted_Probability"
)

# visualization

library(ggplot2)

ggplot()+
  geom_line(
    data = new_data,
    aes( x = False_Positive_Rate,
         y = True_Positive_Rate)
  )

