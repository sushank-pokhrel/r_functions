# This file contains a functoin in RStudio, that can be used to count the raster files based on Polygon Shape file. 

# for instance, we have a polygon shape file of a country which is classified into districts. 
#Now if we want to count the number of pixels of each LULC classes in each of the districts, we can use this function. 

# libraries needed
library(raster)
library(dplyr) # tidyverse library can be loaded. 
library(rlang)  # Load rlang for dynamic column names
library(sf) # to work with shape files. 
# install libraries if not installed previously. 


# Frequency count of rasters for each shapefile parameters ---------

frequency_count <- function(raster_file, shape_file, sf_column_name, raster_column_name){

  # Convert shape_file to XY dimensions if it has a Z dimension
  shape_file <- sf::st_zm(shape_file)
  
  # Initialize an empty data frame to store pixel counts
  pixel_counts <- data.frame()
  
  # Loop over all unique values in the specified column
  for(i in unique(shape_file[[sf_column_name]])){
    
    # Filter shape_file to get the row for the current value of sf_column_name
    evaluation_row <- shape_file %>%
      filter(!!sym(sf_column_name) == i)
    # sym() function from rlang package converts the string into symbol. 
    # !! (i.e. bang bang operator) is the unquote operator which unquotes the symbol. 
    
    # Crop and mask the raster using the current evaluation_row (polygon)
    cropped <- crop(raster_file, evaluation_row)
    masked <- mask(cropped, evaluation_row)
    
    # Convert the masked raster to a data frame and count pixels
    df <- as.data.frame(masked, na.rm = T)
    
    # Use the dynamic raster column name
    raster_column <- df[[raster_column_name]]
    
    # Count the frequency of values in the specified raster column
    pixel_frequency <- table(raster_column) %>% as.data.frame()
    
    # Exclude 0 values (remove rows where frequency is 0)
    pixel_frequency <- pixel_frequency %>%
      mutate(!!sf_column_name := i)  # Dynamically assign the value of `i` to the new column
    # := is a pronoun assignment operator introducted in dplyr to dynamically assign a value to a column. 
    # it is combined with mutate() function in R. 
    
    # e.g. 
    ##df <- tibble(value = c(1, 2, 3, 4))
    ##new_column_name <- "new_col"
    ##df %>%mutate(!!new_column_name := value * 2)
    
    
    # Combine pixel counts across iterations (if needed)
    pixel_counts <- bind_rows(pixel_counts, pixel_frequency)
  }
  
  return(pixel_counts)
}


# Running the functions

frequency_count(
  shape_file = ,          # shape file name
  sf_column_name = ,      # name of the column of a shape file for which data is to be calculated
  raster_file = ,         # name of the raster file
  raster_column_name =    # the column names in raster file, (other than x y coordinate)
)

# you should assign a variable name before running the function.


# Cleaner version of the function 
frequency_count <- function(raster_file, shape_file, sf_column_name, raster_column_name) {
    
  shape_file <- sf::st_zm(shape_file)
  pixel_counts <- data.frame()
  
  for(i in unique(shape_file[[sf_column_name]])) {
    
    evaluation_row <- shape_file %>%
      filter(!!sym(sf_column_name) == i)
    
    cropped <- crop(raster_file, evaluation_row)
    masked <- mask(cropped, evaluation_row)
    
    df <- as.data.frame(masked, na.rm = T)
    
    raster_column <- df[[raster_column_name]]
    
    pixel_frequency <- table(raster_column) %>% as.data.frame()
    
    pixel_frequency <- pixel_frequency %>%
      mutate(!!sf_column_name := i)
    
    pixel_counts <- bind_rows(pixel_counts, pixel_frequency)
  }
  
  return(pixel_counts)
}



