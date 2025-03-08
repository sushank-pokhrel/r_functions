# This function is used in developing elevation profile based on Digital Elevation Model data. 

# load the necessary libraries. 
library(terra)
  library(sf)
  library(dplyr)
  # install them if they aren't installed previously in your machine. 

# the following function is still raw, and was not tested properly, therefore, it might be needed to modify as per necessicity. 


elevation_profile <- function(
    longitude, raster_data, shape_file, sf_column_list, sf_column_name
){
  message("Starting elevation profile extraction...")
  
  # Get the latitude range from the raster extent
  latitude_range <- c(ymin(raster_data), ymax(raster_data))
  
  # Create a line at the specified longitude
  line <- st_linestring(matrix(c(longitude, latitude_range[1], 
                                 longitude, latitude_range[2]), 
                               ncol = 2, byrow = TRUE))
  
  # Convert the line to an sf object with the same CRS as the raster
  line_sf <- st_sf(geometry = st_sfc(line), crs = st_crs(raster_data))
  
  # Drop Z and M dimensions from the shapefile (if any)
  shape_file <- sf::st_zm(shape_file)
  
  # Initialize an empty data frame to store the profile heights
  profile_height <- data.frame()
  
  # Initialize previous_distance to 0
  previous_distance <- 0
  
  for (i in sf_column_list) {
    message(paste("Processing feature:", i))
    
    # Filter the shapefile for the current value
    evaluation_row <- shape_file %>%
      filter(!!sym(sf_column_name) == i)
    
    if (nrow(evaluation_row) == 0) {
      warning(paste("No matching rows found for feature:", i))
      next
    }
    
    # Find the intersection of the line with the current shapefile feature
    actual_line <- tryCatch({
      st_intersection(evaluation_row, line_sf)
    }, error = function(e) {
      warning(paste("Error in st_intersection for feature:", i, "-", e$message))
      return(NULL)
    })
    
    if (is.null(actual_line) || nrow(actual_line) == 0) {
      warning(paste("No intersection found for feature:", i))
      next
    }
    
    # Calculate the length of the intersected line in kilometers
    total_length_km <- as.numeric(st_length(actual_line)) / 1000
    
    # Crop and mask the raster data to the current shapefile feature
    cropped <- tryCatch({ crop(raster_data, evaluation_row) }, 
                        error = function(e) {
                          warning(paste("Error in cropping raster for feature:", i, "-", e$message))
                          return(NULL)
                        })
    
    if (is.null(cropped)) next
    
    masked <- tryCatch({ mask(cropped, evaluation_row) }, 
                       error = function(e) {
                         warning(paste("Error in masking raster for feature:", i, "-", e$message))
                         return(NULL)
                       })
    
    if (is.null(masked)) next
    
    # Extract the raster values along the intersected line
    values <- tryCatch({ terra::extract(masked, actual_line, along = TRUE) }, 
                       error = function(e) {
                         warning(paste("Error in extracting raster values for feature:", i, "-", e$message))
                         return(NULL)
                       })
    
    if (is.null(values) || length(values) == 0) {
      warning(paste("No raster values extracted for feature:", i))
      next
    }
    
    # Convert the extracted values to a data frame
    extracted_values <- unlist(values) |> as.data.frame()
    names(extracted_values)[1] <- "elevation"
    
    # Get the number of observations
    num_observations <- nrow(extracted_values)
    
    if (num_observations == 0) {
      warning(paste("No elevation data available for feature:", i))
      next
    }
    
    # Create a sequence of distances starting from previous_distance
    distance_km <- seq(previous_distance, previous_distance + total_length_km, 
                       length.out = num_observations)
    
    # Add the distance column to the extracted values
    extracted_values$Distance_km <- distance_km
    
    # Add the current shapefile column value as a new column
    extracted_values <- extracted_values %>% 
      mutate(!!sf_column_name := i)
    
    # Append the extracted values to the profile_height data frame
    profile_height <- bind_rows(profile_height, extracted_values)
    
    # Update previous_distance for the next iteration
    previous_distance <- previous_distance + total_length_km
  }
  
  message("Elevation profile extraction completed.")
  
  return(profile_height)
}



# Example usage of the function
lon_86.13 <- elevation_profile(
  longitude = 86.136111, # The longitude in which we have to develop the profile. 
  raster_data = raster_dem, # raster dem file. 
  shape_file = physiography_nepal, # the shape file of a certain region, in which we have to develop the elevation profile. 
  sf_column_name = "Physio", # the name of the column, based on which we have to differentiate the different physiographic regions. 
  sf_column_list = c("Terai", "Siwalik", "Middle Mountains", "High Mountains", "High Himalaya") # the levels of physiographic regions from the lowest elevation to highest. 
)


# this function is especially developed to differentiate physiographic regions, or might be districts.
# this function can be modified so that we can develop the profile data directly, just based on a shape file. 
