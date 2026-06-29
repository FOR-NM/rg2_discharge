##==============================================================================
## Project: FOR-NM 
## Modified from QUEST scripts 
## Script to format all the Solinst Dataloggers to a cleaner state
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

##############
## Packages ##
##############
#library(googledrive) 
library(tidyverse)
library(data.table)


####################################
## Clear folders that we will use ##
####################################
# list and delete all files in the folder
#files <- list.files(path = "slugs", full.names = TRUE)
#file.remove(files)

#files <- list.files(path = "googledrive", full.names = TRUE)
#file.remove(files)

#####################
#### Import Data ####
#####################
# set up path 
Saltslugs <- "/Users/marcelamendoza/Documents/UNM/RG2/code/data /Slugs"

# list and filter CSV files with "SCL" in their names
SCL_files <- list.files(path = Saltslugs, pattern = "\\.csv$")
SCL_files <- SCL_files[grepl("SCL", SCL_files)]

 
# create an empty list to store the cleaned data frames
scl_list <- lapply(seq_along(SCL_files), function(i) {
  
  # read the CSV file, skipping the first 13 rows (header is on row 14)
  fread(paste0(Saltslugs, "/", SCL_files[i]))
  # reading differently for 03_13 files 
})

# assign names to the list elements based on the file names
names(scl_list) <- SCL_files

# check the contents of the list
str(scl_list)

############################
#### Format date column ####
############################
# loop through each data frame in the list
for (i in seq_along(scl_list)) {
  # access the current data frame
  df <- scl_list[[i]]
  
  # make date into date fomat
  df$Date <- as.Date(df$Date, format = "%m/%d/%y")
  # update the data frame in the list
  scl_list[[i]] <- df
}
################################
#### Format time column to match YSI ####
################################
for (i in seq_along(scl_list)) {
  # access the current data frame
  df <- scl_list[[i]]
  
  # make date into date fomat
  df$Time <- format(as.POSIXct(df$Time, format = "%I:%M:%S %p"), format = "%H:%M:%S")
  # update the data frame in the list
  scl_list[[i]] <- df
}



###################################
#### Format names to match YSI ####
###################################
for (i in seq_along(scl_list)) {
  # access the current data frame
  df <- scl_list[[i]]
  
  # rename columns
  df <- df %>%
    dplyr::rename(Temp.C. = TEMPERATURE,
           Conductivity = CONDUCTIVITY)
         
  scl_list[[i]] <-  df
}

# check the contents of the list
str(scl_list)

#########################################
#### Calculate specific conductivity ####
#########################################
for (i in seq_along(scl_list)) {
  # access the current data frame
  df <- scl_list[[i]]
  
  # calculate specific conductivity
  df <- df %>%
    mutate(SPC.uS.cm. = Conductivity/(1+0.02*(Temp.C. - 25)))

  scl_list[[i]] <-  df
}

####################################
#### Save edited slugs to Local folder ####
####################################
# loop through each data frame in the list
for (i in seq_along(scl_list)) {
  # Access the current data frame
  df <- scl_list[[i]]
  filename<- paste0("/Users/marcelamendoza/Documents/UNM/RG2/code/data /formatted/", SCL_files[i])
  
  # Do not overwrite formatted files: To do:  have a flag to switch this on or off 
  if (file.exists(filename)) {
    next
  } else {
    # save new data frame
    write.csv(df, filename, row.names=FALSE, quote=FALSE)
  }
}

####################################
#### End of script ####
####################################