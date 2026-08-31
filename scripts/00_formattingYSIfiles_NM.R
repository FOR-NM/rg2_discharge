##==============================================================================
## Project: FOR-NM 
## Modified from Quest scripts 
## Script to format all the YSI files to a cleaner state
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================
# Modified by Marcela Mendoza 6/16/2026  FOR_NM 



##############
## Packages ##
##############
# library(googledrive) 

### TO DO #### 
# create directory for formatted files 

#####################
#### Import Data ####
#####################
# set up Google Drive folder
Saltslugs <- "data/raw/"
output_path<-"data/formatted/"

# list and filter CSV files with "YSI" in their names
YSI_files <-  list.files(path = Saltslugs, pattern = "\\.csv$")
YSI_files <- YSI_files[grepl("YSI", YSI_files)]

# Create an empty list to store the cleaned data frames
ysi_list <- lapply(seq_along(YSI_files), function(i) {
  # read the CSV file, skipping the first row (header is on row 2)
  read.csv(paste0(Saltslugs, YSI_files[i]), skip = 1, sep = ";", header = TRUE)
})

# assign names to the list elements based on the file names
names(ysi_list) <- YSI_files

# check the contents of the list
str(ysi_list)

############################
#### Format date column ####
############################
# loop through each data frame in the list
for (i in seq_along(ysi_list)) {
  # Access the current data frame
  df <- ysi_list[[i]]
  
  # make date into date format
  df$Date <- as.Date(df$Date, format = "%m/%d/%Y")
  # update the data frame in the list
  ysi_list[[i]] <- df
}

####################################
#### Save edited slugs to local folder ####
####################################

overwrite_flag=TRUE  # can be switched on or off based on if you want to overwrite files 
# loop through each data frame in the list
for (i in seq_along(ysi_list)) {
  # Access the current data frame
  df <- ysi_list[[i]]
  filename<- paste0(output_path, YSI_files[i])
  
  # save new data frame
  if (file.exists(filename)) {
    if(overwrite_flag){
      write.csv(df, filename, row.names=FALSE, quote=FALSE)
      print('successfully overwrote')
    }
    else{
      next
    }
  } else {
    # save new data frame
    write.csv(df, filename, row.names=FALSE, quote=FALSE)
  }
}

####################################
#### Split file for 01_20_26 ####
####################################
# 2026-01-20_NMUSF02_NMUSF24_YSI_Teak has to be separated into two files 
# run split_file.R  (R folder) with appropriate index 

source('R/split_file.R') 
  
####################################
#### End of script ####
####################################
