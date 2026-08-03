##==============================================================================
## Project: FOR-NM 
## Modified from QUEST scripts 
## Script to format all the Solinst Dataloggers to a cleaner state
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

### Manual edits: (to do: automate this)
### `2026-03-13_USF24_Teak03_SCL.csv` is already formatted, deleting from raw folder for now 
### `2026-06-02_NMUSF24_SCL_Sporty.csv` has typo in conductivity column , manual editing for now 

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
Saltslugs <- "data/raw/"
output_path<-"data/formatted/"

# list and filter CSV files with "SCL" in their names
SCL_files <- list.files(path = Saltslugs, pattern = "\\.csv$")
SCL_files <- SCL_files[grepl("SCL", SCL_files)]

 
# create an empty list to store the cleaned data frames
scl_list <- lapply(seq_along(SCL_files), function(i) {
  fread(paste0(Saltslugs, SCL_files[i]))
  # reading automatically without having to skip lines w fread 
})

# assign names to the list elements based on the file names
names(scl_list) <- SCL_files

# check the contents of the list
str(scl_list)

############################
#### Preprocess ####
############################
# loop through each data frame in the list
for (i in seq_along(scl_list)) {
  # access the current data frame
  df <- scl_list[[i]]
  
  # make date into date fomat
  df$Date <- as.Date(df$Date, format = "%m/%d/%Y")
  # Format time column to match YSI ####
  df$Time <- format(as.POSIXct(df$Time, format = "%I:%M:%S %p"), format = "%H:%M:%S")
  # rename columns to match YSI 
  df <- df %>%
    dplyr::rename(Temp.C. = TEMPERATURE,
                  Conductivity = CONDUCTIVITY)
  # calculate specific conductivity
  df <- df %>%
    mutate(SPC.uS.cm. = Conductivity/(1+0.02*(Temp.C. - 25)))
  # update the data frame in the list
  scl_list[[i]] <- df
}



####################################
#### Save edited slugs to Local folder ####
####################################
# loop through each data frame in the list
overwrite_flag=TRUE 

for (i in seq_along(scl_list)) {
  # Access the current data frame
  df <- scl_list[[i]]
  filename<- paste0(output_path, SCL_files[i])
  
  # Do not overwrite formatted files: To do:  have a flag to switch this on or off 
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
#### End of script ####
####################################