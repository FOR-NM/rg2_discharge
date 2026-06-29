##==============================================================================
## Project: FOR-NM
# Modified from QuEST scripts: Github Link: 

#Added rolling mean smoothing, dropping zeros in SpC, improved changepoint detection,
#interactive plots to 

## Script to estimate discharge using dilution gauging for multiple files at a time for the NM site
## press Command+Option+O to collapse all sections and get an overview of the workflow!
##==============================================================================

## Explanation based off: Moore, R.D. 2005. Salt Injection Using Salt in Solution.
## "Streamline Watershed Management Bulletin". Volume 8 (2)

## Two assumptions:
## (1) All the injected mass is recovered downstream
## (2) Tracer is completely mixed in the channel

## The time required for the peak of the wave to move past an observation point 
## depends inversely on the mean velocity of streamflow
## The duration of the salt wave depends on the amount of longitudinal dispersion,
## which depends on how variable velocities are across the stream

## At any time (t) while tracer is passing in the salt wave,
## the discharge of the tracer solution is: q(t) = Q*RC(t)
## Where Q is stream discharge (L/s), and RC(t) is the relative
## concentration of the tracer solution (L/L) at t

## Integrate over the salt wave to get discharge (Q):
## Q = V/integral(RC(t))dt

##################
#### Packages ####
##################
lapply(c("plyr", "dplyr", "ggplot2", "cowplot",
         "lubridate", "tidyverse"), require, character.only = T)

library(changepoint)   # calculate changepoint in data
library(googlesheets4)
library(tibble)
library(janitor)
library(googledrive)
library(pracma) # might not use
library(plotly)
library(ggplot2)

####################################
## Clear folders that we will use ##
####################################
#file.remove(list.files(path = "saltslug_figs", full.names = TRUE))
#file.remove(list.files(path = "googledrive",   full.names = TRUE))
#file.remove(list.files(path = "data",          full.names = TRUE))

#################################
#### Import & Visualize Data ####
#################################
#### load data from Google drive ####
# this is the formatted folder
Saltslugs <- "/Users/marcelamendoza/Documents/UNM/RG2/code/data /formatted/" 
figure_folder <- "/Users/marcelamendoza/Documents/UNM/RG2/code/saltslug_figs/"

# list all CSV files in the folder
Saltslugs_csvs <- list.files(path = Saltslugs, pattern = "\\.csv$")

csv_list <- list()

for (i in seq_along(Saltslugs_csvs)) {
  local_path <- file.path(Saltslugs, Saltslugs_csvs[i])
  csv_list[[Saltslugs_csvs[i]]] <- read.csv(local_path)
}

#### format DateTime and Date columns ####
# NM raw files have separate Date and Time columns that need combining
for (i in seq_along(csv_list)) {
  df          <- csv_list[[i]]
  df$DateTime <- as.POSIXct(paste(df$Date, df$Time, sep = " "), format = "%Y-%m-%d %H:%M:%S")
  df$Date     <- as.Date(df$Date, format = "%Y-%m-%d")
  csv_list[[i]] <- df
}

str(csv_list)


##########################################
####Pre-Processing / Signal Processing####
##########################################
for (i in seq_along(csv_list)) {
  df <- csv_list[[i]]
  #drop trailing and taling rows with 0 conductivity (instrument outside the water)
  # Find first and last non-zero
  first <- which(df$SPC.uS.cm. != 0)[1]
  last <- tail(which(df$SPC.uS.cm. != 0), 1)
  
  # Keep only rows between them
  df<- df[first:last, ]
  
  csv_list[[i]] <- df
}



#####################
#### Plot curves ####
#####################
for (i in seq_along(csv_list)) {
  df <- csv_list[[i]]
  p  <- ggplot(data = df, aes(x = DateTime, y = SPC.uS.cm.)) +
    geom_point() +
    ggtitle(Saltslugs_csvs[i])
  print(p)
}
# Pause and observe data 



######################################################################
#### Calculate average background SpC and Changepoint ####
######################################################################
for (i in seq_along(csv_list)) {
  df <- csv_list[[i]]
  df <- na.omit(df)
  
  
  #  method - Marcela 
  #first use rolling mean 
  df$spc_smooth <- rollmedian(df$SPC.uS.cm., k=7, fill='extend')
  #k sets the half window (total window= 2k+1)
  #t0=3 is the threashold, 3 standard deviations 
  #df$spc_smooth<- hampel(df$spc_smooth, k=3, t0=3)$y
  
  peak_idx <- which.max(df$spc_smooth)
  # data before peak
  y_pre <- df$spc_smooth[1:peak_idx]
  
  # detect first changepoint in the SpC timeseries - Manuela's method 
  cpt               <- cpt.mean(df$spc_smooth, penalty = "SIC", method = "PELT", minseglen = 2)
  #changepoint_index <- tail(cpts(cpt), 1) #last point before the peak 
  idx <- cpts(cpt)
  #enforce positive derivative with linear regression estimation 
  idx <- idx[sapply(idx, function(i) {
    coef(lm(df$spc_smooth[i:min(i+20-1,length(df$spc_smooth))] ~ seq_len(min(20,length(df$spc_smooth)-i+1))))[2] > 0
  })] 
  changepoint_index <- idx[1] # first changepoint which satisfies derivative condition
  
  df$medianSPC            <- median(df$SPC.uS.cm.[1:changepoint_index], na.rm = TRUE)
  df$changepoint_index    <- changepoint_index
  df$changepoint_datetime <- df$DateTime[changepoint_index]
  
  csv_list[[i]] <- df
}

########################
#### Load salt data ####
########################
(Q_sheet <- drive_get("https://docs.google.com/spreadsheets/d/1rIWYWFUoF6UtzTcvN-WpIw2Cw4u9NjI_HuhThoDOvv4/edit?gid=0#gid=0"))

drive_download(as_id(Q_sheet$id), path = "field_data/salt.csv", type = "csv", overwrite = TRUE)

salt <- read.csv("/Users/marcelamendoza/Documents/UNM/RG2/code/data /field_data/salt.csv") %>%
  janitor::clean_names() %>%
  dplyr::rename(
    DataID     = site,
    Date       = date,
    Time24h    = time_24h_rounded_to_nearest_15_min,
    reach      = reach_length_m,
    salt       = salt_added_g,
    inj_time   = injection_time_24h_hh_mm_ss,
    background = background_spc_u_s_cm
  ) %>%
  mutate(
    reach      = as.numeric(as.character(reach)),
    salt       = as.numeric(as.character(salt)),
    background = as.numeric(as.character(background)),
    inj_time   = as.POSIXct(paste(Date, Time24h, sep = " "), format = "%m/%d/%Y %H:%M:%S"),
    Date       = as.Date(Date, format = "%m/%d/%Y"),
    # 1 g salt in 1 L of water gives cond = 2100 uS/cm
    Cond_mass  = salt * 2100
  ) %>%
  # drop the first two columns (row index artifacts from the sheet)
  select(-(1:2))

# replace empty strings with NA
salt[salt == ""] <- NA

###############################
#### Add DataID to loggers ####
###############################
# site name is extracted from the file name (e.g. "20775508_2026_02_04_USF5_HOBO.csv" → "USF5")
for (i in seq_along(csv_list)) {
  df            <- csv_list[[i]]
  df$DataID     <- str_extract(names(csv_list)[i], "USF\\d+")
  csv_list[[i]] <- df
}

# remove duplicate DataID column that YSI files already contain
for (i in seq_along(csv_list)) {
  df <- csv_list[[i]]
  if ("DataID.1" %in% colnames(df)) df <- select(df, -DataID.1)
  csv_list[[i]] <- df
}

###################################
#### Combine salt info to csvs ####
###################################
combine_info <- function(df, info) {
  df   <- mutate(df,   Date = as.Date(Date))
  info <- mutate(info, Date = as.Date(Date))
  merge(df, info, by = c("DataID", "Date"), all.x = TRUE)
}

csvs <- lapply(csv_list, function(df) {
  tryCatch(
    combine_info(df, info = salt),
    error = function(e) { cat("Error merging data frame:\n"); print(e); df }
  )
})

#################################################################
#### Visualize where the change pioint is to start measuring ####
#################################################################
legend_colors <- c("changepoint" = "red", "injection" = "blue")

for (i in seq_along(csvs)) {
  df <- csvs[[i]]
  
  vlines <- data.frame(
    xintercept = c(df$changepoint_datetime[1], df$inj_time[1]),
    type       = c("changepoint", "injection")
  )
  
  p <- ggplot(data = df, aes(x = DateTime, y = SPC.uS.cm.)) +
    geom_point() +
    ggtitle(Saltslugs_csvs[i]) +
    geom_vline(data = vlines, aes(xintercept = xintercept, color = type, linetype = type)) +
    scale_color_manual(values = legend_colors) +
    scale_linetype_manual(values = c("changepoint" = "dashed", "injection" = "dashed")) +
    labs(color = "colors", linetype = "colors")+
    geom_line(data=df, aes(x = DateTime, y = spc_smooth, color = "Curve 2"), size = 1)
  
  ggsave(paste0(figure_folder, Saltslugs_csvs[i], ".png"), plot = p)

  print(ggplotly(p))
}

######################################
#### Find lowest point after peak ####
######################################
result_df <- data.frame(
  DataID      = character(),
  medianSPC   = numeric(),
  final_SPC   = numeric(),
  Date        = as.Date(character()),
  final_index = numeric(),
  stringsAsFactors = FALSE
)

for (i in seq_along(csvs)) {
  df       <- csvs[[i]]
  condcorr <- df$SPC.uS.cm. - df$medianSPC[1]
  
  peak_index               <- which.max(condcorr)
  condcorr_after_peak      <- condcorr[(peak_index + 1):length(condcorr)]
  final_index              <- peak_index + which.min(abs(condcorr_after_peak))
  
  result_df <- rbind(result_df, data.frame(
    DataID      = df$DataID[1],
    Date        = df$Date[1],
    medianSPC   = df$medianSPC[1],
    final_SPC   = df$SPC.uS.cm.[final_index],
    final_index = final_index
  ))
}

######################################
#### Calculate difference in tail ####
######################################
result_df$SPC_difference <- result_df$medianSPC - result_df$final_SPC

###########################
#### Remove curve tail ####
###########################
csv_clean <- list()

for (i in seq_along(csvs)) {
  df   <- csvs[[i]]
  df   <- df[1:result_df$final_index[i], ]
  name <- gsub(".csv", "", Saltslugs_csvs[i])
  csv_clean[[name]] <- df
}

#########################################
#### Plot curves after removing tail ####
#########################################
for (i in seq_along(csv_clean)) {
  df <- csv_clean[[i]]
  p  <- ggplot(data = df, aes(x = DateTime, y = SPC.uS.cm.)) +
    geom_point() +
    ggtitle(Saltslugs_csvs[i])
  print(p)
}

####################
#### Estimate Q ####
####################
## Q = mass / integral of background-corrected conductivity over time
## units: L/sec

Qint <- function(time, cond, bkg, condmass) {
  condcorr <- cond - bkg
  ydiff    <- condcorr[-1] + condcorr[-length(condcorr)]
  condint  <- sum(diff(time) * ydiff / 2)
  condmass / condint
}

for (i in seq_along(csv_clean)) {
  df   <- csv_clean[[i]]
  df$Q <- Qint(as.numeric(df$DateTime), df$SPC.uS.cm., df$medianSPC[[1]], df$Cond_mass[[1]])
  csv_clean[[i]] <- df
}

###############################
#### Add Q values to table ####
###############################
selected_columns <- c("DataID", "Date", "salt", "background", "medianSPC", "Q", "flag", "flag_notes")

first_values <- lapply(csv_clean, function(df) df[selected_columns] %>% slice(1))

combined_data_frame <- bind_rows(first_values)

######################################
#### Add SPC values to salt table ####
######################################
mergedQ <- combined_data_frame %>%
  left_join(result_df, by = c("DataID", "Date")) %>%
  select(-medianSPC.y)   # drop duplicate column from the join

###################################
#### Combine SPC info to csvs ####
###################################
csv_clean <- lapply(csv_clean, function(df) {
  tryCatch(
    merge(df, result_df, by = c("DataID", "Date", "medianSPC"), all.x = TRUE),
    error = function(e) { cat("Error merging data frame:\n"); print(e); df }
  )
})

################################
#### Plot curves with flags ####
################################
for (i in seq_along(csv_clean)) {
  df <- csv_clean[[i]]
  
  p <- ggplot(data = df, aes(x = DateTime, y = SPC.uS.cm.)) +
    geom_point() +
    ggtitle(Saltslugs_csvs$name[i]) +
    annotate("text", x = mean(df$DateTime), y = max(df$SPC.uS.cm.),
             label = df$flag_notes[1], color = "#CD2626", size = 4, fontface = "bold") +
    annotate("text", x = mean(df$DateTime), y = max(df$SPC.uS.cm.) * 0.90,
             label = paste(df$medianSPC[1], "median SPC"), color = "#00688B", size = 4, fontface = "bold") +
    annotate("text", x = mean(df$DateTime), y = max(df$SPC.uS.cm.) * 0.85,
             label = paste(df$final_SPC[1], "final SPC"), color = "plum", size = 4, fontface = "bold") +
    annotate("text", x = mean(df$DateTime), y = max(df$SPC.uS.cm.) * 0.80,
             label = paste(df$background[1], "bckg"), color = "#008B00", size = 4, fontface = "bold") +
    annotate("text", x = mean(df$DateTime), y = max(df$SPC.uS.cm.) * 0.75,
             label = paste(df$Q[1], "L/sec"), color = "goldenrod2", size = 4)
  
  ggsave(paste0("saltslug_figs/", Saltslugs_csvs$name[i], "flags.png"), plot = p)
  print(p)
}

####################################
#### Save Q data frame to Drive ####
####################################
# download existing Q file, append new results, re-upload
(Q_drive <- drive_get("https://drive.google.com/drive/folders/1UkRaYRBePgY9XU90_3DvURNGGEWbCew0"))
Q_files  <- drive_ls(Q_drive)

#drive_download(as_id(Q_files$id), path = "field_data/Q.csv", overwrite = TRUE)

Q <- read.csv("field_data/Q.csv") %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  rbind(mergedQ)

write.csv(Q, "field_data/Q.csv", row.names = FALSE, quote = FALSE)

drive_upload(
  media = "field_data/Q.csv",
  path  = as_id("1UkRaYRBePgY9XU90_3DvURNGGEWbCew0")
)

####################################
#### Save edited slugs to Drive ####
####################################
for (i in seq_along(csvs)) {
  df        <- csvs[[i]]
  file_path <- paste0("slugs/", Saltslugs_csvs$name[i])
  
  write.csv(df, file_path, row.names = FALSE, quote = FALSE)
  
  drive_upload(
    media = file_path,
    path  = as_id("1LePC-TivkFR1xwa65DdoGAIuu5WapW6")
  )
}

####################################
#### End of Script####
####################################
####################################
#### TO DO####
# improve changepoint detection algorithm: tried multiple algorithms and filters
  # tried second derivative method ( did you try on smooth)
  # tried changepoint of peaks and valleys method 
  #tried hampel filtering 
#Fix directories and Gdrive flows 

####################################
