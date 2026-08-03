# modify NMUSF40 and NMUSF24 YSI to be two files ,
# could be a helper function 


# 1. Read the CSV file
file_path<-"data/formatted/"
filename<-"2026-01-20_NMUSF02_NMUSF24_YSI_Teak.csv"
data <- read.csv(paste0(file_path, filename))

# 2. Define your target index (e.g., split after row 100)
split_index <- 1011 # manually inspected, could have code for this USF24 vs USF40 

# 3. Create the two halves
part1 <- data[1:split_index, ]
part2 <- data[(split_index + 1):nrow(data), ]

# 4. Save the two halves into new CSV files
write.csv(part1, paste0(file_path,"2026-01-20_NMUSF02_YSI_Teak.csv"), row.names = FALSE)
write.csv(part2, paste0(file_path,"2026-01-20_NMUSF24_YSI_Teak.csv"), row.names = FALSE)

# 5. erase original file # must be in formatted folder 
file.remove(paste0(file_path, filename))



