
# Read the CSV file
file_path<-"/Users/marcelamendoza/Documents/UNM/RG2/code/data /formatted/"
filename<-"2026-05-07_NMUSF41_SCL_Teak12.csv"
target_time<- "13:58:01" # 24 hr format

data<- read.csv(paste0(file_path, filename))

# Find index with target time 
split_index <- which(data$Time == target_time)[1]

data <- data[(split_index + 1):nrow(data), ]

# plot before to review 
p <- ggplot(data = data, aes(x = 1:nrow(data), y = SPC.uS.cm.))+
  geom_point() +  # Generates a scatter plot
  geom_line() +   # Optional: adds connecting lines
  labs(title=filename, x="Index", y="SPC.uS.cm.")
print(p)

# 4. Save edited file 
write.csv(data, paste0(file_path,filename), row.names = FALSE)