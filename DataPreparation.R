#Data Preparation

library(data.table)

#Read all the Text Files
files <- list.files(pattern = "GasDataWeek*")

#Read Data
data <- rbindlist(lapply(files, function(x) cbind(fread(x))))

#Change Column names for better understanding
colnames(data) <- c("METER_ID", "TIME", "VALUE")
str(data)

#Check how many readings per household
head(table(data$METER_ID))
#25920 readings

#check if there are any housholds with less than 25920 readings
readings <- table(data$METER_ID)
table(readings)

#Remove housholds whihch have less than 25920 readings
times <- data[, .N, METER_ID]
full <- times[N == 25920, .(METER_ID)]
DT <- data[METER_ID %in% full[,METER_ID],]
length(unique(DT$METER_ID))
#1492 houses with full values


#Check NA Values
sum(is.na(DT))
#0 null values


#Order by ID and Time
DT_full <- DT[order(DT$METER_ID, DT$TIME)]

#Create a placeholder for time variable since the TIME contains duplicate values
#Due to Daylights savings
DT_full$t_place <- rep(1:25920, 1492)





DT_test <- DT_full[,list(sumamount = sum(VALUE), freq = .N), by = c("t_place")]


DT_final <- DT_test
DT_final <- DT_final[, -c("freq")]

colnames(DT_final) <- c("TIME", "VALUE")

#write.csv(DT_final, "DT_final.csv")


#Day of the week
n_weekdays <- c("Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday", "Monday")
n_weekdays <- rep(n_weekdays, 48)

n_weekdays <- sort(n_weekdays)

days <- vector()
for (x in unique(n_weekdays)){
  temp <- rep(x, 48)
  days <- append(days, temp)
}



#check for the repetation LHS of the repetation for the weekdays
length(DT_final$TIME) %% length(days)
#48

#So we need 48 extra values for the first day = TUESDAY
days_full <- append(days, rep("Tuesday", 48))


days_final <- c(rep(days, 77), rep("Tuesday", 48))


DT_final$n_weekdays <- days_final


colnames(DT_final) <- c("date_time", "value", "week")



dates_times <- unique(DT$TIME)
dates_times <- sort(dates_times)

head(dates_times)
tail(dates_times)
#The first date is 335 = 1st december 2009
#The final date is 880 = 29th June 2011

dates_original <- seq(as.Date("2009/12/1"), as.Date("2011/6/29"), "days")
dates_full <- vector()
for (x in dates_original){
  dates_full <- append(dates_full, rep(x, 48))
}


#seq.dates("12/01/09", "06/29/11", by = "days")

#Write the File
write.csv(DT_final, "DT_final.csv")












