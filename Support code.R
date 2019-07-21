
library(feather)


#Load feather file and DT_final file
DT_ens <- read_feather("DT_ens.feather")
DT_final <- read.csv("DT_final.csv")


#Paste Values
DT_ens$value <- DT_final$value[1:5712]

#convert to data table
library(data.table)
DT_ens <- data.table(DT_ens)

#Write csv
write.csv(DT_ens, "DT_ens.csv")
