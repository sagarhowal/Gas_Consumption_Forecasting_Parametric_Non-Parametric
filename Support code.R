
library(feather)



DT_ens <- read_feather("DT_ens.feather")

DT_ens$value <- DT_final$value[1:5712]

DT_ens <- data.table(DT_ens)
