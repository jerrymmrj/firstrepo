library(tidyverse)
library(readxl)

veg.1 <- read_xlsx("/Users/billyxiao/schoolworks/spring2018/ma415/MA415/assignment5/veg1.xlsx")

cnames.1 <- colnames(veg.1)
cnames.1
## try
# n_distinct(veg.1[,1])
# 
# n_distinct(veg.1[,2])
# 
# unique(veg.1[,2])

## now get the count for each column

c <- apply(veg.1, 2, n_distinct)
#?n_distinct
c


c[c>1]


d <- names(c[c==1])
d

e <- names(c[c>1])
e


veg.2 <- select(veg.1, e)

cnames.2 <- colnames(veg.2)
cnames.2

apply(veg.2, 2, n_distinct)

veg.3 <- dplyr::rename(veg.2, 
                       Geo = `Geo Level`, 
                       State = `State ANSI`,
                       Data = `Data Item`,
                       Category = `Domain Category`)
#?dplyr
cnames.3 <- colnames(veg.3)
cnames.3

# veg.3
# 
# unique(veg.3[,"Commodity"])
# 
# unique(veg.3[,"Data"]) %>% print(n=60)
# 
# unique(veg.3[,"Domain"])
# 
# unique(veg.3[,"Category"])
# 
# unique(veg.3[,"Value"])


 

              
yy <- separate(veg.3, Category, into = c("label", "quant"), sep=",")
yy
# 
# n_distinct(yy[,2])
# 
# 
# unique(yy[,"label"]) %>% print(n=30)

ru <- filter(yy, label=="RESTRICTED USE CHEMICAL")

ru1 <- ru %>% select(label, quant) %>% unique()

ru1 %>% print(n=30)



## get CAS #

## find info at https://cfpub.epa.gov/ecotox/  (go to beta)
## or
## https://comptox.epa.gov/dashboard


##  Toxicity > Effect Level
#