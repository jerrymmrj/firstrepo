library(tidyverse)
library(readxl)

veg.1 <- read_xlsx("veg1.xlsx")

cnames.1 <- colnames(veg.1)

## try
n_distinct(veg.1[,1])

n_distinct(veg.1[,2])

unique(veg.1[,2])

## now get the count for each column

c <- apply(veg.1, 2, n_distinct)
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

cnames.3 <- colnames(veg.3)
cnames.3

veg.3

unique(veg.3[,"Commodity"])

unique(veg.3[,"Data"]) %>% print(n=60)

unique(veg.3[,"Domain"])

unique(veg.3[,"Category"])

unique(veg.3[,"Value"])

