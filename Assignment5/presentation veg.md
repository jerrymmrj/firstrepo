Presentation veg
========================================================
author:Ruijie Ma,Han Xiao,Tianlang Yang
date: 3/18/2018
autosize: true

Preview
========================================================

- graph
- Analysis

code
========================================================

```
           Year       Geo Level      State ANSI          Region 
             13               2               2               6 
      Commodity       Data Item          Domain Domain Category 
              5              54              13             240 
          Value 
           1271 
```
Graph of Broccoli
========================================================
![plot of chunk unnamed-chunk-2](presentation veg-figure/unnamed-chunk-2-1.png)
Graph of Cauliflower
========================================================
![plot of chunk unnamed-chunk-3](presentation veg-figure/unnamed-chunk-3-1.png)
Analysis
========================================================
##We use data of LD50 for rats as our source of toxicity measurement. These data comes from www.pmep.cce.cornell.edu and pubchem.ncbi.nlm.nih.gov. We tidy the veg1 dataset using dplyr package which provides simple verbs, functions that correspond to the most common data manipulating tasks. For example, we use ???rename??? to rename the columns of a data frame in R. Also, we use ???separate??? to a single character column into multiple columns. After tidying the data, we find out that only broccoli and cauliflower were applied with restricted use chemicals. All the other commodities including brussel sprouts, vegetable totals, and vegetable other are not applied with restricted use chemicals. In these restricted use chemicals, some of the active ingredients have high LD50 levels in unit of mg/kg which is less toxic to rats or any animals.

Analysis
=======================================================
## For example, Abamectin in insecticide has LD50 of 5620-8350 mg/kg and Cyfluthrin in insecticide has LD50 of 5000 mg/kg. However, most of the restricted use chemicals have low LD50 levels which means they???re very harmful to rats or any animals. For example, Naled in insecticide has LD50 of 12-48 mg/kg and Fenpropathrin in insecticide has LD50 of 1.9-12.5 mg/kg. These active ingredients can harm or even kill the animals with relative low doses. Also, animals with low body weights are more vulnerable to these toxic active ingredients. Therefore, we need to be extremely careful about these restricted use chemicals. 
