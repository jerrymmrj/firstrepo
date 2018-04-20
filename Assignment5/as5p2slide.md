as5p2slide
========================================================
author: Han Xiao,Ruijie Ma, Tianlang Yang
date: 1/1/1
autosize: true

First Slide
========================================================

For more details on authoring R presentations please visit <https://support.rstudio.com/hc/en-us/articles/200486468>.

- Bullet 1
- Bullet 2
- Bullet 3

Slide With Code
========================================================


```
           Year       Geo Level      State ANSI          Region 
             13               2               2               6 
      Commodity       Data Item          Domain Domain Category 
              5              54              13             240 
          Value 
           1271 
```

Slide With Plot
========================================================


```r
veg4
```

```
# A tibble: 48 x 6
   Commodity Domain      Type    `Active ingredient o… `EPA Pesticide Che…
   <chr>     <chr>       <chr>   <chr>                 <chr>              
 1 BROCCOLI  RESTRICTED… HERBIC… "PARAQUAT "           " 6160"            
 2 BROCCOLI  RESTRICTED… INSECT… "ABAMECTIN "          " 12280"           
 3 BROCCOLI  RESTRICTED… INSECT… "BETA-CYFLUTHRIN "    " 11883"           
 4 BROCCOLI  RESTRICTED… INSECT… "BIFENTHRIN "         " 12882"           
 5 BROCCOLI  RESTRICTED… INSECT… "CHLORANTRANILIPROLE… " 9010"            
 6 BROCCOLI  RESTRICTED… INSECT… "CHLORPYRIFOS "       " 5910"            
 7 BROCCOLI  RESTRICTED… INSECT… "CYFLUTHRIN "         " 12883"           
 8 BROCCOLI  RESTRICTED… INSECT… "EMAMECTIN BENZOATE " " 12280"           
 9 BROCCOLI  RESTRICTED… INSECT… "ESFENVALERATE "      " 10930"           
10 BROCCOLI  RESTRICTED… INSECT… "IMIDACLOPRID "       " 12909"           
# ... with 38 more rows, and 1 more variable: `Toxicity
#   Measurements` <chr>
```

toxic table
========================================================

```r
toxicity
```

```
# A tibble: 48 x 1
   `Toxicity Measurements`
   <chr>                  
 1 20-150 mg/kg           
 2 5620-8350 mg/kg        
 3 20-150 mg/kg           
 4 11 mg/kg               
 5 869-1271 mg/kg         
 6 54-70 mg/kg            
 7 5000 mg/kg             
 8 82-270 mg/kg           
 9 869-1271 mg/kg         
10 3129 mg/kg             
# ... with 38 more rows
```
