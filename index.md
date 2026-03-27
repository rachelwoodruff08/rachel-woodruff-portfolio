---
layout: default
---

Welcome to my professional portfolio! This repository hosts a collection of my publicly shareable work in **epidemiology**, **public health informatics**, and **data science**.

## About Me
I’m a public health scientist who’s spent over two decades turning complex data into meaningful action through analysis, interpretation and visualization. From leading national surveillance systems to modernizing informatics platforms, I’ve built a career around making data work smarter for public health. I love digging into messy datasets and collaborating across teams to solve real-world problems. With a foundation in statistical analysis—and a Data Science Specialization underway—I’m now focused on bringing those skills to roles where data science drives impact.

---
A selection of projects that demonstrate my work with data and informatics.

## Project 1: [State and City TB Report](https://www.cdc.gov/tb-data/2021-state-city-report/index.html#cdc_report_pub_study_section_6-additional-information)  
The 2021 State and City Tuberculosis Report presents key process and outcome measures, covering a range of topics, for tuberculosis (TB) control programs in the United States. For this report, I did the analysis and interpretation which I used to develop the visualizations and narrative in the report. I worked with web-designers to layout the report using Responsive Web Design. 

<div align="center"> <img src="TB_Report_Screenshot 2026-03-26 153214.png" width = "60%"> </div>

## Project 2: National Health and Nutrition Examination Survey (NHANES) 
NHANES is designed to assess the health and nutritional status of children and adults in the United States. It combines traditional health interviews with physical examinations and laboratory tests. In 1999-2000 and 2011-2012 I co-led the tuberculosis component of NHANES. In the earlier cycle, I designed the questions for the survey and performed quality assurance site visits for TB skin test placement and reading and TB blood test collection. I also spent a lot of time with the data, working alongside a statistician to fix complex measurement issues and co-authoring a major paper on our findings. After the initial study results were published, I did my own deep dive into the data associated with non-U.S.-born persons and published my findings as first author. By the 2011-2012 cycle, I was the primary lead on assuring quality of TB blood test collection, analyzing the data and publishing the findings.

<img src="https://github.com/rachelwoodruff08/portfolio/blob/main/AP_Process%20Map_Screenshot%202026-03-26%20155410.png" width="40%"> <img src="https://github.com/rachelwoodruff08/portfolio/blob/main/AP_Process%20Map_Screenshot%202026-03-26%20155410.png" width="40%">

### Data Processing Highlights
I used R to to manage and analyze NHANES data for the country of birth analysis. Click below to see a snippet of code I wrote (and click again to see the full R script).
<!-- Level 1: Snippet -->
<details>
<summary><strong>View R Code Snippet</strong> (Click to expand)</summary>

<div markdown="1">

```r
# ——— SHORT SNIPPET ———

## create survey design object  
design <- svydesign( id = ~SDMVPSU, 
                     strata = ~SDMVSTRA, 
                     nest = TRUE, 
                     weights = ~WTMEC2YR,
                     data = fbnhabh       #change the dataset for each statistical comparison
)    

## subsetting to 6 years and over 
fb.designs <- update(subset(design, RIDAGEYR>=6))
#fb.designs <- update(subset(design, RIDAGEYR>=6 & yrsUS10=='<10'))
#fb.designs <- update(subset(design, RIDAGEYR>=6 & yrsUS10=='10+'))


## Jacknife weights analysis
fb.designs.jkn<- as.svrepdesign(fb.designs, type="JKn")
```
</div>

<!-- Level 2: Full Script -->

<details>
  
  <summary><strong>View Full R Script</strong> (Click to expand)</summary>

<div markdown="1">

```r
# ——— FULL SCRIPT ———

## set up suitable path
#setwd("Y:/Data")  

setwd("H:/Share/Rachel/SEOIB/NHANES stuff/Country of Birth")
load( "nhanes.1112.rda" ) 

library(survey) 
library(msm)

mec.1112.df <- subset(nhanes.1112.df, RIDSTATR==2, select=-RIDSTATR)

#mec6 <- subset(mec.1112.df, RIDAGEYR>=6 & !is.na(top4))

#table (mec6.civp$RIAGENDR, useNA='always')
#table(mec6$yrsUS5,  useNA="always")

# country of birth DMDBORN4 (excluding refused and unknown)
mec.1112.df$origin <- ifelse(mec.1112.df$DMDBORN4 ==1,0, 
                             ifelse(mec.1112.df$DMDBORN4 ==2,1,NA)
)
mec.1112.df$origin <- factor(mec.1112.df$origin, levels=c(0,1), labels = c("US", "FB")) 

#table(mec.1112.df$origin, useNA="always") 

## race/ethnicity 
## recode RIDRETH3 to represent FB black, hispanic, Asian - 
## RIDRETH3 has no missing     
## 1, 2 Mexican American and other Hispanic -> 1
## 3 non-Hispanic White -> NA
## 4 non-Hispanic Black -> 2
## 6 non-Hispanic Asian -> 3
## 7 other -> NA 
mec.1112.df$raceeth <- ifelse (mec.1112.df$RIDRETH3 %in% c(1,2), 1,
                               ifelse (mec.1112.df$RIDRETH3 == 4,2,
                                       ifelse (mec.1112.df$RIDRETH3 == 6,3,NA
                                       )))

mec.1112.df$raceeth <- factor(mec.1112.df$raceeth, levels=c(1,2,3), 
                              labels=c("HIS","NHB","NHA")
) 

# table (mec.1112.df$raceeth, useNA='always')
# table (nhanes.1112.df$RIDRETH3, nhanes.1112.df$DMDBORN4, useNA='always')

mec.1112.df$raceeth2 <- ifelse (mec.1112.df$RIDRETH3 %in% c(1,2, 4, 6), 1,
                               ifelse (mec.1112.df$RIDRETH3 == 3,2,NA
                                       ))

mec.1112.df$raceeth2 <- factor(mec.1112.df$raceeth2, levels=c(1,2), 
                              labels=c("NHABH","NHW")
) 

# table (mec.1112.df$raceeth2, mec.1112.df$rvct.age, mec.1112.df$igra, useNA='always')

### RECATEGORIZE TO <10, 10+
mec.1112.df$yrsUS10 <- ifelse(mec.1112.df$DMDYRSUS %in% c(1,2,3), 1,                  
                              ifelse(mec.1112.df$DMDYRSUS %in% c(77,88,99), NA, 2
                              )
)	

mec.1112.df$yrsUS10 <- factor(mec.1112.df$yrsUS10, levels=c(1,2), labels = c("<10", "10+")) 

########################################################################### 
##  To combine the FB raceeth subset AND the top 5 countries subset?????
##  Have to create the 'top 5' variable first

mec.1112.df$top5 <- rep(5, nrow(mec.1112.df)) 
mec.1112.df$top5[mec.1112.df$DMQ112 == 14] <- 0	#"Mex"
mec.1112.df$top5[mec.1112.df$DMQ112 == 19 | mec.1112.df$DMQ125 == 18] <- 1	#"Phil" 
mec.1112.df$top5[mec.1112.df$DMQ125 == 7] <- 2	#"Ind"
mec.1112.df$top5[mec.1112.df$DMQ125 == 5] <- 3	#"Chi"  
mec.1112.df$top5[mec.1112.df$DMQ125 == 24] <- 4	#"Viet" 
mec.1112.df$top5[mec.1112.df$top5 == 5] <- NA
mec.1112.df$top5 <- factor(mec.1112.df$top5, levels=0:4, labels=c("Mex","Phil","Ind","Chi","Viet"))

# table(mec.1112.df$top5, useNA="always")

# IGRA calculated using antigen, mitogen and nil variables (this is what we want to use)
mec.1112.df$antigen <- with(mec.1112.df, LBXTBA) 
mec.1112.df$nil <- with(mec.1112.df, LBXTBN) 
mec.1112.df$mitogen <- with(mec.1112.df, LBXTBM) 
# mec.1112.df$igra <- with(mec.1112.df, LBXTBIN)

mec.1112.df$diff.an <- mec.1112.df$antigen-mec.1112.df$nil 
mec.1112.df$diff.am <- mec.1112.df$mitogen-mec.1112.df$nil 


mec.1112.df$igra <- (mec.1112.df$nil <= 8 & mec.1112.df$diff.an >= .35 & 
                       mec.1112.df$diff.an >= 0.25*mec.1112.df$nil) ## 1 is pos, 0 is neg 

mec.1112.df$igra[mec.1112.df$nil <= 8 & mec.1112.df$diff.an < .35 & 
                   mec.1112.df$diff.am < 0.5] <- 3
mec.1112.df$igra[mec.1112.df$nil <= 8 & mec.1112.df$diff.an >= .35 & 
                   mec.1112.df$diff.an < 0.25*mec.1112.df$nil & 
                   mec.1112.df$diff.am < 0.5] <- 3
mec.1112.df$igra[mec.1112.df$nil > 8] <- 3 
mec.1112.df$igra[is.na(mec.1112.df$antigen) | is.na(mec.1112.df$nil) | 
                   is.na(mec.1112.df$mitogen)] <- NA

mec.1112.df$igra <- factor(mec.1112.df$igra, levels=0:1, labels=c("neg","pos"))

#table (mec.1112.df$igra, useNA='always')

############################################################################
##    Positivity 
############################################################################

## all the IGRA with original weights

## add a 'one' variable to use for total weighted counts 
mec.1112.df$one <- 1

## all foreign born
fb      <- mec.1112.df[mec.1112.df$origin=='FB' & (!is.na(mec.1112.df$origin)),]
fb$gp   <-'nusb'

## non-hispanic white
nhw     <-fb[fb$raceeth2=='NHW' & (!is.na(fb$raceeth2)) ,]
nhw$gp  <-'nhw'
fbnhw  <-rbind(fb,nhw)

table(fbnhw$raceeth2, fbnhw$origin, useNA="always")
table(fbnhw$gp, useNA="always")

## non-hispanic Asian, non-hispanic black, hispanic
nhabh   <-fb[fb$raceeth2=="NHABH" & (!is.na(fb$raceeth2)) ,]
nhabh$gp <- 'nhabh'
fbnhabh  <-rbind(fb, nhabh)

table(fbnhabh$raceeth2, fbnhabh$origin, useNA="always")
table(fbnhabh$gp, useNA="always")

## China
chi     <-fb[fb$top5=='Chi' & (!is.na(fb$top5)) ,]
chi$gp  <-'chi'
fbchi  <-rbind(fb,chi)

table(fbchi$top5, fbchi$origin, useNA="always")
table(fbchi$gp, useNA="always")

## India
ind     <-fb[fb$top5=='Ind' & (!is.na(fb$top5)) ,]
ind$gp  <-'ind'
fbind  <-rbind(fb,ind)

table(fbind$top5, fbind$origin, useNA="always")
table(fbind$gp, useNA="always")

## Mexico
mex     <-fb[fb$top5=='Mex' & (!is.na(fb$top5)) ,]
mex$gp  <-'mex'
fbmex  <-rbind(fb,mex)

table(fbmex$top5, fbmex$origin, useNA="always")
table(fbmex$gp, useNA="always")

## Philippines
phi     <-fb[fb$top5=='Phil' & (!is.na(fb$top5)) ,]
phi$gp  <-'phi'
fbphi  <-rbind(fb,phi)

table(fbphi$top5, fbphi$origin, useNA="always")
table(fbphi$gp, useNA="always")

## Vietnam
vie     <-fb[fb$top5=='Viet' & (!is.na(fb$top5)) ,]
vie$gp  <-'vie'
fbvie  <-rbind(fb,vie)

table(fbvie$top5, fbvie$origin, useNA="always")
table(fbvie$gp, useNA="always")

##mec.1112.df$gp  <-  c(rep('c',nrow(mec.1112.df)/2),rep('s',nrow(mec.1112.df)/2))
## round results to numdig digits 
numdig <- 2

## create survey design object  
design <- svydesign( id = ~SDMVPSU, 
                     strata = ~SDMVSTRA, 
                     nest = TRUE, 
                     weights = ~WTMEC2YR,
                     data = fbnhabh       #change the dataset for each statistical comparison
)    

## subsetting to 6 years and over 
fb.designs <- update(subset(design, RIDAGEYR>=6))
#fb.designs <- update(subset(design, RIDAGEYR>=6 & yrsUS10=='<10'))
#fb.designs <- update(subset(design, RIDAGEYR>=6 & yrsUS10=='10+'))


## Jacknife weights analysis
fb.designs.jkn<- as.svrepdesign(fb.designs, type="JKn")

## OVERALL 

#############################################################################
#############################################################################
#############################################################################

igra_gp <- svyby(
  formula = ~igra, 
  by = ~gp, # change this one variable to the group variable
  design = fb.designs.jkn, 
  FUN = svymean, # doesn't work with svyciprop
  na.rm.all = TRUE,
  na.rm = TRUE,
  na.rm.by = TRUE, 
  covmat=TRUE
)
## get the point estimate
T=coef(igra_gp)[3:4] 
Tdif=T[1]-T[2]
## get the covariate matrix
igra_gp.vcov <- vcov(igra_gp)[3:4,3:4]
## calculate SE for difference between two groups
Tdiff.stderr <- sqrt(t(c(1,-1)) %*% igra_gp.vcov %*% c(1,-1) ) 

se <- .80 # 95% CI (0.77, 0.84)
sp <- .97 # 95% CI (0.94, 0.99)
se.u <- .84; se.l <- .77
sp.u <- .99; sp.l <- .94

# approximate the standard errrors 
se.stderr <- mean(abs(se-c(se.l,se.u)))/qnorm(.975) 
sp.stderr <- mean(abs(sp-c(sp.l,sp.u)))/qnorm(.975) 

### USE deltamethod function in msm package to calculate se for the difference in LTBI Prevalence   #######################

prev <- function(x1, x2, x3, x1.se, x2.se, x3.se){
  # point estimate of LTBI prev
  Pdif=x1/(x2-(1-x3))
  
  # delta method to estimate standard error, and calculate CI
  covmt=(as.matrix( rbind(c(x1.se,0,0), c(0,x2.se,0),c(0,0,x3.se))))^2
  ltbi_se=deltamethod(~(x1/(x2-1+x3)),c(x1,x2,x3), covmt)
  
  # calculate CI based on truncated normal distribution
  ltbi_ci_low=qtnorm(0.025,Pdif,ltbi_se,lower=-1,upper=1,log.p=FALSE)
  ltbi_ci_up=qtnorm(0.975,Pdif,ltbi_se,lower=-1,upper=1,log.p=FALSE)
  
  return(c( 
    
    Pdifference=Pdif,
    DifCI_low=ltbi_ci_low,
    DifCI_up=ltbi_ci_up,
    Difse=ltbi_se
    
  )
  )
}

prev(Tdif, se, sp,Tdiff.stderr, se.stderr,sp.stderr)
```
</div>

</details>

</details>


## Project 3: [Capacity Building Assistance Tracking System (CTS)](https://wwwn.cdc.gov/CTS2024)
CTS is a web-based platform that streamlines the submission, triage, planning, and closeout of capacity building assistance requests from health departments and community based organizations. The system ensures consistent workflows, transparent coordination, and accurate reporting of capacity building activities for the HIV workforce. As the informatics lead for CTS, I serve as a bridge between programmatic teams, external partners, and the CTS development team—translating operational needs into clear functional requirements and ensuring the system evolves in alignment with CDC program priorities.

<img src="https://github.com/rachelwoodruff08/portfolio/blob/main/AP_Process%20Map_Screenshot%202026-03-26%20155410.png" width="40%"> <img src="https://github.com/rachelwoodruff08/portfolio/blob/main/TA_System%20Flowchart_Screenshot%202026-03-26%20155653.png" width="40%">

---
## Check out my [publications page](publications) for a list of my peer-reviewed research articles    
---

## 📬 Contact
- **Email:** rachelwoodruff08@gmail.com
- **LinkedIn:** [linkedin.com/in/rachelswoodruff](https://linkedin.com/in/rachelswoodruff)
- **GitHub:** [github.com/rachelwoodruff08](github.com/rachelwoodruff08)

---

### 🔍 Notes
- This portfolio includes only publicly shareable work. For internal projects or additional details, please reach out directly.
