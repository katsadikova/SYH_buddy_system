library(fungible)
library(pheatmap)
library(tidyverse)
library(kableExtra)
library(gtsummary)
library(expss)
library(haven)
library(sjlabelled)
library(readxl)
library(readr)
library(ggplot2)
library(factoextra)
library(psych)

# Read in buddy data (download 10/17/2022)
d <- read_csv("Dropbox/PC/Documents/PHS/Research/WIC study (Suzanne)/SYH/Buddy algorithm/buddy_10172022.csv")

#Prep data - need to recode the categoricals & make dummies for multi-answer questions

d1 <- d %>%
  mutate(
    # Recode categorical variables to have numeric formats
    gender_n = case_when(
      gender=="Woman" ~ 1, 
      gender=="Man"~2, 
      gender=="Non-binary"~3, 
      gender=="Gender non-conforming"~4, 
      gender=="Other - please specify"~99
    ),
    trans_n = case_when(
      trans=="No" ~ 0, 
      trans=="Yes"~1, 
      trans=="Prefer not to disclose" ~ 2, 
      trans=="Don't know"~3
    ),
    hisp_n = case_when(
      hisp=="No, not of Hispanic, Latino, or Spanish origin" ~ 0, 
      hisp=="Yes, Mexican, Mexican American, Chicano"~1, 
      hisp=="Yes, Puerto Rican"~2, 
      hisp=="Yes, Cuban"~3, 
      hisp=="Yes, other Hispanic, Latino, or Spanish origin - please specify below"~99
    ),
    hisp_bin = case_when(
      hisp=="No, not of Hispanic, Latino, or Spanish origin" ~ 0, 
      T ~ 1
    ),
    r_wt=case_when(race=="White"~1, T~0),
    r_bl=case_when(race=="Black or African American"~1, T~0),
    r_ot=case_when(race=="American Indian or Alaska Native" |
                   race=="Asian or Pacific Islander" |
                   race=="Other - please specify below" ~ 1, 
                   T~0),
    lang_eng=case_when(lang=="English"~1, T~0),
    lang_spa=case_when(lang=="Spanish"~1, T~0),
    lang_o=case_when(lang=="Other"~1, T~0),
    rel_n=case_when(
      relationship=="Married"~1, 
      relationship=="Partnered"~2, 
      relationship=="Single"~0, 
      relationship=="Divorced"~3, 
      relationship=="Widowed"~4, 
      relationship=="Other - please specify below"~99
    ),
    child_n=case_when(
      children=="Yes"~1, 
      children=="No"~0
    ),
    pets_n=case_when(
      pets=="Yes"~1, 
      pets=="No, and don't intend to"~2, 
      pets=="No, but would like to"~3
    ),
    pet_want_cat=case_when(pet_want=="Cat(s)"~1, T~0),
    pet_want_dog=case_when(pet_want=="Dog(s)"~1, T~0),
    pet_want_bird=case_when(pet_want=="Bird(s)"~1, T~0),
    pet_want_rept=case_when(pet_want=="Reptile(s)"~1, T~0),
    pet_want_rod=case_when(pet_want=="Rodent(s)"~1, T~0),
    pet_want_oth=case_when(pet_want=="Other - please specify below"~1, T~0),
    edu_n=case_when(
      edu=="Less than high school"~0, 
      edu=="High school or equivalent (GED)"~1, 
      edu=="Some college, no degree"~2, 
      edu=="Vocational or trade school"~3, 
      edu=="Associates degree"~4, 
      edu=="Degree from a 4-year college"~5, 
      edu=="Post-graduate degree"~6, 
      edu=="Other - please specify below"~99
      ),
    occ_ft = case_when(grepl("Full-time job",occup)~1, T~0), 
    occ_pt = case_when(grepl("Part-time job",occup)~1, T~0),
    occ_st = case_when(grepl("Student",occup)~1, T~0),
    occ_hm = case_when(grepl("Homemaker",occup)~1, T~0),
    occ_un = case_when(grepl("Unemployed",occup)~1, T~0),
    occ_o = case_when(grepl("Other - please specify below",occup)~1, T~0),
    relig_n = case_when(
      relig=="Yes, very much"~4, 
      relig=="Yes, somewhat"~3, 
      relig=="Not sure or prefer not to answer"~2, 
      relig=="No, not really"~1, 
      relig=="No, not at all"~0
    ),
    relig_spec = case_when(
      relig == "Not sure or prefer not to answer" | relig == "No, not really" | relig == "No, not at all" ~ "None",
      T~relig_which
    ),
    fun_ex = case_when(grepl("Exercise", fun) ~ 1, T~0),
    fun_cb = case_when(grepl("Cook or bake",fun) ~ 1, T~0),
    fun_ga = case_when(grepl("Garden", fun) ~ 1, T~0),
    fun_fa = case_when(grepl("Spend time with family",fun) ~ 1, T~0),
    fun_fr = case_when(grepl("Spend time with friends",fun) ~ 1, T~0),
    fun_dd = case_when(grepl("Go out to dinner & drinks",fun) ~ 1, T~0),
    fun_tv = case_when(grepl("Watch TV",fun) ~ 1, T~0),
    fun_vg = case_when(grepl("Play video games",fun) ~ 1, T~0),
    fun_ot = case_when(grepl("Other - please specify below",fun) ~ 1, T~0),
    cigs_n = case_when(cigs=="Yes"~2, 
                       cigs=="No"~0, 
                       cigs=="Used to"~1),
    alc_n = case_when(alc=="Yes"~2, 
                      alc=="No"~0, 
                      alc=="Used to"~1),
    mj_n = case_when(mj=="Yes"~2, 
                     mj=="No"~0, 
                     mj=="Used to"~1),
    dm_n = case_when(dm=="No"~0,
                     dm=="Yes"~1,
                     dm=="Don't know"~2),
    disab_n = case_when(disab=="No"~0,
                        disab=="Yes"~1)
  ) %>%
  dplyr::select(c(1,3,4,35:75))

names(d1)

summary(as.factor(d1$cluster))

# Describe data overall and by cluster



# Create unlabeled data set (select only variables that we think predict clusters)
# Drop variables with no variability (they can't be properly standardized)
d2 <- d1 %>%
  dplyr::select(c(4:17, 22:25, 27:29, 31:42))

d3<-scale(d2)

names(d2)

# Run classic k-means on the unlabeled data
set.seed(12345)
kclu=kmeans(d3,6,25,nstart=25)
kcluster=kclu$cluster
d_kclu <- cbind(d1,kcluster)
tab_kclu <- table(as.factor(d$cluster), as.factor(kclu$cluster))
tab_kclu

#Plot agreement
ggplot(d_kclu,aes(cluster,kcluster))+
  geom_count(aes(colour=as.factor(cluster)))
#Plot the clusters
fviz_cluster(kclu, d2, ellipse.type = "norm")

cohen.kappa(tab_kclu)  

# Run kmeans with specified centers:
set.seed(12345)
kclu_c=kmeans(d3,centers=d3[c(1,5,15,19,20,24),],100,nstart=25)

kcluster_c=kclu_c$cluster
d_kclu_c <- cbind(d1,kcluster_c)
tab_kclu_c <- table(as.factor(d$cluster), as.factor(kclu_c$cluster))
tab_kclu_c

#Plot agreement
ggplot(d_kclu_c,aes(cluster,kcluster_c))+
  geom_count(aes(colour=as.factor(cluster)))
#Plot the clusters
fviz_cluster(kclu_c, d2, ellipse.type = "norm")

cohen.kappa(tab_kclu_c) 

#K-medoids
set.seed(245)
kmclu=cluster::pam(d3,6,nstart=25) 
kmcluster=kmclu$cluster
d_kmclu <- cbind(d1,kcluster,kmcluster)
table(as.factor(d1$cluster), as.factor(kmclu$cluster))

check <- data.frame(cbind(d1$cluster,d1$centroid,kcluster,kmcluster)) %>%
  arrange(d1$cluster)

#Plot agreement
ggplot(d_kmclu,aes(cluster,kmcluster))+
  geom_count(aes(colour=as.factor(cluster)))
#Plot the clusters
fviz_cluster(kmclu, d2, ellipse.type = "norm")


