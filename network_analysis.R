library(tidyverse)
library(tidygraph)
library(igraph)
library(ggraph)
library(navdata)
library(corrr)


# Read in buddy data (download 10/17/2022)
d <- read.csv("/Users/Kat/Library/CloudStorage/OneDrive-seeyourselfhealth.com/Data Analysis/Buddy system/buddy_10172022.csv")

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
  dplyr::select(c(2,3,4,35:75))

names(d1)
names(d1[, c(1, 4:17,22:25,27:29,31:42)])
summary(d1[, c(1, 4:17,22:25,27:29,31:42)])

# Create a network using correlations between observations #
res.cor <- d1[, c(1, 4:17,22:25,27:29,31:42)]
rownames(res.cor) <- d1[,1]
res.cor <- res.cor[,-1] %>%     # 1. remove the name label
  t() %>%                       # 2. transpose the matrix s.t. variables are rows, names are cols
  correlate() %>%               # 3. correlate the individuals based on their data
  shave(upper = TRUE) %>%       # 4. take only the top of the cor matrix (bottom redundant)
  stretch(na.rm = TRUE)         # 5. create pairwise correlation data for network analysis

# Standard approach - select pairs with high correlations #
res.cor.high <- res.cor %>%
  filter(r>=0.85)
# Problem: some people, who don't have very high correlations with others, are dropped
print(paste0("We lose ", 25-length(unique(c(res.cor.high$x,res.cor.high$y))), " people - they don't get paired with anyone"))

set.seed(1)
cor.graph.high <- as_tbl_graph(res.cor.high, directed = FALSE)
ggraph(cor.graph.high) + 
  geom_edge_link() + 
  geom_node_point() +
  geom_node_text(
    aes(label = name), size = 3, repel = TRUE
  ) +
  theme_graph()

res.cor.5 <- res.cor %>%
  # Top 5 highest values by group
  arrange(desc(r)) %>% 
  group_by(x) %>%
  slice(1:5) %>%
  mutate(
    y=case_when(r<0.7 ~ x,
                TRUE ~ y),
    r=case_when(x==y ~ 1,
                TRUE ~ r)
  ) 
res.cor.5<-res.cor.5[!duplicated(res.cor.5), ]

cor.graph.5 <- as_tbl_graph(res.cor.5, directed = FALSE)
res.cor.5 %>% 
  graph_from_data_frame() %>% 
  ggraph(layout = "fr") +
  geom_edge_link(alpha = .25, 
                 aes(width = r)) +
  geom_node_point(color = "blue", size = 2) + 
  geom_node_text(aes(label = name),  repel = TRUE)+
  scale_edge_width(range = c(0.5, 5))+ # control size
  theme_graph()+
  labs(title = 'Graph with weighted edges', 
       subtitle = 'Scaling add with scale_edge_width()')


set.seed(1)
cor.graph.5 <- as_tbl_graph(res.cor.5, directed = FALSE)
ggraph(cor.graph.5) + 
  geom_edge_link() + 
  geom_node_point() +
  geom_node_text(
    aes(label = name), size = 3, repel = TRUE
  ) +
  theme_graph()

res.cor.5.success <- res.cor.5 %>%
  mutate(
    orig_clust = case_when(substr(x, 1, 3) == substr(y,1,3) ~ 1,
                           TRUE ~0)
  ) %>%
  group_by(y) %>% 
  summarise(in_orig_clus = sum(orig_clust)) %>%
  mutate(
    found_clust = substr(y,1,3)
  ) %>%
  group_by(found_clust) %>%
  summarise(test = mean(in_orig_clus)+1)
  

clusts <- data.frame(t(table(d$cluster)))[,c(2:3)] %>%
  mutate(
    found_clust = substr(Var2,1,3)
  )
names(clusts) <- c("name", "freq","found_clust")

check.5 <- merge(clusts, res.cor.5.success, by='found_clust', all.x = TRUE) %>%
  mutate(
    prop_in_clust = test/freq
  ) %>% 
  dplyr::select(found_clust,prop_in_clust)
  
mean(check.5$prop_in_clust)


##############################################################################################
# Example 1 from : http://www.sthda.com/english/articles/33-social-network-analysis/136-network-analysis-and-manipulation-using-r/ #

# Example 2 with mtcars data #

res.cor <- mtcars [, c(1, 3:6)] 
# %>%  t()                           # (1) pull relevant vars
# %>% correlate() %>%                # (2) transpose so that car makes are now vars, and correlate car makes based on vars 3:6
# shave(upper = TRUE) %>%            # (3) 
# stretch(na.rm = TRUE) %>%          # (4)
# filter(r >= 0.998)                 # (5)
res.cor

set.seed(1)
cor.graph <- as_tbl_graph(res.cor, directed = FALSE)
ggraph(cor.graph) + 
  geom_edge_link() + 
  geom_node_point() +
  geom_node_text(
    aes(label = name), size = 3, repel = TRUE
  ) +
  theme_graph()

cor.graph %>% 
  activate(edges) %>% 
  arrange(desc(r))

as_tibble(cor.graph)
