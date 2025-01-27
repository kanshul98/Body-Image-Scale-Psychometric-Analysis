library(readxl)
library(tidyverse)
library(psych)

bi_scale <- read_excel("/Users/anshulkandpal/Downloads/BI_7_items_for_analysis.xlsx") # needs to be altered for individual PC
view(bi_scale)

# we convert all the characters into numeric strings
bi_scale <- bi_scale %>%
  mutate(q1 = case_when(
    q1 == "STRONGLY DISAGREE" ~ "1",
    q1 == "DISAGREE" ~ "2",
    q1 == "NEITHER AGREE OR DISAGREE" ~ "3",
    q1 == "AGREE" ~ "4",
    q1 == "STRONGLY AGREE" ~ "5",
    TRUE ~ q1
  ))

bi_scale <- bi_scale %>%
  mutate(q2 = case_when(
    q2 == "STRONGLY DISAGREE" ~ "1",
    q2 == "DISAGREE" ~ "2",
    q2 == "NEITHER AGREE OR DISAGREE" ~ "3",
    q2 == "AGREE" ~ "4",
    q2 == "STRONGLY AGREE" ~ "5",
    TRUE ~ q2
  ))

bi_scale <- bi_scale %>%
  mutate(q3 = case_when(
    q3 == "STRONGLY DISAGREE" ~ "1",
    q3 == "DISAGREE" ~ "2",
    q3 == "NEITHER AGREE OR DISAGREE" ~ "3",
    q3 == "AGREE" ~ "4",
    q3 == "STRONGLY AGREE" ~ "5",
    TRUE ~ q3
  ))

bi_scale <- bi_scale %>%
  mutate(q4 = case_when(
    q4 == "STRONGLY DISAGREE" ~ "1",
    q4 == "DISAGREE" ~ "2",
    q4 == "NEITHER AGREE OR DISAGREE" ~ "3",
    q4 == "AGREE" ~ "4",
    q4 == "STRONGLY AGREE" ~ "5",
    TRUE ~ q4
  ))

bi_scale <- bi_scale %>%
  mutate(q5 = case_when(
    q5 == "STRONGLY DISAGREE" ~ "1",
    q5 == "DISAGREE" ~ "2",
    q5 == "NEITHER AGREE OR DISAGREE" ~ "3",
    q5 == "AGREE" ~ "4",
    q5 == "STRONGLY AGREE" ~ "5",
    TRUE ~ q5
  ))

bi_scale <- bi_scale %>%
  mutate(q6 = case_when(
    q6 == "STRONGLY DISAGREE" ~ "1",
    q6 == "DISAGREE" ~ "2",
    q6 == "NEITHER AGREE OR DISAGREE" ~ "3",
    q6 == "AGREE" ~ "4",
    q6 == "STRONGLY AGREE" ~ "5",
    TRUE ~ q6
  ))

bi_scale <- bi_scale %>%
  mutate(q7 = case_when(
    q7 == "STRONGLY DISAGREE" ~ "1",
    q7 == "DISAGREE" ~ "2",
    q7 == "NEITHER AGREE OR DISAGREE" ~ "3",
    q7 == "AGREE" ~ "4",
    q7 == "STRONGLY AGREE" ~ "5",
    TRUE ~ q7
  ))

# ALTERNATIVELY you can perform the transformation process through a single codeline, given below
bi_scale <- bi_scale %>%
  mutate(across(starts_with("q"), ~ case_when(
    . == "STRONGLY DISAGREE" ~ "1",
    . == "DISAGREE" ~ "2",
    . == "NEITHER AGREE OR DISAGREE" ~ "3",
    . == "AGREE" ~ "4",
    . == "STRONGLY AGREE" ~ "5",
    TRUE ~ .
  )))


# now we convert these numeric strings into pure numerics so that we can analyse the dataset
bi_scale[] <- lapply(bi_scale, as.numeric)

desc_stat <- describe(bi_scale) #descriptive stats of BI scale items
print(desc_stat)

# now, we add up the individual scores to "total_score" and put it alongside the other columns
bi_scale$total_score <- rowSums(bi_scale[, c("q1", "q2", "q3", "q4", "q5", "q6", "q7")])
view(bi_scale)

# calculate item total correlation
item_total_corr <- cor(bi_scale[, c("q1", "q2", "q3", "q4", "q5", "q6", "q7")], bi_scale$total_score)
print(item_total_corr)

# now we calculate the inter-item correlation

# step 1: select only the columns with the items (q1 to q7)
item_data <- bi_scale[, c("q1", "q2", "q3", "q4", "q5", "q6", "q7")]
# step 2: calculate inter-item correlations
inter_item_corr <- cor(item_data, use = "complete.obs") # complete.obs is used to avoid missing values
# step 3: print the correlation matrix
print(inter_item_corr)

# since there is an overloading amongst q6 and q7, we remove q7
bi_scale <- bi_scale %>% 
  select(-q7)

view(bi_scale)

# now we find the rowSum value again for total of all responses
bi_scale$total_score <- rowSums(bi_scale[, c("q1", "q2", "q3", "q4", "q5", "q6")])

# now we calculate the NEW item total correlation
item_total_corr <- cor(bi_scale[, c("q1", "q2", "q3", "q4", "q5", "q6")], bi_scale$total_score)
print(item_total_corr)

#now we calculate the inter-item correlation AGAIN

# step 1
item_data2 <- bi_scale[, c("q1", "q2", "q3", "q4", "q5", "q6")]
# step 2
inter_item_corr2 <- cor(item_data2) # we dont need complete.obs as we know there are no NA values
# step 3
print(inter_item_corr2)



# now that we are done with Inter-Item Correlation,
# We move towards Reliability analysis using Cronbach Alpha

cronbach_alpha <- psych::alpha(item_data2)
print(cronbach_alpha)

# Now we conduct EFA (Exploratory Factor Analysis to identify latent constructs)

# First we conduct KMO
kmo_result <- psych::KMO(item_data2)
print(kmo_result)

install.packages("REdaS") #pre-requisite to conducting Bartlett's Test
library(REdaS) # "Reliability and Exploratory Data Analysis for the Social Sciences"

# Then we conduct Bartlett's Test of Sphericity
bartlett_test <- bart_spher(item_data2)
print(bartlett_test)

# now we conduct Exploratory Factor Analysis (EFA)
efa_result <- fa(item_data2, nfactors = 1, rotate = "oblimin", fm = "pa")
print(efa_result)

# NOW WE GO WITH CONFIRMATORY FACTOR ANALYSIS (CFA)

install.packages("lavaan") # for latent variable analysis
library(lavaan)

cfa_model <- 'Factor1 =~ q1 + q2 + q3 + q4 + q5 + q6' # define CFA model

cfa_result <- cfa(cfa_model, data = item_data2)  #run it

summary(cfa_result, fit.measures = TRUE, standardized = TRUE)
















