
###Code for generating interactive maps
### for the paper: "Navigating the Jungle of Concepts: An Analysis of Emerging Concepts in Biodiversity Financing"
# Script authors: Julia Qian Mao (julia.q.mao@svet.lu.se) & Nils Droste (nils.droste@svet.lu.se) 

## Install and load packages
required_packages <- c("tidyverse", "stm", "LDAvis", "stringr", "bibliometrix", "tm", "servr")
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(required_packages, library, character.only = TRUE)


getwd()

# Set working directory
setwd("/Users/juliamao/Desktop/Test3") 

# Load necessary library
#library(dplyr)

# Function to load and preprocess data
load_and_preprocess <- function(file_path, source_name) {
  read_bibliography(file_path) %>%
    rename(RA = "research_areas") %>%
    filter(!is.na(abstract), !is.na(journal)) %>%
    mutate(source = source_name)
}

# Load data separately using the function
data_bio <- load_and_preprocess("Data/Academic_literature/WoS/D_groups/savedrecs_tit_biofin.bib", "Bio")
data_con <- load_and_preprocess("Data/Academic_literature/WoS/D_groups/savedrecs_tit_confin.bib", "Con")
data_nat <- load_and_preprocess("Data/Academic_literature/WoS/D_groups/savedrecs_tit_natfin.bib", "Nat")


# Merge the datasets
combined_data <- bind_rows(data_bio, data_con, data_nat)

# Remove duplicates
cleaned_data <- distinct(combined_data, title, .keep_all = TRUE)

#Define the function to preprocess the data (this function will be used again later)
preprocess_data <- function(data) {
  data %>%
    select(abstract, year, journal, author, title, RA) %>%
    mutate(RA1 = str_extract(RA, "^[^,;&-]+"),
           RA1 = str_replace(RA1, "[\\\\-]+$", "")) %>%
    mutate(RDG = case_when(
      str_detect(RA1, "Business") ~ "G1:Business",
      str_detect(RA1, "Arts|Cultural Studies|Development Studies|Geography|International Relations|Literature|Philosophy|Psychology|Urban Studies") ~ "G2:Social Sciences and Humanities",
      str_detect(RA1, "Biodiversity|Environmental Sciences|Life Sciences|Meteorology|Oceanography|Physics|Plant Sciences|Water Resources|Science") ~ "G3:Natural Sciences",
      str_detect(RA1, "Agriculture|Computer Science|Energy|Engineering|Forestry|Thermodynamics") ~ "G4:Applied Sciences and Engineering",
      TRUE ~ "Other"  # Add a default case
    ))
}

# Preprocess the dataset: sub_cleaned_data01
sub_cleaned_data01 <- preprocess_data(cleaned_data)
# Preprocess each datasets
sub_data_bio01 <- preprocess_data(data_bio)
sub_data_con01 <- preprocess_data(data_con)
sub_data_nat01 <- preprocess_data(data_nat)



# Define function to process text data and prepare documents for STM
process_and_prepare_stm <- function(data) {
  processed <- textProcessor(data$abstract, metadata = data %>% select(year, journal, author, title, abstract, RA, RDG))
  out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
  out$meta <- out$meta %>%
    mutate(year = as.numeric(year),
           RDG = as.factor(RDG))
  return(out)
}

# Define function to fit STM models
fit_stm_model <- function(docs, vocab, meta, K) {
  model <- stm(documents = docs, vocab = vocab, K = K, prevalence = ~ year + RDG, max.em.its = 500, data = meta, init.type = "Spectral")
  prep <- estimateEffect(1:K ~ year + RDG, model, metadata = meta)
  labels <- labelTopics(model, n = 10)$prob
  list(model = model, prep = prep, labels = labels)
}

# Set the number of topics
K <- 3

# Process and fit STM models for each dataset
bio_out <- process_and_prepare_stm(sub_data_bio01)
bio_results <- fit_stm_model(bio_out$documents, bio_out$vocab, bio_out$meta, K)

con_out <- process_and_prepare_stm(sub_data_con01)
con_results <- fit_stm_model(con_out$documents, con_out$vocab, con_out$meta, K)

nat_out <- process_and_prepare_stm(sub_data_nat01)
nat_results <- fit_stm_model(nat_out$documents, nat_out$vocab, nat_out$meta, K)


# Visualization with LDAvis
toLDAvis(mod = bio_results$model, docs = bio_out$documents)
toLDAvis(mod = con_results$model, docs = con_out$documents)
toLDAvis(mod = nat_results$model, docs = nat_out$documents)


#Step 2: Generate the HTML Files in R

# Generate JSON data for LDAvis using toLDAvis()
bio_json <- toLDAvis(mod = bio_results$model, docs = bio_out$documents)
con_json <- toLDAvis(mod = con_results$model, docs = con_out$documents)
nat_json <- toLDAvis(mod = nat_results$model, docs = nat_out$documents)

# Use serVis() to save the visualizations to the correct directories
serVis(bio_json, out.dir = "bio_vis", open.browser = FALSE) 
serVis(con_json, out.dir = "con_vis", open.browser = FALSE)
serVis(nat_json, out.dir = "nat_vis", open.browser = FALSE)






