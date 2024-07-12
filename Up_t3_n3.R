library(dplyr)
library(stm)
library(LDAvis)
library(servr)


load_and_preprocess <- function(file_path, source_name) {
  read_bibliography(file_path) %>%
    rename(RA = "research_areas") %>%
    filter(!is.na(abstract), !is.na(journal)) %>%
    mutate(source = source_name)
}

data_bio <- load_and_preprocess("Data/Academic_literature/WoS/D_groups/savedrecs_tit_biofin.bib", "Bio")
data_con <- load_and_preprocess("Data/Academic_literature/WoS/D_groups/savedrecs_tit_confin.bib", "Con")
data_nat <- load_and_preprocess("Data/Academic_literature/WoS/D_groups/savedrecs_tit_natfin.bib", "Nat")

preprocess_data <- function(data) {
  data %>%
    select(abstract, year, journal, author, title, RA) %>%
    mutate(RA1 = str_extract(RA, "^[^,;&-]+"),
           RA1 = str_replace(RA1, "[\\\\-]+$", "")) %>%
    mutate(RDG = case_when(
      str_detect(RA1, "Business") ~ "G1",
      str_detect(RA1, "Arts|Cultural Studies|Development Studies|Geography|International Relations|Literature|Philosophy|Psychology|Urban Studies") ~ "G2",
      str_detect(RA1, "Biodiversity|Environmental Sciences|Life Sciences|Meteorology|Oceanography|Physics|Plant Sciences|Water Resources|Science") ~ "G3",
      str_detect(RA1, "Agriculture|Computer Science|Energy|Engineering|Forestry|Thermodynamics") ~ "G4",
      TRUE ~ "Other"  # Add a default case
    ))
}

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

# Define function to save LDAvis visualizations to HTML files and generate links
save_lda_vis <- function(model, documents, file_name) {
  json <- toLDAvis(mod = model, docs = documents)
  vis_dir <- "LDAvis/"
  dir.create(vis_dir, showWarnings = FALSE)
  serVis(json, out.dir = vis_dir, open.browser = FALSE)
  file.rename(paste0(vis_dir, "index.html"), paste0(vis_dir, file_name))
  return(paste0(vis_dir, file_name))
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

# Save LDAvis visualizations and get file paths
bio_vis_path <- save_lda_vis(bio_results$model, bio_out$documents, "bio_lda_vis.html")
con_vis_path <- save_lda_vis(con_results$model, con_out$documents, "con_lda_vis.html")
nat_vis_path <- save_lda_vis(nat_results$model, nat_out$documents, "nat_lda_vis.html")

# Define the base URL for GitHub Pages
github_base_url <- "https://Julia-Q-Mao.github.io/Concept-paper/LDAvis/"

# Generate URLs
bio_vis_url <- paste0(github_base_url, "bio_lda_vis.html")
con_vis_url <- paste0(github_base_url, "con_lda_vis.html")
nat_vis_url <- paste0(github_base_url, "nat_lda_vis.html")

# Print links for external users to view the visualizations
cat("Bio LDAvis Visualization: ", bio_vis_url, "\n")
cat("Con LDAvis Visualization: ", con_vis_url, "\n")
cat("Nat LDAvis Visualization: ", nat_vis_url, "\n")
