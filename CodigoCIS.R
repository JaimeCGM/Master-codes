
setwd("C:/Users/jaime/Desktop/Aarhus semester/Political Science Data Skills/Final project")

Sys.setenv(
  OPENAI_API_KEY = 
)

library(tidyverse)

# for API calls
library(httr)

# for handling json data
library(jsonlite)

# for computing similarities based on embeddings (sim2)
#install.packages("text2vec")
library(text2vec)

# for interactive plotting
# install.packages("plotly")
library(plotly)

# for t-SNE algorithm
# install.packages("Rtsne")
library(Rtsne) 

# for simple word statistics
library(quanteda)

load("Data_for_embeddings.RData")

######cleaning##############
left <- rbind(psoe_combined, podemos_combined)
right <- rbind(pp_combined, ciudadanos_combined)
merged <- rbind(left, right)

left$id <- row.names(left)
left$id <- as.numeric(left$id)

right$id <- row.names(right)
right$id <- as.numeric(right$id)

left$txt <- NULL
right$txt <- NULL


pp_combined <- na.omit(pp_combined)
psoe_combined <- na.omit(psoe_combined)
podemos_combined <- na.omit(podemos_combined)
ciudadanos_combined <- na.omit(ciudadanos_combined)
right <- na.omit(right)
left <- na.omit(left)
merged <- na.omit(merged)
merged$txt <- NULL


Add indexes???

##EMBEDDINGS (alternative to Topic Modelling)


# GPT API call setup:
base_url <- "https://api.openai.com/v1/embeddings"
headers <- c(Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")), 
             `Content-Type` = "application/json")
body <- list()

# setting model for API call:
body[["model"]] <- "text-embedding-ada-002"

#split the dataframe into chunks
split <- split(merged, ceiling(seq_along(merged$combined_paragraphs)/10))

#loop through the chunks
for (i in seq_along(split)){
  body[["input"]] <- split[[i]]$combined_paragraphs
  
  # request and store response
  response <- POST(url = base_url, 
                   add_headers(.headers = headers), 
                   body = body, encode = "json")
  
  # format response:
  completion <- response %>% 
    httr::content(as = "text", encoding = "UTF-8") %>% 
    fromJSON(flatten = TRUE)
  
  # extract embeddings and add to tweet dataset
  embeddingsdf <- data.frame(t(data.frame(completion$data$embedding)))
  names(embeddingsdf)[1:1536] <- paste("dim", 1:1536, sep = "")
  rownames(embeddingsdf) <- NULL
  split[[i]]<- cbind(split[[i]], embeddingsdf)
}

#combine the chunks
merged <- do.call(rbind, split)


###Similarities using embeddings

#Find 5 paragraphs about immigration

# Your code here
body[["input"]] <- "Inmigración"
#

##Run the rest of the code:

# request and store response
response <- POST(url = "https://api.openai.com/v1/embeddings", 
                 add_headers(.headers = headers), 
                 body = body, encode = "json")
# format response:
completion <- response %>% 
  httr::content(as = "text", encoding = "UTF-8") %>% 
  fromJSON(flatten = TRUE)

# extract embeddings, name columns same ways as in paragraphs df
# (required by sim2)
embedding_query <- data.frame(t(data.frame(completion$data$embedding)))
names(embedding_query) <- paste("dim", 1:1536, sep = "")

# compute similarity between query and paragraphs 
as.matrix(embedding_query)
# sim2 wants everything as matrices, not dataframes:
cosine_similarity <- sim2(x = as.matrix(embedding_query), 
                          y = as.matrix(merged %>% select(starts_with("dim"))), 
                          method = "cosine")
ncol(embedding_query)
ncol(merged)

# indices of 5 most similar paragraphs:
indices <- order(cosine_similarity, decreasing = TRUE)[1:5]

# look at 5 most similar paragraphs:
merged$combined_paragraphs[indices]


##############
####K-MEANS###
##############
load("C:/Users/jaime/Desktop/Aarhus semester/Political Science Data Skills/Final project/.RData")
# note: these algorithms always run on complete data.frames; make sure to 
# appropriately select variables

pca_result <- prcomp(merged %>% select(starts_with("dim")), scale. = TRUE)

# Add PCA scores of first two dimensions to paragraphs df
merged[,c("PC1", "PC2")] <- pca_result$x[, 1:2]

ggplot(merged, aes(x = PC1, y = PC2)) +
  geom_point() +
  theme_minimal() +
  labs(x = "PC1", y = "PC2", title = "PCA of #Podemos")

merged %>% filter(PC1 < - 20) %>% select(combined_paragraphs) %>% slice(1:10)
merged %>% filter(PC1 > 10) %>% select(combined_paragraphs) %>% slice(1:20)
merged %>% filter(PC2 > 10) %>% select(combined_paragraphs) %>% slice(1:10)
merged %>% filter(PC2 < -20) %>% select(combined_paragraphs) %>% slice(1:10)

p <- plot_ly(merged, x = ~ PC1, y = ~ PC2, 
             type = 'scatter', mode = 'markers', text = ~ combined_paragraphs, 
             hoverinfo = 'text')


##First part k-means
set.seed(2710) # don't forget to set the seed!
kmeans <- kmeans(merged %>% select(starts_with("dim")),
                 centers = 4)

# add group assignments to tweets df
merged[,c("kmeans_group")] <- kmeans$cluster

# plotting the PC variables and including the k-means groups
p <- plot_ly(merged, x = ~ PC1, y = ~ PC2, 
             color = ~ kmeans_group,
             type = 'scatter', mode = 'markers', text = ~ combined_paragraphs, 
             hoverinfo = 'text')

# Display the plot
print(p)

##Second part of k-means

set.seed(30823)
tsne_result <- Rtsne(merged %>% select(starts_with("dim")), dims = 2,
                     perplexity = 20, check_duplicates = FALSE)

# add t-SNE coordinates to tweet dataframe:
merged[,c("tsne1", "tsne2")] <- data.frame(X = tsne_result$Y[, 1], Y = tsne_result$Y[, 2])

p <- plot_ly(merged, x = ~ tsne1, y = ~ tsne2,
             color = ~ kmeans_group,
             type = 'scatter', mode = 'markers', text = ~ combined_paragraphs, 
             hoverinfo = 'text')

# Display the plot
print(p)


###Groups with k-means

## run k-means: [HERE YOU MODIFY CENTERS]
set.seed(2710) # don't forget to set the seed!
kmeans <- kmeans(merged %>% select(starts_with("dim")),
                 centers = 4)

# add group assignments to tweets df
merged[,c("kmeans_group")] <- kmeans$cluster

## run t-SNE:
set.seed(30823)
tsne_result <- Rtsne(merged %>% select(starts_with("dim")), dims = 2,
                     perplexity = 20, check_duplicates = FALSE)

# add t-SNE coordinates to tweet dataframe:
merged[,c("tsne1", "tsne2")] <- data.frame(X = tsne_result$Y[, 1], Y = tsne_result$Y[, 2])

# notice the very nice way the "text" argument 
# combines content and the kmeans_group variable
# thanks to GPT-4
p <- plot_ly(merged, x = ~ tsne1, y = ~ tsne2,
             color = ~ kmeans_group,
             type = 'scatter', mode = 'markers', 
             text = ~ paste("Text:", substr(merged$combined_paragraphs, 1, 100), 
                            "<br>Group:", merged$kmeans_group), 
             hoverinfo = 'text')

# Display the plot
print(p)

### look at top words by k-means group, using quanteda
# in order to get a better idea of what clusters stand for

# tokenize
toks <- merged$combined_paragraphs %>%
  tokens(what = "word", # 'tokenize articles
         remove_numbers = T,
         remove_punct = T,
         remove_symbols = T,
         remove_separators = T, # like \n
         split_hyphens = F, # do not split "self-aware"
         remove_url = T,
         verbose = T)

toks <- toks %>%
  tokens_tolower() %>% # all words in lower case
  tokens_remove(stopwords("es"))  %>% 
  tokens_wordstem() # stem all words


# add k-means cluster as docvar
docvars(toks, "kmeans_group") <- merged$kmeans_group

# turn into DFM:
dfm <- dfm(toks)

# look at top features by cluster: [ALSO MODIFY HERE CENTERS/GROUPS]
topfeatures(dfm, n = 4, groups = kmeans_group)

## plot just specific groups:
subset <- merged %>% filter(kmeans_group %in% c(3, 4, 10))

##plot every group
subset <- merged %>% filter(kmeans_group %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))


p <- plot_ly(subset, x = ~ tsne1, y = ~ tsne2,
             color = ~ kmeans_group,
             type = 'scatter', mode = 'markers', 
             text = ~ paste("Text:", substr(subset$combined_paragraphs, 1, 100), 
                            "<br>Group:", subset$kmeans_group), 
             hoverinfo = 'text')

# Display the plot
print(p)

#Labing manually data of k-means. 
merged$label[merged$kmeans_group == 1] <- "Foreign policy"
merged$label[merged$kmeans_group == 2] <- "Public administration"
merged$label[merged$kmeans_group == 3] <- "Education"
merged$label[merged$kmeans_group == 4] <- "Infrastructure"
merged$label[merged$kmeans_group == 5] <- "Employment"
merged$label[merged$kmeans_group == 6] <- "Social policy"
merged$label[merged$kmeans_group == 7] <- "Innovation"
merged$label[merged$kmeans_group == 8] <- "Development"
merged$label[merged$kmeans_group == 9] <- "Taxation"
merged$label[merged$kmeans_group == 10] <- "Healthcare"

table(merged$label)
ggplot(merged, aes(x = label)) + geom_histogram(stat = "count")



# Load the ggplot2 library if you haven't already
library(ggplot2)

# Filter the dataframe to include only rows where postvox = 1
filtered_data <- subset(merged, postvox == 1)

# Create the ggplot graph with filtered data using geom_bar
ggplot(filtered_data, aes(x = labels)) + 
  geom_bar()

