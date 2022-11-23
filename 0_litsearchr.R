## 0.0 SET-UP
library(dplyr)
library(ggplot2)
library(ggraph)
library(igraph)
library(readr)

#install litsearchr
library(devtools)
install_github("elizagrames/litsearchr", ref="main")
library(litsearchr) #version 1.0.0

#set working directory
setwd("C:/Users/cmarti26/Documents/2. Projects/1.1 Review/Analysis")

## 1.0 SEARCH WEB OF SCIENCE
#search WoS: (conservation planning) AND (species distribution model*)
#export as .ris file (1000 per download)
#2917 results - November 21, 2022

#load results from search
naive_results1 <- import_results(file = "wos1.ris")
naive_results2 <- import_results(file = "wos2.ris")
naive_results3 <- import_results(file = "wos3.ris")
nrow(naive_results3)
colnames(naive_results3) #they do not have all the same columns, or the same order

#combine three downloaded sets with the same columns and column order
naive_results1 <- naive_results1 |> select(accession_zr, title, abstract, keywords, author, address, source, year,
                         volume, issue, start_page, end_page, doi, language, source_type)
naive_results2 <- naive_results2 |> select(accession_zr, title, abstract, keywords, author, address, source, year,
                                           volume, issue, start_page, end_page, doi, language, source_type)
naive_results3 <- naive_results3 |> select(accession_zr, title, abstract, keywords, author, address, source, year,
                                           volume, issue, start_page, end_page, doi, language, source_type)
naive_results <- rbind(naive_results1, naive_results2, naive_results3)

naive_results[2917, "title"] #check title of a result


## 2.0 EXTRACT SEARCH TERMS
naive_results[1, "keywords"] #keywords from first article
sum(is.na(naive_results[, "keywords"])) #number of articles missing keywords (n=13)

#extract keywords from author-tagged keywords
keywords <- extract_terms(keywords=naive_results[, "keywords"], 
              method="tagged",
              min_freq=10, #keywords appear at least 10 times
              min_n=2, #keywords at least 2 word long
              max_n=4) #keywords up to 4 words long
keywords #227 keywords

#create a list of "stopwords" (general science or data analysis terms unrelated to topic)
conplan_stopwords <- read_lines("conplan_stopwords.txt")
all_stopwords <- c(get_stopwords("English"), conplan_stopwords) #also get list of English stopwords and add to list

#extract search terms in title without stopwords
title_terms <- extract_terms(
  text=naive_results[, "title"],
  method="fakerake", #this performs Rapid Automatic Keyword Extraction (RAKE) method
  min_freq=10, 
  min_n=2,
  max_n=4,
  stopwords=all_stopwords
)
title_terms #59 title terms

#add search terms from article titles and keywords, removing duplicates
terms <- unique(c(keywords, title_terms)) 
#252 total


## 4.0 NETWORK ANALYSIS
docs <- paste(naive_results[, "title"], naive_results[, "abstract"])
docs[1] #check that it did this correctly

#create matrix to record which terms appear in which articles
dfm <- create_dfm(elements=docs, features=terms)
dfm[1:3, 1:4] #matrix with 0 and 1 for absence/presence of search terms within each document

#turn matrix into network of linked search terms
g <- create_network(dfm, min_studies=3)

#plot network
ggraph(g, layout="stress") +
  coord_fixed() +
  expand_limits(x=c(-3, 3)) +
  geom_edge_link(aes(alpha=weight)) +
  geom_node_point(shape="circle filled", fill="white") +
  geom_node_text(aes(label=name), color="white", hjust="outward", check_overlap=TRUE) +
  guides(edge_alpha=FALSE)
#terms that appear near center and linked by darker lines are prob more important

## 5.0 PRUNE SEARCH TERMS
#rank search terms by importance
strengths <- strength(g)

data.frame(term=names(strengths), strength=strengths, row.names=NULL) |>
  mutate(rank=rank(strength, ties.method="min")) |>
  arrange(strength) ->
  term_strengths
term_strengths #terms at top are most weakly linked to others

#visualize first
cutoff_fig <- ggplot(term_strengths, aes(x=rank, y=strength, label=term)) +
  geom_line() +
  geom_point() +
  geom_text(data=filter(term_strengths, rank>5), hjust="right", nudge_y=20, check_overlap=TRUE)
cutoff_fig #terms in ascending order of strength (labeled arbitrarily)

## 5.1 CHANGEPOINTS
#points along ranking where strength increases much more than previous
cutoff_change <- find_cutoff(g, method="changepoint", knot_num=3)
cutoff_change #suggests cutoffs

#visualize
cutoff_fig +
  geom_hline(yintercept = cutoff_change, linetype = "dashed")

#choose a cutoff value
g_redux <- reduce_graph(g, cutoff_change[2]) #specify which cutoff value
selected_terms <- get_keywords(g_redux)
selected_terms

## 6.0 GROUPING SEARCH TERMS
#create groupings by hand as a revised list for new search query
grouped_terms <-list(
  conplan = selected_terms[c(2, 3, 7, 8)], #do not include climate change
  sdm = selected_terms[c(4, 5, 6, 9, 10, 11, 12)]
)
grouped_terms
# $conplan
# [1] "climate change"        "conservation plan"     "conservation planning"
# [4] "protected area"        "protected areas"      
# 
# $sdm
# [1] "distribution models"         "environmental variables"     "habitat suitability"        
# [4] "species distribution"        "species distribution model"  "species distribution models"
# [7] "suitable habitat"  


#write a new search with these grouped terms
write_search(
  grouped_terms,
  languages="English", #can choose multiple languages to translate into
  exactphrase=TRUE, #match terms exactly rather than two separate words
  stemming=FALSE, #TRUE takes all variants of words (e.g., behaviour, behavioural)
  closure="left", #partial matches matched at left end of word 
  writesearch=TRUE #write text file
)
cat(read_file("search-inEnglish.txt"))
#closure = "left", but change to "conservation planning"

#FINAL SEARCH TERMS:
#(\("conservation planning" OR "protected area"\) AND 
    #\("distribution models" OR "environmental variables" OR 
    #"habitat suitability" OR "species distribution" OR "suitable habitat"\)\)

## 7.0 SEARCH WITH NEW TERMS and FINAL CHECKS
#search WoS and export results
#1,974 results, November 21, 2022
new_results1 <- import_results(file = "wos_newterms1.ris")
new_results2 <- import_results(file = "wos_newterms2.ris")
new_results1 <- new_results1 |> select(accession_zr, title, abstract, keywords, author, address, source, year,
                                           volume, issue, start_page, end_page, doi, language, source_type)
new_results2 <- new_results2 |> select(accession_zr, title, abstract, keywords, author, address, source, year,
                                       volume, issue, start_page, end_page, doi, language, source_type)

new_results <- rbind(new_results1, new_results2)
nrow(new_results)

#check if all of naive search results were still included
naive_results |>
  mutate(in_new_results=title %in% new_results[, "title"]) ->
  naive_results

naive_results |>
  filter(!in_new_results) |>
  select(title, keywords)

#Finally, check if our search still contains important articles that we know
important_titles <- c(
  "Species distribution models for conservation planning in fire-prone landscapes",
  "What are the roles of species distribution models in conservation planning?",
  "Spatially explicit species distribution models: A missed opportunity in conservation planning?"
)

data.frame(check_recall(important_titles, new_results[, "title"]))

write.csv(new_results, file = "new_results.csv")
#total of 1,974 papers! a lot, but down from 2,917
#take first 1000?


#tutorial: https://luketudge.github.io/litsearchr-tutorial/litsearchr_tutorial.html

biblio <- read.csv("C:/Users/cmarti26/Documents/2. Projects/1.1 Review/Analysis/new_results.csv")

