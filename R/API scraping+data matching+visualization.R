library(openalexR)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(readr)
library(stringdist)

setwd("C:/Users/Wan Hok Ming/Documents/Big data/Final project clean/")
concepts <- read.csv("openalex_concepts.csv")

# get the vectors of the concept id of the solar, wind, nuclear, tourism and natural resources
concept_extract <- function(target) {
  parents <- concepts[grepl(target,concepts$display_name, ignore.case = TRUE), ]
  posterity <- concepts[grepl(target, concepts$parent_display_names, ignore.case = TRUE), ]
  df <- rbind(parents, posterity)
  df <- distinct(df)
  vec <- df$openalex_id
return(vec)
}
solar_vec <- concept_extract("solar energy|solar power|solar cell|photovoltaic system|Cadmium telluride photovoltaics")
wind_vec <- concept_extract("wind power")
tour_vec <- concept_extract("tourism")
petroleum_vec <- concept_extract("Petroleum|Mining Engineering|Natural gas|Fossil fuel")

# only posterity would be extracted from nuclear research works 
nuclear_posterity <- concepts[grepl("nuclear engineering", concepts$parent_display_names, ignore.case = TRUE), ]
nuclear_df_post <- distinct(nuclear_posterity)
nuclear_post_vec <- nuclear_df_post$openalex_id

# scraping OpenAlex
oa_scraper <- function(concept_vec) {
  saudi <- oa_fetch(
    entity = "works",
    institutions.country_code = "sa",
    institutions.type = "education",
    concepts.id = concept_vec,
    from_publication_date = "2016-01-01",
    to_publication_date = "2023-12-31", 
    verbose = TRUE
  )
  saudi <- distinct(saudi)
  return(saudi)
}

saudi_solar <- oa_scraper(solar_vec)
saudi_wind <- oa_scraper(wind_vec)
saudi_nuclear <- oa_scraper(nuclear_post_vec)
saudi_tour <- oa_scraper(tour_vec)
saudi_oil <- oa_scraper(petroleum_vec)

# Ploting trends in research publications:
work_freq <- function(df, concept) {
  research_counts <- table(df$publication_year)
  rc_df <- as.data.frame(research_counts)
  colnames(rc_df) <- c("publication_year", "research_number")
  rc_df$concept <- concept
  return(rc_df)
}

rc_solar_df <- work_freq(saudi_solar, "solar")
rc_wind_df <- work_freq(saudi_wind, "wind")
rc_nuclear_df <- work_freq(saudi_nuclear, "nuclear")
rc_tour_df <- work_freq(saudi_tour, "tourism")
rc_oil_df <- work_freq(saudi_oil, "natural resources")
research_trend_df <- rbind(rc_solar_df, rc_wind_df, rc_nuclear_df, rc_tour_df, rc_oil_df)

ggplot(research_trend_df, aes(x = publication_year, y = research_number, color = concept)) +
  geom_line(aes(group = concept, linetype = concept)) +
  labs(title = "Trends of Numbers of Published Research Works (2016-2023)",
       x = "Publication Year", y = "Number of Research Works", color = "Discipline", linetype = "Discipline")

# calculate the average number of citations (with normalization)

citation_mean <- function(df, conc) {
  mean_df <- df %>%
    group_by(publication_year) %>%
    summarize(cited_by_count_mean = mean(cited_by_count))
  mean_df$cited_by_count_mean <- (mean_df$cited_by_count_mean / (2023-mean_df$publication_year+1))
  mean_df$concept <- conc
  return(mean_df)
}

solar_cite_mean <- citation_mean(saudi_solar, "solar")
wind_cite_mean <- citation_mean(saudi_wind, "wind")
nuclear_cite_mean <- citation_mean(saudi_nuclear, "nuclear")
tour_cite_mean <- citation_mean(saudi_tour, "tourism")
oil_cite_mean <- citation_mean(saudi_oil, "natural resource")
saudi_cite_mean <- rbind(solar_cite_mean, wind_cite_mean, nuclear_cite_mean, tour_cite_mean, oil_cite_mean)

ggplot(saudi_cite_mean, aes(x = publication_year, y = cited_by_count_mean, color = concept)) +
  geom_line(aes(group = concept, linetype = concept)) +
  labs(title = "Trends of the mean of citation of research works (2016-2023)",
       x = "Publication Year", y = "Number of Citation", color = "Discipline", linetype = "Discipline")

# calculate the median of number of citations (with normalization)
citation_median <- function(df, conc) {
  median_df <- df %>%
    group_by(publication_year) %>%
    summarize(cited_by_count_median = median(cited_by_count))
  median_df$cited_by_count_median <- (median_df$cited_by_count_median / (2023-median_df$publication_year+1))
  median_df$concept <- conc
  return(median_df)
}

solar_cite_median <- citation_median(saudi_solar, "solar")
wind_cite_median <- citation_median(saudi_wind, "wind")
nuclear_cite_median <- citation_median(saudi_nuclear, "nuclear")
tour_cite_median <- citation_median(saudi_tour, "tourism")
oil_cite_median <- citation_median(saudi_oil, "natural resource")
saudi_cite_median <- rbind(solar_cite_median, wind_cite_median, nuclear_cite_median, tour_cite_median, oil_cite_median)

ggplot(saudi_cite_median, aes(x = publication_year, y = cited_by_count_median, color = concept)) +
  geom_line(aes(group = concept, linetype = concept)) +
  labs(title = "Trends of the median of citation of research works (2016-2023)",
       x = "Publication Year", y = "Number of Citation", color = "Discipline", linetype = "Discipline")

citation_upper_quartile <- function(df, conc) {
  upper_quartile_df <- df %>%
    group_by(publication_year) %>%
    summarize(cited_by_count_upper_quartile = quantile(cited_by_count, probs = 0.75, na.rm = TRUE))
  upper_quartile_df$cited_by_count_upper_quartile <- (upper_quartile_df$cited_by_count_upper_quartile / (2023 - upper_quartile_df$publication_year + 1))
  upper_quartile_df$concept <- conc
  return(upper_quartile_df)
}

solar_cite_uq <- citation_upper_quartile(saudi_solar, "solar")
wind_cite_uq <- citation_upper_quartile(saudi_wind, "wind")
nuclear_cite_uq <- citation_upper_quartile(saudi_nuclear, "nuclear")
tour_cite_uq <- citation_upper_quartile(saudi_tour, "tourism")
oil_cite_uq <- citation_upper_quartile(saudi_oil, "natural resource")
saudi_cite_uq <- rbind(solar_cite_uq, wind_cite_uq, nuclear_cite_uq, tour_cite_uq, oil_cite_uq)

ggplot(saudi_cite_uq, aes(x = publication_year, y = cited_by_count_upper_quartile, color = concept)) +
  geom_line(aes(group = concept, linetype = concept)) +
  labs(title = "Trends of the upper quartile of citation of research works (2016-2023)",
       x = "Publication Year", y = "Number of Citation", color = "Discipline", linetype = "Discipline")

#solar_cite <- saudi_solar$counts_by_year
#citation_count <- do.call(rbind, solar_cite) %>%
#  group_by(year) %>%
#  summarize(cited_by_count = sum(cited_by_count))

# import SJR journal rankings csv and preprocess the journal title column

transform_sjr <- function(sjr_data, year) {
  sjr_data <- read_delim(sjr_data)
  sjr_data$year <- year
  sjr_data <- sjr_data %>%
    select("Title", "SJR Best Quartile", "Categories", "year")
  sjr_data$journal_name <- sjr_data$Title
  sjr_data$Title <- tolower(sjr_data$Title)
  sjr_data$Title <- gsub("[[:punct:]]", "",sjr_data$Title)
  sjr_data$Title <- gsub("ieeeosa", "", sjr_data$Title)
  sjr_data$Title <- trimws(sjr_data$Title)
  return(sjr_data)
}

sjr2016 <- transform_sjr("scimagojr 2016.csv", 2016)
sjr2017 <- transform_sjr("scimagojr 2017.csv", 2017)
sjr2018 <- transform_sjr("scimagojr 2018.csv", 2018)
sjr2019 <- transform_sjr("scimagojr 2019.csv", 2019)
sjr2020 <- transform_sjr("scimagojr 2020.csv", 2020)
sjr2021 <- transform_sjr("scimagojr 2021.csv", 2021)
sjr2022 <- transform_sjr("scimagojr 2022.csv", 2022)
sjr2023 <- transform_sjr("scimagojr 2023.csv", 2023)

# function for preprocessing the jounral name columns from openalex dataframes
transform_oa <- function(oa) {
  oa_data <- oa
  oa_data$so <- tolower(oa_data$so)
  oa_data$so <- gsub("&", "and", oa_data$so)
  oa_data$so <- gsub("/.*", "", oa_data$so)
  oa_data$so <- gsub("[[:punct:]]", "", oa_data$so)
  oa_data$so <- gsub("??the ??journal of physical chemistry letters", "journal of physical chemistry letters", oa_data$so)
  oa_data$so <- trimws(oa_data$so)
  oa_data <- oa_data %>%
    drop_na(so)
  return(oa_data)
}

solar_sjr <- transform_oa(saudi_solar)
wind_sjr <- transform_oa(saudi_wind)
nuclear_sjr <- transform_oa(saudi_nuclear)
tour_sjr <- transform_oa(saudi_tour)
oil_sjr <- transform_oa(saudi_oil)

# create subsets of openalex dataframe by year
oa_subset <- function(oa, year) {
  oa_data <- oa %>%
    filter(publication_year == year)
}

# solar
solar_sjr2016 <- oa_subset(solar_sjr, 2016)
solar_sjr2017 <- oa_subset(solar_sjr, 2017)
solar_sjr2018 <- oa_subset(solar_sjr, 2018)
solar_sjr2019 <- oa_subset(solar_sjr, 2019)
solar_sjr2020 <- oa_subset(solar_sjr, 2020)
solar_sjr2021 <- oa_subset(solar_sjr, 2021)
solar_sjr2022 <- oa_subset(solar_sjr, 2022)
solar_sjr2023 <- oa_subset(solar_sjr, 2023)

# wind
wind_sjr2016 <- oa_subset(wind_sjr, 2016)
wind_sjr2017 <- oa_subset(wind_sjr, 2017)
wind_sjr2018 <- oa_subset(wind_sjr, 2018)
wind_sjr2019 <- oa_subset(wind_sjr, 2019)
wind_sjr2020 <- oa_subset(wind_sjr, 2020)
wind_sjr2021 <- oa_subset(wind_sjr, 2021)
wind_sjr2022 <- oa_subset(wind_sjr, 2022)
wind_sjr2023 <- oa_subset(wind_sjr, 2023)

# nuclear
nuclear_sjr2016 <- oa_subset(nuclear_sjr, 2016)
nuclear_sjr2017 <- oa_subset(nuclear_sjr, 2017)
nuclear_sjr2018 <- oa_subset(nuclear_sjr, 2018)
nuclear_sjr2019 <- oa_subset(nuclear_sjr, 2019)
nuclear_sjr2020 <- oa_subset(nuclear_sjr, 2020)
nuclear_sjr2021 <- oa_subset(nuclear_sjr, 2021)
nuclear_sjr2022 <- oa_subset(nuclear_sjr, 2022)
nuclear_sjr2023 <- oa_subset(nuclear_sjr, 2023)

# tourism
tour_sjr2016 <- oa_subset(tour_sjr, 2016)
tour_sjr2017 <- oa_subset(tour_sjr, 2017)
tour_sjr2018 <- oa_subset(tour_sjr, 2018)
tour_sjr2019 <- oa_subset(tour_sjr, 2019)
tour_sjr2020 <- oa_subset(tour_sjr, 2020)
tour_sjr2021 <- oa_subset(tour_sjr, 2021)
tour_sjr2022 <- oa_subset(tour_sjr, 2022)
tour_sjr2023 <- oa_subset(tour_sjr, 2023)

# natural resources
oil_sjr2016 <- oa_subset(oil_sjr, 2016)
oil_sjr2017 <- oa_subset(oil_sjr, 2017)
oil_sjr2018 <- oa_subset(oil_sjr, 2018)
oil_sjr2019 <- oa_subset(oil_sjr, 2019)
oil_sjr2020 <- oa_subset(oil_sjr, 2020)
oil_sjr2021 <- oa_subset(oil_sjr, 2021)
oil_sjr2022 <- oa_subset(oil_sjr, 2022)
oil_sjr2023 <- oa_subset(oil_sjr, 2023)

# matching openalex dataframe with sjr dataframe using levenshtein distance
# create function fuzzymatching for computing Levenshtein Distance and matching data
fuzzymatching <- function(oa, sjr) {
dist.name<-stringdistmatrix(oa$so, sjr$Title, method = "lv")

# take the pairs with the minimum distance
min.name<-apply(dist.name, 1, min)

# matched journal names based on minimum distance
match.s1.s2<-NULL
for(i in 1:nrow(dist.name))
{
  s2.i<-match(min.name[i],dist.name[i,])
  s1.i<-i
  match.s1.s2<-rbind(data.frame(s2.i=s2.i,s1.i=s1.i,sjr_name=sjr[s2.i,]$ Title, 
                                oa_name=oa[s1.i,]$ so, adist=min.name[i]), 
                     s2name=sjr[s2.i,]$ Title, match.s1.s2)
}

# merge openalex dataframe and sjr dataframe based on matched results
match.s1.s2 <- match.s1.s2 %>% 
  filter(!str_detect(adist, "[^0-9]"))
match.s1.s2$adist <- as.numeric(match.s1.s2$adist)
match.s1.s2 <- match.s1.s2 %>%
  filter(adist <= 3) %>%
  select(sjr_name, oa_name)

oa_sjr_matched <- merge(oa, match.s1.s2, 
                     by.x=c("so"), by.y=c("oa_name"), all.x=TRUE)
oa_sjr_matched <- distinct(oa_sjr_matched)
oa_sjr_matched <- merge(oa_sjr_matched, sjr, 
                        by.x=c("sjr_name"), by.y=c("Title"), all.x=TRUE)
oa_sjr_matched <- distinct(oa_sjr_matched)
return(oa_sjr_matched)
}

matched_solar2016 <- fuzzymatching(solar_sjr2016, sjr2016)
matched_solar2017 <- fuzzymatching(solar_sjr2017, sjr2017)
matched_solar2018 <- fuzzymatching(solar_sjr2018, sjr2018)
matched_solar2019 <- fuzzymatching(solar_sjr2019, sjr2019)
matched_solar2020 <- fuzzymatching(solar_sjr2020, sjr2020)
matched_solar2021 <- fuzzymatching(solar_sjr2021, sjr2021)
matched_solar2022 <- fuzzymatching(solar_sjr2022, sjr2022)
matched_solar2023 <- fuzzymatching(solar_sjr2023, sjr2023)
matched_solar <- rbind(matched_solar2016,matched_solar2017,matched_solar2018,matched_solar2019,
                       matched_solar2020,matched_solar2021,matched_solar2022,matched_solar2023)

matched_wind2016 <- fuzzymatching(wind_sjr2016, sjr2016)
matched_wind2017 <- fuzzymatching(wind_sjr2017, sjr2017)
matched_wind2018 <- fuzzymatching(wind_sjr2018, sjr2018)
matched_wind2019 <- fuzzymatching(wind_sjr2019, sjr2019)
matched_wind2020 <- fuzzymatching(wind_sjr2020, sjr2020)
matched_wind2021 <- fuzzymatching(wind_sjr2021, sjr2021)
matched_wind2022 <- fuzzymatching(wind_sjr2022, sjr2022)
matched_wind2023 <- fuzzymatching(wind_sjr2023, sjr2023)
matched_wind <- rbind(matched_wind2016,matched_wind2017,matched_wind2018,matched_wind2019,
                       matched_wind2020,matched_wind2021,matched_wind2022,matched_wind2023)

matched_nuclear2016 <- fuzzymatching(nculear_sjr2016, sjr2016)
matched_nuclear2017 <- fuzzymatching(nculear_sjr2017, sjr2017)
matched_nuclear2018 <- fuzzymatching(nculear_sjr2018, sjr2018)
matched_nuclear2019 <- fuzzymatching(nculear_sjr2019, sjr2019)
matched_nuclear2020 <- fuzzymatching(nculear_sjr2020, sjr2020)
matched_nuclear2021 <- fuzzymatching(nculear_sjr2021, sjr2021)
matched_nuclear2022 <- fuzzymatching(nculear_sjr2022, sjr2022)
matched_nuclear2023 <- fuzzymatching(nculear_sjr2023, sjr2023)
matched_nuclear <- rbind(matched_nuclear2016,matched_nuclear2017,matched_nuclear2018,matched_nuclear2019,
                       matched_nuclear2020,matched_nuclear2021,matched_nuclear2022,matched_nuclear2023)

matched_tour2016 <- fuzzymatching(tour_sjr2016, sjr2016)
matched_tour2017 <- fuzzymatching(tour_sjr2017, sjr2017)
matched_tour2018 <- fuzzymatching(tour_sjr2018, sjr2018)
matched_tour2019 <- fuzzymatching(tour_sjr2019, sjr2019)
matched_tour2020 <- fuzzymatching(tour_sjr2020, sjr2020)
matched_tour2021 <- fuzzymatching(tour_sjr2021, sjr2021)
matched_tour2022 <- fuzzymatching(tour_sjr2022, sjr2022)
matched_tour2023 <- fuzzymatching(tour_sjr2023, sjr2023)
matched_tour <- rbind(matched_tour2016,matched_tour2017,matched_tour2018,matched_tour2019,
                       matched_tour2020,matched_tour2021,matched_tour2022,matched_tour2023)

matched_oil2016 <- fuzzymatching(wind_sjr2016, sjr2016)
matched_oil2017 <- fuzzymatching(oil_sjr2017, sjr2017)
matched_oil2018 <- fuzzymatching(oil_sjr2018, sjr2018)
matched_oil2019 <- fuzzymatching(oil_sjr2019, sjr2019)
matched_oil2020 <- fuzzymatching(oil_sjr2020, sjr2020)
matched_oil2021 <- fuzzymatching(oil_sjr2021, sjr2021)
matched_oil2022 <- fuzzymatching(oil_sjr2022, sjr2022)
matched_oil2023 <- fuzzymatching(oil_sjr2023, sjr2023)
matched_oil <- rbind(matched_oil2016,matched_oil2017,matched_oil2018,matched_oil2019,
                      matched_oil2020,matched_oil2021,matched_oil2022,matched_oil2023)

# forming frequency tables for plotting trends in numbers of research publication based on best quartile overtime
quart_freq <- function(df) {
  quartile_counts <- table(df$publication_year, df$`SJR Best Quartile`)
  qc_df <- as.data.frame(quartile_counts)
  colnames(qc_df) <- c("publication_year", "best_quartile", "research_number")
  return(qc_df)
}

qc_solar_df <- quart_freq(matched_solar)
qc_wind_df <- quart_freq(matched_wind)
qc_nuclear_df <- quart_freq(matched_nuclear)
qc_tour_df <- quart_freq(matched_tour)
qc_oil_df <- quart_freq(matched_oil)

# Plotting trends in numbers of research publication based on best quartile overtime
ggplot(qc_solar_df, aes(x = publication_year, y = research_number, color = best_quartile)) +
  geom_line(aes(group = best_quartile, linetype = `best_quartile`)) +
  labs(title = "Trends of journal quality of published research works related solar energy (2016-2023)",
       x = "Publication Year", y = "Number of Research Works", color = "SJR Best Quartile", linetype = "SJR Best Quartile")

ggplot(qc_wind_df, aes(x = publication_year, y = research_number, color = best_quartile)) +
  geom_line(aes(group = best_quartile, linetype = `best_quartile`)) +
  labs(title = "Trends of journal quality of published research works related wind energy (2016-2023))",
       x = "Publication Year", y = "Number of Research Works", color = "SJR Best Quartile", linetype = "SJR Best Quartile")

ggplot(qc_nuclear_df, aes(x = publication_year, y = research_number, color = best_quartile)) +
  geom_line(aes(group = best_quartile, linetype = `best_quartile`)) +
  labs(title = "Trends of journal quality of published research works related nuclear energy (2016-2023))",
       x = "Publication Year", y = "Number of Research Works", color = "SJR Best Quartile", linetype = "SJR Best Quartile")

ggplot(qc_tour_df, aes(x = publication_year, y = research_number, color = best_quartile)) +
  geom_line(aes(group = best_quartile, linetype = `best_quartile`)) +
  labs(title = "Trends of journal quality of published research works related tourism (2016-2023))",
       x = "Publication Year", y = "Number of Research Works", color = "SJR Best Quartile", linetype = "SJR Best Quartile")

ggplot(qc_oil_df, aes(x = publication_year, y = research_number, color = best_quartile)) +
  geom_line(aes(group = best_quartile, linetype = `best_quartile`)) +
  labs(title = "Trends of journal quality of published research works related Natural Resources (2016-2023))",
       x = "Publication Year", y = "Number of Research Works", color = "SJR Best Quartile", linetype = "SJR Best Quartile")
