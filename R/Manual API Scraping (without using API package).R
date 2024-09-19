library(httr)
library(jsonlite)
library(dplyr)
library(purrr)

setwd('C:/Users/Wan Hok Ming/Documents/Big data/Final Project')


#Adjust publication data of the endpoint
api_base <- 
  "https://api.openalex.org/works?filter=institutions.country_code%3Asa%2Cinstitutions.type%3Aeducation%2Cconcepts.id%3Ahttps%3A%2F%2Fopenalex.org%2Fc2779471803%2Cfrom_publication_date%3A2016-01-01%2Cto_publication_date%3A2023-12-31&page="

all_results <- list()

# Loop through all pages (adjust range according number of pages based on publication date)
for (page in 1:231) {
  api_endpoint <- paste0(api_base, page)
  
  # Send an HTTP GET request to the API endpoint
  response <- GET(url = api_endpoint)
  
  # Check if the request was successful (status code 200)
  if (status_code(response) == 200) {
    # Extract and append results to the list
    response_text <- content(response, "text", encoding = "UTF-8")
    response_json <- fromJSON(response_text, flatten = TRUE)
    all_results[[page]] <- response_json$results
  } else {
    # Handle error or break the loop if needed
    warning(paste("Error fetching page", page, "- Status code:", status_code(response)))
    break
  }
}


# Combine all results into a single data frame
# Combine all results into a single data frame
works_Inprog <- bind_rows(all_results)
works_Inprog <- works_Inprog %>%
  #select(-starts_with("abstract"))
  select(id, doi, title, display_name, publication_year, publication_date, language, type, authorships,
         cited_by_count, topics, keywords, concepts, primary_location.source.display_name)
works_Inprog <- distinct(works_Inprog)
works_Inprog <- rename(works_Inprog, so = primary_location.source.display_name)

solar_2 <-works_Inprog

#merge dataframe

saudi_oil <- rbind(oil_1, oil_2, oil_3)
saudi_oil <- distinct(saudi_oil)

saudi_solar <- rbind(solar_1, solar_2)
saudi_solar <- distinct(saudi_solar)

saveRDS(works_Inprog, file = "wind.rds")
my_loaded_data <- readRDS("wind.rds")

saveRDS(saudi_nuclear, file = "nuclear.rds")
my_loaded_nuclear <- readRDS("nuclear.rds")

saveRDS(saudi_oil, file = "oil.rds")
my_loaded_oil <- readRDS("oil.rds")

saveRDS(saudi_solar, file = "solar.rds")
my_loaded_data <- readRDS("solar.rds")
