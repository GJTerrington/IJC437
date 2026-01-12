library(dplyr)
library(stringr)
library(lubridate)
library(tidyverse)
library(randomForest)
library(pROC)

# load the datasets in. use read_tsv as the tabs are separated with \t
acoustic <- read_tsv("acoustic_features.csv")
albums   <- read_tsv("albums.csv")
artists  <- read_tsv("artists.csv")
songs    <- read_tsv("songs.csv")
tracks   <- read_tsv("tracks.csv")

# merge songs and acoustic features
song_data <- songs %>%
  left_join(acoustic, by = "song_id")

# add release data and album_id using the tracks dataset
song_data <- song_data %>%
  left_join(
    tracks %>% select(song_id, album_id, release_date, release_date_precision),
    by = "song_id"
  ) %>%
  mutate(
    release_date = ymd(release_date),
    release_year = year(release_date)
  )
# you can check this has done it correctly by using the find feature in the datasets

# add album information. again check this has worked by looking at a few random columns and ensuring the songs are in the correct album
song_data <- song_data %>%
  left_join(
    albums %>% select(album_id, album_type, total_tracks, billboard),
    by = "album_id"
  )
  
# songs$artists contains colon-separated artist names
# artists.csv contains artist name + genre + popularity

# to add artist genre and popularity, we must standardise artist name from the songs dataset, as in its current format, the join does not work
extract_artist_names <- function(x) {
  # Extract all quoted items
  items <- stringr::str_extract_all(x, "'([^']+)'")[[1]]
  
  # If nothing extracted then this returns NA
  if (length(items) == 0) return(NA_character_)
  
  # If only one quoted item then this assumes it is the artist's name
  if (length(items) == 1) {
    return(gsub("'", "", items))
  }
  
  # Otherwise keep only even-indexed items, which are always artist names
  values <- items[seq(2, length(items), by = 2)]
  
  # Remove quotes
  values <- gsub("'", "", values)
  
  return(values)
}

# then create the new data frame
songs_clean <- songs %>%
  mutate(artists = lapply(artists, extract_artist_names)) %>%
  unnest(artists)

# deduplicate artist names in the artists table
artists_unique <- artists %>%
  distinct(name, .keep_all = TRUE)

# now do the join
artist_map <- songs_clean %>%
  select(song_id, artists) %>%
  left_join(
    artists_unique %>%
      select(name, main_genre, popularity, artist_type),
    by = c("artists" = "name")
  )

# summarise to single row per song
song_artist_info <- artist_map %>%
  group_by(song_id) %>%
  summarise(
    genre = first(na.omit(main_genre)),
    artist_popularity = if (all(is.na(popularity))) NA_real_ else max(popularity, na.rm = TRUE),
    artist_type = first(artist_type)
  )

# then you can join this data into the main dataset
song_data <- song_data %>%
  left_join(song_artist_info, by = "song_id")


# create decade variable
song_data <- song_data %>%
  mutate(
    decade = floor(release_year / 10) * 10
  )


# now create the final dataset with all the variables you want
final_data <- song_data %>%
  select(
    song_id,
    song_name,
    artists,
    genre,
    artist_popularity,
    artist_type,
    popularity,
    release_date,
    release_year,
    decade,
    explicit,
    song_type,
    album_id,
    duration_ms,
    key,
    mode,
    time_signature,
    acousticness,
    danceability,
    energy,
    instrumentalness,
    liveness,
    loudness,
    speechiness,
    valence,
    tempo
  )

# now assess where the NA values are
colSums(is.na(final_data))
# we can see release date is causing a lot of NA values

# we can flag the ones that have missing release years for the models
final_data <- final_data %>%
  mutate(
    release_year_missing = ifelse(is.na(release_year), 1, 0)
  )

# we can then have 'unknown' put into release decade where there is NA
final_data <- final_data %>%
  mutate(
    decade = case_when(
      is.na(release_year) ~ "Unknown",
      TRUE ~ paste0(floor(release_year / 10) * 10, "s")
    )
  )

# there are some missing values for popularity, so we will input these as -1, as this is not on the 1-100 scale
# we will also add a flag for the model to pick up that this was missing
final_data <- final_data %>%
  mutate(
    artist_pop_missing = ifelse(is.na(artist_popularity), 1, 0),
    artist_popularity = ifelse(is.na(artist_popularity), -1, artist_popularity)
  )

# many of the NA values are inputted as '-' so we need to rectify this
final_data <- final_data %>%
  mutate(across(c(genre, artist_type), ~na_if(., "-")))

# then we can replace NA values with Unknown
final_data <- final_data %>%
  mutate(
    genre = replace(genre, is.na(genre), "Unknown"),
    artist_type = replace(artist_type, is.na(artist_type), "Unknown"),
  )

# Remove unwanted decades
final_data <- final_data %>%
  filter(!decade %in% c("1900s", "1940s", "Unknown"))

# Add a binary measure of popularity for the classification
final_data <- final_data %>%
mutate(
  popularity_binary = ifelse(popularity >= 70, 1, 0) %>% as.factor()
)
final_data <- final_data %>%
  mutate(
    popularity_binary = ifelse(popularity >= 70, 1, 0) %>% as.factor()
  )





