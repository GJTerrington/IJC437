

# filter to popular songs onlt (popularity ≥ 70)
pop_data <- final_data %>%
  filter(popularity >= 70)

# keep only 1950s onwards and remove unknown decades
pop_data <- pop_data %>%
  filter(decade != "Unknown") %>%
  mutate(
    decade_num = as.numeric(str_remove(decade, "s"))
  ) %>%
  filter(decade_num >= 1950)

# select acoustic features
acoustic_features <- c(
  "acousticness", "danceability", "energy", "instrumentalness",
  "liveness", "loudness", "speechiness", "valence", "tempo"
)


# summary table (mean by decade)
acoustic_summary <- pop_data %>%
  group_by(decade) %>%
  summarise(across(all_of(acoustic_features), mean, na.rm = TRUE)) %>%
  ungroup()

# long format for faceted charts
acoustic_long <- acoustic_summary %>%
  pivot_longer(
    cols = all_of(acoustic_features),
    names_to = "feature",
    values_to = "value"
  )

# acoustic characteristics by decade faceted chart
ggplot(acoustic_long, aes(x = decade, y = value)) +
  geom_col(fill = "#2C7BB6") +
  facet_wrap(~ feature, scales = "free_y") +
  theme_minimal(base_size = 14) +
  labs(
    title = "Mean Acoustic Characteristics of Popular Songs (1960s–2010s)",
    subtitle = "Only songs with popularity ≥ 70, raw values as units",
    x = "Decade",
    y = "Mean"
  ) +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



# Map all detailed genres into broad categories

# Full genre list as a vector:
raw_genres <- c(
  "8-bit","a cappella","acid house","acid jazz","acoustic blues","acoustic opm",
  "acoustic pop","adult standards","afrobeat","afropop","alabama indie",
  "alabama metal","alabama rap","alaska indie","alberta country","album rock",
  "albuquerque indie","alternative country","alternative dance",
  "alternative hip hop","alternative metal","alternative pop",
  "alternative pop rock","alternative r&b","alternative rock",
  "american folk revival","anglican liturgy","anthem worship","anti-folk",
  "arkansas country","art pop","art rock","atl hip hop","audiophile vocal",
  "aussietronica","australian alternative rock","australian children's music",
  "australian country","australian dance","australian hip hop","australian indie",
  "australian pop","australian rock","austropop","avant-garde","azonto",
  "bachata","ballroom","baroque pop","bass trap","bass trip","bassline",
  "baton rouge rap","battle rap","bboy","beach music","bebop","belgian dance",
  "belgian pop","belgian rock","big band","big beat","big room","bluegrass",
  "blues","blues-rock","bolero","boogaloo","bossa nova","bounce",
  "bow pop","boy band","boy pop","brazilian house","brill building pop",
  "brit funk","british blues","british folk","british indie rock",
  "british invasion","british soul","britpop","broadway","brooklyn indie",
  "brostep","bubble trance","bubblegum dance","bubblegum pop",
  "buffalo ny metal","c86","cali rap","canadian ccm","canadian celtic",
  "canadian contemporary r&b","canadian country","canadian folk",
  "canadian hip hop","canadian indie","canadian latin","canadian pop",
  "canadian pop punk","canadian rock","canadian singer-songwriter",
  "canadian soundtrack","candy pop","cantautor","ccm","cello","celtic",
  "celtic rock","chamber pop","channel pop","chanson","chicago drill",
  "chicago house","chicago rap","chicago soul","chicano rap",
  "children's music","chinese hip hop","choro","christian metal",
  "classic belgian pop","classic canadian rock","classic danish pop",
  "classic garage rock","classic girl group","classic italian pop",
  "classic norwegian pop","classic nz pop","classic rock",
  "classic russian rock","classic schlager","classic soul",
  "classic soundtrack","classic swedish pop","classic uk pop",
  "classical","classical performance","classify","colombian pop","comedy",
  "comic","comic metal","conscious hip hop","contemporary country",
  "contemporary gospel","contemporary post-bop","cool jazz","cosmic american",
  "country","country dawn","country gospel","country road","country rock",
  "coverchill","cowpunk","crunk","crust punk","cumbia","cyberpunk",
  "czech rock","dance-punk","dance pop","dance rock","dancehall","danish pop",
  "deep acoustic pop","deep adult standards","deep classic garage rock",
  "deep contemporary country","deep disco","deep east coast hip hop",
  "deep free jazz","deep freestyle","deep funk","deep house",
  "deep melodic hard rock","deep motown","deep new americana","deep new wave",
  "deep northern soul","deep pop r&b","deep progressive trance",
  "deep soft rock","deep southern soul","deep southern trap",
  "deep talent show","deep vocal house","detroit hip hop","detroit house",
  "detroit trap","dfw rap","didgeridoo","dirty south rap","disco",
  "disco house","disney","diva house","doo-wop","downtempo","dub","dubstep",
  "dutch indie","dutch pop","east coast hip hop","easy listening","easycore",
  "ectofolk","ecuadorian pop","edm","electric blues","electro","electro house",
  "electro latino","electroclash","electrofox","electronic","electropop",
  "electropowerpop","emo","emo rap","escape room","eurodance","europop",
  "experimental","experimental rock","fake","florida rap","folk","folk-pop",
  "folk rock","freakbeat","freestyle","french pop","french shoegaze","funana",
  "funk","funk metal","funk rock","funky tech house","g funk","gabba",
  "gangster rap","garage rock","german hard rock","german metalcore",
  "german pop","german soundtrack","german techno","girl group","glam metal",
  "glam rock","go-go","gospel","gospel r&b","gospel reggae","gothic metal",
  "groove metal","grunge pop","hard rock","hardcore hip hop",
  "hardcore techno","hawaiian hip hop","healing","heartland rock","hi-nrg",
  "hip hop","hip house","hip pop","hollywood","hopebeat","horror punk",
  "houston rap","hyphy","icelandic indie","idol","indian indie","indie folk",
  "indie jazz","indie pop","indie pop rap","indie poptimism","indie r&b",
  "indie rock","indie shoegaze","instrumental funk","irish rock","israeli pop",
  "italian disco","j-metal","j-pop girl group","jam band","jangle pop",
  "japanese r&b","jazz blues","jazz electric bass","jazz funk","jazz guitar",
  "jazz organ","jazz saxophone","jazz trumpet","jazz vibraphone",
  "jazz violin","jewish pop","jump blues","k-indie","k-pop","karaoke",
  "kids dance party","la indie","latin","latin jazz","latin pop",
  "latin viral pop","lgbtq+ hip hop","lilith","lo-fi beats",
  "louisiana blues","lovers rock","lullaby","manitoba indie","melancholia",
  "mellow gold","memphis soul","merseybeat","metal","metropopolis",
  "miami bass","miami hip hop","michigan indie","minneapolis sound",
  "mod revival","modern alternative rock","modern country rock","modern rock",
  "motown","movie tunes","mpb","music box","musica infantil",
  "musica para ninos","muzica crestina","nashville sound",
  "native american","nc hip hop","nederpop","neo-rockabilly",
  "neo-singer-songwriter","neo-traditional country","neo mellow","neo r&b",
  "neo soul","neo soul-jazz","new jack smooth","new jack swing",
  "new jersey rap","new orleans rap","new romantic","new wave pop",
  "northern soul","nu-metalcore","nyc pop","nyc rap","old school hip hop",
  "operatic pop","orchestra","outlaw country","outsider","p funk","permanent wave",
  "philly rap","philly soul","pianissimo","piano cover","piano rock",
  "pinoy praise","pop","pop edm","pop punk","pop rap","pop rock",
  "post-disco","post-disco soul","post-grunge","post-teen pop","power pop",
  "progressive rock","psychedelic blues-rock","psychedelic folk",
  "psychedelic rock","pub rock","quiet storm","r&b","rap","rap conscient",
  "rap rock","rare groove","reggae","reggae fusion","reggaeton",
  "reunion pop","rhythm and blues","rock","rock-and-roll","rock steady",
  "romantico","roots rock","scorecore","scottish americana",
  "scottish new wave","scottish singer-songwriter","screamo punk",
  "sertanejo","show tunes","sleep","smooth jazz","soca","soft rock",
  "song poem","soul","soul blues","soul jazz","southern rock",
  "southern soul","space age pop","steelpan","sunshine pop","surf music",
  "swamp pop","swedish alternative rock","synthpop","talent show","teen pop",
  "tejano","traditional country","traditional soul","trap music","trip hop",
  "tropical house","uk reggae","Unknown","urban contemporary","vapor pop",
  "viral trap","vocal house","vocal jazz","workout product","yacht rock"
)

# Put into a data frame
genre_df <- data.frame(genre = raw_genres, stringsAsFactors = FALSE)

# Categorisation rules
genre_categories <- genre_df %>%
  mutate(
    genre_group = case_when(
      str_detect(genre, "pop|synth|electro|teen|bubble|art pop|pop rock|pop punk") ~ "Pop",
      str_detect(genre, "rock|metal|punk|garage|grunge|hard rock|alternative rock") ~ "Rock/Metal",
      str_detect(genre, "hip hop|rap|trap|drill|crunk") ~ "Hip Hop/Rap",
      str_detect(genre, "edm|house|techno|trance|electronic|dubstep|dance") ~ "Electronic/Dance",
      str_detect(genre, "soul|r&b|funk|motown|disco") ~ "Soul/R&B/Funk",
      str_detect(genre, "latin|bachata|cumbia|reggaeton|bolero|salsa") ~ "Latin",
      str_detect(genre, "country|americana|bluegrass|folk|singer") ~ "Country/Folk",
      str_detect(genre, "jazz|blues|bebop|swing|ragtime") ~ "Jazz/Blues",
      TRUE ~ "Other"
    )
  )

# preview
head(genre_categories, 20)

# join datasets by genre
final_data <- final_data %>%
  left_join(genre_categories, by = "genre")

# create data frame for genre proportions of popular songs 
genre_props <- final_data %>%
  filter(popularity > 70) %>%
  group_by(decade, genre_group) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(decade) %>%
  mutate(prop = n / sum(n))


# Prepare label positions
genre_props_plot <- genre_props %>%
  group_by(decade) %>%
  arrange(desc(genre_group)) %>%
  mutate(
    ypos = cumsum(prop) - 0.5 * prop
  ) %>%
  ungroup()

# Donut chart
ggplot(genre_props_plot, aes(x = 2, y = prop, fill = genre_group)) +
  geom_col(color = "white", width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~ decade) +
  xlim(0.5, 2.5) +
  theme_void() +
  theme(
    legend.position = "right",
    strip.text = element_text(size = 12, face = "bold")
  ) +
  labs(
    title = "Genre Distribution of Popular Songs by Decade",
    fill = "Genre"
  )


# distribution of song popularity
ggplot(final_data, aes(x = popularity)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  theme_minimal() +
  labs(title = "Distribution of Song Popularity")

# now we have genre_group, we can keep the variables we need to use for the models
ml_data <- final_data %>%
  select(
    popularity_binary,
    genre,
    artist_popularity,
    artist_pop_missing,
    artist_type,
    explicit,
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
    tempo,
    decade,
    genre_group
  )

