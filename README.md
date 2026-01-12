# IJC437
IJC437 module report code.
The code can be used to explore how the features of opoular music have evolved over time, since the 1950s. It also investigates whether machine learning models can effectively classify songs as popular using acoustic features and contextual metadata.

Research questions:
1. How have the acoustic features and musical characteristics of popular songs changed across different decades?
2. Can machine learning models accurately classify songs as ‘popular’ or ‘not popular’ based on their acoustic features, genre, and artist popularity?
3. Can acoustic features alone successfully classify songs as ‘popular’ or ‘not popular’?

Key findings:
1. Popular songs have become louder, more energetic and less acoustic.
2. The genre proportions of popular music has changed substantially since the 1960s.
3. Logistic regression models achieved high accuracy but were poor at identifying popular songs due to class imbalance.
4. Balanced random forest models improved recall for popular songs.
5. Acoustic features alone were ineffective for predicting song popularity.

Instructions for downloading and running the code:

The code utilises the 'MusicOSet' which can be found on the following link: https://marianaossilva.github.io/DSW2019/. The code only uses:
- songs.csv (includes song names, song popularity (on Spotify) and artist ID)
- acoustic_features.csv (includes acoustic feature values for each song. These included acousticness, danceability, energy, instrumentalness, liveness, loudness, speechiness, valence and tempo)
- albums.csv (includes album IDs, album names, artist IDs and album popularity (on Spotify))
- artists.csv (includes artist popularity scores (on Spotify), the artists’ main genre, and artist types)
- tracks.csv (includes release dates and album metadata)

These datasets need to be in the same folder as your R project in order to import them into R using the code provided.

The code is in three separate R documents:
1. musicpreprocessing.R
2. EDA.R
3. ML.R

Run them in the above order. Packages may need to be installed if not already.

Each code document can be run in its entirety at once. However, is imperative they are ran in the above order.

The code is commented throughout to aid understanding.



