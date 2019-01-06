library('tidyverse')
library('countrycode')
library('tidytext')

speakers <- read_csv('Speakers_by_session.csv')

countries <- countrycode(c('Canada', 
                           'Australia', 
                           'United States', 
                           'New Zealand', 
                           'United Kingdom'), 
                         'country.name', 'iso3c')

speakers_of_interest <- speakers %>%
    filter(`ISO Code` %in% countries) %>%
    mutate(file = paste0('sessions_transcripts/Session ', Session, ' - ', Year, 
                              '/', `ISO Code`, '_', Session, '_', Year, '.txt')) %>%
    select(-c('Language', 'Notes'))

parse_files <- function(Session, Year, `ISO Code`, ...){
    file_name <- paste0('sessions_transcripts/Session ', Session, ' - ', Year, 
                        '/', `ISO Code`, '_', Session, '_', Year, '.txt')
    
    text_file <- read_table(file_name, col_names = FALSE)
    
    words <- unnest_tokens(text_file, word, X1) %>%
        mutate(file = file_name)
    
    return(words)
}

all_words <- pmap_df(speakers_of_interest, parse_files) %>%
    select(word, file)

words_of_interest <- speakers_of_interest %>%
    left_join(all_words, by = c('file' = 'file')) %>%
    rename(year = Year, session = Session, iso_code = `ISO Code`, country = Country, person_speaking =`Name of Person Speaking`, post = Post) %>%
    select(-file)

words_of_interest %>%
    write_csv('united_nations_five_data.csv')
