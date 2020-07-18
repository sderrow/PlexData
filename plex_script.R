library(tidyverse)
library(readxl)
library(lubridate)

fix_errs = function(x) {
  x = str_replace_all(x, '-', '-')
  x = str_replace_all(x, "'", "'")
  x = str_replace_all(x, '&#34;', '"')
  x = str_replace_all(x, "&#39;", "'")
  x = str_replace_all(x, '"', '"')
  x = str_replace_all(x, '"', '"')
  str_replace_all(x, '-', '-')
}

get_subtitle_info = function(df) {
  df %>%
    select(Title, Year, `Subtitle Stream Codec`, `Subtitle Stream Language`, `Subtitle Stream Title`) %>%
    filter(!grepl("srt", `Subtitle Stream Codec`) | grepl("forced", `Subtitle Stream Title`))
}

get_audio_info = function(df) {
  df %>%
    select(Title, `Audio Title`, `Audio Languages`, `Audio Stream Bitrate`)
}

create_bitrate_lookup_table = function(df) {
  File_Sizes = df %>%
    mutate(Seconds = as.numeric(Duration)) %>%
    select(Title, Year, Added,  Updated, Duration, `Part Size as Bytes`, 
           `Video Resolution`, Width, Height, `Video Stream Bitrate`, 
           `Audio Languages`, `Audio Stream Codec`, `Audio Stream Audio Channel Layout`, `Audio Stream Bitrate`, 
           `Subtitle Stream Codec`, `Subtitle Stream Language Code`, `Subtitle Stream Title`, Seconds
           ) %>%
    arrange(Title, Year)
  
  write_excel_csv(File_Sizes, "sizes.csv", col_names = FALSE)
}

create_lookup_to_rename_movies = function(df) {
  ans = df %>%
    mutate(ext = str_sub(`Part File`, regexpr("\\.[^\\.]*$", `Part File`), -1),
           New_Name = paste0(Title, " (", Year, ")", ext)) %>%
    select(Original_Name = `Part File`, New_Name, Title, `MetaDB Link`) %>%
    arrange(Original_Name)
  write_delim(ans, "movies.txt", delim="|", col_names = FALSE)
}

create_lookup_to_rename_tv = function(df) {
  tv = df %>%
    mutate(`Episode Title` = fix_errs(`Episode Title`)) %>%
    group_by(`Series Title`, Season, `Part File`) %>%
    summarize(counter = n(),
              i1 = which.min(Episode),
              i2 = which.max(Episode),
              e1 = Episode[i1],
              e2 = Episode[i2],
              e_title_1 = `Episode Title`[i1],
              e_title_2 = `Episode Title`[i2]
    ) %>%
    ungroup() %>%
    mutate(ext = str_sub(`Part File`, regexpr("\\.[^\\.]*$", `Part File`), -1),
           New_Name = ifelse(counter == 1,
                             sprintf("%s - s%02de%02d - %s%s", `Series Title`, Season, e1, e_title_1, ext),
                             ifelse(str_sub(e_title_1, 1,-5) == str_sub(e_title_2,1,-5),
                                    sprintf("%s - s%02de%02d-e%02d - %s%s", `Series Title`, Season, e1, e2, str_sub(e_title_1, 1,-5), ext),
                                    sprintf("%s - s%02de%02d-e%02d - %s - %s%s", `Series Title`, Season, e1, e2, e_title_1, e_title_2, ext)
                             )
           )
    ) %>%
    select(`Part File`, New_Name)
  write.table(tv, "tvepisodes.txt", sep="|", quote = FALSE, col.names = FALSE, row.names = FALSE)
}

read_movies = function(pathname) {
  read_csv(pathname, guess_max = 10000) %>%
    mutate_all(list(fix_errs)) %>%
    mutate(Duration = hms::as_hms(Duration)) %>%
    mutate_at(c("Release Date", "Added", "Updated"), ymd) %>%
    mutate_at(c("Rating", "Aspect Ratio"), as.numeric) %>%
    mutate_at(c("Media ID", "Year", "Extras", "Bitrate", "Width", "Height", "Audio Channels"), as.integer)
}

read_tv = function(pathname) {
  read_csv(pathname, guess_max = 10^5) %>%
    mutate_all(list(fix_errs)) %>%
    mutate_at(c("Duration", "Media Video Duration"), hms::as.hms) %>%
    mutate_at(c("Originally Aired", "Added", "Updated"), ymd) %>%
    mutate_at(c("Media Video Aspect Ratio"), as.numeric) %>%
    mutate_at(c("Media ID", "Year", "Season", "Episode", "Extras", "Media Video Bitrate", "Media Video Width", "Media Video Height", "Media Video Audio Channels"), as.integer)
}

raw_movies = read_movies("ExportTools/1b. Temp Movies-Level 6-20200717-100316.csv")
new_movies = raw_movies %>%
  filter(grepl("New Additions", `Part File Path`))
create_bitrate_lookup_table(new_movies)
create_lookup_to_rename_movies(new_movies)


raw_tv = read_tv("ExportTools/2b. Temp TV-Level 6-20200717-100316.csv")
some = raw_tv %>%
  filter(Added >= "2020-06-12")
dawg = some %>%
  select(`Series Title`, Season, Episode, `Media Video Resolution`, `Video Stream Bitrate`, `Audio Stream Codec`, `Audio Stream Channels`, `Audio Stream Language`, `Audio Stream Bitrate`, `Subtitle Stream Codec`, `Subtitle Stream Language`, `Subtitle Stream Title`)
create_lookup_to_rename_tv(some)

forced = dawg %>%
  filter(grepl("Forced", `Subtitle Stream Title`))
