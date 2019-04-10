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
  
  write_excel_csv(File_Sizes, "sizes.csv")
}

create_lookup_to_rename_movies = function(df) {
  ans = df %>%
    mutate(ext = str_sub(`Part File`, regexpr("\\.[^\\.]*$", `Part File`), -1),
           New_Name = paste0(Title, " (", Year, ")", ext)) %>%
    select(Original_Name = `Part File`, New_Name) %>%
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
    mutate_all(funs(fix_errs)) %>%
    mutate(Duration = hms::as.hms(Duration)) %>%
    mutate_at(c("Release Date", "Added", "Updated"), ymd) %>%
    mutate_at(c("Rating", "Aspect Ratio"), as.numeric) %>%
    mutate_at(c("Media ID", "Year", "Extras", "Bitrate", "Width", "Height", "Audio Channels"), as.integer)
}

read_tv = function(pathname) {
  read_csv(pathname, guess_max = 10^5) %>%
    mutate_all(funs(fix_errs)) %>%
    mutate_at(c("Duration", "Media Video Duration"), hms::as.hms) %>%
    mutate_at(c("Originally Aired", "Added", "Updated"), ymd) %>%
    mutate_at(c("Media Video Aspect Ratio"), as.numeric) %>%
    mutate_at(c("Media ID", "Year", "Season", "Episode", "Extras", "Media Video Bitrate", "Media Video Width", "Media Video Height", "Media Video Audio Channels"), as.integer)
}

get_anti_intersection_of_movies = function(mov, tmp_mov) {
  mov = mov %>%
    mutate(uid = paste0(Title, " (", Year, ")")) %>%
    select(uid)
  tmp_mov = tmp_mov %>%
    mutate(uid = paste0(Title, " (", Year, ")")) %>%
    select(uid)
  tmp_mov %>%
    anti_join(mov, by = "uid")
}

raw_movies = read_movies("ExportTools/1c. Temp Movies-Level 6-20190401-220215.csv")
new_movies = raw_movies %>%
  filter(grepl("New Additions", `Part File Path`))

create_bitrate_lookup_table(new_movies)
create_lookup_to_rename_movies(new_movies)

newbs = get_anti_intersection_of_movies(movies_raw, raw_movies)

torrents = list.files(path = "U:/Torrent Files/", full.names = T)
info = file.info(torrents)
final_info = as_tibble(info, rownames = "torrent") %>%
  mutate_at(c("mtime", "ctime", "atime"), as_date)
write_csv(final_info, "torrent_info.csv")

raw_tv = read_tv("ExportTools/2b. Temp TV-Level 6-20190404-081842.csv")
some = raw_tv %>%
  filter(!(`Series Title` %in% c("Altered Carbon")))
dawg = some %>%
  select(`Series Title`, Season, Episode, `Media Video Resolution`, `Video Stream Bitrate`, `Audio Stream Codec`, `Audio Stream Channels`, `Audio Stream Language`, `Audio Stream Bitrate`, `Subtitle Stream Codec`, `Subtitle Stream Language`, `Subtitle Stream Title`)
create_lookup_to_rename_tv(some)

forced = dawg %>%
  filter(grepl("Forced", `Subtitle Stream Title`))

fsize = function(x) {
  if (x > 1024^3) {
    ans = sprintf("%.2f GB", as.numeric(x) / 1024^3)
  } else {
    ans = sprintf("%.0f MB", as.numeric(x) / 1024^2)
  }
  ans
}

yrange = function(a, b) {
  if (a == b) {
    ans = as.character(a)
  } else {
    ans = sprintf("%d-%d", a, b)
  }
  ans
}

clean_coll = function(x, y) {
  x = ifelse(x=="N/A",y,x)
  x = gsub("^The ", "", x)
  x = gsub("^A ", "", x)
  x
}

movies_raw= read_movies("ExportTools/1a. Movies-Level 6-20190311-223114.csv")
movies_clean = movies_raw %>%
  select(Collections,
         Title,
         Year,
         Genres,
         Duration,
         Resolution = `Video Resolution`,
         `File Size` = `Part Size`,
         Rating = `Content Rating`,
         Directors,
         Summary,
         `Video Bitrate` = `Video Stream Bitrate`,
         `Audio Tracks` = `Audio Stream Audio Channel Layout`,
         `Audio Bitrate` = `Audio Stream Bitrate`,
         Subtitles = `Subtitle Languages`,
         Container,
         `Date Added` = Added,
         `Sort title`
         ) %>%
  mutate(Collections = clean_coll(Collections, `Sort title`),
         Subtitles = ifelse(Subtitles == "N/A", "No", "Yes")) %>%
  arrange(Collections, Year)
write_excel_csv(movies_clean, "ExportTools/SeanMovieLibrary.csv")

tv_raw = read_tv("ExportTools/TV Shows-Level 6-20190117-152141.csv")
tv_raw$`Part Size as Bytes` = unlist(lapply(lapply(str_split(tv_raw$`Part Size as Bytes`, ';'), as.numeric), mean))
tv_clean = tv_raw %>%
  group_by(`Series Title`, Season) %>%
  summarize(Episodes = n(),
            Year = yrange(min(Year, na.rm = TRUE), max(Year, na.rm = TRUE)),
            `Average Duration` = format(as.POSIXct(mean(Duration)), format = "%T"),
            Resolution = toupper(names(which.max(table(`Media Video Resolution`)))),
            `Average File Size` = fsize(mean(`Part Size as Bytes`)),
            Rating = names(which.max(table(`Content Rating`))),
            `Date Added` = max(Added, na.rm = TRUE)
            ) %>%
  ungroup()
write_excel_csv(tv_clean, "ExportTools/SeanTVLibrary.csv")

names(a)
df = as.matrix(a)
df["Duration",1] = "Time"
df["Part Duration", 1] = "Time"
dawg = tibble(Column = rownames(df), Type = as.character(df))
