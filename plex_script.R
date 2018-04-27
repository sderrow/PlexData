library('readr')
library('dplyr')
library('stringr')

fix_errs = function(x) {
  x = str_replace_all(x, '–', '-')
  x = str_replace_all(x, '’', "'")
  str_replace_all(x, '—', '-')
}

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

movies_raw = read_csv("Data/PlexMovieInput.csv")
movies_clean = movies_raw[, c("Title", "Year", "Genres", "Duration", "Video Resolution", "Part Size", "Content Rating", "Summary", "Added", "Sort title")]
colnames(movies_clean) = c("Title", "Year", "Genres", "Duration", "Resolution", "File Size", "Rating", "Summary", "Date Added", "Sort title")
movies_clean = mutate_all(movies_clean, funs(fix_errs))
write_excel_csv(movies_clean, "Data/SeanMovieLibrary.csv")

tv_raw = read_csv("Data/PlexTVInput.csv")
tv_raw$Year = as.integer(tv_raw$Year)
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
write_excel_csv(tv_clean, "Data/SeanTVLibrary.csv")
