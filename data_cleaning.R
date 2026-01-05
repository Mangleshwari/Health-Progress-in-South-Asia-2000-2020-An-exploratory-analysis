##importing data set
data = read.csv("data/health_progress_in_SA.csv", header = T)
attach(data)
##inspecting structure
summary(data)
##removing empty rows
sum(rowSums(is.na(data)))
rowSums(is.na(data))
ncol(data)
cleaned_data = data[-(73:87),]
sum(rowSums(is.na(cleaned_data)))
sum(colSums(is.na(cleaned_data)))
##changing years name
names(cleaned_data) = gsub("^X(\\d{4})\\.\\.YR\\d{4}\\.$", "\\1", names(cleaned_data))
sapply(cleaned_data, is.numeric)
detach(data)
attach(cleaned_data)
cleaned_data[5:29] = lapply(cleaned_data[5:29], as.numeric)
cleaned_data$`2024`= NULL   #deleting year 2024 column
years = grep("^20", names(cleaned_data), value = TRUE)
data_long = reshape(cleaned_data, varying = years, v.names = "value", timevar = "Year", times = years, direction = "long")
rownames(data_long) = NULL
data_long$Year = as.numeric(data_long$Year)
data_long$id = NULL
names(data_long) = tolower(gsub("[^a-zA-Z0-9]", "_", names(data_long))) #standardized column names
data_long$year = as.numeric(data_long$year)
data_long$value = as.numeric(data_long$value)
detach(cleaned_data)
attach(data_long)

data_wider <- reshape(
  data_long,
  timevar = "year",
  idvar = c("country_name", "series_name"),
  direction = "wide"
)
names(data_wider) <- gsub("value\\.", "", names(data_wider))
write.csv(data_wider, file = "health_progress_wide.csv", row.names = FALSE)

