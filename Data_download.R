#The following file is how I obtained the data from the website https://datahub.io/machine-learning/bioresponse

library("jsonlite")

json_file <- 'https://datahub.io/machine-learning/bioresponse/datapackage.json'
json_data <- fromJSON(paste(readLines(json_file), collapse=""))

# get list of all resources:
print(json_data$resources$name)

# print all tabular data(if exists any)
for(i in 1:length(json_data$resources$datahub$type)){
  if(json_data$resources$datahub$type[i]=='derived/csv'){
    path_to_file = json_data$resources$path[i]
    data <- read.csv(url(path_to_file))
    print(data)
  }
}

saveRDS(data, file = "data.rds")

