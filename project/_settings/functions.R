
####################################################
# Loading 
load_libraries <- function(libraries) {
  for (library in libraries) {
    if (!require(library, character.only = TRUE)) {
      install.packages(library)
    }
    
    library(library, character.only = TRUE)
  }  
}

load_sources <- function(sources) {
  for (s in sources) {
    source(s)
  }  
}

load_datasets <- function(folder = "data") {
  datasets <- list()
  
  for (file in list.files(folder)) {
    path <- paste(folder, "/", file, sep="")
    name <- gsub("\\.\\w+$", "", file)
    
    datasets[[name]] <- read.csv(path)
  } 
  
  return(datasets)
}


####################################################


####################################################
## Models

####################################################