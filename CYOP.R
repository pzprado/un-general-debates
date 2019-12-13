
######################################################
## Create edx set, validation set
######################################################

# Note: this script could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(tidytext)) install.packages("tidytext", repos = "http://cran.us.r-project.org")

data <- read.csv("~/Downloads/un-general-debates.csv")


dl_csv <- tempfile()
download.file("https://www.kaggle.com/unitednations/un-general-debates#un-general-debates.csv", dl_csv)

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

token <-"a96d947981acb34c8009101debd29d6b"