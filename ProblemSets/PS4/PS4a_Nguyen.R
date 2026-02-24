# Practice with JSON files

# Load/Install Packages
# install.packages("jsonlite")
# install.packages("tidyverse")

library(jsonlite)
library(tidyverse)

#(a) Download the JSON file
system('wget -O dates.json "https://www.vizgr.org/historical-events/search.php?format=json&begin_date=00000101&end_date=20240209&lang=en"')

#(b) Print the file to the console

# system("cat dates.json")

#(c) Clean the data
mylist <- fromJSON('dates.json')
mydf <- bind_rows(mylist$result[-1])

#(d) What type of object is mydf
class(mydf)
class(mydf$date)

#(e) First nth rows

# First 10 rows
head(mydf, 10)

# First 20 rows
head(mydf, 20)


