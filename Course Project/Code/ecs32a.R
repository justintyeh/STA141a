library(tidyverse)
file = read_csv(file.choose())
ggplot(data = file, aes(x = Year,y = Value)) +
         geom_line() + 
         ggtitle("Sacramento July Temperatures (Moving Ave k = 19)") +
         xlab("Year") + ylab("Temperature Anomaly") + geom_hline(yintercept = 0, linetype = "dashed", color = "red")
       
nrow(file)
