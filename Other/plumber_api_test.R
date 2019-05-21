


library("plumber")

r <- plumb("/Users/mdturse/Desktop/Analytics/hard_drive_failure/Other/plumber_script.R")
r$run(port = 8000)


