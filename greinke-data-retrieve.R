ff_dt = read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_943/datasets/ff_dt2.csv") 
ff_dt$game_date = as.Date(ff_dt$game_date) 
greinke_ff = read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_943/datasets/greinke_ff.csv") 
greinke_ff$game_date = as.Date(greinke_ff$game_date) 


# Code from previous exercise, don't change this plot(ff_dt$start_speed ~ ff_dt$game_date, lwd = 4, type = "l", ylim = c(88, 95), main = "Greinke 4-Seam Fastball Velocity", xlab = "Date", ylab = "Velocity (mph)") # Add jittered points to the plot points(greinke_ff$start_speed ~ jitter(as.numeric(greinke_ff$game_date)), pch = 16, col = "#99004450") # Code from previous exercise, don't change this plot(ff_dt$start_speed ~ ff_dt$game_date, lwd = 4, type = "l", ylim = c(88, 95), main = "Greinke 4-Seam Fastball Velocity", xlab = "Date", ylab = "Velocity (mph)") # Add jittered points to the plot points(greinke_ff$start_speed ~ jitter(as.numeric(greinke_ff$game_date)), pch = 16, col = "#99004450") - Read more at: http://scl.io/Cf3og70m#gs.gA7wrUY