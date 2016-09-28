######
# Data Camp
######
# Exploring Pitch Data with R
######

greinke = read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_943/datasets/greinke_ff.csv") 


# Print the first 6 rows of the data
head(greinke)

# Print the number of rows in the data frame
nrow(greinke)

# Summarize the start_speed variable
summary(greinke$start_speed)

# Get rid of data without start_speed
greinke <- subset(greinke, !is.na(start_speed))

# Print the number of complete entries
nrow(greinke)

# Print the structure of greinke
str(greinke)

# Check if dates are formatted as dates
class(greinke$game_date)

# Change them to dates
greinke$game_date <-  as.Date(greinke$game_date, "%m/%d/%Y")


# Check that the variable is now formatted as a date
class(greinke$game_date)

library(tidyr)

# Separate game_date into "year", "month", and "day"
greinke <- separate(data = greinke, col = game_date,
                    into = c("year", "month", "day"),
                    sep = "-", remove = FALSE)

# Convert month to numeric
greinke$month <- as.numeric(greinke$month)

# Create the july variable
greinke$july <- as.factor(ifelse(greinke$month == 7, "july", "other"))

# View the head() of greinke
head(greinke)

# Print a summary of the july variable
summary(greinke$july)

# Make a histogram of Greinke's start speed
hist(greinke$start_speed)

# Create greinke_july
greinke_july = subset(greinke, july == "july")
str(greinke_july)
# Create greinke_other
greinke_other = subset(greinke, july == "other")
str(greinke_other)

# Use par to format your plot layout
par(mfrow = c(1, 2))

# Plot start_speed histogram from july
hist(greinke_july$start_speed)

# Plot start_speed histogram for other months
hist(greinke_other$start_speed)

# Create july_ff
july_ff <- subset(greinke_july, pitch_type == "FF")

# Create other_ff
other_ff <- subset(greinke_other, pitch_type == "FF")

# Formatting code, don't change this
par(mfrow = c(1, 2))

# Plot histogram of July fastball speeds
hist(july_ff$start_speed)

# Plot histogram of other month fastball speeds
hist(other_ff$start_speed)

# Make a fastball speed histogram for other months
hist(other_ff$start_speed,
     col = "#00009950", freq = FALSE,
     ylim = c(0, .35), xlab = "Velocity (mph)",
     main = "Greinke 4-Seam Fastball Velocity")

# Add a histogram for July
hist(july_ff$start_speed,
     add = TRUE,
     col = "#99000050", freq = FALSE,
     ylim = c(0, .35), xlab = "Velocity (mph)",
     main = "Greinke 4-Seam Fastball Velocity")

# Draw vertical line at the mean of other_ff
abline(v = mean(other_ff$start_speed), col = "#00009950", lwd = 2)

# Draw vertical line at the mean of july_ff
abline(v = mean(july_ff$start_speed), col = "#99000050", lwd = 2)

# Summarize velocity in July and other months
tapply(greinke$start_speed, greinke$july, mean)

# Create greinke_ff
greinke_ff <- subset(greinke, pitch_type == "FF")

# Calculate mean fastball velocities: ff_velo_month
ff_velo_month <- tapply(greinke_ff$start_speed, greinke_ff$july, mean)

# Print ff_velo_month
ff_velo_month

# Create ff_dt
ff_dt <- data.frame(tapply(greinke_ff$start_speed, greinke_ff$game_date, mean))

# Print the first 6 rows of ff_dt
head(ff_dt)

#show the row names
row.names(ff_dt)

# Create game_date in ff_dt
ff_dt$game_date <- as.Date(row.names(ff_dt), "%Y-%m-%d")

# Rename the first column
colnames(ff_dt)[1] <- "start_speed"

# Remove row names
row.names(ff_dt) <- NULL

# View head of ff_dt
head(ff_dt)

dev.off()

# Plot game-by-game 4-seam fastballs
plot(ff_dt$start_speed ~ ff_dt$game_date,
     lwd = 4, type = "l", ylim = c(88, 95),
     main = "Greinke 4-Seam Fastball Velocity",
     xlab = "Date", ylab = "Velocity (mph)")
par(new = TRUE)
# Add jittered points to the plot
plot(greinke_ff$start_speed ~ 
       jitter(as.numeric(greinke_ff$game_date)), pch = 16, col = "#99004450",
     axes = FALSE, ## don't redraw the axes 
     xlab = '', ylab = '',
     ylim = c(88, 95))


############
# Pitch Mix and Location
############
#greinke = read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_943/datasets/greinke_ff.csv") 
#greinke = read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_943/datasets/greinke.csv") 

# Subset the data to remove pitch types "IN" and "EP"
greinke <- subset(greinke, pitch_type != "IN" | pitch_type != "EP")
greinke <- subset(greinke, pitch_type != "EP")
#subset(greinke, pitch_type == "EP")
#summary(greinke$pitch_type)
# Drop the IN and EP levels from pitch_type
levels(greinke$pitch_type)
greinke$pitch_type <- droplevels(greinke$pitch_type)
# Create type_tab as an object storing a table() of the number of pitches of each type thrown, 
# broken down by the greinke$july variable.
type_tab <- table(greinke$pitch_type, greinke$july)

# Print type_tab
type_tab

# Create type_prop table with margin of 2 and round to 3 places
type_prop <- round(prop.table(type_tab, margin = 2), 3)

# Print type_prop
type_prop

# Create ff_prop a new object using only the third row of the type_prop table to get only 4-seam fastball data.
ff_prop <- type_prop[3,]

# Print ff_prop
ff_prop

# Print ff_velo_month
ff_velo_month

# Create type_prop$Difference by subtracting the Other column from the July column in the data frame 
# and dividing that result by the value in the Other column.
type_prop$Difference <- (type_prop$July - type_prop$Other)/type_prop$Other

# Print type_prop
type_prop

# Plot a barplot
barplot(type_prop$Difference, names.arg = type_prop$Pitch,
        main = "Pitch Usage in July vs. Other Months", 
        ylab = "Percentage Change in July", 
        ylim = c(-0.3, 0.3))



# While pitch types are interesting in their own right, it might be more useful to think about 
# what types of pitches Greinke uses in different ball-strike counts. Before getting to that, 
# you'll first take make a table to examine the rate at which Greinke throws pitches in each of 
# the ball-strike counts
# Create bs_table as a table() using the balls and strikes variables from greinke. 
# Put the balls values as rows in the table.
bs_table <- table(greinke$balls, greinke$strikes)
bs_table
# Create bs_prop_table
bs_prop_table <- round(prop.table(bs_table), 3)

# Print bs_prop_table
bs_prop_table

# Print row sums
rowSums(bs_prop_table)

# Print column sums
colSums(bs_prop_table)

# To simplify future exercises, you'll now create a new variable called bs_count that combines 
# the balls and strikes variables into a single ball-strike count.

# paste() together the balls and strikes columns of greinke to create a new column called bs_count. 
# Use sep = "-" to separate the values with a dash

# Create bs_count
greinke$bs_count <- paste(greinke$balls, greinke$strikes, sep = "-")

# Print the first 6 rows of greinke
head(greinke)

