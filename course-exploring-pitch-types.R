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

# Now that you've created the bs_count variable, it's time to put it to use. In this exercise, 
# you'll identify the percentage change in the rate at which Greinke put himself in each of the 
# ball-strike counts. Let's get more practice making a proportions table.

# Create bs_count_tab
bs_count_tab <- table(greinke$bs_count, greinke$july)

# Create bs_month
bs_month <- round(prop.table(bs_count_tab, margin = 2), 3)

# Print bs_month
bs_month

# As you did with pitch type changes, here you'll create a bar plot to visualize how common 
# each ball-strike count was in July vs. other months. Let's get started.

# As you did with pitch type changes, here you'll create a bar plot to visualize how common 
# each ball-strike count was in July vs. other months. Let's get started.

# Create diff_bs
diff_bs <- round((bs_month[,1] - bs_month[,2])/bs_month[,2], 3)

# Print diff_bs
diff_bs

# Create a bar plot of the changes
barplot(diff_bs, main = "Ball-Strike Count Rate in July vs. Other Months", 
        ylab = "Percentage Change in July", ylim = c(-0.15, 0.15), las = 2)


# Interestingly, Greinke was in more hitter friendly counts in July than the other months in which 
# he pitched. That's a somewhat unexpected result.
# 
# It could be that he was more willing to use off-speed pitches earlier in the count, and if 
# those are thrown for strikes less often, then this might be driving this result. You will look at these outcomes a bit more in the last chapter of this course.
# 
# For this exercise, you will take a look to see if Greinke used certain pitches more or less 
# often in specific counts overall. In particular, you'll tabulate the proportion of times he throws 
# each pitch for each count

# Create type_bs as a table() comparing the pitch_type with bs_count. Put the pitch_type values as 
# the rows in the table.

# Create type_bs as a table() comparing the pitch_type with bs_count. Put the pitch_type values 
# as the rows in the table.

# Create type_bs
type_bs <- table(greinke$pitch_type, greinke$bs_count)

# Print type_bs
type_bs

# Create type_bs_prop
type_bs_prop <- round(prop.table(type_bs, margin = 2), 3)

# Print type_bs_prop
type_bs_prop


# There is often talk about pitchers having more trouble late in games. There are a number of reasons 
# for this. They could be getting tired and losing velocity, or batters may have already seen pitches 
# they throw. Given this, we'll try to see if Greinke resorts more to his off-speed pitches later in games.
# 
# First, you will create a variable indicating that a pitch was thrown late in a game, defined as any 
# pitch past the 5th inning. Then, you can make a table of pitch selection for late-game pitches.

# Create a new variable called late_in_game inside the greinke dataset and set it equal to 1 if 
# greinke$inning > 5 and to 0 otherwise.
# Create the late_in_game column

# Create greinke_table
greinke_table <- table(greinke_sub$zone)

# Create zone_prop
zone_prop <- round(prop.table(greinke_table), 3)

# Plot strike zone grid, don't change this
plot_grid()

# Add text from zone_prop[1]
text(-1.5, 4.5, zone_prop[1], cex = 1.5)


greinke$late_in_game <- ifelse(greinke$inning > 5, 1, 0)

# Convert late_in_game
greinke$late_in_game <- as.factor(greinke$late_in_game)

# Create type_late
type_late <- table(greinke$pitch_type, greinke$late_in_game)

# Create type_late_prop
type_late_prop <- round(prop.table(type_late, margin = 2), 3)

# Print type_late_prop
type_late_prop


# This exercise will make use of a grouped barplot, so that you can assess whether there are 
# changes in pitch selection for specific pitches early vs. late in the game.
# 
# You will again use the barplot() function, which also allows for creation of grouped barplots, 
# paired with the transpose function, t(), and the parameter beside = TRUE.

# Transpose the type_late table from the last exercise using the t() function. Store the result as t_type_late.

# Create t_type_late
t_type_late <- t(type_late)

# Print dimensions of t_type_late
dim(t_type_late)

# Print dimensions of type_late
dim(type_late)

# Change row names
rownames(t_type_late) <- c("Early", "Late")

# Make barplot using t_type_late
barplot(t_type_late, beside = TRUE, col = c("red", "blue"), 
        main = "Early vs. Late In Game Pitch Selection", 
        ylab = "Pitch Selection Proportion", 
        legend = rownames(t_type_late))



#####
# Pitch location and Greinke's July
#####
# In this exercise, you will start to use the horizontal and vertical location variables: 
#   px and pz, respectively. You'll begin by calculating the average pitch height pz for 
# Greinke in July relative to other months using the code provided. Note that it's multiplied 
# by 12 so that your answer is in inches, while the variable is recorded in feet.
# 
# In the second part of the exercise, you'll find the average horizontal location to 
# left-handed batters (LHB) and right-handed batters (RHB), respectively. Remember that a 
# positive px value is outside against righties and inside against lefties. Do this by subsetting 
# the data into RHB and LHB subsets, then using tapply() as you have in previous exercises.

# Calculate average pitch height in inches in July vs. other months
tapply(greinke$pz, greinke$july, mean) * 12

# Create greinke_lhb
greinke_lhb <- subset(greinke, batter_stand == "L")

# Create greinke_rhb
greinke_rhb <- subset(greinke, batter_stand == "R")

# Compute average px location for LHB
tapply(greinke_lhb$px, greinke_lhb$july, mean) * 12

# Compute average px location for RHB
tapply(greinke_rhb$px, greinke_rhb$july, mean) * 12


# As you saw in the previous exercise, Greinke was pitching much closer to the center of the plate 
# to both left- and right-handed batters in July. But it's often more helpful to visualize the pitch 
# location, rather than guess based on averages of horizontal location numbers.
# 
# You should begin by looking at a single plot, color-coded by month. In the subsequent exercises, 
# you will adjust the plots to help elucidate differences in pitch location using different plotting strategies.
# 
# The first line of code is completed for you, plotting Greinke's pitches to all batters, color-coded 
# by the july variable. However, it is somewhat difficult to see due to all of the overlap. Let's try 
# plotting the groups on side-by-side scatter plots.

# Plot location of all pitches
plot(greinke$pz ~ greinke$px,
     col = factor(greinke$july),
     xlim = c(-3, 3))

# Formatting code, don't change this
par(mfrow = c(1, 2))

# Plot the pitch loctions for July
plot(pz ~ px, data = greinke_july,
     col = "red", pch = 16,
     xlim = c(-3, 3), ylim = c(-1, 6),
     main = "July")

# Plot the pitch locations for other months
plot(pz ~ px, data = greinke_other,
     col = "black", pch = 16,
     xlim = c(-3, 3), ylim = c(-1, 6),
     main = "Other months")

# Plotting each group on the different panels didn't seem to help much. One way to get around the lack of 
# useful interpretation from a scatter plot is to bin the data. Binning data into groups and plotting it 
# as a grid is a way of summarizing the location of pitches. While we won't cover density estimation in 
# this course, there are direct relationships between visualizing locational density and the simple binning 
# you will perform here.
# 
# You will begin by subsetting the data to exclude any pitch well outside the strike zone. You can define 
# "well outside the strike zone" as any pitch more than 2 feet inside/outside from the center of the plate, 
# below 0 feet (i.e. bouncing in front of the plate), or above 5 feet.

# Create greinke_sub as a subset() of greinke to keep only pitches that are not well outside of the strike 
# zone. In other words, keep pitches with px values strictly greater than -2 and strictly less than 2 and pz 
# values strictly greater than 0 and strictly less than 5.

# Create greinke_sub
greinke_sub <- subset(greinke, px > -2 & px < 2 & pz > 0 & pz < 5)

# Plot pitch location window
plot(x = c(-2, 2), y = c(0, 5), type = "n",
     main = "Greinke Locational Zone Proportions",
     xlab = "Horizontal Location (ft.; Catcher's View)",
     ylab = "Vertical Location (ft.)")

# Add the grid lines
grid(lty = "solid", col = "black")



