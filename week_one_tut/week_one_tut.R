library(plotrix)
N <- 10000

# Create random uniform distribution of N
x <- runif(N, min = -1, max = 1)
y <- runif(N, -1, 1)


# Plot x and y
plot(x, y)

# Create dataframe using x and y
df <- data.frame(x=x, y=y)
head(df)

# Add third column, in circle
df$in_circle <- ifelse(df$x^2 + df$y^2 < 1, 1, 2)
head(df)

plot(df$x, df$y, col = df$in_circle, asp = 1)
draw.circle(0, 0, 1)

# Count number inside circle
in_circle_sum <- sum(df$in_circle == 1)

sum(df$in_circle == 1) / N * 4

# Load wine quality data
wine <- read.table(file = 'week_one_tut/winequality-red.csv', sep = ';', header = T)

head(wine)
