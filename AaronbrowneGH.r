# Start of project
# Just stating definitions to make my life easier.
# Also making sure that variables are considered numeric so I can do calculations
# SINCE THIS IS THE RIGHT ORDER OF DATES USE THIS
ab_bets_clean <- read.csv("~/Desktop/ab/R code/Statistical Arbitrage in Sports Price Inefficiencies/ab_bets_clean.csv")
class(ab_bets_clean$Date)
ab_bets_clean$Date <- as.Date(as.character(ab_bets_clean$Date))
ab_bets_clean$Date_UK <- format(ab_bets_clean$Date, format = "%d/%m/%Y")
bets <- ab_bets_clean

bets$Date_UK     <- as.Date(bets$Date_UK, format = "%d/%m/%Y") 
bets$Profit    <- as.numeric(bets$Profit)
bets$Liability <- as.numeric(bets$Liability) 
bets$Bookies.Odds <- as.numeric(bets$Bookies.Odds)
bets$Lay.Decimal  <- as.numeric(bets$Lay.Decimal)
bets$Win.Loss  <- factor(bets$Win.Loss)  

# New definitions, Implied Probability, edge probability, exposure/roi exposure
# Using these new definitions for plots later to make conclusions.
bets$implied_prob_back <- 1/ bets$Bookies.Decimal
bets$implied_prob_lay <- 1/ bets$Lay.Decimal
bets$edge_prob <- -(bets$implied_prob_back - bets$implied_prob_lay)

#Excluding E1s trades only care about mine (AB)
ab_bets <- bets[bets$AB.E1 %in% c("AB", "Both"), ]

# More definitions
ab_bets$exposure <- ab_bets$Bookie.Stake + ab_bets$Liability
ab_bets$roi_exposure <- ab_bets$AB.Profit/ ab_bets$exposure

# Creating if else statement for winning and losing to quantify result. 
# 1s and 0s
ab_bets$win01 <- ifelse(ab_bets$Win.Loss == "Win", 1, 
    ifelse(ab_bets$Win.Loss == "Loss", 0, NA))

# Excluding void bets as they will cause errors in my data later
ab_bets <- ab_bets[tolower(ab_bets[["Win.Loss"]]) != "void", , drop = FALSE]

# Checking for any N/A's in my win loss column
table(ab_bets[["Win.Loss"]], useNA = "ifany")

# Creating a new variable to not mess with ab_bets
d <- ab_bets
 d$Date_UK <-as.Date(d$Date_UK)

# Creates a vector of chosen variables/arguments
# Complete case comman gets rid of any N/As 
core <- c("AB.Profit", "roi_exposure", "edge_prob", "Date")
d <- d[complete.cases(d[, core]), ]

# Checking the amount of Profits above the 99 percentile 
sum(abs(d$AB.Profit) > quantile(abs(d$AB.Profit), 0.99, na.rm = TRUE))


# Doing the same thing with roi exposure (should match profits)
sum(abs(d$roi_exposure) > quantile(abs(d$roi_exposure), 0.99, na.rm = TRUE))


# Creating a 2x2 matrix where it fits Profit histogram /box plot
# And also fits roi_exposure histogram/Box plot into 2x2 matrix
par(mfrow=c(2,2))
hist(d$AB.Profit, breaks = 40, main = "AB Profit distribution", xlab = "AB.Profit")
boxplot(d$AB.Profit, horizontal = TRUE, main = "AB Profit (Box Plot)", xlab = "AB.Profit")
hist(d$roi_exposure, breaks = 40, main = "ROI exposure distribution", xlab = "roi_exposure")
boxplot(d$roi_exposure, horizontal = TRUE, main = "ROI exposure (Box Plot)", xlab = "roi_exposure")



# This shows the P&L each day 
daily_pnl <- aggregate(AB.Profit~Date_UK, data=d, sum) 

# Finding the cumulative P&L (P&L added each day)
daily_pnl$cum_pnl <- cumsum(daily_pnl$AB.Profit)

# Cumulative plot with regards to the date (type l connects each point with a line)
plot(daily_pnl$Date_UK, daily_pnl$cum_pnl, type='l', main = "Cumulative P&L", xlab = "Date", ylab = "Cumulative AB Profit")
abline(h=0, lty=2) # 0 line
plot(daily_pnl$Date_UK, daily_pnl$cum_pnl, main = "Cumulative P&L", xlab = "Date", ylab = "Cumulative AB Profit")












