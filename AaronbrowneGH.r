matched_with_value_ratio_and_lay_win_fixed_v3_nopounds_ukdates <- read.csv("~/Downloads/matched_with_value_ratio_and_lay_win_fixed_v3_nopounds_ukdates.csv")
   View(matched_with_value_ratio_and_lay_win_fixed_v3_nopounds_ukdates)
bets <- matched_with_value_ratio_and_lay_win_fixed_v3_nopounds_ukdates
bets$Date      <- as.Date(bets$Date, format = "%d/%m/%Y") 
bets$Profit    <- as.numeric(bets$Profit)
bets$Liability <- as.numeric(bets$Liability) 
bets$Bookies.Odds <- as.numeric(bets$Bookies.Odds)
bets$Lay.Decimal  <- as.numeric(bets$Lay.Decimal)
bets$Win.Loss  <- factor(bets$Win.Loss)  
bets$implied_prob_back <- 1/ bets$Bookies.Decimal
bets$implied_prob_lay <- 1/ bets$Lay.Decimal
bets$edge_prob <- -(bets$implied_prob_back - bets$implied_prob_lay)
ab_bets <- bets[bets$AB.E1 %in% c("AB", "Both"), ]
ab_bets$exposure <- ab_bets$Bookie.Stake + ab_bets$Liability
ab_bets$roi_exposure <- ab_bets$AB.Profit/ ab_bets$exposure


> ab_bets$win01 <- ifelse(ab_bets$Win.Loss == "Win", 1, 
    ifelse(ab_bets$Win.Loss == "Loss", 0, NA))

ab_bets <- ab_bets[tolower(ab_bets[["Win.Loss"]]) != "void", , drop = FALSE]
table(ab_bets[["Win.Loss"]], useNA = "ifany")

d <- ab_bets
 d$Date <-as.Date(d$Date)
core <- c("AB.Profit", "roi_exposure", "edge_prob", "Date")
d <- d[complete.cases(d[, core]), ]

sum(abs(d$AB.Profit) > quantile(abs(d$AB.Profit), 0.99, na.rm = TRUE))
[1] 4

sum(abs(d$roi_exposure) > quantile(abs(d$roi_exposure), 0.99, na.rm = TRUE))
[1] 4

par(mfrow=c(2,2))
hist(d$AB.Profit, breaks = 40, main = "AB Profit distribution", xlab = "AB.Profit")
boxplot(d$AB.Profit, horizontal = TRUE, main = "AB Profit (Box Plot)", xlab = "AB.Profit")
hist(d$roi_exposure, breaks = 40, main = "ROI exposure distribution", xlab = "roi_exposure")
boxplot(d$roi_exposure, horizontal = TRUE, main = "ROI exposure (Box Plot)", xlab = "roi_exposure")

daily_pnl <- aggregate(AB.Profit~Date, data=d, sum) ## need to double check dates in d as there is somethings wrong









