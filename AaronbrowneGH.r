matched_with_value_ratio_and_lay_win_fixed_v3_nopounds_ukdates <- read.csv("~/Downloads/matched_with_value_ratio_and_lay_win_fixed_v3_nopounds_ukdates.csv")
>   View(matched_with_value_ratio_and_lay_win_fixed_v3_nopounds_ukdates)
> bets <- matched_with_value_ratio_and_lay_win_fixed_v3_nopounds_ukdates
> bets$Date      <- as.Date(bets$Date, format = "%d/%m/%Y") 
> bets$Profit    <- as.numeric(bets$Profit)
> bets$Liability <- as.numeric(bets$Liability) 
> bets$Bookies.Odds <- as.numeric(bets$Bookies.Odds)
> bets$Lay.Decimal  <- as.numeric(bets$Lay.Decimal)
> bets$Win.Loss  <- factor(bets$Win.Loss)  
> bets$implied_prob_back <- 1/ bets$Bookies.Decimal
> bets$implied_prob_lay <- 1/ bets$Lay.Decimal
> bets$edge_prob <- -(bets$implied_prob_back - bets$implied_prob_lay)
> ab_bets <- bets[bets$AB.E1 %in% c("AB", "Both"), ]
> ab_bets$exposure <- ab_bets$Bookie.Stake + ab_bets$Liability
> ab_bets$roi_exposure <- ab_bets$AB.Profit/ ab_bets$exposure