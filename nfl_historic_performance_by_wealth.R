# NFL Historic Data
# Want to understand price by earnings/winnings (performance)
# Performance is measured in winning pct as well as a homebaked formula
# which adds points for making playoffs and winning superbowl
# Price is measured by the 2014 Forbes Wealth ranking of NFL teams
# http://www.forbes.com/sites/mikeozanian/2014/08/20/the-nfls-most-valuable-teams/

setwd("~/Google Drive/NFL/pe_ratio")

# Load libraries
require(ggplot2)

df.nfl <- read.csv("nfl_historic_forbes_data.csv")

# Massage the data
# Reduce the Forbes 2014 Value field from billions
df.nfl$Forbes2014Value <- df.nfl$Forbes2014Value / 1000000000

# Append some necessary calculated fields

# Team Age
df.nfl$Age = (as.numeric(format(Sys.time(), "%Y")) - df.nfl$YearInc)

# Standard W-L Percentage (W / (W+L))
df.nfl$WinLossPer <- round( (df.nfl$W/ (df.nfl$W + df.nfl$L)), 3 )

# Custom variable, WinLossPerPost
# Let's give some additional weight to teams that make the playoffs and win the
# Super Bowl (or championship before the superbowl), normalized by team age
# .25 for every year in the playoffs, .75 for winning the superbowl
df.nfl$WinLossPerPost <- round((df.nfl$WinLossPer + (.75*df.nfl$Chmp) + (.25*df.nfl$Yr.plyf))/df.nfl$Age,3)

# Now calculate PERatio by dividing Forbes2014Value / WinLossPerPost
df.nfl$PERatio <- df.nfl$Forbes2014Value / df.nfl$WinLossPerPost

# Order and rank by our three variables
# WinLoss %
df.nfl <- df.nfl[order(df.nfl$WinLossPer),]
df.nfl$WinLossPerRank <- c(1:32)

# Win Loss % + Postseason
df.nfl <- df.nfl[order(df.nfl$WinLossPerPost),]
df.nfl$WinLossPerPostRank <- c(1:32)

# Forbes 2014 Value
df.nfl <- df.nfl[order(df.nfl$Forbes2014Value),]
df.nfl$Forbes2014ValueRank <- c(1:32)

# PERatio
df.nfl <- df.nfl[order(df.nfl$PERatio),]
df.nfl$PERatioRank <- c(1:32)

# Plot by WLRank
png("nfl_historical_winning.png", width=480, height=480, units="px", res=72)
ggplot(df.nfl, aes(x=WinLossPerRank, y=WinLossPer, label=Team, fill=Conference))  + geom_bar(stat="identity")  + geom_text(aes(x=WinLossPerRank, y=WinLossPer, label=Team, size=1, angle=90)) + ylab("Historical Winning %") + xlab("Rank Order") + ggtitle("Historical Win/Loss % of NFL Teams")   
dev.off()

# Plot by WLRank
png("nfl_historical_postseason_winning.png", width=480, height=480, units="px", res=72)
ggplot(df.nfl, aes(x=WinLossPerPostRank, y=WinLossPerPost, label=Team, fill=Conference))  + geom_bar(stat="identity")  + geom_text(aes(x=WinLossPerPostRank, y=WinLossPerPost, label=Team, size=1, angle=90)) + ylab("Historical Winning % (Including Postseason)") + xlab("Rank Order") + ggtitle("Historical Win/Loss % (Including Postseason) of NFL Teams")   
dev.off()


# Then plot by Forbes Ranking
png("nfl_2014_forbes_ranking.png", width=480, height=480, units="px", res=72)
ggplot(df.nfl, aes(x=Forbes2014ValueRank, y=Forbes2014Value, label=Team, fill=Conference))  + geom_bar(stat="identity")  + geom_text(aes(x=Forbes2014ValueRank, y=Forbes2014Value, label=Team, size=1, angle=90)) + ylab("2014 Forbes Value (in Billions USD)") + xlab("Rank Order") + ggtitle("2014 Worth of NFL Teams")
dev.off()

# Then plot the new ordering
png("nfl_pe_sorted.png", width=480, height=480, units="px", res=72)
ggplot(df.nfl, aes(x=PERatioRank, y=PERatio, label=Team, fill=Conference))  + geom_bar(stat="identity")  + geom_text(aes(x=PERatioRank, y=PERatio, label=Team, size=1, angle=90)) + ylab("Historical P/E Ratio (in $/Win%)") + xlab("Rank Order") + ggtitle("Historical Price-Earnings (Winnings) Ratio by Team")
dev.off()

###############################
