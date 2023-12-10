library(dplyr)
library(stringr)

gdp_df <- read.csv("C:/Users/mabse/Downloads/GDP by Country 1999-2022.csv")
happiness_df <- read.csv("C:/Users/mabse/Downloads/2022.csv")

df <- merge(x = gdp_df, y = happiness_df,
            by.x = "Country",
            by.y = "Country",
            all.x = TRUE
)


df$"X1999" <- NULL
df$"X2000" <- NULL
df$"X2001" <- NULL
df$"X2002" <- NULL
df$"X2003" <- NULL
df$"X2004" <- NULL
df$"X2005" <- NULL
df$"X2006" <- NULL
df$"X2007" <- NULL
df$"X2008" <- NULL
df$"X2009" <- NULL
df$"X2010" <- NULL
df$"X2011" <- NULL
df$"X2012" <- NULL
df$"X2013" <- NULL
df$"X2014" <- NULL
df$"X2015" <- NULL
df$"X2016" <- NULL
df$"X2017" <- NULL
df$"X2018" <- NULL
df$"X2019" <- NULL

columns_to_modify <- c("Happiness.score", "Whisker.high", "Whisker.low", "Dystopia..1.83....residual",
                       "Explained.by..GDP.per.capita", "Explained.by..Social.support",
                       "Explained.by..Healthy.life.expectancy", "Explained.by..Freedom.to.make.life.choices",
                       "Explained.by..Generosity", "Explained.by..Perceptions.of.corruption")

df[, columns_to_modify] <- lapply(df[, columns_to_modify], function(x) as.numeric(gsub(",", ".", x)))
df <- na.omit(df)
rownames(df) <- 1:nrow(df)

df_sorted <- df[order(-df$Happiness.score), ]

df$Ranked_Country <- df_sorted$Country

df$happy_index <- df$Happiness.score[order(-df$Happiness.score)]

df_sorted$gdp_fix <- as.numeric(gsub(",", "", df_sorted$X2022))

sum(is.na(df_sorted$gdp_fix))

df_sorted$Country[df_sorted$Country == "United States"] <- "USA"

df_sorted$Country[df_sorted$Country == "United Kingdom"] <- "UK"

df_sorted$Country[df_sorted$Country == "Taiwan Province of China"] <- "Taiwan"

summary_df <- summarise(group_by(df), 
                           AverageHappiness = mean(happy_index, na.rm = TRUE), 
                           MedianHappiness = median(happy_index, na.rm = TRUE),
                           SDHappiness = sd(happy_index, na.rm = TRUE),
                           MinHappiness = min(happy_index, na.rm = TRUE),
                           MaxHappiness = max(happy_index, na.rm = TRUE),
                           AverageGDP = mean(Explained.by..GDP.per.capita, na.rm = TRUE),
                           MedianGDP = median(Explained.by..GDP.per.capita, na.rm = TRUE),
                           SDGDP = sd(Explained.by..GDP.per.capita, na.rm = TRUE),
                           MinGDP = min(Explained.by..GDP.per.capita, na.rm = TRUE),
                           MaxGDP = max(Explained.by..GDP.per.capita, na.rm = TRUE))