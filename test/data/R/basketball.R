baskets.of.Geraldine <-
     c(5,3,2,2,12,9)
Intro <- "It is amazing! The All Star Grannies scored
a total of"
Outro <- "baskets in the last six games!"
Total.baskets <- baskets.of.Granny +
        baskets.of.Geraldine
Text <- paste(Intro,
       sum(Total.baskets),
       Outro)
cat(Text)