## elections
library(data.table)
library(dplyr)

N <- 40221
KommRossii <- 2300
KPRF <- 13240
LDPR <- 7679
SR <- 500
ER <- 16322
votes <- c(KommRossii,KPRF,LDPR,SR,ER)
distr <- function() {
        if(sum(votes)< N/2) {stop('less than 50%, reelection')} else
        {annex <- c(1:(5*19))}
        names(annex) <- rep(c("KommRossii","KPRF","LDPR","SR","ER"),19)
        mip <- 0.05
        map <- 0.95
        nmin <- round(N*mip)
        nmax <- round(N*map)
        annex[1:5] <- sapply(votes, function(x) ifelse (x %in% (nmin:nmax), x/2, 0))
        for(i in 3:20) {annex[(5*(i-2)+1):(5*(i-1))] <- round(votes/i, digits = 6)}
        distr <- tibble(party=names(annex),annex=annex)
        distr <- tibble(party=names(annex),annex=annex) %>%
                arrange(desc(annex)) %>% 
                slice(1:20) %>% group_by(party) %>% summarise(seats=n()) %>%
                arrange(desc(seats))
        distr
}
result <- distr()
result
