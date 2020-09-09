## elections
library(data.table)
library(dplyr)

## random example, voter turnout = 100%
## N - number of voters
example <- function(N) {
        r <- runif(5)
        r <- sort(r)
        r[5] <- (r[5]-r[4])
        r[4] <- (r[4]-r[3])
        r[3] <- (r[3]-r[2])
        r[2] <- (r[2]-r[1])
        KommRossii <- round(N*r[1])
        KPRF <- round(N*r[2])
        LDPR <- round(N*r[3])
        SR <- round(N*r[4])
        ER <- round(N*r[5])
        votes <- c(KommRossii,KPRF,LDPR,SR,ER)
        names(votes) <- c("KommRossii","KPRF","LDPR","SR","ER")
        votes
}
N <- 95043 ## number of voters
votes <- example(N)
votes

## example with given protocol results
## явка: 40221, Коммунисты - 2300, КПРФ - 13420, ЛДПР - 7679,  СР - 500 ,ЕР - 16322
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
