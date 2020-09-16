## elections
library(data.table); library(dplyr)
options(tibble.print_min=Inf); sixdec <- function(x) trimws(format(x, nsmall=6))

####### the main function
distr <- function(res) {
  seatsTot <- 20
  mis <- 0.5 ## shortlist threshold
  mip <- 0.05 ## party threshold
  nmin <- floor(Vt*mip)
  nmax <- ceiling(Vt*(1-mip))
  votes <- res[1:5]
  Vt <- res[6]
  
if(sum(votes) > Vt) {stop('voter turnout < total votes earned, ENTER OTHER DATA')}
  if(sum(votes)< mis*Vt) {warning('shortlist is less than 50%, reelection')}
  votes <- tibble(party=c("Communists","KPRF","LDPR","SR","ER"), votes=votes)
  for (i in 1:nrow(votes)) {if(votes$votes[i] >= nmax) {
    print(votes[i,])
    warning('there is a top-winner with more than 95%, reelection')}}
  calc <- tibble(party=rep(c("Communists","KPRF","LDPR","SR","ER"),(seatsTot -1)),
                 votes =rep(votes$votes, (seatsTot -1)), annex=0, denom=0, seats=0)
  calc[1:5,] <- calc[1:5,] %>%
    mutate(annex=if_else(votes %in% c(nmin:Vt), votes/2, 0), denom=2)
  
  for(i in 3:seatsTot) {
    calc[(nrow(votes)*(i-2)+1):(nrow(votes)*(i-1)),] <-
      calc[(nrow(votes)*(i-2)+1):(nrow(votes)*(i-1)),] %>%
      mutate(annex=if_else(votes %in% c(nmin:Vt), votes/i, 0), denom=i)
  }
  
  calc <- calc %>% filter(annex !=0) %>%
    arrange(desc(annex), desc(votes)) %>% mutate(annex=sixdec(annex))
  print('all denominators')
  print(calc)
  top <- calc %>% mutate(annex=as.numeric(annex)) %>%
    arrange(desc(annex), desc(votes)) %>%
    slice(1:seatsTot) %>% group_by(party) %>%
    mutate(party, seats = n()) %>% mutate(across(seats, as.numeric)) %>%
    ungroup() %>% arrange(desc(annex), desc(votes))
  print('top20 denominators & seats')
  print(top)
  
  byes <- c(which(!(unique(votes$party) %in% unique(calc$party))))
  distr <- top %>% select(party,seats) %>% distinct()
  if(length(byes)>0){
    distr <- distr %>% add_row(party=votes$party[byes], seats=0)     
  }
  
  losers <-c(which(!(unique(calc$party) %in% unique(top$party))))
  if(length(losers)>0) {
    distr <- right_join(distr, votes, by= c("party")) %>%
      arrange(desc(votes)) %>%
      add_row(party = "Vt", seats= sum(distr$seats, na.rm = TRUE), votes = Vt)
    print('provisional distribution')
    print(distr)
    
    for(i in 1:length(losers)){
      helpers <- top %>%
        mutate(seats = na_if(seats,1)) %>%
        arrange(annex,votes) %>%
        slice(1) %>% select(party)
      top <- top %>%
        mutate(seats=if_else(party %in% helpers, seats-1, seats))
    }
    
    losers <- unique(calc$party)[losers]
    losers <- calc %>% filter(party %in% losers) %>%
      mutate(annex=as.numeric(annex), seats=1) %>%
      group_by(party) %>% mutate(annex=max(annex)) %>%
      filter(row_number()==1)
    top <- top %>%
      mutate(seats=if_else(is.na(seats),1,seats))
    top <- bind_rows(top, losers) %>% arrange(desc(annex), desc(votes))
    warning('losers were here, calculated by paragraph §63.5 rule')
    print(c('losers:', losers$party))
    print(c('helpers:', helpers$party))
    distr <- top %>% select(party,seats) %>% distinct()
  }
  
  distr <- right_join(distr, votes, by= c("party")) %>%
    mutate(seats=if_else(is.na(seats),0,seats)) %>%
    arrange(desc(seats), desc(votes)) %>%
    add_row(party = "Vt", seats= sum(distr$seats, na.rm = TRUE), votes = Vt)
  print('final disrtibution')
  distr
}

## example with given protocol results
## for the program to work, you need to ENTER ALL following data:
########## `Vt, Communists, KPRF, LDPR, SR, ER` ############
### and MUST generate a vector `votes <- c(Communists,KPRF,LDPR,SR,ER)`,
## AND THAN run `distr()` (just with empty brackets)
## явка: 40221, Коммунисты - 2300, КПРФ - 13420, ЛДПР - 7679,  СР - 500 ,ЕР - 16322
## turnout: 40221, Communists: 2300, KPRF: 13420, LDPR: 7679, SR: 500 ,ER: 16322

rm(Communists, ER, KPRF,LDPR,res,SR,Vt)
Communists <- 1370
KPRF <- 2393
LDPR <- 2838
SR <- 2500
ER <- 9957
Vt <- 20142 ## voter turnout
res <- c(Communists,KPRF,LDPR,SR,ER,Vt)
distr(res)

rm(Communists, ER, KPRF,LDPR,res,SR,Vt)
Communists <- 2300
KPRF <- 13240
LDPR <- 7679
SR <- 500
ER <- 16322
Vt <- 40221 ## voter turnout
res <- c(Communists,KPRF,LDPR,SR,ER, Vt)
distr(res)

rm(Communists, ER, KPRF,LDPR,res,SR,Vt)
Communists <- 108261
KPRF <- 82459
LDPR <- 14870
SR <- 3318
ER <- 5634
Vt <- 240221 ## voter turnout
res <- c(Communists,KPRF,LDPR,SR,ER,Vt)
distr(res)

rm(Communists, ER, KPRF,LDPR,res,SR,Vt)
Communists <- 0
KPRF <- 10000
LDPR <- 7679
SR <- 0
ER <- 16322
Vt <- 40221 ## voter turnout
res <- c(Communists,KPRF,LDPR,SR,ER,Vt)
distr(res)

########
rm(Communists, ER, KPRF,LDPR,res,SR,Vt)
Communists <- 1370
KPRF <- 2393
LDPR <- 2838
SR <- 2500
ER <- 9957
Vt <- 20142 ## voter turnout
res <- c(Communists,KPRF,LDPR,SR,ER,Vt)
distr(res)

rm(Communists, ER, KPRF,LDPR,res,SR,Vt)
Communists <- 7333
KPRF <- 10976
LDPR <- 21175
SR <- 34749
ER <- 3846
Vt <- 40221 ## voter turnout
res <- c(Communists,KPRF,LDPR,SR,ER,Vt)
distr(res)

rm(Communists, ER, KPRF,LDPR,res,SR,Vt)
Communists <- 542
KPRF <- 5543
LDPR <- 22048
SR <- 3159
ER <- 21582
Vt <- 40221 ## voter turnout
res <- c(Communists,KPRF,LDPR,SR,ER,Vt)
distr(res)

rm(Communists, ER, KPRF,LDPR,res,SR,Vt)
Communists <- 5423
KPRF <- 5543
LDPR <- 22048
SR <- 3159
ER <- 21582
Vt <- 57800 ## voter turnout
res <- c(Communists,KPRF,LDPR,SR,ER,Vt)
distr(res)

rm(Communists, ER, KPRF,LDPR,res,SR,Vt)
Communists <- 8799
KPRF <- 3001
LDPR <- 389
SR <- 1171
ER <- 40455
Vt <- 70737 ## voter turnout
res <- c(Communists,KPRF,LDPR,SR,ER,Vt)
distr(res)

##########################
## random results function, for experiments
## Nv - number of voters (by default = 95043)
rndres <- function(Nv=95043) {
  r <- runif(6)
  r <- sort(r[1:6])
  r[6] <- r[6] - r[5]
  r[5] <- (r[5]-r[4])
  r[4] <- (r[4]-r[3])
  r[3] <- (r[3]-r[2])
  r[2] <- (r[2]-r[1])
  Communists <- round(Nv*r[1])
  KPRF <- round(Nv*r[2])
  LDPR <- round(Nv*r[3])
  SR <- round(Nv*r[4])
  ER <- round(Nv*r[5])
  Vt <- round(sum(r)*Nv)
  rndres <- c(Communists,KPRF,LDPR,SR,ER, Vt)
  names(rndres) <- c("Communists","KPRF","LDPR","SR","ER", "Vt")
  print(rndres)
  rndres
}
## for the program `distr` w/ random set to work, run `distr(res)`, then `exper`
##Nv <- ## == 95043 by default  
exper <- distr(rndres())
exper

##############
suppressWarnings(rm(sampres))
sampres <- c()
for(i in 1:500){
        exper <- distr(rndres())
        exper
        if(max(exper$seats[1:5])==12) {
                sampres <- c(sampres, exper)       
        }
}
length(sampres)/3

###############
suppressWarnings(rm(temp))
l <- length(sampres)/3
temp<- tibble("N"=integer(), "party"=character(),"seats"=numeric(), "votes"=integer())
for(i in 1:l){ j1<- i*6-5; j2 <- i*6; k1 <- i*3-2; k2 <- i*3
        temp[j1:j2,] <- tibble(i, sampres[k1:k2]$party,
                                       sampres[k1:k2]$seats,
                                       sampres[k1:k2]$votes)
}

temp12 <- temp %>% filter(seats==12)
hist(temp12$votes)
boxplot(temp12$votes)

tempPart <- temp %>% filter(seats!=20)
plot(tempPart$seats,tempPart$votes)

tempVt <- temp %>% filter(seats==20)
plot(tempVt$votes, temp12$votes)
