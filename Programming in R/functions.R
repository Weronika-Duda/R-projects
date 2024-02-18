# Weronika Duda
set.seed(123)

# zad 1
minmaxK <- function(x, K = 5) {
  if (length(x) < K * 2) {
    stop("wektor musi zawierać co najmniej 2K elementów")
  }
  
  min <- sort(x)[1:K]
  max <- sort(x)[(length(x) - K + 1):length(x)]
  
  return(list(min = min, max = max))
}

# test zad 1
wektor_1 <- sample(1:4000, 100, replace = FALSE)
minmaxK(wektor_1)

# test zad 1
wynik_1 <- minmaxK(wektor_1)
print(wynik_1)

# zad 2
lDsknl <- function(x) {
  if(x == 1) 
    return(FALSE)
  
  doskonale <- which(x %% 1:(x/2) == 0)
    return(sum(doskonale) == x )
}

# test zad 2
system.time({
  l_doskonale <- which(sapply(c(1:10000), lDsknl))
  print(l_doskonale)
})


# zad 3
myNorm <- function(x) {
  return(x = (x - min(x))/(max(x)-min(x)))
}

# zad 4
myCorr <- function(x, y) {
  if (length(x) != length(y)) {
    return("Wektory maja rozne dlugosci")
  } else {
    pearson <- cor(x, y, method = "pearson")
    kendall <- cor(x, y, method = "kendall")
    spearman <- cor(x, y, method = "spearman")
    return(list(Pearson = pearson, Kendall = kendall, Spearman = spearman))
  }
}

# test zad 4
x <- runif(100, 0, 5)  
e <- rnorm(100)
y <- x + e

myCorr(x, y)

# zad 5
myStats <- function(x,p){
  if(p == 0){
    srednia = mean(x)
    odch_std = sd(x)
    return(list(srednia, odch_std))
    
  } else if(p==1) {
    mediana = median(x)
    medianowe_odchylenie_bezwzgledne = mad(x)
    return(list(mediana, medianowe_odchylenie_bezwzgledne))
  }
}

# test zad 5 na wektorach: wektor_1 i x
myStats(wektor_1, 0)
myStats(x, 1)

# zad 6
install.packages("rootSolve")
library(rootSolve)

myFun <- function(x) 10*sin(1.5*x)* cos(.5*x^3) + (1/2)*sqrt(abs(x))

# a 
# uniroot(myFun, c(6, 7) => wykonanie tej linii powoduje bład
uniroot(myFun, c(1, 2))[1]
uniroot(myFun, c(-5,5))[1]

# b
library(rootSolve)
uniroot.all(myFun, c(-3, 3))

# c
plot(seq(-5,5,by=.1),myFun(seq(-5,5,by=.1)),
     type="l",main='f(x)=10*sin(1.5*x)* cos(.5*x^3) + (1/2)*sqrt(abs(x))',
     cex.main =0.8,xlab='x',ylab='f(x)')

wynik_6c <- uniroot.all(myFun, c(-5, 5))
points(wynik_6c,myFun(wynik_6c),col = "red", pch=19)


# zad 7
library(rootSolve)
myLin <- function(x) {
  x1 <- x[1]
  x2 <- x[2]
  x3 <- x[3]
  
  eq1 <- 2*x1 + x2 - 2*x3 + 2
  eq2 <- x1 + 2*x2 - 2*x3 - 1
  eq3 <- 2*x1 + x2 - x3 + 3
  
  return(c(eq1, eq2, eq3))
}

# test zad 7
wynik_7 <- multiroot(myLin, c(0, 0, 0))
print(wynik_7$root)

# zad 8
install.packages("nleqslv")
library(nleqslv)

myNonLin <- function(x) {
  x1 <- x[1]
  x2 <- x[2]
  x3 <- x[3]
  
  eq1 <- 2*x1 + x2^2 - 2*x3 - 2
  eq2 <- x1^2 + 2*x2 - 2*x3 - 3
  eq3 <- 2*x1 + x2 - x3 - 3
  
  return(c(eq1, eq2, eq3))
}

# test zad 8
wynik_8 <- nleqslv(c(0, 0, 0), myNonLin)
print(wynik_8$x)

# zad 9

# pakiety
library(stringr)
library(dplyr)
library(httr)
library(rvest)
library(readr)

# przykladowy url
url <- 'https://pl.wikipedia.org/wiki/Najwi%C4%99ksze_przedsi%C4%99biorstwa_%C5%9Bwiata'


# funkcja
myDane <- function(url) {
  resp <- GET(url)
  page <- read_html(url)
  
  tab <- page %>% 
    html_nodes(css = "table.wikitable")
  tabela <- tab[[1]] %>% 
    html_table(fill=T)
  return(tabela)
}

# preprocesing: 
dane_9 <- myDane(url)
str(dane_9)
colnames(dane_9) <- gsub(" ", "_", colnames(dane_9))

# 1) zamiana tekstu na liczby
dane_9[[4]] <- as.numeric(gsub("[^0-9]", "", dane_9[[4]]))

# 2) zmiana nazwy kol 4 i zamiana braków wartości na wartości NA, żeby nastepnie je usunąć
dane_9 <- dane_9 %>%
  rename(Przychód_w_mln_dol = `Przychód(mln $)`) %>%
  mutate_all(~ ifelse(. == "", NA, .)) %>%
  na.omit()

# 3) normalizacja
dane_9[[4]] <- myNorm(dane_9[[4]])

# 4) usunięcie białych znaków za przecinkiem w kolumnie 7
dane_9[[7]] <- gsub(",\\s*(.+)", ",\\1", dane_9[[7]], perl = TRUE)
