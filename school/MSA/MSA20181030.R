##Old Data
library(tidyverse)
table5 %>%
  unite(new, century, year, sep = "")

string1 <- "This is a string"
string2 <- 'If I want to include a "quote" inside a string, I use single quotes'

x <- c("\"", "\\")
x
writeLines(x)
x <- "\u00b5"
x
str_length("R for data science")
str_length(c("a", "R for data science", NA))
str_c("x", "y")
str_c("x", "y", "z")
str_c("x", "y", sep = ", ")
str_c("prefix-", c("a", "b", "c"), "-suffix")
name <- "Hadley"
time_of_day <- "morning"
birthday <- T

str_c(
  "Good ", time_of_day, " ", name,
  if (birthday) " and HAPPY BIRTHDAY",
  "."
)

str_c(c("x", "y", "z"))

str_c(c("x", "y", "z"), collapse = ", ")

str_sub("Apple", 1, 3)
x <- c("Apple", "Banana", "Pear")
str_sub(x, 1, 3)
str_sub(x, -3, -1)
str_sub("a", 1, 5)
str_sub("a", 2, 5)
?str_sub

x <- c("apple", "banana", "pear")
str_view(x, "an")
str_view(x, ".a.")
str_view_all(x, ".a.")
str_view(c("abc", "a.c", "bef"), "a\\.c")
str_view("a\\b", "\\\\")
str_view(x, "^a")
str_view(x, "a$")
x <- c("apple pie", "apple", "apple cake")
str_view(x, "^apple$")

str_view(c("grey", "gray"), "gr(e|a)y")
x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
str_view(x, "CC?")
x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
str_view(x, "CC*")
str_view(x, "C{2}")
str_view(x, "C{2,}")
str_view(x, "C{2,3}")

str_view(fruit, "(..)\\1", match = TRUE)

x <- c("apple", "banana", "pear")
str_detect(x, "e")

length(words)
head(words)
sum(str_detect(words, "^t"))
mean(str_detect(words, "[aeiou]$"))
words[str_detect(words, "x$")]
str_subset(words, "x$")
df <- tibble(
  word = words, 
  i = seq_along(word)
)
df %>% 
  filter(str_detect(words, "x$"))
x <- c("apple", "banana", "pear")
str_count(x, "a")
mean(str_count(words, "[aeiou]"))
str_count("abababa", "aba")
str_view_all("abababa", "aba")

df %>% 
  mutate(
    vowels = str_count(word, "[aeiou]"),
    consonants = str_count(word, "[^aeiou]")
  )

length(sentences)

colours <- c("red", "orange", "yellow", "green", "blue", "purple")
colour_match <- str_c(colours, collapse = "|")
colour_match

has_colour <- str_subset(sentences, colour_match)
matches <- str_extract(has_colour, colour_match)
head(matches)

more <- sentences[str_count(sentences, colour_match) > 1]
str_view_all(more, colour_match)

str_extract_all(more, colour_match)

noun <- "(a|the) ([^ ]+)"

has_noun <- sentences %>%
  str_subset(noun) %>%
  head(10)
has_noun %>%
  str_extract(noun)
has_noun %>%
  str_match(noun)

tibble(sentence = sentences) %>%
  tidyr::extract(
    sentence, c("article", "noun"), "(a|the) ([^ ]+)",
    remove = FALSE
  )

x <- c("apple", "pear", "banana")
str_replace(x, "[aeiou]", "-")
str_replace_all(x, "[aeiou]", "-")

x <- c("1 house", "2 cars", "3 people")
str_replace_all(x, c("1" = "one", "2" = "two", "3" = "three"))

sentences %>% 
  str_replace("([^ ]+) ([^ ]+) ([^ ]+)", "\\1 \\3 \\2") %>% 
  head(5)

sentences %>%
  head(5) %>% 
  str_split(" ")

sentences %>%
  head(5) %>% 
  str_split(" ", simplify = TRUE)

##Multivariate Statistical Analysis Lab
###USArrests data
USArrests
str(USArrests)
head(USArrests)

apply(USArrests, 2, mean)
apply(USArrests, 2, sd)
apply(USArrests, 2, var)

(pr.out<-prcomp(USArrests,scale=T))
str(pr.out)

pr.out$center
pr.out$scale
pr.out$rotation

dim(pr.out$x)
str(pr.out$x)

biplot(pr.out, scale=0)
biplot(pr.out, scale=0, choices=c(2,3))
pr.out$rotation <- -pr.out$rotation
pr.out$x <- -pr.out$x
biplot(pr.out, scale=0)
pr.out$sdev
(pr.var <- pr.out$sdev^2)
(pve <- pr.var/sum(pr.var))

plot(pve,ylim=c(0,1),type='b',
     xlab="Principal Component",ylab="Proportion of Variance Explained")
plot(cumsum(pve),ylim=c(0,1),type='b',
     xlab="Principal Component",ylab="Cumulative Proportion of Variance Explained")

###Automobility Logo Data
##### install.packages(c("readbitmap","pixmap","png"))
library(readbitmap)
library(pixmap)
library(png)

# p0 = 120 or 240
p0 <- 240
wd <- "C:/"
setwd(wd)
path <- paste(wd,"carlogo/car",p0,"/",sep="")
filenames <- list.files(path)
logo_matrix_rgb <- t( apply(matrix(paste(path,filenames,sep=""), ncol=1),
                            1, readPNG) )
logo_matrix_r <- logo_matrix_rgb[,1:(p0^2)]
logo_matrix_g <- logo_matrix_rgb[,((p0^2)+1):(2*(p0^2))]
logo_matrix_b <- logo_matrix_rgb[,(2*(p0^2)+1):(3*(p0^2))]
logo_matrix_mean <- (logo_matrix_r+logo_matrix_g+logo_matrix_b)/3
logo_matrix_centered <- apply(logo_matrix_mean, 2, function(x) scale(x,scale=F))
n <- dim(logo_matrix_mean)[1] # n = 113
p <- dim(logo_matrix_mean)[2] # p = p0 x p0

plot( pixmapGrey( matrix(logo_matrix_mean[filenames=="bmw.png",], ncol=p0) ) )

logo_pca <- prcomp(logo_matrix_centered,scale=FALSE)
## 위아래 뒤집고 transpose 취해주어야 제대로된 그림을 얻을 수 있다.
logo_bmw <- matrix(logo_matrix_centered[filenames=="bmw.png",],p0,p0)
logo_bmw <- apply(logo_bmw, 2, rev)
image(t(logo_bmw))
title(paste("PC",j,sep=""))

for(j in 1:4){
  pc_j <- matrix(logo_pca$rotation[,j],p0,p0)
  pc_j <- apply(pc_j, 2, rev)
  pdf(paste(wd,"image/pc",j,".pdf",sep=""),width=7,height=7);
  par(mar=c(5,5,3,2));
  image(t(pc_j));
  title(paste("PC",j,sep=""));
  dev.off();
}

for(i in 1:n){
  x <- matrix(logo_matrix_centered[filenames=="bmw.png",],p0,p0)
  x_tilde <- matrix(0,p0,p0)
  L <- NULL
  for(j in 1:i){
    pc_j <- matrix(logo_pca$rotation[,j],p0,p0)
    l <- sum(x*pc_j) # inner product
    x_tilde <- x_tilde + l*pc_j
  }
  x_tilde <- apply(x_tilde, 2, rev)
  png(paste(wd,"image/bmw_pc/bmw_pc",i,".png",sep=""),width=480,height=480);
  par(mar=c(5,5,3,2));
  image(t(x_tilde));
  title(paste("PC",j,sep=""));
  dev.off();
}

plot(logo_pve,ylim=c(0,1),type='b',
     xlab="Principal Component",ylab="Proportion of Variance Explained")
plot(cumsum(logo_pve),ylim=c(0,1),type='b',
     xlab="Principal Component",ylab="Cumulative Proportion of Variance Explained")

##Hitters Data
##### install.packages(c("pls","ISLR"))
library(pls)
library(ISLR)
Hitters <- na.omit(Hitters)
x <- model.matrix(Salary ~.,Hitters)[, -1]
y <- Hitters$Salary
set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]

str(Hitters)
head(Hitters)

set.seed(2)
pcr.fit <- pcr(Salary~.,data=Hitters,scale=TRUE,validation="CV")
summary(pcr.fit)

validationplot(pcr.fit,val.type="MSEP")

set.seed(1)
pcr.fit <- pcr(Salary~.,data=Hitters,subset=train,scale=TRUE,validation="CV")
validationplot(pcr.fit,val.type="MSEP")

pcr.pred <- predict(pcr.fit,x[test,],ncomp=7)
mean((pcr.pred-y.test)^2)

pcr.fit <- pcr(y~x,scale=TRUE,ncomp=7)
summary(pcr.fit)

###Using USArrests Data
x0 <- USArrests
set.seed(1)
x5 <- USArrests[,2]+rnorm(50,0.01)
x <- cbind(x0,x5)
x <- scale(x)
y <- 1+1*x[1]+2*x[2]+3*x[3]+4*x[4]+5*x[5]+rnorm(50)
pr.out <- prcomp(x,scale=T)
pr.out$sdev

pcscore <- pr.out$x[,1:2] # 2개의 principal component 사용
load <- pr.out$rotation[,1:2]
plm <- lm(unlist(y)~pcscore)
pbeta <- plm$coef[-1] # intercept 제외
betaest <- rep(0,5)
for(k in 1:5){ betaest[k] <- pbeta[1]*load[k,1] }
betaest

for(k in 1:5){ betaest[k] <- pbeta[1]*load[k,1]+pbeta[2]*load[k,2] }
betaest

library(pls)
pcr.fit <- pcr(y~x,scale=T,ncomp=2)
pcr.fit$coef
