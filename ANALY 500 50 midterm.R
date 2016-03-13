## Mid-term Test

library(readr)
library(dplyr)
library(ggplot2)
library(survey)

midterm <- read_csv("/Users/aaronan/Analytics/MS Analytics/1. ANLY 500 50/data/midterm.csv")

## data is structured in a very excel way, re-structure it into reasonal way. 

midterm %>% 
  select(c(1, 2:6)) %>% 
  mutate(res = "r1") -> res_1

midterm %>% 
  select(c(1, 7:11)) %>% 
  mutate(res = "r2") -> res_2

names(res_1) = c("cid", "wt", "cs", "ppp", "s", "pppws", "res")
names(res_2) = c("cid", "wt", "cs", "ppp", "s", "pppws", "res")

res = rbind(res_1, res_2)

------------------------------------------------------------------------------------

## 2c: Is the customer satisfaction more consistent at Restaurant 1 or at Restaurant 2?

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

res %>% 
  group_by(res) %>% 
  summarise(var_cs = var(cs),
            sd_cs = sd(cs),
            avg_cs = mean(cs),
            mode_cs = Mode(cs))

ggplot(res, aes(x=cs, fill=res)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity")

## Resturant 2 is more consistent, most people reate around 2-3

## Q5: Are the data collected normal?

qqnorm(midterm$r1wt);qqline(midterm$r1wt, col = 2)
qqnorm(midterm$r2wt);qqline(midterm$r2wt, col = 2)

apply(res[,c(2:4,6)], 2, shapiro.test)

ggplot(res) + 
  stat_qq(aes(sample = wt, colour = factor(res)))

qqnorm(res$wt);qqline(res$wt, col = 2)

------------------------------------------------------------------------------------

## Q6: boxplot on the waittime
  
ggplot(res, aes(factor(res), wt)) +
  geom_boxplot(aes(fill = factor(res))) + geom_jitter()

res %>% 
  group_by(res) %>% 
  summarise(sd_wt = sd(wt),
            avg_wt = mean(wt))

## Q7: t-test of two independent sample. 

t.test(midterm$r2wt, midterm$r1wt)

t.test(wt ~ res, data=res) 

## Q10: un-equal varaince t.test

t.test(midterm$r2wt, midterm$r1wt, var.equal = F)

## Q11, one-sided t.test

t.test(midterm$r1wt, midterm$r2wt, mu = 3.55, alternative = "less")

2.2e-16 < 3.645e-07

## Q14

table(res$res, res$s)

## 4b: Did choose specials affect how much they spend?

res %>% 
  group_by(res, s) %>% 
  summarise(avg_ppp = mean(ppp, na.rm = T))

t.test(ppp ~ s, data=res_1)

t.test(ppp ~ s, data=res_2) 


## 4c: 
