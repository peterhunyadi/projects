library(readr)
library(dplyr)
library(tibble)
library(ggplot2)
library(RColorBrewer)

rnase <- read_csv("C:/Users/Don Pepe/Desktop/rnase.csv", 
                  col_names = c("AA","amount","percent"))
bsa <- read_csv("C:/Users/Don Pepe/Desktop/bsa.csv", 
                  col_names = c("AA","amount","percent"))
hb <- read_csv("C:/Users/Don Pepe/Desktop/hb.csv", 
                  col_names = c("AA","amount","percent"))

bsa2 = add_column(bsa,new="BSA")
rnase2 = add_column(bsa,new="RNase")
hb2 = add_column(hb,new="Hb")

bsa %>%
  filter(AA!="W" & AA!="Y" & AA!="R" & AA!="H" & AA!="K") %>%
  summarise(add_amount=sum(amount),add_perc=sum(percent)) %>%
  View()

rnase %>%
  filter(AA!="W" & AA!="Y" & AA!="R" & AA!="H" & AA!="K") %>%
  summarise(add_amount=sum(amount),add_perc=sum(percent)) %>%
  View()

hb %>%
  filter(AA!="W" & AA!="Y" & AA!="R" & AA!="H" & AA!="K") %>%
  summarise(add_amount=sum(amount),add_perc=sum(percent)) %>%
  View()

bsa2 = add_row(bsa2, AA="all other",amount=480,percent=79.07743,new="BSA")
rnase2 = add_row(rnase2, AA="all other",amount=100,percent=80.64516,new="RNase")
hb2 = add_row(hb2, AA="all other",amount=231,percent=80.4878,new="Hb")

#adding Tyr and Trp
bsa %>%
  filter(AA=="W" | AA=="Y") %>%
  summarise(add_amount=sum(amount),add_perc=sum(percent)) %>%
  View()

rnase %>%
  filter(AA=="W" | AA=="Y") %>%
  summarise(add_amount=sum(amount),add_perc=sum(percent)) %>%
  View()

hb %>%
  filter(AA=="W" | AA=="Y") %>%
  summarise(add_amount=sum(amount),add_perc=sum(percent)) %>%
  View()

bsa2 = add_row(bsa2, AA="Tyr and Trp",amount=24,percent=3.953871,new="BSA")
rnase2 = add_row(rnase2, AA="Tyr and Trp",amount=6,percent=4.83871,new="RNase")
hb2 = add_row(hb2, AA="Tyr and Trp",amount=9,percent=3.135889,new="Hb")


bsa3 = bsa %>%
  filter(AA=="R" | AA=="H" | AA=="K") %>%
  summarise(add_amount=sum(amount),add_perc=sum(percent)) %>%
  View()

rnase %>%
  filter(AA=="R" | AA=="H" | AA=="K") %>%
  summarise(add_amount=sum(amount),add_perc=sum(percent)) %>%
  View()

hb %>%
  filter(AA=="R" | AA=="H" | AA=="K") %>%
  summarise(add_amount=sum(amount),add_perc=sum(percent)) %>%
  View()

bsa2 = add_row(bsa2, AA="positives",amount=103,percent=16.9687,new="BSA")
rnase2 = add_row(rnase2, AA="positives",amount=18,percent=14.51613,new="RNase")
hb2 = add_row(hb2, AA="positives",amount=47,percent=16.37631,new="Hb")

#combine 3 tables
all_prot = bind_rows(bsa2,hb2,rnase2)
all_prot2 = filter(all_prot, AA=="positives" | AA=="Tyr and Trp" )
                   #| AA=="all other")

#finally plot
ggplot(all_prot2) +
  aes(x=AA,y=percent,fill=AA)+
  geom_col(colour = "black")+
  facet_wrap(~new,nrow=1) +
  scale_fill_brewer(palette = "Reds")+
  labs(title="Distribution of different aminoacids in our examined proteins",
       x="Aminoacids",y="Percentage of all aminoacids [%]")+
  guides(fill="none")+
  theme_bw()

write_csv(all_prot,"C:/Users/Don Pepe/Desktop/all_prot.csv")