library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)

if(!file.exists("Storm data.csv.bz2")){
  url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
  download.file(url, "Storm data.csv.bz2", method = "curl", mode = "wd")
}
a = read.csv("repdata_data_StormData.csv")
str(a)

c = as_tibble(a)
c = c %>% select(EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)
?filter
c = c %>% filter(FATALITIES > 0 | INJURIES > 0 | PROPDMG > 0 | CROPDMG > 0)

table(c$PROPDMGEXP)
table(c$CROPDMGEXP)

seq = 1 : nrow(c)

for (i in seq){
  if (c$PROPDMGEXP[i] == "" | c$PROPDMGEXP[i] == "-" | c$PROPDMGEXP[i] == "+" | c$PROPDMGEXP[i] == "0"){
    c$PROPDMGEXP[i] = 10^0
  }
  else if (c$PROPDMGEXP[i] == "1"){
    c$PROPDMGEXP[i] = 10^1
  }
  else if (c$PROPDMGEXP[i] == "2" | c$PROPDMGEXP[i] == "h" | c$PROPDMGEXP[i] == "H") {
    c$PROPDMGEXP[i] = 10^2
  }
  else if (c$PROPDMGEXP[i] == "3" | c$PROPDMGEXP[i] == "K" | c$PROPDMGEXP[i] == "k"){
    c$PROPDMGEXP[i] = 10^3
  }
  else if (c$PROPDMGEXP[i] == "4"){
    c$PROPDMGEXP[i] = 10^4
  }
  else if (c$PROPDMGEXP[i] == "5"){
    c$PROPDMGEXP[i] = 10^5
  }
  else if (c$PROPDMGEXP[i] == "6" | c$PROPDMGEXP[i] == "m" | c$PROPDMGEXP[i] == "M"){
    c$PROPDMGEXP[i] = 10^6
  }
  else if (c$PROPDMGEXP[i] == "7"){
    c$PROPDMGEXP[i] = 10^7
  }
  else if (c$PROPDMGEXP[i] == "8"){
    c$PROPDMGEXP[i] = 10^8
  }
  else if (c$PROPDMGEXP[i] == "9" | c$PROPDMGEXP[i] == "B"){
    c$PROPDMGEXP[i] = 10^9
  }
}

table(c$PROPDMGEXP)

for (i in seq){
  if (c$CROPDMGEXP[i] == "" | c$CROPDMGEXP[i] == "?" | c$CROPDMGEXP[i] == "0"){
    c$CROPDMGEXP[i] = 10^0
  }
  else if (c$CROPDMGEXP[i] == "K" | c$CROPDMGEXP[i] == "k"){
    c$CROPDMGEXP[i] = 10^3
  }
  else if (c$CROPDMGEXP[i] == "m" | c$CROPDMGEXP[i] == "M"){
    c$CROPDMGEXP[i] = 10^6
  }
  else if (c$CROPDMGEXP[i] == "B"){
    c$CROPDMGEXP[i] = 10^9
  }
}

table(c$CROPDMGEXP)

str(c)

c$PROPDMGEXP = as.numeric(c$PROPDMGEXP)
c$CROPDMGEXP = as.numeric(c$CROPDMGEXP)
names(c)

d = c

d$PROPDMG = d$PROPDMG * d$PROPDMGEXP
d$CROPDMG = d$CROPDMG * d$CROPDMGEXP



popuDMG.byEVENT = aggregate(d[, c(2, 3)], by = list(EVENT = d$EVTYPE), FUN = sum)
popuDMG.byEVENT$TOTAL = popuDMG.byEVENT$FATALITIES + popuDMG.byEVENT$INJURIES

econDMG.byEVENT = aggregate(d[, c(4,6)], by = list(EVENT = d$EVTYPE), FUN = sum)
econDMG.byEVENT$TOTAL = econDMG.byEVENT$PROPDMG + econDMG.byEVENT$CROPDMG

popuDMG.byEVENT = popuDMG.byEVENT[order(popuDMG.byEVENT$TOTAL, decreasing = TRUE), ]
econDMG.byEVENT = econDMG.byEVENT[order(econDMG.byEVENT$TOTAL, decreasing = TRUE), ]

TOP.popuDMG.byEVENT = head(popuDMG.byEVENT, 10)
TOP.econDMG.byEVENT = head(econDMG.byEVENT, 10)

TOP.econDMG.byEVENT = melt(data = TOP.econDMG.byEVENT, id.vars = "EVENT", variable.name = "DAMAGE", value.name = "VALUE")
TOP.popuDMG.byEVENT = melt(data = TOP.popuDMG.byEVENT, id.vars = "EVENT", variable.name = "MISHAPS", value.name = "COUNT")


g = ggplot(data = TOP.popuDMG.byEVENT, aes(x = reorder(EVENT, -COUNT), y = COUNT))
g + geom_bar(stat = "identity", aes(fill = MISHAPS ), position = "dodge") +
  labs(title = "Top 10 most harmful events for humans", x = "Event", y = "COUNT") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_fill_discrete(name = "MISHAPS", labels = c("Fatalities", "Injuries", "Total"))


h = ggplot(TOP.econDMG.byEVENT, aes(x = reorder(EVENT, -VALUE), y = VALUE))
h + geom_bar(stat = "identity", aes(fill = DAMAGE), position = "dodge") +
  labs(title = "Top 10 most harmful events for economy", x = "Event", y = "Value") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_fill_discrete(name = "Damage", labels = c("Property Damage", "Crop Damage", "Total"))


?scale_fill_discrete
popudmg.TOTAL = aggregate(d$FATALITIES + d$INJURIES ~ d$EVTYPE, data = d, FUN = sum)
parse_date_time(gsub('^(?=.{6,7}$)', '0', perl=T, gsub('^\\d\\K(?!\\d{6})', '0', x, perl=T)), 'mdy')