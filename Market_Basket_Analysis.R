installing required libraries
install.packages('tidyverse')
install.packages('dplyr')
install.packages('arules')

#loading required libraries
library(tidyverse)
library(dplyr)
library(arules)
#library(arulesViz)

mbr_data <- read.csv(sprintf("/content/Amazon_Smartphone_Dataset.csv"))

glimpse(mbr_data)

"""**Getting Details of Single Basket**"""

bas_1 = mbr_data %>%
                filter(Invoice_Number == "INV00015")

bas_1

"""**Cleaning the Data**"""

#Cleaning Data by removing missing entries
mbrc_data = mbr_data %>%
  filter(complete.cases(.))

"""**Analysing specific items**

"""

# Number of total and distinct items for CHILLI LIGHTS
mbrc_data %>%
  filter(Data_Description == "Nillkin Cover/Case")  %>%
  summarize(n_tot_items = n(),
            n_basket_item = n_distinct(Invoice_Number))

# Number of baskets containing both items
mbrc_data %>%
  filter(Data_Description %in% c("Nillkin Cover/Case", "Vivo Smartphone")) %>%
  group_by(Invoice_Number) %>%
  dplyr::summarize(n = n()) %>%
  filter(n==2) %>%
  dplyr::summarize(n_distinct(Invoice_Number))

"""**Apriori algorithm**

***Transactionalizing the dataset***
"""

# Splitting transactions
data_list = split(mbrc_data$Data_Description,
                  mbrc_data$Invoice_Number)

glimpse(data_list)

# Transform data into a transactional dataset
Online_trx = as(data_list, "transactions")

glimpse(Online_trx)

# Summary of transactions
summary(Online_trx)

# Create an item frequency plot for the top 20 items
if (!require("RColorBrewer")) {
  # install color package of R
install.packages("RColorBrewer")
#include library RColorBrewer
library(RColorBrewer)
}
itemFrequencyPlot(Online_trx,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")

"""**Generating Rules!**"""

# Min Support as 0.001, confidence as 0.8.
association.rules <- apriori(Online_trx, parameter = list(supp=0.01, conf = 0.3, minlen=2))

summary(association.rules)

inspect(association.rules[2:12])

"""**Finding Rules related to given items**"""

# Find the confidence and lift measures
rhs_rules_Vivo=
    apriori(Online_trx,
            parameter = list(supp=0.03, conf=0.1, minlen=2),
            appearance = list(rhs="Vivo Smartphone"),
            control = list (verbose=F)
)

inspect(rhs_rules_Vivo)

# Find the confidence and lift measures
lhs_rules_Vivo =
    apriori(Online_trx,
            parameter = list(supp=0.03, conf=0.1, minlen=2),
            appearance = list(lhs="Vivo Smartphone"),
            control = list (verbose=F)
)

inspect(lhs_rules_Vivo)

rules_Vivo = arules::union(rhs_rules_Vivo,
                               lhs_rules_Vivo)

inspect(rules_Vivo)


install.packages('arulesViz')

library(arulesViz)

# Filter rules with confidence greater than 0.4 or 40%
subRules<-association.rules[quality(association.rules)$confidence>0.1]
#Plot SubRules
plot(subRules)

subRules2<-head(association.rules, n=20, by="lift")
plot(subRules2, method="paracoord")

top10subRules <- head(association.rules, n = 10, by = "confidence")

rules_html=plot(top10subRules, method = "graph",  engine = "htmlwidget")

# Save the interactive graph as an html file
saveWidget(rules_html, file = "rules.html")

install.packages('htmlwidgets')
library(htmlwidgets)
