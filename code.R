install.packages("arules")
library(arules)


tr <- read.transactions("~/Uottawa/the assignments/DS/Assignment 2/transactions.csv", format = 'basket', sep=',')
inspect(tr)
size(tr) # number of items in each observation
#> [1] 4 3 1 4 4 5
LIST(tr) # convert 'transactions' to a list, note the LIST in CAPS
frequentItems <- eclat (tr, parameter = list(supp = 0.07, maxlen = 10)) # calculates support for frequent items
inspect(frequentItems)
itemFrequencyPlot(tr, topN=10, type="absolute", main="Item Frequency") # plot frequent items
#**************************************************************************************

rules_max3 <- apriori (tr, parameter = list(supp = 0.002, conf = 0.2, maxlen=3)) # Min Support as 0.002, confidence as 0.2.
inspect(head(rules_max3))

rules_max3_lift <- sort (rules_max3, by="lift", decreasing=TRUE) # 'high-lift' rules.
inspect(head(rules_max3_lift)) # show the support, lift and confidence for all rules

max3_lift=inspect(sort (rules_max3, by="lift", decreasing=TRUE)[1])
max3_lift["lift"]

rules_max3_support <- sort (rules_max3, by="support", decreasing=TRUE) # 'high-lift' rules.
inspect(head(rules_max3_support)) # show the support, lift and confidence for all rules

max3_support=inspect(sort (rules_max3, by="support", decreasing=TRUE)[1])
max3_support["support"]

#**************************************************************************************


rules_max2 <- apriori (tr, parameter = list(supp = 0.002, conf = 0.2, maxlen=2)) # Min Support as 0.002, confidence as 0.2.
inspect(head(rules_max2))

rules_max2_lift <- sort (rules_max2, by="lift", decreasing=TRUE) # 'high-lift' rules.
inspect(head(rules_max2_lift)) # show the support, lift and confidence for all rules

max2_lift=inspect(sort (rules_max2, by="lift", decreasing=TRUE)[1])
max2_lift["lift"]

rules_max2_support <- sort (rules_max2, by="support", decreasing=TRUE) # 'high-lift' rules.
inspect(head(rules_max2_support)) # show the support, lift and confidence for all rules

max2_support=inspect(sort (rules_max2, by="support", decreasing=TRUE)[1])
max2_support["support"]

if(max3_lift$lift > max2_lift$lift){
  print("Rule1 with maximum length of 3 has a better Lift") 
}else{
  print("Rule2 with maximum length of 2 has a better Lift")
}


if(max3_support$support > max2_support$support){
  print("Rule1 with maximum length of 3 has a greater Support") 
}else{
  print("Rule2 with maximum length of 2 has a greater Support")
}


