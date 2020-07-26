# install required package(s)
if(!require(readr)) { install.packages("readr")}
if(!require(dplyr)) { install.packages("dplyr")}
if(!require(arules)) { install.packages("arules")}

# load libraries
library(readr)
library(dplyr)
library(arules)

# import data
ordr_pr <- read_csv("data/order_products__prior.csv")
head(ordr_pr)

prods <- read_csv("data/products.csv")
head(prods)

depts <- read_csv("data/departments.csv")
head(depts)

# products with department
products <- prods %>%
  inner_join(depts, by="department_id")
head(products)

# orders
orders <- ordr_pr %>% 
  inner_join(products, by="product_id") 
head(orders)

# create order baskets
order_baskets <- orders %>% 
  group_by(order_id) %>%
  summarise(basket = as.vector(list(product_name)), department = as.vector(list(department)))
head(order_baskets)

# compute transactions for the product names from the order baskets
transactions <- as(order_baskets$basket, "transactions")

# 1. Frequent itemsets for products in orders dataset
itemsets <- apriori(transactions, parameter = list(target = "frequent itemsets", supp=0.01, minlen=2))
itemsets_order_supp <- sort(itemsets, by="support", decreasing = TRUE)
inspect(itemsets_order_supp)

# 2. Association rules for products in orders dataset
rules1 <- apriori(transactions, parameter = list(supp = 0.0001, conf = 0.6))
summary(rules1)
rules1_order_conf <- sort(rules1, by="conf", decreasing = TRUE)
inspect(head(rules1_order_conf, 10))
rules1_order_lift <- sort(rules1, by="lift", decreasing = TRUE)
inspect(head(rules1_order_lift, 10))

rules2 <- apriori(transactions, parameter = list(supp = 0.001, conf = 0.4))
summary(rules2)
rules2_order_conf <- sort(rules2, by="conf", decreasing = TRUE)
inspect(head(rules2_order_conf, 10))
rules2_order_lift <- sort(rules2, by="lift", decreasing = TRUE)
inspect(head(rules2_order_lift, 10))

# compute transactions for the product departments from the order baskets
transactions <- as(order_baskets$department, "transactions")

# 3. Frequent itemsets for departments in orders dataset
itemsets <- apriori(transactions, parameter = list(target = "frequent itemsets", supp=0.2, minlen=2))
itemsets_order_supp <- sort(itemsets, by="support", decreasing = TRUE)
inspect(itemsets_order_supp)

# 4. Association rules for departments in orders dataset
rules1d <- apriori(transactions, parameter = list(supp = 0.1, conf = 0.6))
summary(rules1d)
rules1d_order_conf <- sort(rules1d, by="conf", decreasing = TRUE)
inspect(head(rules1d_order_conf, 10))
rules1d_order_lift <- sort(rules1d, by="lift", decreasing = TRUE)
inspect(head(rules1d_order_lift, 10))

rules2d <- apriori(transactions, parameter = list(supp = 0.2, conf = 0.4))
summary(rules2d)
rules2d_order_conf <- sort(rules2d, by="conf", decreasing = TRUE)
inspect(head(rules2d_order_conf, 10))
rules2d_order_lift <- sort(rules2d, by="lift", decreasing = TRUE)
inspect(head(rules2d_order_lift, 10))
