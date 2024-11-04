
###################################
# effect of Maternal age and Birth order on Down Syndrome and Leukemia  (Stark and Mantell, 1966)

library(dsrTest)
data("downs.mi")
head(downs.mi)
library(dplyr)


df = downs.mi %>% group_by(Age) %>% summarise(total_cases = sum(Cases), total_births = sum(Births))
prop_down = round(df$total_cases/df$total_births, digits = 6)
df = cbind(df, prop_down)

df2 = downs.mi %>% group_by(BirthOrder) %>% summarise(total_cases = sum(Cases), total_births = sum(Births))
prop_down2 = round(df2$total_cases/df2$total_births, digits = 6)
df2 = cbind(df2, prop_down2)


### birth order vs down syndrome risk
plot(df2$BirthOrder, df2$prop_down2, col = "blue", xlab = "Birth order", ylab = "Proportion of Down syndrome cases",type="l", lty=2, main = "Association between birth order and risk of down syndrome" )
points(df2$BirthOrder, df2$prop_down2, col = "red", pch = 17)


### maternal age vs down syndrome risk
plot(df$Age, df$prop_down, col = "blue", xlab = "Age group", ylab = "Proportion of Down syndrome cases", pch = 16,  main = "Association between mother's age and risk of down syndrome" )
lines(df$Age, df$prop_down, col = "red")


###########################
library(plot3D)
df3 = downs.mi %>% group_by(Age,BirthOrder) %>% summarise(total_cases = sum(Cases), total_births = sum(Births))
prop_down3 = round(df3$total_cases/df3$total_births, digits = 6)
df3 = cbind(df3, prop_down3)
colnames(df3) = c("Age","BirthOrder","total_cases","total_births", "prop_down3")


### grouping data by age and birth order
tabledata = df3[,c(1,2,5)]
library(tidyverse)

z_data = tabledata %>% pivot_wider(names_from = BirthOrder, names_prefix = "birth_order_", values_from = prop_down3)
z_data[is.na(z_data)] <- 0
age_gps = z_data$Age
z_df = z_data[,-1]
rownames(z_df) = as.character(age_gps)
z_df


###############################

library(ggplot2)
df.heatmap <- ggplot(data = df3, mapping = aes(x = BirthOrder, y = Age, fill = prop_down3)) +
  geom_tile() +  xlab(label = "Birth Order") + ylab(label = "Age group") +
  scale_fill_gradient(name = "Prop. of DS",
                      low = "#FFFFFF",
                      high = "#062344") + theme_bw()


df.heatmap


##################################

## H0: p1 = p2 =...= p (two sided alternative)
prop.test(x = df2$total_cases, n = df2$total_births)








