#' 
#' Replication of Saiarav's code


library(rdd)
library(dplyr)

#Read data from csv
data <- read.csv("All_States_GE.csv")

##Prepare data for McCrary Test 

#Take data for LS2019 Elections and combine Constituency Name with State Name since Constituency Names may be the same
data_f <- data[data$Assembly_No == 17 & data$Poll_No == 0, ]
data_f$Constituency_Name <- paste(data_f$Constituency_Name, "_", data_f$State_Name, sep = "")

#Calculate the vote share of the winner and the runner up of each seat
data_w <- data_f[data_f$Position == 1, c('Constituency_Name', 'Vote_Share_Percentage')]
colnames(data_w) <- c('Constituency_Name', 'Vote_Share_Percentage_W')
data_r <- data_f[data_f$Position == 2, c('Constituency_Name', 'Vote_Share_Percentage')]
colnames(data_r) <- c('Constituency_Name', 'Vote_Share_Percentage_RU')
data_f$Vote_Share_Percentage_W <- data_w$Vote_Share_Percentage_W[match(data_f$Constituency_Name, data_w$Constituency_Name)]
data_f$Vote_Share_Percentage_RU <- data_r$Vote_Share_Percentage_RU[match(data_f$Constituency_Name, data_w$Constituency_Name)]

#Calculate Win Margin for each candidate
data_f$Win_Margin <- ifelse(data_f$Position == 1, data_f$Vote_Share_Percentage - data_f$Vote_Share_Percentage_RU, data_f$Vote_Share_Percentage - data_f$Vote_Share_Percentage_W)
data_f$Win_Margin <- data_f$Win_Margin/100

#Filter for BJP
data_f <- data_f[data_f$Party == "BJP", ]

BJP_ruled_UT <- c("Assam", "Bihar", "Goa", "Gujarat", "Haryana", "Himachal_Pradesh", "Jharkhand", "Maharashtra",
               "Manipur", "Nagaland", "Tripura", "Uttar_Pradesh", "Uttarakhand", "Dadra_&_Nagar_Haveli", "Daman_&_Diu", "Andaman_&_Nicobar_Islands", "Lakshadweep", "Chandigarh")

BJP_ruled <- c("Assam", "Bihar", "Goa", "Gujarat", "Haryana", "Himachal_Pradesh", "Jharkhand", "Maharashtra",
               "Manipur", "Nagaland", "Tripura", "Uttar_Pradesh", "Uttarakhand")

# `data_fb`: data for BJP-ruled states
# `data_fnb`: data for non-BJP states
data_fb <- data_f[data_f$State_Name %in% BJP_ruled, ]
data_fnb <- data_f[!(data_f$State_Name %in% BJP_ruled), ]

#Run McCrary test. (Bandwidth (bw) can be changed here)
DCdensity(data_f$Win_Margin)
DCdensity(data_fb$Win_Margin)
DCdensity(data_fnb$Win_Margin)

##Prepare data for RDD 
data_f17e <- data %>%
  filter(Assembly_No == 17, Poll_No == 0, Position == 1) %>%
  mutate(Constituency_Name = paste(Constituency_Name, State_Name, sep = "_")) %>%
  select(Constituency_Name, Electors) %>%
  rename(Electors_19 = Electors)

data_f16e <- data %>%
  filter(Assembly_No == 16, Poll_No == 0, Position == 1) %>%
  mutate(Constituency_Name = paste(Constituency_Name, State_Name, sep = "_")) %>%
  select(Constituency_Name, Electors) %>%
  rename(Electors_14 = Electors)

data_g <- data_f %>%
  select(Constituency_Name, Win_Margin)

# Fix discrepancies in Constituency Names between elections
repl_col <- c("BARDHAMAN DURGAPUR_West_Bengal" = "BURDWAN - DURGAPUR_West_Bengal",
              "BIKANER (SC)_Rajasthan" = "BIKANER_Rajasthan",
              "CHEVELLA_Andhra_Pradesh" = "CHELVELLA_Andhra_Pradesh",
              "DADRA AND NAGAR HAVELI_Dadra_&_Nagar_Haveli" = "DADAR & NAGAR HAVELI_Dadra_&_Nagar_Haveli",
              "JAYNAGAR_West_Bengal" = "JOYNAGAR_West_Bengal")

data_f17e$Constituency_Name <- gsub("^(.*?)_Telangana", "\\1_Andhra_Pradesh", data_f17e$Constituency_Name)
data_f17e$Constituency_Name <- ifelse(data_f17e$Constituency_Name %in% names(repl_col), repl_col[data_f17e$Constituency_Name], data_f17e$Constituency_Name)

data_g$Constituency_Name <- gsub("^(.*?)_Telangana", "\\1_Andhra_Pradesh", data_g$Constituency_Name)
data_g$Constituency_Name <- ifelse(data_g$Constituency_Name %in% names(repl_col), repl_col[data_g$Constituency_Name], data_g$Constituency_Name)

data_e <- inner_join(data_f16e, data_f17e, by = "Constituency_Name") %>%
  mutate(g = (Electors_19 - Electors_14) / Electors_14)

data_e <- inner_join(data_e, data_g, by = 'Constituency_Name')

#Run RDD test
x<-data_e$Win_Margin
y<-data_e$g
res<-RDestimate(y~x, bw=0.107)
plot(res)
abline((sum(data_e$Electors_19) - sum(data_e$Electors_14))/sum(data_e$Electors_14), 0)
print(res$est)
print(res$se)
print(res$p)

#Run RDD test removing some outliers
data_ex = data_e %>% filter(!(Win_Margin < 0) | !(Win_Margin > -0.015) | !(g > 0.15))

x<-data_ex$Win_Margin
y<-data_ex$g
res<-RDestimate(y~x, bw=0.107)
plot(res)
abline((sum(data_e$Electors_19) - sum(data_e$Electors_14))/sum(data_e$Electors_14), 0)
print(res$est)
print(res$se)
print(res$p)

