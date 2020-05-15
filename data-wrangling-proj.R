#load packages
library("ggplot2")
library("tidyr")
library("dplyr")
library("stringr")
library("scales")

#import dataset
banks <- read.csv("banks.csv")

#display dimension, class, and variable name of dataset
dim(banks)
class(banks)
colnames(banks)

#check missing values
colSums(is.na(banks))

#drop insignificant variables
banks <- banks %>% 
  select(-Financial.Institution.Number, -Certificate.Number)

#split "Headquarters" and "Failure Date" variables
banks <- banks %>% 
  separate(Headquarters, c("Headquarters City", "State"), sep=",") %>% 
  separate(Failure.Date, c("Month", "Date", "Year"), sep="/")
banks$State <- str_replace(banks$State, "MAUI", "HI")

#change column names
column_names <- c("Institution_Name", "Institution_Type", "Charter_Type",
                  "Headquarters_city", "State", "Month", "Date", "Year",
                  "Insurance_Fund", "Transaction_Type", "Total_Deposits", 
                  "Total_Assets", "Estimated_Loss_till_2015")
colnames(banks) <- column_names

#check the structure of dataset
str(banks)

#summary statistics
Total_Deposits <- summary(banks$Total_Deposits)
Total_Assets <- summary(banks$Total_Assets)
Estimated_Loss <- summary(banks$Estimated_Loss_till_2015)
summary_stats <- rbind(Total_Deposits, Total_Assets, Estimated_Loss)

#distribution of Instituion Type
table(banks$Institution_Type)
inst_type_bar <- ggplot(banks) +
  geom_bar(aes(x=Institution_Type), fill='palegreen4', stat="count") +
  theme_minimal(base_size=13) + xlab("Institution Type") + ylab(" ") +
  ggtitle("Distribution of Institution Type") +
  theme(plot.title = element_text(hjust=0.5))
inst_type_bar

#distribution of charter type
table(banks$Charter_Type)
charter_type_bar <- ggplot(banks) +
  geom_bar(aes(x=Charter_Type), fill='palegreen4', stat="count") +
  theme_minimal(base_size=13) + xlab("Charter Type") + ylab(" ") +
  ggtitle("Distribution of Charter Type") +
  theme(plot.title = element_text(hjust=0.5))
charter_type_bar

#distribution of insurance fund
table(banks$Insurance_Fund)
insurance_fund_bar <- ggplot(banks) +
  geom_bar(aes(x=Insurance_Fund), fill='palegreen4', stat="count") +
  theme_minimal(base_size=13) + xlab("Insurance Fund") + ylab(" ") +
  ggtitle("Distribution of Insurance Fund") +
  theme(plot.title = element_text(hjust=0.5))
insurance_fund_bar

#distribution of transaction type
table(banks$Transaction_Type)
trans_type_bar <- ggplot(banks) +
  geom_bar(aes(x=Transaction_Type), fill='palegreen4', stat="count") +
  theme_minimal(base_size=13) + xlab("Transaction Type") + ylab(" ") +
  ggtitle("Distribution of Transaction Type") +
  theme(plot.title = element_text(hjust=0.5))
trans_type_bar

#bank failures over the year
banks_failure_year <- ggplot(banks) +
  geom_bar(aes(x=Year), fill='#ff9933', stat="count", alpha=0.85) +
  theme_minimal(base_size=13) + xlab("Year") + 
  ylab("Number of Institution Failed") + 
  ggtitle("A History of Bank Failures in the United States") +
  scale_y_continuous(expand = c(0, 1)) +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5),
        plot.title = element_text(hjust=0.5),
        panel.grid.major.x = element_blank())
banks_failure_year

#state-wise failure rate over the years
state_count <- banks %>% 
  group_by(State, Year) %>% 
  summarise("failure_count" = n())
state_failure <- ggplot(state_count, aes(Year, State)) +
  geom_tile(aes(fill=failure_count), colour="white") +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5, size=8, margin(3,3,3,3)),
        axis.text.y = element_text(hjust=1, size=8, margin(2,2,2,2)),
        plot.margin = margin(10,10,10,10),
        plot.title = element_text(hjust=0.5)) +
  scale_fill_gradient(low="light blue", high="dark blue") +
  xlab(" ") + ylab(" ") + ggtitle("State-wise Bank Failure Rate over the Years") +
  guides(fill=FALSE)
state_failure

#failure over institution types
inst_failure <- banks %>% 
  group_by(Institution_Type, Year) %>% 
  summarise(Count = n()) %>% 
  ggplot(aes(x=Year, y=Count, group=Institution_Type, col=Institution_Type)) +
    theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5, size=7, margin(3,3,3,3)),
          plot.margin = margin(10,10,10,10),
          legend.position = c(0.9, 0.9),
          plot.title = element_text(hjust=0.5)) +
    ggtitle("Bank Failures Accross the Institution Types") +
    ylab("Number of Institutions Failed") + 
    scale_color_discrete(name="Institution Type") + geom_line(size=0.8)
inst_failure

#estimated loss across institution types
inst_loss <- banks %>% 
  ggplot(aes(x=Institution_Type, y=Estimated_Loss_till_2015, col=Institution_Type)) +
    ggtitle("Estimated Losses for Different Institution Types") +
    theme(axis.text.x = element_text(size=7, margin(4,4,4,4)),
          axis.text.y = element_text(size=7, margin(6,15,15,6)),
          plot.margin = margin(10,20,10,15),
          plot.title = element_text(hjust=0.5),
          legend.position = "none") + 
    ylab("Estimated Losses (in thousands)") + xlab("Institution Type") +
    scale_y_continuous(labels=dollar) +
    geom_point(alpha=0.5) + geom_jitter(size=2)
inst_loss

#failure over transaction types
trans_failure <- banks %>% 
  group_by(Transaction_Type, Year) %>% 
  summarise(Count = n()) %>% 
  ggplot(aes(x=Year, y=Count, group=Transaction_Type, col=Transaction_Type)) +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5, size=7, margin(3,3,3,3)),
        plot.margin = margin(10,10,10,10),
        legend.position = c(0.9, 0.9),
        plot.title = element_text(hjust=0.5)) +
  ggtitle("Bank Failures Accross the Transaction Types") +
  ylab("Number of Institutions Failed") + 
  scale_color_discrete(name="Transaction Type") + geom_line(size=0.8)
trans_failure

#estimated loss across transaction types
trans_loss <- banks %>% 
  ggplot(aes(x=Transaction_Type, y=Estimated_Loss_till_2015, col=Transaction_Type)) +
  ggtitle("Estimated Losses for Different Transaction Types") +
  theme(axis.text.x = element_text(size=7, margin(4,4,4,4)),
        axis.text.y = element_text(size=7, margin(6,15,15,6)),
        plot.margin = margin(10,20,10,15),
        plot.title = element_text(hjust=0.5),
        legend.position = "none") + 
  ylab("Estimated Losses (in thousands)") + xlab("Transaction Type") +
  scale_y_continuous(labels=dollar) +
  geom_point(alpha=0.5) + geom_jitter(size=2)
trans_loss

#failure over charter types
charter_failure <- banks %>% 
  group_by(Charter_Type, Year) %>% 
  summarise(Count = n()) %>% 
  ggplot(aes(x=Year, y=Count, group=Charter_Type, col=Charter_Type)) +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5, size=7, margin(3,3,3,3)),
        plot.margin = margin(10,10,10,10),
        legend.position = c(0.9, 0.9),
        plot.title = element_text(hjust=0.5)) +
  ggtitle("Bank Failures Accross the Charter Types") +
  ylab("Number of Institutions Failed") + 
  scale_color_discrete(name="Charter Type") + geom_line(size=0.8)
charter_failure

#correlation between total assets and total deposits
temp <- banks %>% na.omit()
cor(temp$Total_Assets, temp$Total_Deposits)