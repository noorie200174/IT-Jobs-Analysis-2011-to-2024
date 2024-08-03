
# r script for IT job analysis code
library("dplyr")
library("readr")
library("ggplot2")
library("tidyr")

df <- read_csv("jobs_01.csv")
head(df, 6)

job_df <- df %>%
  group_by(Year, `Job Type`) %>%
  summarise(Total=sum(`Estimated Number`))

job_df

job_data_analyst <- job_df %>%
  filter(`Job Type` == "Data Analyst")
job_data_analyst

ggplot(data=job_data_analyst, aes(x=Year, y=Total)) +
  geom_line() + geom_point() + scale_x_continuous(breaks = seq(2011, 2024, by=1)) +
  theme_minimal() + 
  labs(title = "Data Analyst Analysis", subtitle = "Analysis from 2011 to 2024" ,x = "Years" ,y= "Estimated Numbers")

growth_rate_data <- df %>%
  group_by(Region, `Job Type`) %>%
  mutate(`Growth Rate`=(`Estimated Number`-lag(`Estimated Number`))/lag(`Estimated Number`))
growth_rate_data  

region_wise_DES <- growth_rate_data %>%
  filter(Region == "Europe", `Job Type`== "Data Entry Specialist")
region_wise_DES

ggplot(data=region_wise_DES, aes(x=Year, y=`Growth Rate`)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks=seq(2011,2024, by=1)) +
  scale_y_continuous(labels=scales::percent) +
  theme_minimal() +
  labs(title="Data Entry Specialist Analysis", subtitle= "Analysis from 2011 to 2024", x= "Years", y="Growth Rate in Percentage")

regional <- growth_rate_data %>%
  filter(Year == max(Year)) %>%
  group_by(Region) %>%
  summarise(Total = sum(`Estimated Number`))
regional

ggplot(data=regional, aes(x=reorder(Region, -Total), y=Total)) +
  geom_bar(stat="identity", aes(fill=Region)) +
  theme_minimal() +
  coord_flip() +
  theme(axis.text.x=element_blank()) +
  theme(axis.title.y = element_blank()) +
  labs(title="Job Analysis 2024", subtitle="Analysis on total number of estimated jobs in each region", x= "Regions", y="Total Estimated Jobs")


role_popularity <- df %>%
  group_by(`Job Type`) %>%
  summarise(Total = sum(`Estimated Number`))

ggplot(data=role_popularity, aes(x=reorder(`Job Type`, -Total), y=Total)) +
  geom_bar(stat="identity", aes(fill=`Job Type`)) +
  theme_minimal() +
  coord_flip() +
  theme(axis.text.x = element_blank()) +
  theme(axis.text.y = element_blank()) +
  labs(title="IT Job Analysis", subtitle = "Analysis on total number of estimated jobs for each role", x= "Job Roles", y="Total Estimated Jobs" )
  
yearly_comparison <- growth_rate_data %>%
  group_by(Year) %>%
  summarise(Total = sum(`Estimated Number`))

ggplot(data=yearly_comparison, aes(x=reorder(Year, Total), y=Total)) +
  geom_bar(stat="identity", fill="lightgreen") +
  theme_minimal() +
  coord_flip() +
  theme(axis.text.x = element_blank()) +
  labs(title="IT Job Analysis", subtitle = "Yearly comparison of Jobs 2011-2024", x="Years", y="Total IT Jobs")
  




  



