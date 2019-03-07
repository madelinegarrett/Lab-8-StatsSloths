# Lab-8-StatsSloths

## Team Section
* Main Question: Would the number of crime related deaths and injuries in Denver decrease if guns were harder to obtain?



## Individual Parts
### Katie's Section
* Question: How does the level of casualty and total amount of each casualty vary by weapon used?

* Findings: 

```{r}
crime <- read.csv("denvercrime.csv", header = TRUE, sep = ",", na.strings =c("N/A", " ")) %>%
  separate(INCIDENT_DATE, into=c("YEAR", "MONTH", "DAY"), sep = "-", convert = TRUE) %>%
  separate(DAY, into=c("DAY", "ZEROS"), sep = " ", convert = TRUE)
crime$ZEROS <- NULL
```
```{r}
subject <- filter(crime, ROLE == "Subject")
```



### Kevin's Section
* Question: Which age group among subjects is most likely to fire their gun?

* Findings: The data showed that when divided into the age groups, under 20, between 20 and 29, and over 30, the group between 20 and 29 is most likely to fire their gun when confronted. The group of those aged above 30 is also more likely to shoot than not, but the chances are not as high as the 20-29 group. The group of those under 20 years old is very unlikely to shoot when confronted.

```{r}
shootings <- read_csv("denver_police_officer_involved_shootings.csv") %>%
  separate(INCIDENT_DATE, into = c("Year", "Month", "Day"), sep="-") %>%
  separate(INCIDENT_TIME, into = c("Hour", "Min"), sep=2, convert = TRUE)

mod_shootings <- shootings %>%
  select(1, 2, 3, 4, 5, 6, 7, 8, 9, 13, 14, 19, 20, 22, 23, 24)

individ <- mod_shootings %>%
  mutate(Age_Group = case_when((AGE<20)~"<20", (AGE >= 20 & AGE < 30)~"20-29", (AGE >=30)~">30")) %>%
  filter(ROLE=="Subject") %>%
  filter(ARMED_WITH=="Firearm")

ggplot(data = individ) + 
  geom_jitter(mapping = aes(DISCHARGED_FIREARM, Age_Group, color=Age_Group, shape=DISCHARGED_FIREARM), size=2.5, height = 0.25, width = 0.25) + 
  scale_y_discrete(limits=c("<20","20-29",">30")) +
  labs(title = "Decision to Fire Weapon on Police by Age", x = "Fired?", y = "Age Group")
```
### Madeline's Section
* Question: Does initial contact influence the basis of contact that occurs? 
* Findings: The data showed that most citizen initiated contact escalated to a weapon/concealed weapon being drawn. This was the most frequent incident found overall with a count of about 15. For officer initiated interactions it was found that vehicle stops were the contact that escalated to a gun being drawn. This was the second most common in the data set at around 11 occurances. Warrent was found to be the most even for both citizen initatied and officer initiated. 

```{r}
library(tidyverse)

data <- read_csv("denverpolice.csv") %>%
separate(INCIDENT_DATE, into = c("Year", "Month", "Day"), sep="-") 

data2 <- as_tibble(data)

shootings  <- data2[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16, 17, 18 )]

shootings 

ggplot(data = shootings) + 
  geom_bar(mapping = aes(x = INITIAL_CONTACT, fill = CONTACT_BASIS), position = "dodge")+ 
  scale_fill_manual(values=c("#CC6666", "#9999CC", "#66CC99","cadetblue", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#0066CC", "pink1", "#F8766D", "#00BA38", "plum3", "lightgoldenrod3", "chocolate3"))

```

## Team Summary:
* I, Kevin Luth, asked which age group, when confronted by the police, fired their weapon the most. I found that those aged between 20 and 29 were the most likely to fire their gun. I mutated a column to represent the age group of the subject. the three groups I created were those under 20 years old, those between 20 and 29, and those 30 and up. I also filtered the dataset to show only subjects, not officers, and only those with firearms as their weapon. I then created a jitter plot showing the occurances of those who fired their gun and those who did not among each of the age groups. I changed the color of the age groups and the shape of the yes or no decisions to make it easier to tell who did what. I also made the height and width of each jitter smaller to make it clearer which age group and decision each point belonged to. I also changed the labels and title to more accurately display what they represent.

* I, Madeline Garrett, answered the question of how initial contact influences the basis of contact that occurs. I did this by sorting through the data into only the categories that I needed, and then by seperating the month and year and date. I then created a bar graph that was seperated into citizen and officer initiated and then seperated those into basis of contact. Becasue there were so many various types of basis of contact, the built in color pallette made it really hard to tell apart. To fix this I assigned each type of basis of contact its own unique color. This made it easier to find and tell apart. 
