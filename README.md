# Lab-8-StatsSloths

## Team Section
* Main Question: Would the number of crime related deaths and injuries in Denver decrease if guns were harder to obtain?

* Question Importance: This is an important question to address because there are lives on the line. When we are talking about a topic as sensitive as citizens or officers lives we must take every precaution necessary to ensure that people are sare. In this way it is important that we analyze this question so that we can know what is causing the msot amount of deaths in Denver and how that relates to gun control in our area. Most importantly if taking more guns off the streets would help to save lives. 

 * Conclusion: Our data showed that among people who are involved in a police confrontation, those who have a gun and are more likely to be injured or killed than those who do not have a gun. Therefore, making guns harder to obtain would decrease the number of people with guns, which in turn would decrease the number of crime related deaths and injuries.	

 * Recommendation: 	

```{r}
shootings <- read_csv("denver_police_officer_involved_shootings.csv")
team <- shootings %>%
  mutate(Gun = case_when((ARMED_WITH=="Firearm")~"yes", (ARMED_WITH!="Firearm")~"no")) %>%
  filter(ROLE=="Subject")

ggplot(data = team) +
  geom_bar(mapping = aes(Gun, fill = CASUALTY), position = position_dodge()) +	  geom_bar(mapping = aes(Gun, fill = CASUALTY), position = position_dodge())
  labs(title = "Number of Casualties by Gun Possession", x = "Gun", y = "Count") +	
  theme(axis.title.x=element_blank())
 ```
 
 * Dataset Explanation: Our dataset shows incidents in which one or more Denver police officer discharged their firearm. It begins on January 1st 2015 and was last updated on August 22nd 2018. There are 24 columns in our data set. The data was retrieved from the following website.  https://www.denvergov.org/content/denvergov/en/police-department/crime-information/crime-statistics-maps.html.html


## Individual Parts
### Katie's Section
* Question: How does the level of casualty and total amount of each casualty vary by weapon used?

* Findings: Overall the most amount of casualties resulted from firearms. 10 out of the 37 subjects were killed when the weapon was a firearm. An additional 10 out of the 37 subjects were injured when the weapon was a firearm. Out of the 37 subject cases 23 of those were involving a firearm which is significantly the largest appearance of one weapon. The knife and replica/air gun tie for 2nd place at 3 observations each. That is an extremely large gap. The most for deceased, injured and not injured casualties all include guns. 

```{r}
crime <- read.csv("denvercrime.csv", header = TRUE, sep = ",", na.strings =c("N/A", " ")) %>%
  separate(INCIDENT_DATE, into=c("YEAR", "MONTH", "DAY"), sep = "-", convert = TRUE) %>%
  separate(DAY, into=c("DAY", "ZEROS"), sep = " ", convert = TRUE)
crime$ZEROS <- NULL
```
```{r}
subject <- filter(crime, ROLE == "Subject")
```
```{r}
ggplot(data = subject)+
  geom_bar(mapping = aes(x=CASUALTY, fill = ARMED_WITH))+
  labs(title = "Casualty by Weapon", x = "Type of Casualty", y = "Number of Casualty", fill = "Weapon")+
  scale_fill_manual(values=c("khaki1", "lightblue3", "pink3", "salmon2", "lightcyan4", "plum3", "palegreen3"))
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

### Zandy's Section
* Question: how does the contact-basis change whether a firearm is discharged or not
* Findings:

* Total # of discharged firearms = 65
* Total # of non-discharged firearms = 21
* discharged vs non-discharged firearms for weapon/concealed weapon = 12 discharged weapons vs 2 non-discharged firearms = 85.7% discharged vs 18.5% discharged total
* discharged vs non-discharged firearms for shooting = 2 discharged firearms vs 1 non-discharged firearm = 67% discharged vs 3.1% discharged total
* discharged vs non-discharged firearms for vehicle stop = 8 discharged firearms vs 2 non-discharged firearms = 80% discharged vs 12.3% discharged total
* discharged vs non-discharged firearms for a suicidal person/suicide = 2 discharged firearms vs 1 non-discharged firearms = 67% discharged vs 3.1% discharged total
* discharged vs non-discharged firearms for a warrant = 9 discharged firearms vs 4 non-discharged firearms = 64.3% discharged vs 13.8% discharged total
* discharged vs non-discharged firearms for a robbery = 4 discharged firearms vs 2 non-discharged firearms = 67% discharged vs 6.25% discharged total
* discharged vs non-discharged firearms for BOLO = 2 discharged firearms vs 0 non-discharged firearms = 100% discharged vs 3.1% discharged total
* discharged vs non-discgarded firearms for burglary = 2 discharged firearms vs 2 non-discharged firearms = 50% discharged vs 3.1% discharged total
* discharged vs non-discharged firearms for a suspicious vehicle = 4 discharged firearms vs 0 non-discharged firearms = 100% discharged vs 6.15% discharged total
* discharged vs non-discharged firearms for a domestic violence = 1 discharged firearm vs 1 non-discharged firearm = 50% discharged vs 1.54% discharged total
* discharged vs non-discharged firearms for a family disturbance = 2 discharged firearms vs 2 non-discharged firearms = 50% discharged vs 3.1% discharged total
* discharged vs non discharged firearms for a street robbery = 1 discharged firearm vs 1 non-discharged firearm = 50% discharged vs 1.54% discharged total
* discharged vs non-discharged firearms for a vehicle check = 3 discharged firearms vs 0 non-discharged firearms = 100% discharged vs 4.6% discharged total
* discharged vs non-discharged firearms for a bank robbery suspect = 3 discharged firearms vs 1 non-discharged firearm = 75% discharged vs 4.6% discharged total
* discharged vs non-discharged firearms for surveillance = 3 discharged firearms vs 2 non-discharged firearms = 60% discharged vs 4.6% discharged total
* discharged vs non discharged firearms for a shot spotter = 1 discharged firearm vs 1 non-discharged firearm = 50% discharged vs 1.54% discharged total

```{r}
library(tidyverse)
library(dplyr)
library(readr)
denver_police_officer_involved_shootings <-read_csv("C:/Users/zandy/Downloads/denver_police_officer_involved_shootings.csv")
Parsed with column specification:
cols(
  .default = col_character(),
  INCIDENT_NUMBER = col_double(),
  INCIDENT_DATE = col_datetime(format = ""),
  INCIDENT_TIME = col_double(),
  AGE = col_double()
)
 
mod_shootings <- denver_police_officer_involved_shootings %>%
  select(6, 20)
discharged_firearm <- mod_shootings %>%
  filter(DISCHARGED_FIREARM == "Yes")
non_discharged <- mod_shootings %>%
  filter(DISCHARGED_FIREARM == "No")

weapon <- mod_shootings %>%
  filter(CONTACT_BASIS == "Weapon / Concealed Weapon")

shooting <- mod_shootings %>% 
  filter(CONTACT_BASIS == "Shooting")


vehicle_stop <- mod_shootings %>%
  filter(CONTACT_BASIS == "Vehicle Stop")

suicide <- mod_shootings %>%
  filter(CONTACT_BASIS == "Suicidal Person / Suicide")

warrant <- mod_shootings %>% 
  filter(CONTACT_BASIS == "Warrant")

robbery <- mod_shootings %>%
  filter(CONTACT_BASIS == "Robbery - In Progress / Just Occurred")

BOLO <- mod_shootings %>%
  filter(CONTACT_BASIS == "BOLO (Be on the lookout)")

burglary <- mod_shootings %>%
  filter(CONTACT_BASIS == "Burglary - In Progress")

suspicios_vehicle <- mod_shootings %>%
  filter(CONTACT_BASIS == "Suspicious Vehicle")

domestic_violence <- mod_shootings %>%
  filter(CONTACT_BASIS == "Domestic Violence - In Progress")

family_disturbance <- mod_shootings %>%
  filter(CONTACT_BASIS == "Family Disturbance")

street_robbery <- mod_shootings %>%
  filter(CONTACT_BASIS == "Street Robbery")

vehicle_check <- mod_shootings %>%
  filter(CONTACT_BASIS == "Vehicle Check")

bank_robbery_suspect <- mod_shootings %>%
  filter(CONTACT_BASIS == "Bank Robbery Suspect")

surveillance <- mod_shootings %>%
  filter(CONTACT_BASIS == "Surveillance")

shot_spotter <- mod_shootings %>%
  filter(CONTACT_BASIS == "Shot Spotter")
  
  ggplot(data = mod_shootings ) +
  geom_bar(mapping = aes(x =DISCHARGED_FIREARM, fill = CONTACT_BASIS)) 
  
  ```

## Team Summary:
* I, Kevin Luth, asked which age group, when confronted by the police, fired their weapon the most. I found that those aged between 20 and 29 were the most likely to fire their gun. To find this, I sorted the dataset, removing unnecessary columns by selecting only the ones of interest to me. I mutated a column to represent the age group of the subject. the three groups I created were those under 20 years old, those between 20 and 29, and those 30 and up. I also filtered the dataset to show only subjects, not officers, and only those with firearms as their weapon. I then created a jitter plot showing the occurances of those who fired their gun and those who did not among each of the age groups. I changed the color of the age groups and the shape of the yes or no decisions to make it easier to tell who did what. I also made the height and width of each jitter smaller to make it clearer which age group and decision each point belonged to. I also changed the labels and title to more accurately display what they represent.

* I, Madeline Garrett, answered the question of how initial contact influences the basis of contact that occurs. I did this by sorting through the data into only the categories that I needed, and then by seperating the month and year and date. I then created a bar graph that was seperated into citizen and officer initiated and then seperated those into basis of contact. Becasue there were so many various types of basis of contact, the built in color pallette made it really hard to tell apart. To fix this I assigned each type of basis of contact its own unique color. This made it easier to find and tell apart. 

* I, Katie Stewart, analyzed the data to determine which weapon caused the most causlties among subjects. I did this by filtering the data to only look at cases with the subject. I then explored the cases in which people were deceased, injured or not injured at all. I used the geom_bar function to create a graph displaying the number of casulties by each weapon. I altered the graph by adding a title and changing the x and y axis along with the legend title. I also decided to change the colors of the weapons in the legend. 
