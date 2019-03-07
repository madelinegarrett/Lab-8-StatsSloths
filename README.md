# Lab-8-StatsSloths

## Team Section



## Individual Parts

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
  labs(title = "Decision to Fire Weapon on Police by Age", x = "Discharged?", y = "Age Group")
```
