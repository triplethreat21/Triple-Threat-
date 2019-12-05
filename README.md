# Triple-Threat-providing information about video game sales across the globe.

# Video Game Sales Across the Globe

# Statistical Analysis on Video Game Sales using dataset from kaggle.com

The dataset was compiled by Data Engineer Gregory Smith, who sourced his data from a network known as vgchartz.
Vgchartz collects their data through calculated estimates, polls with video game retailers and video game communities, studying resale prices to determine popularity and consulting directly with developers and retail stores.The data sources obtained was from kaggle.com (vgsales.csv). The data obtained This dataset contains a list of video games with sales greater than 100,000 copies. 

# Data
(vgsales.csv)
Rank - Ranking of overall sales
Name - The games names
Platform - Platform of the games release (i.e. PC,PS4, etc.)
Year - Year of the game's release
Genre - Genre of the game
Publisher - Publisher of the game
NorthAmerican Sales - Sales in North America (in millions)
EUROPE Sales - Sales in Europe (in millions)
JAPAN Sales - Sales in Japan (in millions)
Other Sales - Sales in the rest of the world (in millions)
Global Sales - Total worldwide sales.
# What is the problem?
Video games are constantly being played across the world, now more than ever before. The reason we should care because it is causing video games to increase over the years this notebook will analyze the consumption of video games around the world and consider the trends in the popularity of genres across time.
# Motivation for your team and why the class should care?
Video Game sales were a motivation for the team to choose this because statstics show that Video game consumption of the Top 200 games are at a all time high   in North America, followed by Europe and Japan. Most  Action, Shooter and Platform genres are most popular across all regions, though certain regions further prefer particular genres instead.

# Visualization
![gaj](https://user-images.githubusercontent.com/58538685/70268355-4c2bc600-176e-11ea-8742-728d27997331.png)
![gg](https://user-images.githubusercontent.com/58538685/70268359-4e8e2000-176e-11ea-9e50-5daf462ab6a1.png)
![ggg](https://user-images.githubusercontent.com/58538685/70268390-5a79e200-176e-11ea-9cbf-bb4f348cc3b4.png)
![obs](https://user-images.githubusercontent.com/58538685/70268398-5ea5ff80-176e-11ea-9047-a8a3d94ea14f.png)
![P11](https://user-images.githubusercontent.com/58538685/70268406-64034a00-176e-11ea-929d-209b2d09231a.png)
![Picture12](https://user-images.githubusercontent.com/58538685/70268414-66fe3a80-176e-11ea-80c4-673ee27b0b02.png)
[000003](https://user-images.githubusercontent.com/58538685/70260958-64481900-175f-11ea-9f7b-8c7c502aeb88.png)
![000004 (1)](https://user-images.githubusercontent.com/58538685/70260967-6a3dfa00-175f-11ea-8c76-6247e3b4da7f.png)
![000005](https://user-images.githubusercontent.com/58538685/70260971-6d38ea80-175f-11ea-98a8-a66dccc14628.png)

# What are the sub-problems and how did you solve each?
What does video game consumption look like around the world?
Which Genres were popular over the years? How has gaming trends change over time? These sub-problems were solved by using  hypothesis testing, prediction (regression) analysis, classification,  time-series analysis, clustering, and verification. 

# #What is the error/accuracy of each model?

# Model 1: Global sales vs. North America Sales
fit <- lm(Global ~ NorthAm, data=vgsales)
fit$coefficients

```
# Model 2: Global sales vs. European sales
```{r}
fit <- lm(Global ~ Europe, data=vgsales)
fit$coefficients
```

#Model 3:Global sales vs. Japan sales

```{r}
fit <- lm(Global ~ Japan, data=vgsales)
fit$coefficients
```

#Model 4:Global Sales vs. other sales

```{r}
fit <- lm(Global ~ Other, data=vgsales)
fit$coefficients
```

##Model 4: Global Sales vs. other sales
```{r}
fit1 <- lm(Global ~ Other + NorthAm + Europe + Japan,vgsales)
fit$coefficients
```


## Required library for sample.split()
```{r}
install.packages("caTools")
```

## Load caTools Library
```{r}
library(tidyverse)
library(caTools)
```

## Get Training and Testing Sets
sample.split(fit1$AnyColumn, SplitRatio = decimal)
```{r}
# Set the random seed for repeatability
set.seed(123) 

# Split the data into 3:1 ratio
sample = sample.split(NA4$Global, SplitRatio = .75)
train = subset(NA4, sample == TRUE)
test  = subset(NA4, sample == FALSE)
```

# Build Model on Training Set
```{r}
model <- lm(Global ~ Europe, data = train)
model
```

# Predict the testing set
```{r}
# Predict the position with the model and test data
predictedPosition <- predict(model, test)
predictedPosition

# Create data frame with actual and predicted values
salesPrediction <- data.frame(NorthAm = test$Europe, 
                               ActualPosition = test$NorthAm,
                               PredictedPosition = predictedPosition)

salesPrediction
```
## Visualize the prediction
```{r}
ggplot(data = salesPrediction, mapping = aes(x =ActualPosition, y=predictedPosition )) +
  geom_point(data = salesPrediction, 
             mapping = aes(y = predictedPosition),
             color = "blue") +
  geom_point(data = salesPrediction, 
             mapping = aes(y = PredictedPosition), 
             color = "red") +
  labs(title = "North America sales Prediction" ,
        subtitle = "Actual (Blue) vs. Predicted (Red)",
        y = "Position")

```

## Fitness 
```{r}
summary(model)
```
# Load measurement libraries
```{r}
# Using the Metrics library for MSE
install.packages("Metrics")
library(Metrics)
```

## Mean Absolute Error
Measures the average  the difference between predicted and actual.
```{r}
mae <- mae(salesPrediction$ActualPosition, salesPrediction$PredictedPosition)
cat("Mean Absolute Error:", mae)
```

## Mean Squared Error
Mean Squared Error (MSE) determines how much error there
is in the model. The smaller the error, the higher the accuracy.
"The squaring is done so negative values do not cancel positive values. The smaller the Mean Squared Error, the closer the fit is to the data."
- https://www.vernier.com/til/1014/


The mse(<actual>, <predicted>) function takes in 2 vectors:
Actual: the vector of actual values
Predicted: the vector of what the model predicted
```{r}
mse <- mse(salesPrediction$ActualPosition, salesPrediction$PredictedPosition)
cat("Mean Squared Error:", mse)
```

###Using statistical analysis, what values were significantly different?

```{r}
library(readr)
vgsales <- read_csv("vgsales.csv", col_types = cols(Europe = col_number(), 
    Global = col_number(), Japan = col_number(), 
    NorthAm = col_number(), Other = col_number()))
View(vgsales)
```

```{r}
summary(vgsales)
```

```{r}
names(vgsales)
```
```{r}
dim(vgsales)
```

```{r}
name = factor(vgsales$Name)
platform = factor(vgsales$Platform)
year = factor(vgsales$Year)
genre = factor(vgsales$Genre)
publisher = factor(vgsales$Publisher)
```

```{r}
str(vgsales)
```

```{r}
## Isolating variables
rank = vgsales$Rank
name = vgsales$Name
platform = vgsales$Platform
year = vgsales$Year
genre = vgsales$Genre
publisher = vgsales$Publisher
northam = vgsales$NorthAm
europe = vgsales$Europe
japan = vgsales$Japan
other = vgsales$Other
global = vgsales$Global
```


# Load Libraries
```{r}
library(tidyverse)
```



# Exploratory analysis
#Scatter plot of Video Game Sales between NA, EU, JP, and Global
```{r}
ggplot(data = vgsales, 
       mapping = aes(x = Year, y = NorthAm , color = "Red", size = 10)) +
  geom_point(show.legend = FALSE) +
  labs(title = "North America Video Game Sales", 
       subtitle = "North America: 1980-2020", 
       y = "Video Game sales($B)")
```
```{r}
ggplot(data = vgsales, 
       mapping = aes(x = Year, y = global , color = "Red", size = 10)) +
  geom_point(show.legend = FALSE) +
  labs(title = "Global Video Game Sales", 
       subtitle ="Global: 1980-2020", 
       y = "Video Game sales($B)")
```
```{r}
ggplot(data = vgsales, 
       mapping = aes(x = Year, y = japan , color = "Red", size = 10)) +
  geom_point(show.legend = FALSE) +
  labs(title = "Japan Video Game Sales", 
       subtitle = "Japan: 1980-2020", 
       y = "Video Game sales($B)")
```


```{r}
ggplot(data = vgsales, 
       mapping = aes(x = Year, y = europe , color = "Red", size = 10)) +
  geom_point(show.legend = FALSE) +
  labs(title = "Europe Video Game Sales", 
       subtitle = "Europe: 1980-2020", 
       y = "Video Game sales($B)")
```



```{r}
salesGroup <- group_by(vgsales,NorthAm)
salesSummary <- summarize(salesGroup, startYear = min(Year),
                                            endYear = max(Year),
                                            count = n(),
                                            max = max(NorthAm),
                                            min = min(NorthAm),
                                            avg = mean(NorthAm),
                                            avg = mean(NorthAm))

salesSummary
```
```{r}
NA1 <- subset(vgsales, Publisher == "Atari")
NA2 <- subset(vgsales, Publisher == "Activision")
NA3 <- subset(vgsales, Publisher == "Sony Computer Entertainment")
NA4<-rbind(NA1,NA2,NA3)
```

```{r}
ggplot(data = NA4, 
       mapping = aes(x =Publisher, y = Year)) +
  geom_col(show.legend = FALSE)+
  labs(title = "North America Sales", 
       subtitle = "North America: 1980-2020", 
       x = "North America Sales", 
       y = "Sales ($B)")
```

# Statistical Analysis: Hypothesis Testing
# Welch Two Sample T-Test (Unpaired). 
Is there significant difference between Publishers

```{r}
# Subset data intoAtari Pubisher Activison
Atari <- subset(vgsales,publisher  == "Atari")
Atarimean <- mean(Atari$NorthAm)

parkerb <- subset(vgsales, publisher == "Parker Bros.")
parkermean <- mean(parkerb$NorthAm)

Sony <- subset(vgsales, publisher == "Sony Computer Entertainment")
Sonymean <- mean(Sony$NorthAm)

Activision <- subset(vgsales, publisher == "Activision")
Sonymean <- mean(Activision$NorthAm)

cat("Activison Average Sales: $", Sonymean, "B")
```

```{r}
t.test(Atari$NorthAm, Sony$NorthAm,  paired = FALSE)
cat("p-value > alpha (0.05). The null hypothesis fails to be rejected.\n")
cat("The Average sales for Atari and Sony are NOT significantly different.\n")
```

```{r}
#Select the first and last Publisher
first2 <- head(vgsales, "publisher")
last2 <- tail(vgsales,"publisher")

# Paired T-test:pubilsher in north America 
t.test(first2$NorthAm, last2$NorthAm, paired = TRUE)

```

# ANOVA
ANOVA performs a significant difference test on means of 3 or more treatments.
ANOVA determines if there is a significant difference among the treatments.
The aov(<data>, <formula>) function performs the analysis.
  The <data> must be a single data frame with each of the rows labels as treatments.
  The <formula> parameter is the variable for the average ~ the treatment variable.
  Example: aov(dataframe, formula = attribute ~ treatment)

#Is there a significant difference North American sales between publishers?
```{r}
anovasales <- aov(data = vgsales, NorthAm ~ publisher)
summary(anovasales)
```

#Tukey HSD
ANOVA only determines if there is a significant difference but does not say
which treatments differ.
Tukey HSD analysis takes ANOVA's output to determine which treatments different
from each other.

TukeyHSD(anova): The tukey function takes in the aov output and outputs p-values.

```{r}
TukeyHSD(anovasales)
```

###What is the answer to your problem?
We dound that in 2010 & 2011 were responsible for the release of the most sold games between 1982-2015. The Action, Shooter and Sports genres have increased in popularity over the years, while Puzzle-Based and Platform games have decreased in popularity. Video games but particularly in North America, Europe and Japan, are heavily consumed, with action-oriented genres proving particularly popular today.

###Show your GitHub repository and its README.md
