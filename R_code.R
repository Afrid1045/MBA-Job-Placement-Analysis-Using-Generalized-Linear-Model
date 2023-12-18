df <- read.csv("C:/Users/mirwa/OneDrive/Documents/Classes/semester-3/LS/MBA_Dataset.csv")
df

#Introducing 'mbavg' column, representing the average of spring and fall grades
df$mbavg <- (df$s_avg + df$f_avg)/2
head(df)

#Checking for the count of '0' (no placement) and '999,998' values
table(df$salary==0)
table(df$salary==999)
df

#Filtering the dataset to create "placed_df" containing information only for placed students, excluding unplaced and missing data.
placed_df <- subset(df, salary!=0)
placed_df <- subset(placed_df, salary!=999)
placed_df <- subset(placed_df, salary!=998)
placed_df

# Create data frame for unplaced students
unplaced_df <- subset(df, salary==0)
unplaced_df

# Summary statistics of "placed_df"
summary(placed_df)

# Boxplot visualizations
boxplot(placed_df$age)
boxplot(placed_df$sex)
boxplot(placed_df$gmat_tot)
boxplot(placed_df$gmat_qpc)
boxplot(placed_df$gmat_vpc)
boxplot(placed_df$gmat_tpc)
boxplot(placed_df$s_avg)
boxplot(placed_df$f_avg)
boxplot(placed_df$quarter)
boxplot(placed_df$work_yrs)
boxplot(placed_df$frstlang)
boxplot(placed_df$salary)
boxplot(placed_df$mbavg)


par(mfrow = c(4, 4), mar = c(2, 2, 1, 1))
variables <- c("age", "sex", "gmat_tot", "gmat_qpc", "gmat_vpc", "gmat_tpc",
               "s_avg", "f_avg", "quarter", "work_yrs", "frstlang", "salary", "mbavg")
for (variable in variables) {
  boxplot(placed_df[[variable]], main = variable)
}
par(mfrow = c(1, 1))

# Pair-wise scatterplot visualizations.
plot(placed_df$age, placed_df$work_yrs)
plot(placed_df$gmat_tot, placed_df$gmat_tpc)
plot(placed_df$gmat_tot, placed_df$gmat_qpc)
plot(placed_df$gmat_qpc, placed_df$gmat_vpc)
plot(placed_df$gmat_tpc, placed_df$gmat_qpc)
plot(placed_df$gmat_tot, placed_df$s_avg)
plot(placed_df$gmat_tot, placed_df$work_yrs)
plot(placed_df$gmat_tot, placed_df$mbavg)
plot(placed_df$gmat_tot, placed_df$frstlang)
plot(placed_df$gmat_vpc, placed_df$frstlang)
plot(placed_df$salary, placed_df$work_yrs)
plot(placed_df$salary, placed_df$gmat_qpc)
plot(placed_df$salary, placed_df$gmat_vpc)
plot(placed_df$salary, placed_df$gmat_tpc)
plot(placed_df$salary, placed_df$s_avg)
plot(placed_df$salary, placed_df$mbavg)


# Select a subset of variables for scatterplots.
selected_vars <- c("age", "work_yrs", "gmat_tot", "gmat_qpc", "gmat_vpc", "gmat_tpc", "s_avg", "mbavg", "frstlang", "salary")

# Create pair-wise scatterplot matrix
pairs(placed_df[selected_vars], main = "Pair-wise Scatterplot Matrix")

# Plotting a Corrgram of each variable in "placed_df"
library(corrgram)
corrgram(placed_df, order = TRUE, 
         lower.panel = panel.shade, 
         upper.panel = panel.pie,text.panel = panel.txt,
         main="Corrgram of placed student intercorrelation")

# Calculate the correlation matrix
cor_matrix <- cor(placed_df)

# Handle missing values by replacing them with 0
cor_matrix[is.na(cor_matrix)] <- 0

my_palette <- colorRampPalette(c("#4477AA", "white", "#EE6677"))(20)
heatmap(cor_matrix, 
        col = my_palette,main = "Correlation Heatmap of Placed Student Intercorrelation",
        cex.main = 1.2,Rowv = NA,Colv = NA)

cor(placed_df)

#Generating contingency tables and conducting chi-square tests, followed by t-tests
mytable1 <- xtabs(~ age + work_yrs, data = placed_df)
#chisq.test(mytable1)
fisher.test(mytable1, simulate.p.value = TRUE)
t.test(placed_df$age, placed_df$work_yrs)
cor.test(placed_df$age, placed_df$work_yrs)
mytable2 <- xtabs(~ gmat_tot + gmat_tpc, data = placed_df)
#chisq.test(mytable2)
t.test(placed_df$gmat_tot, placed_df$gmat_tpc)
t.test(placed_df$salary, placed_df$work_yrs)
t.test(placed_df$salary, placed_df$gmat_tot)
t.test(placed_df$salary, placed_df$mbavg)
t.test(placed_df$salary, placed_df$satis)
t.test(placed_df$salary, placed_df$sex)
t.test(placed_df$salary, placed_df$frstlang)

#Performing Stepwise Multiple Regression
#Iteratively refining the regression model with multiple variables 
#to enhance the adjusted R-squared value for improved fitting.
#The process involves systematically eliminating variables 
#with the least significance (highest p-value) in each step.
model1 <- lm(salary ~ age + work_yrs + mbavg + gmat_tot + sex + 
               satis + gmat_vpc + gmat_qpc + gmat_tpc + s_avg + f_avg, data = placed_df)
summary(model1)

#The process involves systematically eliminating variables 
#with the least significance (highest p-value) in each step.
model1A <- lm(salary ~ age + work_yrs + mbavg + sex + 
                satis + gmat_vpc + gmat_qpc + gmat_tpc + s_avg, data = placed_df)
summary(model1A)
model1B <- lm(salary ~ age + work_yrs + sex + 
                satis + gmat_vpc + gmat_qpc + gmat_tpc + s_avg, data = placed_df)
summary(model1B)
model1C <- lm(salary ~ age + sex + 
                satis + gmat_vpc + gmat_qpc + gmat_tpc + s_avg, data = placed_df)
summary(model1C)
model1D <- lm(salary ~ age + sex + 
                satis + gmat_vpc + gmat_qpc + gmat_tpc, data = placed_df)
summary(model1D)
model1E <- lm(salary ~ age + sex + 
                gmat_vpc + gmat_qpc + gmat_tpc, data = placed_df)
summary(model1E)

model1F <- lm(salary ~ age + 
                gmat_vpc + gmat_qpc + gmat_tpc, data = placed_df)
summary(model1F)
model1G <- lm(salary ~ age + gmat_qpc + gmat_tpc, data = placed_df)
summary(model1G)

#Exploring Alternative Regression Models
#Selecting variables based on previous analyses and tests to investigate alternative regression models.
model2 <- lm(salary ~ age + work_yrs + gmat_tot + sex + satis + gmat_tpc + mbavg, data = placed_df)
summary(model2)
model2A <- lm(salary ~ age + work_yrs + gmat_tot + sex + satis + gmat_tpc, data = placed_df)
summary(model2A)
model2B <- lm(salary ~ age + gmat_tot + sex + satis + gmat_tpc, data = placed_df)
summary(model2B)
model3 <- lm(salary ~ age + work_yrs + gmat_tot + sex + gmat_tpc + mbavg, data = placed_df)
summary(model3)
model4 <- lm(salary ~ age + work_yrs + sex + gmat_tpc + mbavg, data = placed_df)
summary(model4)
model5 <- lm(salary ~ age + work_yrs + gmat_tot + mbavg, data = placed_df)
summary(model5)
model6 <- lm(salary ~ age  + gmat_tpc + mbavg, data = placed_df)
summary(model6)
model7 <- lm(salary ~ age  + gmat_tot + mbavg, data = placed_df)
summary(model7)

# Plotting best and fit ( model1f) and few others too for visualization of residual vs fitted line
plot(model1F)
plot(model1)
plot(model7)

#Introducing the "job" column to "placed_df" and "unplaced_df" for job visualization, 
#using 1 for 'placed' and 0 for 'unplaced'.
placed_df$job <- 1
unplaced_df$job <- 0

# Adding both data frames into one data frame called "jobdata" (row-wise) 
jobdata <- rbind(placed_df, unplaced_df)

# Viewing structure of data set
str(jobdata)

# Making column 'job' a factor from numeric , making it catagorical for logistic regression
jobdata$job <- as.factor(jobdata$job)
jobdata


# Creating few contigency tables and running chi-sq test and afterwards running t.tests ( checking variables with 'job')
mytable3 <- xtabs(~ job + gmat_tot,data = jobdata)
#chisq.test(mytable3)
t.test(age ~ job, data = jobdata)
t.test(mbavg ~ job, data = jobdata)
t.test(gmat_tpc ~ job, data = jobdata)
t.test(gmat_tot ~ job, data = jobdata)
t.test(sex ~ job, data = jobdata)
t.test(quarter ~ job, data = jobdata)
t.test(s_avg ~ job, data = jobdata)
t.test(work_yrs ~ job, data = jobdata)
t.test(satis ~ job, data = jobdata)
t.test(frstlang ~ job, data = jobdata)

#Iterative logistic regression, systematically eliminating variables 
#with the least significance (highest p-value) in each step 
#until achieving the best fit indicated by a significant reduction in residual deviance.
modelL1 <- glm(job ~ age + work_yrs + mbavg + gmat_tot + sex + 
                 satis + gmat_vpc + gmat_qpc + gmat_tpc + s_avg + 
                 f_avg, data = jobdata, family = binomial(link = logit))
summary(modelL1)
modelL2 <- glm(job ~ age + work_yrs + gmat_tot + sex + 
                 satis + gmat_vpc + gmat_qpc + gmat_tpc + 
                 s_avg, data = jobdata, family = binomial(link = logit))
summary(modelL2)
modelL3 <- glm(job ~ age + work_yrs + gmat_tot + 
                 satis + gmat_qpc + gmat_tpc + s_avg, 
               data = jobdata, family = binomial(link = logit))
summary(modelL3)
modelL4 <- glm(job ~ age + work_yrs + gmat_tot + 
                 satis + gmat_tpc + s_avg, data = jobdata, family = binomial(link = logit))
summary(modelL4)
modelL5 <- glm(job ~ age + gmat_tot + 
                 satis + gmat_tpc + s_avg, data = jobdata, family = binomial(link = logit))
summary(modelL5)
modelL6 <- glm(job ~ age + gmat_tot + 
                 s_avg, data = jobdata, family = binomial(link = logit))
summary(modelL6)

modelL7 <- glm(job ~ age + s_avg, data = jobdata, family = binomial(link = logit))
summary(modelL7)

#Visualizing Residuals vs. Fitted Values for the Optimal Model
par(mfrow = c(2,2))
plot(modelL7)













