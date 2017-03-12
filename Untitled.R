setwd("~/Desktop/airbnb")
test <- read.csv("test_users.csv")
train <- read.csv("train_users_2.csv")
plot(train$age)
age_counts <- table(train$age)
barplot(age_counts, main = "Age exploration", xlab = "age")

train[train > 100] <- NA
train[train < 18] <- NA
age_counts <- table(train$age)
barplot(age_counts, main = "Age exploration", xlab = "age")
summary(train$age)

create_count <- table(train$date_account_created, maxsum = 213451)

library(plotly)
library(plyr)

y <- as.data.frame(create_count)
y <- setNames(cbind(rownames(y), y, row.names = NULL), 
         c("date", "create_count"))
data <- data.frame(y$date, y$create_count)
#data$date <- factor(data$date, levels = data[["date"]])
p <- plot_ly(data, x = ~y.date, y = ~y.create_count, type = 'scatter', mode = 'lines')


firstbook <- summary(train$date_first_booking, maxsum = 213451)
y_firstbook <- as.data.frame(firstbook)
y_firstbook <- tail(y_firstbook, 1976)
y_firstbook <- setNames(cbind(rownames(y_firstbook), y_firstbook, row.names = NULL),
                      c("date", "first_book_count"))
firstbook_data <- data.frame(y_firstbook$date, y_firstbook$first_book_count)
p2 <- plot_ly(firstbook_data, x = ~y_firstbook.date, y = ~y_firstbook.first_book_count, 
              type = 'scatter', mode = 'lines')


datediff<-subset(train, select = c(date_first_booking, date_account_created) )
datediff$date_first_booking <- as.Date(datediff$date_first_booking, format = "%Y-%m-%d")
datediff$date_account_created <- as.Date(datediff$date_account_created)
datediff$diff <- difftime(datediff$date_first_booking, 
                          datediff$date_account_created, units = "days")
diff <- table(datediff$diff)
y_diff <- as.data.frame(diff)
plot(y_diff, xlab = "Time difference by date", ylab = "Number count", main = "Date difference between first book and first account created" )
y_diff <- tail(y_diff, 366)

library(prophet)
colnames(data)<-c("ds", "y")
prophet_test <- prophet(data)
future <- make_future_dataframe(prophet_test, periods = 365)
forecast <- predict(prophet_test, future)

result <- Cbind(data, forecast$yhat, first = TRUE)


s <- as.Date("2010-01-01")
e <- as.Date("2015-06-21")
date <- seq(from = s, to = e, by = 1)
result <- cbind(date, result)
result$data_ds <- NULL

result$`forecast$yhat` <- round(result$`forecast$yhat`)
names(result)[2] <- paste("origin")
names(result)[3] <- paste("forecast")

p3 <- plot_ly(result, x = ~date, y = ~origin, name = 'Origin', type = 'scatter', mode = 'lines', 
              line = list(color = 'rgb(22, 96, 167)', width = 4)) %>% 
      add_trace(y = ~forecast, name = "Forecast", line = list(color = 'rgb(205, 12, 24)', width = 4)) %>%  
      layout(title = "Comparison of Account Created Forecast", xaxis = list(title = "Dates"),
             yaxis = list (title = "Account created"))


Sys.setenv("plotly_username"="poweihuang")
Sys.setenv("plotly_api_key"="2Z3cMNaUWtyQHH4eIi9k")
plotly_POST(p, filename = "create-count")
plotly_POST(p2, filename = "firstbook-count")
plotly_POST(p3, filename = "airbnbforecast")
