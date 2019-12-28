# Set Directory
setwd("C:/Users/s0d00n5/Downloads")
#Read File
dataset <- read.csv("Uber Request Data.csv", stringsAsFactors = F)

#Load Libraries
library(ggplot2)
library(ggthemes)
library(dplyr)


#Replace "/" character to "-" to get uniform date format
dataset$Request.timestamp = gsub("/", "-", dataset$Request.timestamp)
dataset$Drop.timestamp = gsub("/", "-", dataset$Drop.timestamp)

#convert dataset into uniform format; Seconds are skipped since it is not needed for this analysis
dataset$request_time = as.POSIXlt(dataset$Request.timestamp, format = "%d-%m-%Y %H:%M")
dataset$drop_time = as.POSIXlt(dataset$Drop.timestamp, format = "%d-%m-%Y %H:%M")

#Get Request Date and Day of the week
dataset$request_date = as.POSIXlt(dataset$Request.timestamp, format = "%d-%m-%Y")
dataset$request_day = weekdays(dataset$request_date)


#Get Hour from DateTime
dataset$req_hour = format(dataset$request_time, "%H")
dataset$drop_hour = format(dataset$drop_time, "%H")



#Problem 1: Identifying Uber's Problem
#1.1 Plot date-wise Requests  to rule out days of week as a variable affecting traffic. Conclusion : All dates have uniform bookings
p1 <- ggplot(data = dataset, aes(x = request_day)) + geom_bar()
p1



#1.2: Label the hours of the day by demand traffic for analysis
grouped_demand_data = dataset[, c(1, 2, 4, 11)] %>%
  group_by(req_hour) %>%
  summarise(count_of_requests = n())

p2 <-
  ggplot(data = grouped_demand_data,
         aes(
           x = as.numeric(req_hour),
           y = count_of_requests,
           label = count_of_requests
         ))

p2 + geom_line() + theme_bw() +
  theme(panel.grid = element_blank()) + labs(y = "No.of Requests", x = "Hour of Request") + geom_vline(xintercept =
                                                                                                         c(1, 5, 12, 17, 21), linetype = "dotted") + theme_economist() +
  annotate(
    "text",
    x = 3,
    y = 450,
    label = "Early Morning",
    size = 3
  ) +
  
  annotate(
    "text",
    x = 9,
    y = 450,
    label = "Peak Morning",
    size = 3
  ) +
  annotate(
    "text",
    x = 14,
    y = 450,
    label = "After Morning",
    size = 3
  ) +
  annotate(
    "text",
    x = 19,
    y = 450,
    label = "Peak Evening",
    size = 3
  ) +
  annotate(
    "text",
    x = 22,
    y = 450,
    label = "Late Night",
    size = 3
  )

#Looking at the graph, hours in day have been divided into  5 parts- 1am-5am : early morning, 5 am - 12 noon : peak morning hours, 12 noon - 5 pm : after morning hours, 5pm- 9pm:peak evening hours, 9pm-1am:late night
dataset$req_hour = as.numeric(dataset$req_hour)
label = rep(
  c(
    "late night hours",
    "early morning",
    "peak-morning hours",
    "after-morning-hours",
    "peak evening hours",
    "late night hours"
  ),
  c(1, 4, 7, 5, 4, 3)
)
label = as.data.frame(label)
label$req_hour = 0:23
dataset=merge(dataset,label)


#1.3 Pyramid Plot done by hour, to compare hourly bookings when Pickup is from Airport or City
p3 <- ggplot(data = dataset, aes(x = req_hour, fill = Status)) +
  geom_bar(data = subset(dataset, Pickup.point == "Airport")) +
  
  geom_bar(
    data = subset(dataset, Pickup.point == "City"),
    mapping = aes(y = -..count..),
    position = "identity"
  ) +
  geom_hline(yintercept = c(0:1),
             colour = "white",
             lwd = 1) +
  labs(y = "No.of Requests", x = "Hour of Request") + coord_flip() +
  scale_y_continuous(limits = c(-400, 400), breaks = seq(-400, 400, 50)) +
  annotate(
    "text",
    x = 15,
    y = -300,
    label = "City to Airport",
    size = 4
  ) +
  annotate(
    "text",
    x = 15,
    y = 300,
    label = "Airport to City",
    size = 4
  ) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + theme_economist() + labs(fill = "Request Status")

p3

#  1.4 Cancellation Comparison between City and Airport Pickups
p4 <- ggplot(data = dataset, aes(x = Status))
p4 + geom_bar(aes(fill = Pickup.point), position = position_dodge()) + theme_fivethirtyeight()

#1.5 Calculate Average time taken for an airport ride
dataset$diff_time = abs(dataset$request_time - dataset$drop_time)
avg_time_taken = mean(dataset$diff_time, na.rm = T)
print(avg_time_taken)


#Problem 2.1 : Compare the Supply-Demand Problem

grouped_data = dataset[, c(1, 3, 5, 14)] %>%
  group_by(Pickup.point, Status, req_hour) %>%
  summarise(count_of_requests = n())

p5 <-
  ggplot(
    data = grouped_data,
    aes(
      x = as.numeric(req_hour),
      y = count_of_requests,
      colour = Status,
      label = count_of_requests
    )
  )

p5 + geom_line() + facet_grid(. ~ Pickup.point) + theme_bw() +
  theme(panel.grid = element_blank()) + labs(y = "No.of Requests", x = "Hour of Request")


# Problem 2.2: Calculate  % availability at different time of the day

#Calculate availability at Pickup Point, Status and Label level
perc_availability = dataset[, c(1, 3, 5, 13)] %>% group_by(Pickup.point, Status, label) %>%
  summarise(count_of_requests = n())

#Calculate total requests at Pick up Point and different times of the day
total = dataset[, c(1, 3, 5, 13)] %>% group_by(Pickup.point, label) %>%
  summarise(total_requests = n())

#Get Total Percent Availability
perc_availability = merge(perc_availability, total)
perc_availability$percentage = perc_availability$count_of_requests / perc_availability$total_requests *
  100

# Plot %availability at different time labels of the day

p6 <-
  ggplot(data = perc_availability,
         aes(
           x = label,
           y = percentage,
           fill = factor(Status),
           label = percentage
         )) + geom_bar(stat = "identity", position = "fill") + labs(y = "% Uber Requests", x =
                                                                      "Time Slot") + facet_wrap(~ Pickup.point)
p6 + coord_flip() + theme_economist() + labs(fill = "Request Status")
