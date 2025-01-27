# Data Cleaning

hotel_booking <- read.csv("hotel_bookings.csv")
hotel_booking$distribution_channel <-factor(hotel_booking$distribution_channel)
levels(hotel_booking$distribution_channel)
levels(hotel_booking$distribution_channel)[1]<-1
levels(hotel_booking$distribution_channel)[2]<-2
levels(hotel_booking$distribution_channel)[3]<-3
levels(hotel_booking$distribution_channel)[4]<-4
levels(hotel_booking$distribution_channel)[5]<-5

hotel_booking$deposit_type<-factor(hotel_booking$deposit_type)
levels(hotel_booking$deposit_type)
levels(hotel_booking$deposit_type)[1]<-1
levels(hotel_booking$deposit_type)[2]<-2
levels(hotel_booking$deposit_type)[3]<-3

smallhotel<-hotel_booking[c(2,3,16,18,23,28)]
write.csv(smallhotel, "smallhotel.csv" , row.names = FALSE)

## Association Analysis

#renaming variables

is_canceled <- smallhotel$is_canceled
lead_time <- smallhotel$lead_time
distribution_channel <- smallhotel$distribution_channel
previous_cancellations <-smallhotel$previous_cancellations
deposit_type <- smallhotel$deposit_type
adr <- smallhotel$adr

#turn is_canceled into a factor

is_canceled <- as.factor(is_canceled)
levels(is_canceled) <- c(0,1)

str(smallhotel)
str(is_canceled)

# Association Analysis code

library(regclass)

associate(is_canceled~deposit_type, data=smallhotel, prompt=FALSE, plot=TRUE, col=TRUE)

associate(is_canceled~adr, data=smallhotel, prompt=FALSE, plot=TRUE, col=TRUE)

associate(is_canceled~lead_time, data=smallhotel, prompt=FALSE, plot=TRUE, col=TRUE)

associate(is_canceled~distribution_channel, data=smallhotel, prompt=FALSE, plot=TRUE, col=TRUE)

associate(is_canceled~previous_cancellations, data=smallhotel, prompt=FALSE, plot=TRUE, col=TRUE)
