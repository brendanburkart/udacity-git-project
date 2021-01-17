
ny = read.csv('new_york_city.csv')
wash = read.csv('washington.csv')
chi = read.csv('chicago.csv')

head(ny)

head(wash)

head(chi)

wash['Gender'] = NA
wash['Birth.Year'] = NA
head(wash)

# new york
print('nyc before:')
print(dim(ny))

ny['city'] = 'New York'

print('nyc after:')
print(dim(ny))

# washington
print('wash before:')
print(dim(wash))

wash['city'] = 'Washington'

print('wash after:')
print(dim(wash))

# chicago
print('chi before:')
print(dim(chi))

chi['city'] = 'Chicago'

print('chi after:')
print(dim(chi))


head(ny, 1)

head(chi, 1)

head(wash, 1)

df <- rbind(ny, chi, wash)

print(dim(df))

head(df)

print(names(df))
print('                                                                ')
print('                                                                ')
names(df) <- tolower(names(df))
names(df) <- chartr('.', '_', names(df))

print(names(df))

summary(df)

df[is.na(df$trip_duration), ]

print(dim(df))

df <- df[!is.na(df$trip_duration), ]

print(dim(df))

str(df)

df$x <- as.character(df$x)
df$x <- as.factor(df$x)

str(df)

# start_time to start_datetime
print(names(df)[2])
names(df)[2] <- 'start_datetime'
print(names(df)[2])

# end_time to end_datetime
print(names(df)[3])
names(df)[3] <- 'end_datetime'
print(names(df)[3])

df$start_datetime <- as.character(df$start_datetime)
df$start_datetime <- strptime(df$start_datetime, format='%Y-%m-%d  %H:%M:%S')

df$end_datetime <- as.character(df$end_datetime)
df$end_datetime <- strptime(df$end_datetime, format='%Y-%m-%d  %H:%M:%S')

str(df)

df$start_day_of_week <- weekdays(df$start_datetime)

df$start_day_of_week <- as.factor(df$start_day_of_week)

df$start_day_of_week <- ordered(df$start_day_of_week
                                , c('Saturday', 'Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))

df$weekend <- ifelse(df$start_day_of_week == 'Saturday' | df$start_day_of_week == 'Sunday'
                     , 'weekend'
                     , 'weekday'
                    )

df$weekend <- as.factor(df$weekend)

head(df)

# trip_duration to seconds
print(names(df)[4])
names(df)[4] <- 'seconds'
print(names(df)[4])

# covert to minutes
df['minutes'] <- df$seconds / 60

# covert to hours
df['hours'] <- df$minutes / 60

# covert to days
df['days'] <- df$hours / 24

head(df)

str(df)

df$start_station <- as.character(df$start_station)
df$end_station <- as.character(df$end_station)

df[df$start_station == df$end_station, ]

df['location_match'] <- ifelse(df$start_station == df$end_station, TRUE, FALSE)

head(subset(df, df$location_match == TRUE))

df['trip'] <- paste(df$start_station, ' --> ', df$end_station)

head(df)

df$start_station <- as.factor(df$start_station)
df$end_station <- as.factor(df$end_station)
df$trip <- as.factor(df$trip)

str(df)

df$birth_year <- as.factor(df$birth_year)
df$city <- as.factor(df$city)

str(df)

df[df$user_type == '', ]

summary(df$user_type)

df[df$user_type == '', 7] <- NA

df$user_type <- as.character(df$user_type)

df$user_type <- as.factor(df$user_type)

summary(df$user_type)

df[df$gender == '', ]

summary(df$gender)

df$gender[df$gender == ''] <- NA

df$gender <- as.character(df$gender)

df$gender <- as.factor(df$gender)

summary(df$gender)

for(each in names(df)){
    print(each)
}

df <- df[c("x"
           , "city"
           , "user_type"
           , "gender"
           , "birth_year"
           , "start_day_of_week"
           , "weekend"
           , "start_datetime"
           , "end_datetime"
           , "seconds"
           , "minutes"
           , "hours"
           , "days"
           , "trip"
           , "start_station"
           , "end_station"
           , "location_match"
          )]

print(dim(df))

t(head(df))

str(df)

summary(df)

library(ggplot2)

by(df$start_day_of_week, df$city, summary)

temp <- df

levels(temp$start_day_of_week) <- list(Sa = 'Saturday'
                                       , Su = 'Sunday'
                                       , Mo = 'Monday'
                                       , Tu = 'Tuesday'
                                       , We = 'Wednesday'
                                       , Th = 'Thursday'
                                       , Fr = 'Friday'
                                       )

by(temp$start_day_of_week, temp$city, summary)

qplot(data = temp, x = start_day_of_week, fill = start_day_of_week) + 
    facet_wrap(~city, scales = 'free_y') + 
    theme(legend.position = 'none')

# create trips dataframe to fill
trips_df <- data.frame()

# iterate over cities in data
for(city_name in unique(df$city)){
    
    print(city_name)
    # get the value counts of trips
    temp <- head(summary(subset(df,  city == city_name)$start_station
                        )
                 , 3
                )

    # format these as a dataframe
    temp <- data.frame(unlist(temp))

    # rename column
    names(temp) <- c('count')

    # extract trip column from index
    temp <- cbind('station' = rownames(temp), temp)

    # rename index
    rownames(temp) <- 1:nrow(temp)

    # add city column
    temp$city <- city_name

    # re-order columns
    temp <- temp[c('city', 'station','count')]
    
    # fill dataframe
    trips_df <- rbind(trips_df, temp)
}

# combine city and station columns and reorganize
trips_df$start_station <- paste(trips_df$city, ' :: ', trips_df$station)

trips_df <- trips_df[c('start_station', 'count')]

# review final dataframe
trips_df

ggplot(data = trips_df
      , aes(x = reorder(start_station, count), y = count)
      ) + 
    geom_col(aes(fill = start_station)
             , color = 'black'
             , fill = 'lightgreen' 
            ) + 
    coord_flip() + 
    ggtitle('Count of Top 3 Starting Station for each City') + 
    xlab('Starting Station') + 
    ylab('# of Rentals')

ggplot(data = subset(df[df$minutes < 60, ])
      , aes(x = minutes)
      ) + 
    geom_histogram(binwidth = 2
                   , color = 'black'
                   , fill = 'lightblue' 
                  ) +
    ggtitle('Distribution of Rental Duration by Whether Bike was Returned to Starting Point') + 
    xlab('Minutes of Rental') + 
    ylab('# of Rentals') + 
    facet_wrap(~location_match, scales = 'free_y')

system('python -m nbconvert Explore_bikeshare_data.ipynb')
