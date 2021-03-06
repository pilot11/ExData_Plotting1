# function to write data from the dates 2007-02-01 and 2007-02-02 to a separate file.
# for not need to read the entire dataset into R.
# read the entire dataset lines one by one,
# get the title line and the lines start with "1/2/2007" or "2/2/2007",
# replace the symbolic ";" to " " in these line string,for use read.table() function.
# ignore missing values line,these line contains "?",
# write these lines to a new file.
write_two_days_file <- function(){
    sour_file_name <- "household_power_consumption.txt"
    dest_file_name <- "towdays.txt"
    sour_con <- file(sour_file_name, "r")
    dest_con <- file(dest_file_name, "w")
    
    line=readLines(sour_con,n=1)   # first line is title line ,writedown
    writeLines(gsub(";"," ",line),dest_con)
    
    line=readLines(sour_con,n=1)
    while( length(line) != 0 ) {
        # find 2007-02-01 and 2007-02-02 data and writedown
        if( length(grep("^1/2/2007",line)) == 1 || length(grep("^2/2/2007",line)) == 1){
            # ignore missing values line,these line contains "?"
            if(length(grep("\\?",line)) != 1){
                writeLines(gsub(";"," ",line),dest_con)
            }
        }
        line=readLines(sour_con,n=1)
    }
    
    close(sour_con)
    close(dest_con)
}

# read 2-day dataset into R
write_two_days_file()
two_days_data <- read.table("towdays.txt",header = TRUE)

# sub metering data
sub_metering_1 <- two_days_data$Sub_metering_1
sub_metering_2 <- two_days_data$Sub_metering_2
sub_metering_3 <- two_days_data$Sub_metering_3

# plot 
png(file="plot3.png",width=480,height=480)
plot(sample_time,sub_metering_1,type="l",xlab = "",ylab="Energy sub metering",col = "black")
plot.xy(xy.coords(sample_time,sub_metering_2),type="l",col = "red")
plot.xy(xy.coords(sample_time,sub_metering_3),type="l",col = "blue")
legend("topright",legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),col = c("black","red","blue"),lwd = 2)
dev.off()



