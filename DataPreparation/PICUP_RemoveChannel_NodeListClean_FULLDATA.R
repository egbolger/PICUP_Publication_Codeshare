library(dplyr)
library(ggplot2)
library(igraph)
library(tidyverse)
library(ggraph)
library(readxl)
library(matrixStats)


# LOAD in PICUP Data
SLACK_PICUP_EDGE_LIST_FULL <- read_csv("slack_PICUP.csv")
picup <- SLACK_PICUP_EDGE_LIST_FULL[2:10]


colnames(picup)[3] <- "Userid_From"
colnames(picup)[4] <- "Name_From"
colnames(picup)[6] <- "Userid_To"
colnames(picup)[7] <- "At"
colnames(picup)[8] <- "Post_Category"
colnames(picup)[9] <- "Channel"

picup["Post_Category"] <- sapply(picup["Post_Category"], as.character)
picup["Post_Category"][is.na(picup["Post_Category"])] <- "0"

# picup %>% select(Post_Category) %>% print(n=10)


# LOAD in Node data
nodelist <- read_csv("users.csv")
nodelist_cleaned <- nodelist %>% select("id", "display_name", "real_name")
# rename userid here so it matches 
colnames(nodelist_cleaned)[1] <- "userid"
colnames(nodelist_cleaned)[3] <- "fullname"
# WE JUST USE THE SLACK GIVEN IDS FOR THE FULL DATA. THIS IS HOW THE FULL DATA REFERS TO OTHER USERS AND IS MORE ACCURATE THAN BY NAME 


# REMOVE bots from node list
`%!in%` <- Negate(`%in%`)
bots <- c("PaperBot", "Google Drive", "Outlook Calendar", "Dropbox", "Doodle Bot",  "GitHub", "Zoom", "paperbot", "googledrive", "outlook_calendar", "dropbox", "doodle_bot",  "github", "multi", "zoom")
nodelist_cleaned <- nodelist_cleaned %>% filter(fullname %!in% bots)

# REMOVE bots if they sent message (rare)
#picup %>% filter(Name_From %in% bots) %>% print(n = Inf)
picup <- picup %>% filter(Name_From %!in% bots)
picup <- picup %>% filter(At %!in% bots)

# ADD a new column in node list for each unique channel name - one for join, one for leave
unique_channels <- as.vector(unique(picup %>% select(Channel) %>% arrange(Channel)))
unique_channels <- unique_channels[!is.na(unique_channels)] 
unique_channels_left <- gsub(" ", "", paste(unique_channels, "_left"))  
for (col_ind in 1:length(unique_channels)){
    nodelist_cleaned[,unique_channels[col_ind]] <- NA
    nodelist_cleaned[,unique_channels_left[col_ind]] <- NA
}
nodelist_cleaned %>% print(n = 10)


# INPUT: userid as a string
# OUTPUT: user name as string
getFullName <- function(userid){
    # check to make sure the userid provided is in userid column 
    if (!is.na(match(userid, nodelist_cleaned[["userid"]]))){
        # get the index of the userid to get the full name of user
        user_index <- match(userid, nodelist_cleaned[["userid"]])
        username <- nodelist_cleaned[["fullname"]][user_index]
    } else{
        username <- NA
    }
    return (username)

}

### I ALREADY HAVE THIS INFORMATION 
# # ADD the userid to a to/from userid column, for each name in a to/from column, . This will ensure consistency as to what user we are referring to, even if they changed from the 'username' to 'fullname' throughout the time of the channel. 
# Userid_From <- NA
# Userid_To <- NA
To_Channel <- FALSE
# picup <- add_column(picup, Userid_From, .after = 3)
# picup <- add_column(picup, Userid_To, .after = 6)

# # ADD column for 'to channel' flag - for later
picup <- add_column(picup, To_Channel, .after = 10)

# for (nn in 1:nrow(picup)){
#     # get name of user that sent the message
#     user_name_from <- picup[["Name_From"]][nn]
#     # get the userid of the user and put it in the respective column
#     picup[["Userid_From"]][nn] <- getUserID(user_name_from)

#     # repeat for the to column
#     user_name_to <- picup[["At"]][nn]
#     picup[["Userid_To"]][nn] <- getUserID(user_name_to)  
#     #print(user_name_from)
#     #print(getUserID(user_name_from))   
# }




# GOAL: For each channel name, if the post category is 'joined' (similiarly for 'left'), we get the user, date, and time and put that information in the respective entry of the nodelist_cleaned df. 
#(ie each entry will be the join/ leave date for the given user)

for (ii in 1:nrow(picup)){
    # whenever the post category is join
    if (!is.na(picup[["Post_Category"]][ii])){
        if (picup[["Post_Category"]][ii] == "joined"){
        # pull the user who sent the message, the channel, the time and date 
            user_joined_id = picup[["Userid_From"]][ii]
            user_join_channel = picup[["Channel"]][ii]
            user_join_date = picup[["Date"]][ii]
            user_join_time = picup[["Time"]][ii]

            # convert time and date of join to datetime object for easy comparison later
            user_join_date_time <- as.POSIXct(strptime(paste(user_join_date,user_join_time), "%Y-%m-%d %H:%M:%S", tz= "GMT"))   

            # match the user_joined_id with the ids in the 'userids' column to get the index of where to paste date/time of joined 
            if (!is.na(match(user_joined_id, nodelist_cleaned[["userid"]]))){
                join_index <- match(user_joined_id, nodelist_cleaned[["userid"]])
                nodelist_cleaned[[user_join_channel]][join_index] <- user_join_date_time
            } else{
                # This should never get hit, because all users should have an id
                nodelist_cleaned[[user_join_channel]][join_index] <- NA
            }  
        }
    # if the user has left
        else if (picup[["Post_Category"]][ii] == "left"){
            # pull the user who sent the message, the channel, the time and date 
            user_left_id = picup[["Userid_From"]][ii]
            user_left_channel = picup[["Channel"]][ii]
            user_left_date = picup[["Date"]][ii]
            user_left_time = picup[["Time"]][ii]

            user_left_channel_name = gsub(" ", "", paste(user_left_channel, "_left"))
            # convert time and date of join to datetime object for easy comparison later
            user_left_date_time <- as.POSIXct(strptime(paste(user_left_date,user_left_time), "%Y-%m-%d %H:%M:%S", tz= "GMT"))   

            # match the user_joined_id with the ids in the 'userids' column to get the index of where to paste date/time of joined 
            if (!is.na(match(user_left_id, nodelist_cleaned[["userid"]]))){   
                join_index <- match(user_left_id, nodelist_cleaned[["userid"]])
                nodelist_cleaned[[user_left_channel_name]][join_index] <- user_join_date_time
            } else{
                # This should never get hit, because all users should have an id
                nodelist_cleaned[[user_left_channel_name]][join_index] <- NA
            }
        }
    }
}

# if join_date for random channel is NA, make it the same as join for General (since default in Slack - note this is a discrepancy with later times)
    # if random is NA and so in general, random stays NA
for (ran_join_index in 1:length(nodelist_cleaned[["random"]])){
    if (is.na(nodelist_cleaned[["random"]][ran_join_index])){
        nodelist_cleaned[["random"]][ran_join_index] <- nodelist_cleaned[["general"]][ran_join_index]
    }
}

# repeat, if general NA then fill with random time stamp
    # if NA should still be NA
for (ran_join_index in 1:length(nodelist_cleaned[["general"]])){
    if (is.na(nodelist_cleaned[["general"]][ran_join_index])){
        nodelist_cleaned[["general"]][ran_join_index] <- nodelist_cleaned[["random"]][ran_join_index]
    }
}





# GOAL : Make all messages that are to channel are actually to each user in the channel. 

# Find all messages that are 'to @channel', find the current users in the channel, create an edge between the user who sent message and all current users in channel 
    # current users are ones that have joined before and left after the message was sent
    # if the user does not have a 'join date', make the first message they are a part of (sent or received) their join date
        # keep track of 'forgotten users'

# we use a subset to simplify things, could easily run for all channels
picup_subset <- picup %>% filter( Channel == "random")
picup_wchannel_edges <- picup_subset

# FOR THE FULL DATASET, THIS ONLY APPLIES TO VERY FEW CASES

# LOOP through all the messages, if the user does not have a join date time, set it to be the joindate time of the FIRST MESSAGE THEY ARE A PART OF EITHER SENT OR RECEIVED message in that channel (from the function above)
forgotten_users <- c()
for (jj in 1:nrow(picup_subset)){
    # get the message user sent
    message_sent_user = picup_subset[["Userid_From"]][jj]
    message_to_user = picup_subset[["Userid_To"]][jj]
    message_sent_channel = picup_subset[["Channel"]][jj]

    message_sent_date = picup_subset[["Date"]][jj]
    message_sent_time = picup_subset[["Time"]][jj]
    message_sent_date_time <- as.POSIXct(strptime(paste(message_sent_date,message_sent_time), "%Y-%m-%d %H:%M:%S", tz= "GMT")) 

    # gets the indices of users who joined channel
    users_in_wholechannel_indices <- which(!is.na(nodelist_cleaned[[message_sent_channel]]))

    # if the user who sent the message does not have a 'joined' datetime, set it to be the datetime of the current message
    if (is.na(match(message_sent_user, nodelist_cleaned[["userid"]])) == FALSE && match(message_sent_user, nodelist_cleaned[["userid"]]) %in% users_in_wholechannel_indices == FALSE){
        # add the user to forgotten user's list
        forgotten_user_id <- match(message_sent_user, nodelist_cleaned[["userid"]])
        forgotten_users <- c( forgotten_users, forgotten_user_id )

        # set the joined time stamp of forgotten user as the first message timestamp
        nodelist_cleaned[[message_sent_channel]][forgotten_user_id] <- message_sent_date_time
    }
    # same for user who received message
    if (is.na(match(message_to_user, nodelist_cleaned[["userid"]])) == FALSE && match(message_to_user, nodelist_cleaned[["userid"]]) %in% users_in_wholechannel_indices == FALSE){
        # add the user to forgotten user's list
        forgotten_user_id <- match(message_to_user, nodelist_cleaned[["userid"]])
        forgotten_users <- c( forgotten_users, forgotten_user_id )

        # set the joined time stamp of forgotten user as the first message timestamp
        nodelist_cleaned[[message_sent_channel]][forgotten_user_id] <- message_sent_date_time
    }
}


# LOOP through the '@channel' messages and build an edge for all the current users in the channel based on their join time as defined in the previous loop
for ( mm in 1:nrow(picup_subset) ){
    # find all the messages that are  '@channel'
    if ( picup_subset[["At"]][mm] == "channel" ){
        # get all the message information 
        message_sent_user = picup_subset[["Userid_From"]][mm]
        message_sent_text = picup_subset[["Message"]][mm]
        message_sent_channel = picup_subset[["Channel"]][mm]
        message_sent_postcat = picup_subset[["Post_Category"]][mm]
        message_sent_date = picup_subset[["Date"]][mm]
        message_sent_time = picup_subset[["Time"]][mm]

        message_sent_date_time <- as.POSIXct(paste(message_sent_date,message_sent_time), "%Y-%m-%d %H:%M:%S", tz= "GMT")
        # message_sent_date_time <- as.POSIXct(strptime(message_sent_date, "%Y-%m-%d", tz= "GMT"))  

        # get the all the users in the channel and time stamps
        users_in_wholechannel_indices <- which(!is.na(nodelist_cleaned[[message_sent_channel]]))
        # print("Users in channel")
        # print(users_in_wholechannel_indices)
        users_time_join_channel <- nodelist_cleaned[[message_sent_channel]][users_in_wholechannel_indices]

        # get all the users who have left the channel and time stamps
        message_left_channel <- gsub(" ", "", paste(message_sent_channel, "_left")) 

        users_out_wholechannel_indices <- which(!is.na(nodelist_cleaned[[message_left_channel]]))
        users_time_left_channel <- nodelist_cleaned[[message_left_channel]][users_out_wholechannel_indices]

        # get indices of current users - ie joined before message was sent
        users_current_indices <- users_in_wholechannel_indices[users_time_join_channel <= message_sent_date_time]

        # deal with 'left' users
            # the 'joined' loop above accounts for users that have 'left' the chat without sending a message bc they have sent the 'left' message
            # here, we need to remove all the users that left before the message was sent
        # if all users have left time greater than message send time do nothing
            # NA in time left defaults to TRUE
        left_channel_indicator <- message_sent_date_time <= users_time_left_channel
        
        # if all users have left time greater than message time, keep them all 
        if ( all(left_channel_indicator)){
            user_currents_indices <- users_current_indices
        } 
        else{
            # if not, only want to keep values where message time is less than left time
            keep_values <- which(left_channel_indicator == TRUE)
            user_currents_indices <- users_current_indices[keep_values]
        }
        # get userids of current users from their indices
        users_currents_ids <- nodelist_cleaned[['userid']][users_current_indices]

        # add a new edge from the user who sent the message to ever user currently in the channel
        # if we have new edges to add - ie there are people in the channel
        # print("User ids")
        # print(users_currents_ids)
        # print(length(users_currents_ids))
        # print(message_sent_text)

        if(is_empty(users_currents_ids) == FALSE){
            # if there is only one user in the channel, send the message to itself
            if (length(users_currents_ids) == 1 && users_currents_ids[1]==message_sent_user){   
                # get the fullname_from
                username_toadd <- getFullName(message_sent_user)

                picup_wchannel_edges <- picup_wchannel_edges %>% add_row(Date = message_sent_date, Time = message_sent_time, Name_From = username_toadd, Userid_From = message_sent_user, Message = message_sent_text, At = username_toadd, Userid_To = message_sent_user, Post_Category = message_sent_postcat, Channel = message_sent_channel, To_Channel = TRUE)
            }
            else{
                # otherwise, send the message to everyone in the channel excluding self
                for (each in users_currents_ids){
                    if (each != message_sent_user){
                        # get the fullname_from
                        username_from_toadd <- getFullName(message_sent_user)
                        # get the fullname_to 
                        username_to_toadd <- getFullName(each)
                        # add a new edge
                        picup_wchannel_edges <- picup_wchannel_edges %>% add_row(Date = message_sent_date, Time = message_sent_time, Name_From = username_from_toadd, Userid_From = message_sent_user, Message = message_sent_text, At = username_to_toadd, Userid_To = each, Post_Category = message_sent_postcat, Channel = message_sent_channel, To_Channel = TRUE)
                    } 
                }
            }   
        }

    }
} 
# picup_wchannel_edges %>% print(n = Inf)

## It's okay that the DISPLAY NAME DOESNT ALWAYS MATCH, WE USE THE ID TO MAKE THE NETWORKS ANYWAY

# REMOVE all the '@channel' messages now that we have sent the message to all the users in the channel at the current time
picup_nochannels <- picup_wchannel_edges %>% filter(At != 'channel')
picup_nochannels %>% select(Post_Category) %>%print(n = Inf)

# EXPORT as new df to import into a new r script to make networks from
write_csv(picup_nochannels, "FULLpicup_nochannels_random.csv")  







