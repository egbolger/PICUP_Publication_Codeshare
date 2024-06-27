#!/usr/bin/env Rscript --vanilla

# This file contains the code that has been created for the PICUP Slack Network Analysis Project, specifically for building, viewing, and analyzing networks. 

# LOADING relevant libraries
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(grid))
suppressPackageStartupMessages(library(gtable))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(igraph))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(ggraph))
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(formatR))
suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(DirectedClustering)) 
suppressPackageStartupMessages(library(tnet))
# suppressPackageStartupMessages(library(argparse))
suppressPackageStartupMessages(library(methods))
suppressPackageStartupMessages(library(argparser))


### FUNCTIONS

## LOADING in the data and CREATING networks

## fulldata_To_Network
# The function `fulldata_To_Network` takes in the full dataset, selects the From and To columns from the dataset and returns a network object. This function does not add any edge attributes.  

# INPUTS: Full Dataset - filename, OUTPUT: Network object

fulldata_To_Network <- function(file_name){
    # loading Data
    picup_nochannels <- read_csv(file_name)

    # pull just the to and from columns 
    picup_nochannels_tofrom <- picup_nochannels %>% select(Userid_From, Userid_To)
    # build network
    picup_network_channel <- graph_from_data_frame(picup_nochannels_tofrom, directed = TRUE)
    return(picup_network_channel)
}


## fulldata_To_NetworkNoJL
# The function `fulldata_To_Network_NoJL` takes the full dataset and creates a network without the join and leave categories. This function does not add any edge attributes. 

# INPUTS: Full Dataset, OUTPUTS: Network Object without join/leave post categories

fulldata_To_NetworkNoJL <- function(file_name){
    # loading Data
    picup_nochannels <- read_csv(file_name)
    # remove join/leave post categories
    picup_nochannels_nojoin <- picup_nochannels %>% filter(Post_Category != 'joined' & Post_Category != 'left')
    # pull just the to and from columns 
    picup_nochannels_tofrom_nojoin <- picup_nochannels_nojoin %>% select(Userid_From, Userid_To)
    # build network
    picup_network_channel_nojoin <- graph_from_data_frame(picup_nochannels_tofrom_nojoin, directed = TRUE)
    return(picup_network_channel_nojoin)
}

## fulldata_To_JustPostCat
# The function `fulldata_To_JustPostCat` pulls the post category column from the dataset. The boolean join_leave determines whether the join leave information is included or not.

# INPUTS: filename, boolean join_leave, OUTPUT: Post Category column
fulldata_To_JustPostCat <- function(file_name, join_leave){
    # loading data
    picup_nochannels <- read_csv(file_name)
    # keep join/leave
    if (join_leave == TRUE){
        # pull post categories column for later use
        picup_nochannels_postcat <- picup_nochannels %>% select(Post_Category)
    }
    else if (join_leave == FALSE){
            # remove join/leave
        picup_nochannels_nojoin <- picup_nochannels %>% filter(Post_Category != 'joined' & Post_Category != 'left')
        # pull post categories column for later use
        picup_nochannels_postcat <- picup_nochannels_nojoin %>% select(Post_Category)

    }
    return(picup_nochannels_postcat)
}

## fulldata_To_noPostCatCount
# The function `fulldata_To_noPostCatCount` takes in the file name and a boolean and returns a network with all the edges and sets the post category as an edge attribute. The `join_leave` boolean specificies whether the join/leave edges should be kept (TRUE) or removed (FALSE). We also add a flag indicating whether the message was a part of a 'to channel' message or not. 

# INPUTS: file name, join_leave boolean, OUTPUT: network with post category as edge attribute

fulldata_To_noPostCatCount <- function(file_name, join_leave){
    # read in data
    picup_nochannels <- read_csv(file_name)
    if (join_leave == TRUE){
        # keep join/leaves
        picup_nochannels_tofrom <- picup_nochannels %>% select(Userid_From, Userid_To, Post_Category, To_Channel)
    }
    else if (join_leave == FALSE){
        # remove join/leaves
        picup_nochannels_tofrom <- picup_nochannels %>% filter(Post_Category != 'joined' & Post_Category != 'left')
        picup_nochannels_tofrom <- picup_nochannels_tofrom %>% select(Userid_From, Userid_To, Post_Category, To_Channel)
    }
    # get to/from and make graph 
    picup <- picup_nochannels_tofrom %>% select(Userid_From, Userid_To)
    picup_network_channel <- graph_from_data_frame(picup, directed = TRUE)

    # get post cat and add as edge attribute
    E(picup_network_channel)$post_cat <- picup_nochannels_tofrom$Post_Category

    # add to_channel flag as edge attribute
    E(picup_network_channel)$to_channel <- picup_nochannels_tofrom$To_Channel

    return (picup_network_channel)
}

## fulldata_To_GraphwPostCatCount
# The `fulldata_To_GraphwPostCatCount` function creates a network that contains the post category as an edge attribute as well as how many times that post category has occurred between two users. For example, if there are two 'reply' messages from user1 to user2, the edge is only accounted for once with a 'post category' of 'reply' and a 'count' value of 2.  The reciprocated ties are not combined. Also, a join_leave boolean is required to specify whether (TRUE) or not (FALSE) to keep the join/leave messages.  There is a to_channel boolean that specifies whether there should be an edge attribute called to_channel, if the message was a part of a channel message. As TRUE, this may result in a few more edges (user1 sent reply to whole channel, user1 send reply to user 2, would now be counted as separate).

# INPUTS: file name, join_leave boolean, boolean to_channel (default = FALSE), OUTPUT: Network with postcat and count edge attributes

fulldata_To_GraphwPostCatCount <- function(file_name, join_leave, to_channel = FALSE){
    if(to_channel == TRUE){# read in data
        picup_nochannels <- read_csv(file_name)
        if (join_leave == TRUE){
            # add post cat count
            picup_nochannels_postcatadd <- picup_nochannels %>% select(Userid_From, Userid_To, Post_Category, To_Channel) %>% group_by(Userid_From, Userid_To, Post_Category, To_Channel) %>% summarize(Count = n())
        }
        else if(join_leave == FALSE){
            # remove join/leave
            picup_nochannels <- picup_nochannels  %>% filter(Post_Category != 'joined' & Post_Category != 'left')
            # add post cat count
            picup_nochannels_postcatadd <- picup_nochannels %>% select(Userid_From, Userid_To, Post_Category, To_Channel) %>% group_by(Userid_From, Userid_To, Post_Category, To_Channel) %>% summarize(Count = n())
        }
        # get to/from and make graph 
        picup <- picup_nochannels_postcatadd %>% select(Userid_From, Userid_To, Post_Category)
        picup_network_channel <- graph_from_data_frame(picup, directed = TRUE)
        # get post cat and add as edge attribute
        E(picup_network_channel)$post_cat <- picup_nochannels_postcatadd$Post_Category
        # get count and add as edge attribute
        E(picup_network_channel)$count <- picup_nochannels_postcatadd$Count
        # add to_channel flag
        E(picup_network_channel)$to_channel <- picup_nochannels_postcatadd$To_Channel
    }

    if(to_channel == FALSE){
        # read in data
        picup_nochannels <- read_csv(file_name)
        if (join_leave == TRUE){
            # add post cat count
            picup_nochannels_postcatadd <- picup_nochannels %>% select(Userid_From, Userid_To, Post_Category) %>% group_by(Userid_From, Userid_To, Post_Category) %>% summarize(Count = n())
        }
        else if(join_leave == FALSE){
            # remove join/leave
            picup_nochannels <- picup_nochannels  %>% filter(Post_Category != 'joined' & Post_Category != 'left')
            # add post cat count
            picup_nochannels_postcatadd <- picup_nochannels %>% select(Userid_From, Userid_To, Post_Category) %>% group_by(Userid_From, Userid_To, Post_Category) %>% summarize(Count = n())
        }
        # get to/from and make graph 
        picup <- picup_nochannels_postcatadd %>% select(Userid_From, Userid_To)
        picup_network_channel <- graph_from_data_frame(picup, directed = TRUE)
        # get post cat and add as edge attribute
        E(picup_network_channel)$post_cat <- picup_nochannels_postcatadd$Post_Category
        # get count and add as edge attribute
        E(picup_network_channel)$count <- picup_nochannels_postcatadd$Count
    }
    return(picup_network_channel)

}

## fulldata_To_GraphwCount
# The `fulldata_To_GraphwCount` function creates a network with the number of interactions between the two users as an edge attribute. This does not combine reciprocated ties. Also, a join_leave boolean is required to specify whether (TRUE) or not (FALSE) to keep the join/leave messages. There is a to_channel boolean that specifies whether there should be an edge attribute called to_channel, if the message was a part of a channel message. As TRUE, this may result in a few more edges (user1 sent reply to whole channel, user1 send reply to user 2, would now be counted as separate).

# INPUTS: file name, join_leave boolean, boolean to_channel, OUTPUT: Network with count as an edge attributes

fulldata_To_GraphwCount <- function(file_name, join_leave, to_channel = FALSE){
    if(to_channel == TRUE){
        # read in data
        picup_nochannels <- read_csv(file_name)
        if (join_leave == TRUE){
            # add count
            picup_nochannels_count <- picup_nochannels %>% select(Userid_From, Userid_To, To_Channel) %>% group_by(Userid_From, Userid_To, To_Channel) %>% summarize(Count = n())
        }
        else if(join_leave == FALSE){
            # remove join/leave
            picup_nochannels <- picup_nochannels  %>% filter(Post_Category != 'joined' & Post_Category != 'left')

            # add count
            picup_nochannels_count <- picup_nochannels %>% select(Userid_From, Userid_To, To_Channel) %>% group_by(Userid_From, Userid_To, To_Channel) %>% summarize(Count = n())
        }
        # get to/from and make graph 
        picup <- picup_nochannels_count %>% select(Userid_From, Userid_To)
        picup_network_channel <- graph_from_data_frame(picup, directed = TRUE)
        # get count and add as edge attribute
        E(picup_network_channel)$count <- picup_nochannels_count$Count
        # to channel 
        E(picup_network_channel)$to_channel <- picup_nochannels_count$To_Channel

    }
    if(to_channel == FALSE){
        # read in data
        picup_nochannels <- read_csv(file_name)
        if (join_leave == TRUE){
            # add count
            picup_nochannels_count <- picup_nochannels %>% select(Userid_From, Userid_To) %>% group_by(Userid_From, Userid_To) %>% summarize(Count = n())
        }
        else if(join_leave == FALSE){
            # remove join/leave
            picup_nochannels <- picup_nochannels  %>% filter(Post_Category != 'joined' & Post_Category != 'left')
            # add count
            picup_nochannels_count <- picup_nochannels %>% select(Userid_From, Userid_To) %>% group_by(Userid_From, Userid_To) %>% summarize(Count = n())
        }
        # get to/from and make graph 
        picup <- picup_nochannels_count %>% select(Userid_From, Userid_To)
        picup_network_channel <- graph_from_data_frame(picup, directed = TRUE)
        # get count and add as edge attribute
        E(picup_network_channel)$count <- picup_nochannels_count$Count
    }
    return(picup_network_channel)
}

## get_Name
# The function `get_Name` returns the name of the user as a string given the userid. 

# INPUT: dataframe nodelist, string given_userid, OUTPUT: string name
get_Name <- function(picup_nodelist_dataframe, given_userid){
    # load node list
    userlist <- read_csv(picup_nodelist_dataframe, col_types = cols())
    # get the first index of where userid is located
    userindex <- which(userlist$userid == given_userid)
    # get name of user 
    username <- userlist$fullname[userindex]
    # print(username)
    return(username)
}

## get_Name_Dataframe
# The function `get_Name_Dataframe` returns a dataframe containing the userid and the username of the users in the network. 

# INPUT: dataframe nodelist, network, OUTPUT: dataframe of userid, username 

get_Name_Dataframe <- function(picup_nodelist_dataframe, picup_network_channel){
    username_list <- data.frame(matrix(NA, nrow = length(V(picup_network_channel)$name), ncol = 2))
    names(username_list) <- c("Userid","Username")
    # for each user in the network 
    for (i in 1:length(V(picup_network_channel)$name)){
        username_list[i, "Userid"] <- V(picup_network_channel)$name[i]
        username_list[i, "Username"] <- get_Name(picup_nodelist_dataframe, V(picup_network_channel)$name[i])
    }
    return(username_list)
}

## SETTING AND GETTING ATTRIBUTES

## set_Node_Attributes
# The function `set_Node_Attributes` adds the network's degree, out degree, and in degree to the vertex attribute of the network. If the network contains all the edges (frequency == FALSE), we calculate degree, in-degree, and out-degree as expected. If the network incorporates the frequency of the edges (frequency == TRUE), either by post-category or overall, we instead calculate the strength of the node, which sums the values of the edges. These two values are equivalent and for brevity are both referred to as 'degree' in the node attribute. Lastly, we add the username of the user as a vertex attribute. 

# INPUTS: Network, nodelist dataframe, boolean frequency, OUTPUT: Network with added attributes

set_Node_Attributes <- function (picup_network_channel, nodelist, frequency){ 
    if (frequency == TRUE){
        # Degree - number of connections
        V(picup_network_channel)$degree <- strength(picup_network_channel, weights = E(picup_network_channel)$count, mode = 'all') 
        # Out Degree - number of edges away from the node
        V(picup_network_channel)$out_degree <- strength(picup_network_channel, weights = E(picup_network_channel)$count, mode = 'out') 
        # In Degree - number of edges towards node
        V(picup_network_channel)$in_degree <- strength(picup_network_channel, weights = E(picup_network_channel)$count, mode = 'in') 
    } else if (frequency == FALSE) { 
        # Degree - number of connections
        V(picup_network_channel)$degree <- degree(picup_network_channel) 
        # Out Degree - number of edges away from the node
        V(picup_network_channel)$out_degree <- degree(picup_network_channel, mode = 'out')
        # In Degree - number of edges towards node
        V(picup_network_channel)$in_degree <- degree(picup_network_channel, mode = 'in')
    }
    # add username as node attr 
    # for (id_index in 1: length(V(picup_network_channel)$name)){
    #     V(picup_network_channel)$username[id_index] <- get_Name(nodelist, V(picup_network_channel)$name[id_index] ) 
    # }
    return(picup_network_channel)
}

##  get_PostCat_count - Calculate Number of Post Categories
# The function `get_PostCat_count` calculates the number of each post categories used in the network. The frequency boolean specifies whether we want the frequency of the edges (TRUE) or all the edges (FALSE). The to_channel boolean specifies whether or not we want the 'to channel' messages to have dashed edges. 

# INPUTS: Network, OUTPUT: List of counts for each post category

# *MUST have added the edge attributes first as it uses the post category edge attribute. Used for post cato*

get_PostCat_count <- function(picup_network_channel, frequency = FALSE, to_channel = FALSE){
    # get the possible post categories
    possible_postcats <- sort(unique(E(picup_network_channel)$post_cat))
    unique_post_counts <- c()
    
    # calculate how many of each post category
        # no frequency
    if (frequency == FALSE && to_channel == FALSE | frequency == FALSE && to_channel == TRUE){
        for (each_cat in possible_postcats) {
            post_counts <- length(E(picup_network_channel)$post_cat[E(picup_network_channel)$post_cat == each_cat])
            unique_post_counts <- c(unique_post_counts, post_counts)
        } 
        #frequency - need sum of count
    } else if (frequency == TRUE && to_channel == FALSE | frequency == TRUE && to_channel == TRUE ){
        for (each_cat in possible_postcats) {
            post_counts <- sum(E(picup_network_channel)$count[E(picup_network_channel)$post_cat == each_cat])
            unique_post_counts <- c(unique_post_counts, post_counts)
        } 
    }  
    return (unique_post_counts)
}


## get_PostCat_Dataframe
#  The function `get_PostCat_Dataframe` returns a dataframe of the total number of each post category a user is involved in (regardless of send/receive). 

#  INPUT: network with post cateogry frequencies,OUTPUT: Dataframe containing the count for each type of post category for each user

get_PostCat_Dataframe <- function(picup_network_channel_pcfreq) {
    total_postcat_dataframe <- data.frame(matrix(NA, nrow = length(V(picup_network_channel_pcfreq)$name), ncol = length(unique(sort(E(picup_network_channel_pcfreq)$post_cat)))+2 ))

    colnames(total_postcat_dataframe) <- c("Userid", "Username", unique(sort(E(picup_network_channel_pcfreq)$post_cat)))

    # for each user in the channel
    for (u in 1: length(V(picup_network_channel_pcfreq)$name)){
        # place userid
        total_postcat_dataframe[u, "Userid"] <- V(picup_network_channel_pcfreq)$name[u]
        total_postcat_dataframe[u, "Username"] <- V(picup_network_channel_pcfreq)$username[u]

        # get all incident edges 
        incident_edges <- get_Incident_edges(picup_network_channel_pcfreq,V(picup_network_channel_pcfreq)$name[u] )[[1]]  

        # find unique post cats for that user
        unique_postcat <- unique(E(incident_edges)$post_cat )
        # count them   
        for ( i in 1: length(unique_postcat)){
            # find the indices that use that post cateogry 
            postcat_indices <- which(E(incident_edges)$post_cat == unique_postcat[i]) 
            # get the counts for that post category
            postcat_count <- E(incident_edges)$count[postcat_indices]
            # sum the values 
            postcat_sum <- sum(postcat_count)
            # add to list
        total_postcat_dataframe[u, unique_postcat[i]] <- postcat_sum

        }
    }
    return(total_postcat_dataframe)
}

## get_NonMutualIncident_Names
# The function `get_NonMutualIncident_Names` takes in an incident frequency network and returns the names of the users that do not have a mutual connection with the user who's incident graph it is as well as the count in a dataframe. 

# INPUT: incident network with frequency, file with userid and username, OUTPUT: Dataframe containing the names with non mutual edges to the incident user and how many connections

get_NonMutualIncident_Names <- function(picup_network_incident_freq, nodelist_cleaned){
    # get users that do not have mutual connection with most connected user
    incident_mutualtiesbool_freq <- boolean_Mutual_Ties(picup_network_incident_freq[[1]])
    non_mutualties_freq <- subgraph.edges(picup_network_incident_freq[[1]], E(picup_network_incident_freq[[1]])[incident_mutualtiesbool_freq == FALSE] )

    head_df_freq <- data.frame(matrix(NA, nrow = length(E(non_mutualties_freq)), ncol = 3 ))
    colnames(head_df_freq) <- c("Userid", "Username", "Num_Ties")
    
    # get the names 
    for (i in 1: length(E(non_mutualties_freq))) {
        # add name of head and tail to list
        head_df_freq[i, "Userid"] <- head_of(non_mutualties_freq, E(non_mutualties_freq)[i])$name
        head_df_freq[i, "Username"] <- get_Name(nodelist_cleaned, head_of(non_mutualties_freq, E(non_mutualties_freq)[i])$name)
        head_df_freq[i, "Num_Ties"] <- E(non_mutualties_freq)$count[i]

    }
    return (head_df_freq)
}

### PLOTTING

## split_Edges
# The function `split_Edges` returns a subgraph of either just channel messages (channel = TRUE) or just direct messages (channel = FALSE). 

# INPUTS: network object, boolean channel, OUTPUT: subgraph based on the input
split_Edges <- function (picup_network_channel, channel){
    if (channel == TRUE){
        split_Edges <- subgraph.edges(picup_network_channel, E(picup_network_channel)[E(picup_network_channel)$to_channel == TRUE])
    
    } else if (channel == FALSE){
        split_Edges <- subgraph.edges(picup_network_channel, E(picup_network_channel)[E(picup_network_channel)$to_channel == FALSE])
    }
    return(split_Edges)
}

## PLOT THE NETWORK
## plot_network 
# The function `plot_network` plots the given network. The size of the node is given by the degree. The frequency boolean specifies whether we want the frequency of the edges (TRUE) or all the edges (FALSE). The to_channel boolean specifies whether or not we want the 'to channel' messages to have dashed edges. 

# INPUTS: Network object, boolean frequency (default = FALSE), boolean to_channel (default = FALSE), string channel_name, OUTPUT: Graph of network


plot_network <- function(picup_network_channel, frequency = FALSE, to_channel = FALSE, directed = TRUE, channel_name, savedirectory){
    # Basic Plot
    basic_network <- ggraph(picup_network_channel, layout = 'kk') +
        geom_node_point(aes(size = degree), color = "royalblue3") +
        labs(size = "Degree Size") +
        scale_size_continuous(range = c(1, 15), breaks = c(min(V(picup_network_channel)$degree), median(V(picup_network_channel)$degree), max(V(picup_network_channel)$degree))) +
        theme_void() +
        theme(plot.title = element_text( hjust = 0.5, size = 15, face = "bold"), legend.key.height = unit(3,"mm"), legend.title = element_text( size = 10), legend.key.size = unit(.45, "cm"), legend.key.width = unit(1.75,"cm") ) 
    if( frequency == FALSE && to_channel == FALSE && directed == TRUE){
        # no frequency, no to_channel
        plot_network <- basic_network + geom_edge_fan(arrow = arrow(length = unit(0.5, "lines"), type = "closed"), start_cap = circle(7.5, "mm"), end_cap = circle(7.5, "mm")) +
        geom_edge_loop(arrow = arrow(length = unit(0.5, "lines"), type = "closed"), start_cap = circle(7.5, "mm"), end_cap = circle(7.5, "mm"), strength = 2) +
        geom_node_point(aes(size = degree), color = "royalblue3") +
        ggtitle(paste("Network with All Edges:\n", channel_name)) 

    } else if (frequency == TRUE && to_channel == FALSE && directed == TRUE) {
        plot_network <- basic_network +
        geom_edge_fan(aes(width = E(picup_network_channel)$count), arrow = arrow(length = unit(0.5, "lines"), type = "closed"), 
        start_cap = circle(8, "mm"),
        end_cap = circle(8, "mm"), strength = 2) +
        geom_edge_loop(arrow = arrow(length = unit(0.5, "lines"), type = "closed"), start_cap = circle(7.5, "mm"), end_cap = circle(7.5, "mm")) +
        scale_edge_width(name = "Edge Width", range = c(0.1,3.5)) +
        geom_node_point(aes(size = degree), color = "royalblue3") +
        ggtitle(paste("Network with Frequency of Edges:\n", channel_name)) 
       
    } else if (frequency == FALSE && to_channel == TRUE && directed == TRUE) {
        plot_network <- basic_network + geom_edge_fan(aes(linetype = E(picup_network_channel)$to_channel), arrow = arrow(length = unit(0.5, "lines"), type = "closed"), 
        start_cap = circle(8, "mm"),
        end_cap = circle(8, "mm"), strength = 2) +
        geom_edge_loop(arrow = arrow(length = unit(0.5, "lines"), type = "closed"), start_cap = circle(7.5, "mm"), end_cap = circle(7.5, "mm")) +
        scale_edge_linetype_manual(name = "To Channel", values = c("FALSE" = "solid", "TRUE" = "dashed"), labels = c("Direct", "Channel")) +
        geom_node_point(aes(size = degree), color = "royalblue3") +
        ggtitle(paste("Network with Channel Messages Noted: \n", channel_name)) 
        
    } else if(frequency == TRUE && to_channel == TRUE && directed == TRUE){
        plot_network <- basic_network + geom_edge_fan(aes(width = E (picup_network_channel)$count, linetype = E(picup_network_channel)$to_channel), arrow = arrow(length = unit(0.5, "lines"), type = "closed"), 
        start_cap = circle(8, "mm"),
        end_cap = circle(8, "mm"), strength = 2) +
        geom_edge_loop(arrow = arrow(length = unit(0.5, "lines"), type = "closed"), start_cap = circle(7.5, "mm"), end_cap = circle(7.5, "mm")) +
        scale_edge_width(name = "Edge Width", range = c(0.1,3.5)) +
        scale_edge_linetype_manual(name = "To Channel", values = c("FALSE" = "solid", "TRUE" = "dashed"), labels = c("Direct", "Channel")) +
        geom_node_point(aes(size = degree), color = "royalblue3") +
        ggtitle(paste("Network with Frequency of Edges \n and Channel Messages Noted:", channel_name)) 

    } else if(frequency == FALSE && to_channel == FALSE && directed == FALSE){
        print("here2")
         plot_network <- basic_network + geom_edge_fan(start_cap = circle(7.5, "mm"), end_cap = circle(7.5, "mm")) +
        geom_edge_loop(arrow = arrow(length = unit(0.5, "lines"), type = "closed"), start_cap = circle(7.5, "mm"), end_cap = circle(7.5, "mm"), strength = 2) +
        geom_node_point(aes(size = degree), color = "royalblue3") +
        ggtitle(paste("Network with All Edges:\n", channel_name)) 
    }
    
    ggsave(paste(savedirectory, "PlotNetwork.png", sep=""),plot_network)
    return(plot_network)
}

## PLOTTING WITH POST CATEGORIES
## plot_channel_bypostcat
# The function `plot_channel_bypostcat` creates a grid of plots, where each section of the grid is the network for a specific post cateogry. The node size is uniform to make viewing the edges easier. The user must specify whether the data contains frequency information or not - including frequency produces a weighted graph. The user must also specify whether or not the to_channel edges should be denoted with a dashed line. For each of the categories, we include the count for how many times each occurred. 

# INPUTS: Network object, frequency (default = FALSE), to_channel (default = FALSE), string channel_name, OUTPUT: Grid of networks
plot_channel_bypostcat <- function(picup_network_channel, frequency = FALSE, to_channel = FALSE, channel_name, savedirectory){
    # get possible post categories
    possible_postcats <- sort(unique(E(picup_network_channel)$post_cat))
    # get the number of each type of post category
    post_counts <- get_PostCat_count(picup_network_channel, frequency, to_channel)
    # set edge attribute of post cat
    edge_attr_post_cat <- E(picup_network_channel)$post_cat
    # Capitalize Post Category and add count
    capitalize_postcount <- function(string) {
        substr(string, 1, 1) <- toupper(substr(string, 1, 1))
        string <- paste(string,":", post_counts)
        string
    }
    # basic graph 
    basic_graph <- ggraph(picup_network_channel, layout = 'kk') +
        scale_size_continuous(range = c(1, 5)) +
        theme_classic() +
        scale_x_discrete(labels = NULL, breaks = NULL) + labs(x = "") + 
        scale_y_discrete(labels = NULL, breaks = NULL) + labs(y = "") + 
        theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"), legend.title = element_text( size = 10), legend.key.size = unit(.45, "cm"), legend.key.width = unit(1.75,"cm") )
    if(frequency == FALSE && to_channel == FALSE){
        picup_network_channel_graph_postcat<- basic_graph + 
            geom_edge_fan(start_cap = circle(0.5, "mm"), end_cap = circle(0.5, "mm"), aes(color = edge_attr_post_cat), show.legend = FALSE) +
            facet_edges(~edge_attr_post_cat, ncol = 3, labeller = labeller( edge_attr_post_cat = capitalize_postcount)) +
            geom_node_point() +
            ggtitle(paste("Channel by Post Category: \n", channel_name))
    } else if (frequency == TRUE && to_channel == FALSE){
        picup_network_channel_graph_postcat<- basic_graph + 
            geom_edge_fan(start_cap = circle(0.5, "mm"), end_cap = circle(0.5, "mm"), aes(color = edge_attr_post_cat, width = E(picup_network_channel)$count), show.legend = FALSE) +
            facet_edges(~edge_attr_post_cat, ncol = 3, labeller = labeller( edge_attr_post_cat = capitalize_postcount)) +
            scale_edge_width(name = "Edge Width", range = c(0.2,0.9)) +
            geom_node_point() +
            ggtitle(paste("Channel by Post Category \n with Weighted Edges: \n", channel_name))
    } else if (frequency == FALSE && to_channel == TRUE){
        picup_network_channel_graph_postcat <- basic_graph + 
            geom_edge_fan(start_cap = circle(0.5, "mm"), end_cap = circle(0.5, "mm"), aes(color = edge_attr_post_cat, linetype = E(picup_network_channel)$to_channel), show.legend = FALSE) +
            facet_edges(~edge_attr_post_cat,  ncol = 3, labeller = labeller( edge_attr_post_cat = capitalize_postcount)) +
            scale_edge_linetype_manual(name = "To Channel", values = c("FALSE" = "solid", "TRUE" = "dashed"), labels = c("Direct", "To Channel")) +
            geom_node_point() +
            ggtitle(paste("Channel by Post Category \n with To Channel Denoted: \n", channel_name))
    } else if (frequency == TRUE && to_channel == TRUE) {
        picup_network_channel_graph_postcat <- basic_graph + 
            geom_edge_fan(start_cap = circle(0.5, "mm"), end_cap = circle(0.5, "mm"), aes(color = edge_attr_post_cat, width = E(picup_network_channel)$count, linetype = E(picup_network_channel)$to_channel), show.legend = FALSE) +
            facet_edges(~edge_attr_post_cat, ncol = 3, labeller = labeller( edge_attr_post_cat = capitalize_postcount)) +
            scale_edge_width(name = "Edge Width", range = c(0.2,0.9)) +
            scale_edge_linetype_manual(name = "To Channel", values = c("FALSE" = "solid", "TRUE" = "dashed"), labels = c("Direct", "To Channel")) +
            geom_node_point() +
            ggtitle(paste("Channel by Post Category \n with Weighted Edges & \n To Channel Denoted: ", channel_name))
    }
    ggsave(paste(savedirectory, "PostcatbyGrid.png", sep=""),picup_network_channel_graph_postcat)
    return(picup_network_channel_graph_postcat)
}

###  plot_postcat - Plotting the Network with the Ties as Post Category 
#The function `plot_postcat` plots the network with the edges colored corresponding to the post category of the interaction. The node size is the overall degree. The user must specify whether the data contains post cateogory count information or not - including frequency produces a weighted graph. The user must also specify whether or not the to_channel edges should be denoted with a dashed line. 

#INPUTS: Network, boolean post_cat_count (default = FALSE), boolean to_channel (default = FALSE), string channel_name, OUTPUT: Graph with post category type as edges

plot_postcat <- function(picup_network_channel, post_cat_count = FALSE, to_channel = FALSE, channel_name, savedirectory){
    # basic graph
    postcat_graph_basic <- ggraph(picup_network_channel, layout = 'kk') +
            labs(size = "Degree Size") +
            scale_size_continuous(range = c(1, 15),breaks = c(min(V(picup_network_channel)$degree), median(V(picup_network_channel)$degree), max(V(picup_network_channel)$degree))) +
            theme_void() +
            theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"), legend.title = element_text( size = 10), legend.key.size = unit(.45, "cm"), legend.key.width = unit(1.75,"cm") ) +
            guides(fill = guide_legend(override.aes = list(size=4, stroke=.5))) 
    
    if (post_cat_count == FALSE && to_channel == FALSE){
        # Neither freq or to channel
        plot_postcat_graph <- postcat_graph_basic +
            geom_edge_fan(arrow = arrow(length = unit(0.5, "lines"), type = "closed"), 
            start_cap = circle(8, "mm"),
            end_cap = circle(8, "mm"), aes(color = E(picup_network_channel)$post_cat), strength = 2) +
            scale_edge_color_discrete(name = "Post Category") +
            geom_node_point(aes(size = degree)) +
            ggtitle(paste("Network with Post Category as Edge \n Color:", channel_name))
    } else if (post_cat_count == TRUE && to_channel == FALSE){
        # freq, not to channel
        plot_postcat_graph <- postcat_graph_basic +
            geom_edge_fan(aes(width = E(picup_network_channel)$count, color = E(picup_network_channel)$post_cat), arrow = arrow(length = unit(0.5, "lines"), type = "closed"), start_cap = circle(8, "mm"), end_cap = circle(8, "mm"), strength = 2) +
            scale_edge_color_discrete(name = "Post Category") +
            scale_edge_width(name = "Edge Width", range = c(0.1, 3.5)) +
            geom_node_point(aes(size = degree)) +
            ggtitle(paste("Network with Post Category as Weighted Edge \nColor:", channel_name)) 
    } else if (post_cat_count == FALSE && to_channel == TRUE){
        # no freq  & to channel
        plot_postcat_graph <- postcat_graph_basic +
            geom_edge_fan(aes(color = E(picup_network_channel)$post_cat, linetype = E(picup_network_channel)$to_channel), arrow = arrow(length = unit(0.5, "lines"), type = "closed"), start_cap = circle(8, "mm"), end_cap = circle(8, "mm"), strength = 2) +
            scale_edge_color_discrete(name = "Post Category") +
            scale_edge_linetype_manual(name = "To Channel", values = c("FALSE" = "solid", "TRUE" = "dashed"), labels = c("Direct", "To Channel")) +
            geom_node_point(aes(size = degree)) +
            ggtitle(paste("Network with Post Category as Edge \nColor with Channel Noted:", channel_name)) 
    } else if (post_cat_count == TRUE && to_channel == TRUE) {
         plot_postcat_graph <- postcat_graph_basic +
            geom_edge_fan(aes(width = E(picup_network_channel)$count, color = E(picup_network_channel)$post_cat, linetype = E(picup_network_channel)$to_channel), arrow = arrow(length = unit(0.5, "lines"), type = "closed"), start_cap = circle(8, "mm"), end_cap = circle(8, "mm"), strength = 2) +
            scale_edge_color_discrete(name = "Post Category") +
            scale_edge_width(name = "Edge Width", range = c(0.1, 3.5)) +
            scale_edge_linetype_manual(name = "To Channel", values = c("FALSE" = "solid", "TRUE" = "dashed"), labels = c("Direct", "To Channel")) +
            geom_node_point(aes(size = degree)) +
            ggtitle(paste("Network with Post Category as Edge \nColor with Channel Noted:", channel_name)) 

    }
    # print(paste(savedirectory, "/PostCat.png", sep=""))
    ggsave(paste(savedirectory, "PostCat.png", sep=""), plot_postcat_graph)
    return(plot_postcat_graph)
}

## plot_postcat_wlabels
# The function `plot_postcat_wlabels` plots the network with the edges colored corresponding to the post category of the interaction. The node size is the overall degree, with the labels of the nodes on the graph. The frequency boolean determines whether to plot the frequency of the interaction. The to_channel boolean allows the user to decide whether the messages to the channel should be dashed. The username boolean decides whether the username will be the label (TRUE) or the userid (FALSE). 

# INPUTS: Network, boolean post_cat_count (default = FALSE), boolean to_channel (default = FALSE), boolean username (default = FALSE), string channel_name, OUTPUT: Graph with post category type as edges and nodes labelled
 
 plot_postcat_wlabels <- function(picup_network_channel, post_cat_count = FALSE, to_channel = FALSE, username = FALSE, channel_name, savedirectory){
    postcat <- plot_postcat(picup_network_channel, post_cat_count, to_channel, channel_name, savedirectory)

    if (username == TRUE){
        plot_postcat_wlabels <- postcat + geom_node_text(aes(label = username), nudge_x = postcat$data$x * 0.1, nudge_y = postcat$data$y * 0.2)
    } else if (username == FALSE){
        plot_postcat_wlabels <- postcat + geom_node_text(aes(label = name), nudge_x = postcat$data$x * 0.1, nudge_y = postcat$data$y * 0.2)
    }
    ggsave(paste(savedirectory, "PostCatwLabel.png", sep=""),plot_postcat_wlabels)
    return(plot_postcat_wlabels)
}

## PLOTTING WITH MUTUAL TIES
## boolean_Mutual_Ties
# The function `boolean_Mutual_Ties` returns a list of True/False values determining whether the tie is mutual or not. This works for networks with or without the frequency calculated. 
# INPUTS: Network, OUTPUT: List of boolean values

# edge (A,B) denoted mutual if edge (B,A) also in graph
boolean_Mutual_Ties <- function(picup_network_channel){
    mutual_ties <- which_mutual(picup_network_channel, eids = E(picup_network_channel)) == TRUE
    return(mutual_ties)
}

## count_Mutual_Ties
# The function `count_Mutual_Ties` counts the number of mutual and one-sided ties in the network. The user must specify whether the network contains frequency or not to ensure correct calculation. 

# INPUTS: Network, boolean frequency (default = FALSE), OUTPUT: List of count of mutual ties and one-sided ties

# As long as one tie is reciprocated in the relationship, all ties in that relationship are counted as mutual. 

count_Mutual_Ties <- function(picup_network_channel, frequency = FALSE){
    if (frequency == TRUE){
        # get boolean list of mutal ties
        mutual_ties <- boolean_Mutual_Ties(picup_network_channel)
        mutual <- sum(E(picup_network_channel)$count[mutual_ties == TRUE]  )  
        onesided <- sum(E(picup_network_channel)$count[mutual_ties == FALSE]  )  
        mutual_ties_list <- c(mutual, onesided)

    } else if (frequency == FALSE){
        # get boolean list of mutal ties
        mutual_ties <- boolean_Mutual_Ties(picup_network_channel)
        # count number of mutual and one-sided ties
        mutual <- length(mutual_ties[mutual_ties==TRUE]) # mutual 
        onesided <- length(mutual_ties[mutual_ties==FALSE])  # one-sided
        mutual_ties_list <- c(mutual, onesided)
    }
    return(mutual_ties_list)
}


## boolean_Larger_Outdegree
# The function `boolean_Larger_Outdegree` returns a list of True/False values that determines whether the node has a larger outdegree or not. This works for networks with or without the frequency calculated. 

# INPUTS: Network, OUTPUT: List of boolean values

boolean_Larger_Outdegree <- function(picup_network_channel){
    larger_out_degree <- V(picup_network_channel)$out_degree >= V(picup_network_channel)$in_degree 
    return(larger_out_degree)
}

## count_Larger_Outdegree
# The function `count_Larger_Outdegree` counts the number of nodes with a larger outdegree and those with a larger in degree. This works for networks with or without the frequency calculated. 

# INPUTS: Network, OUTPUT: List of count of larger outdegree and larger in degree
count_Larger_Outdegree <- function(picup_network_channel){
    # get boolean list of mutal ties
    larger_out_degree <- boolean_Larger_Outdegree(picup_network_channel)
    # count number of mutual and one-sided ties
    larger_out_degree_count <- length(larger_out_degree[larger_out_degree==TRUE]) # mutual 
    larger_in_degree_count <- length(larger_out_degree[larger_out_degree==FALSE])  # one-sided
    larger_degree_count <- c(larger_out_degree_count, larger_in_degree_count)
    return(larger_degree_count)
}

## plot_compare_degreeties
# The function `plot_compare_degreeties` plots the network such that we can compare mutual one-sided ties as well as node degree. A edge that is one-sided is colored black whereas a tie that is mutual is colored grey. A node that is dark blue has a larger in degree whereas a node that is light blue has a larger out degree. The node size corresponds to the overall degree. The user must specify whether the data contains frequency information or not - including frequency produces a weighted graph. The user must also specify whether or not the to_channel edges should be denoted with a dashed line. 

# INPUTS: Network, boolean frequency (default = FALSE), boolean to_channel (default = FALSE), string channel_name, OUTPUT: Graph of network as described above

plot_compare_degreeties <- function(picup_network_channel, frequency = FALSE, to_channel = FALSE, channel_name, args){
    # get boolean list of mutual ties
    mutual_ties <- boolean_Mutual_Ties(picup_network_channel)
    # get tie count
    mutual_tie_list <- count_Mutual_Ties(picup_network_channel, frequency)
    # get boolean list of larger out degree
    larger_out_degree <- boolean_Larger_Outdegree(picup_network_channel)
    # get larger out/in degree count
    larger_degree_list <- count_Larger_Outdegree(picup_network_channel)

    comparedegreetie <- ggraph(picup_network_channel, layout = 'kk') + 
        # scale_size_continuous(range = c(3, 15), breaks = c(min(V(picup_network_channel)$degree), median(V(picup_network_channel)$degree), max(V(picup_network_channel)$degree))) +
        # scale_fill_manual(name = "Out vs In Degree", values = c("FALSE" = "darkblue", "TRUE" = "lightsteelblue2" ), labels = c(paste("Larger In: ", larger_degree_list[2]), paste("Larger Out:",larger_degree_list[1]) )) +
        # # labs(size="Degree Size") +
        theme_classic() +
        theme(plot.title = element_text( hjust = 0.5, size = 17.5, face = "bold"), legend.title = element_text( size = 15), legend.text = element_text(size=13), legend.key.size = unit(.45, "cm"), legend.key.width = unit(1.75,"cm"), panel.border = element_rect(color = "black", fill = NA, size = 2), axis.title.x = element_blank(),  axis.text.x = element_blank(),  axis.ticks.x = element_blank(), axis.title.y = element_blank(),  axis.text.y = element_blank(),  axis.ticks.y = element_blank() ) + 
        guides(fill = guide_legend(override.aes = list(size=4, stroke=.5)))

    if (frequency == FALSE && to_channel == FALSE){
        # NO FREQUENCY OR TO CHANNEL
        picup_network_channel_graph_comparedegreetie <- comparedegreetie + 
        scale_size_continuous(range = c(3, 15), breaks = c(min(V(picup_network_channel)$degree), median(V(picup_network_channel)$degree), max(V(picup_network_channel)$degree))) +
        # scale_fill_manual(name = "Out vs In Degree", values = c("FALSE" = "darkblue", "TRUE" = "lightsteelblue2" ), labels = c(paste("Larger In: ", larger_degree_list[2]), paste("Larger Out:",larger_degree_list[1]) )) +
        labs(size="Degree Size") +
        # geom_edge_link( start_cap = circle(7, "mm"), end_cap = circle(7, "mm"), aes(color = mutual_ties), strength = 2) +
        geom_edge_link( start_cap = circle(7, "mm"), end_cap = circle(7, "mm"), strength = 2) +

        # geom_edge_fan(arrow = arrow(length = unit(0.5, "lines"), type = "closed"), start_cap = circle(7, "mm"), end_cap = circle(7, "mm"), aes(color = mutual_ties), strength = 2) +
        # scale_edge_color_manual(name = "Mutual Ties", values = c("FALSE" = "black", "TRUE" = "gray60"), labels = c(paste("One-sided:", mutual_tie_list[2]), paste("Mutual:", mutual_tie_list[1]))) +
        # geom_node_point(aes(size = degree, fill = larger_out_degree), color = "black", pch = 21) +
        # scale_edge_color_manual(name = "Mutual Ties", values = c("FALSE" = "black", "TRUE" = "gray60"), labels = c(paste("One-sided:", mutual_tie_list[2]), paste("Mutual:", mutual_tie_list[1]))) +
        geom_node_point(aes(size = degree), color = "royalblue3") + 
        # ggtitle("Advanced Thermodynamics Channel")
        ggtitle(paste("Compare Degree Size and Type of Tie: \n", channel_name)) 
        

    } else if (frequency == TRUE && to_channel == FALSE) {
        # JUST FREQ
        picup_network_channel_graph_comparedegreetie <- comparedegreetie + 
            labs(size="Strength Size") +
            geom_edge_fan(arrow = arrow(length = unit(0.5, "lines"), type = "closed"), 
            start_cap = circle(7, "mm"),
            end_cap = circle(7, "mm"),
            aes(color = mutual_ties, width = E(picup_network_channel)$count), strength = 2) +
            scale_edge_color_manual(name = "Mutual Ties", values = c("FALSE" = "black", "TRUE" = "gray60"), labels = c(paste("One-sided:", mutual_tie_list[2]), paste("Mutual:", mutual_tie_list[1]))) + 
            scale_edge_width(name = "Edge Width", range = c(0.25, 2)) +

            geom_node_point(aes(size = degree, fill = larger_out_degree), color = "black", pch = 21) +
            # ggtitle(paste("Mutual Ties - DW \n", channel_name))
            # ggtitle("Mutual Ties - DW \n Glowscript")

            ggtitle(paste("Compare Degree Size and Type of Tie \n with Weighted Ties: \n", channel_name))    

    } else if (frequency == FALSE && to_channel == TRUE) {
        # JUST TO CHANNEL
        picup_network_channel_graph_comparedegreetie <- comparedegreetie + 
            labs(size="Degree Size") +
            geom_edge_fan(arrow = arrow(length = unit(0.5, "lines"), type = "closed"), start_cap = circle(7, "mm"), end_cap = circle(7, "mm"),
            aes(color = mutual_ties, linetype = E(picup_network_channel)$to_channel), strength = 2) +
            scale_edge_color_manual(name = "Mutual Ties", values = c("FALSE" = "black", "TRUE" = "gray60"), labels = c(paste("One-sided:", mutual_tie_list[2]), paste("Mutual:", mutual_tie_list[1]))) + 
            scale_edge_linetype_manual(name = "To Channel", values = c("FALSE" = "solid", "TRUE" = "dashed"), labels = c("Direct", "To Channel")) +
            geom_node_point(aes(size = degree, fill = larger_out_degree), color = "black", pch = 21) +
            ggtitle(paste("Compare Degree Size and Type of Tie \n with Channel Messages Noted: \n", channel_name)) 

    } else if (frequency == TRUE && to_channel == TRUE){
        # BOTH
        picup_network_channel_graph_comparedegreetie <- comparedegreetie + 
            scale_size_continuous(range = c(3, 15), breaks = c(min(V(picup_network_channel)$degree), median(V(picup_network_channel)$degree), max(V(picup_network_channel)$degree))) +
            scale_fill_manual(name = "Out vs In Degree", values = c("FALSE" = "darkblue", "TRUE" = "lightsteelblue2" ), labels = c(paste("Larger In: ", larger_degree_list[2]), paste("Larger Out:",larger_degree_list[1]) )) +
            labs(size="Strength Size") +
            
            geom_edge_fan(arrow = arrow(length = unit(0.5, "lines"), type = "closed"), start_cap = circle(7, "mm"), end_cap = circle(7, "mm"),
            aes(color = mutual_ties, width = E(picup_network_channel)$count, linetype = E(picup_network_channel)$to_channel), strength = 2) +
            
            scale_edge_color_manual(name = "Mutual Ties", values = c("FALSE" = "black", "TRUE" = "gray60"), labels = c(paste("One-sided:", mutual_tie_list[2]), paste("Mutual:", mutual_tie_list[1]))) + 
            
            scale_edge_width(name = "Edge Width", range = c(0.25, 3)) +
            
            scale_edge_linetype_manual(name = "To Channel", values = c("FALSE" = "solid", "TRUE" = "dashed"), labels = c("Direct", "To Channel")) +
            geom_node_point(aes(size = degree, fill = larger_out_degree), color = "black", pch = 21) +
            ggtitle(paste(args$channelname, "Channel"))
            ggtitle(paste("Compare Degree Size and Type of Tie \n with Channel Messages Noted & Weighted Ties: \n", channel_name)) 
    }
    # print(paste(savedirectory, "CompareDegreeTies.png", sep=""))
    ggsave(paste(savedirectory, "CompareDegreeTies.png", sep=""),picup_network_channel_graph_comparedegreetie)

    # save_str = paste( args$plot, args$channelname, "CompareDegreeTies_UNDUNW.png", sep="_")
    # ggsave(gsub(" ", "", paste(args$sd,save_str)), picup_network_channel_graph_comparedegreetie)

    return(picup_network_channel_graph_comparedegreetie)
}

## plot_compare_degreeties_wlabels
# The function `plot_compare_degreeties_wlabels` is the same as the `plot_compare_degreeties` function, but adds labels to the nodes of the plot. The username option specifies whether the label should be the username or the userid. 

# INPUTS: Network, boolean frequency (default = FALSE), boolean to_channel (defualt = FALSE), boolean username (default = FALSE), string channel_name, OUTPUT: Graph of network as described above
plot_compare_degreeties_wlabels <- function(picup_network_channel, frequency = FALSE, to_channel = FALSE, username = FALSE, channel_name, savedirectory){
    comparedegreetie <- plot_compare_degreeties(picup_network_channel, frequency, to_channel, channel_name, savedirectory)
    if (username == TRUE){
        picup_network_channel_graph_comparedegreetie <- comparedegreetie + geom_node_text(aes(label = username), nudge_x = comparedegreetie$data$x * 0.1, nudge_y = comparedegreetie$data$y * 0.2)
    } else if (username == FALSE){
        picup_network_channel_graph_comparedegreetie <- comparedegreetie + geom_node_text(aes(label = name), nudge_x = comparedegreetie$data$x * 0.1, nudge_y = comparedegreetie$data$y * 0.2)
    }
    ggsave(paste(savedirectory,"CompareDegreeTieswLabels.png", sep=""),picup_network_channel_graph_comparedegreetie)
    return(picup_network_channel_graph_comparedegreetie)
}

## EGO NETWORK
# An Ego network of a particular node contains the ego, the nodes it is connected to by one tie (this could be increased), as well as all the ties between the nodes that are included. Due to the '@channel' messages, these ego network (especially for the most connected users) look very similar if not identical to the original network. 
# For this reason, we also plot a version of the ego graph where we only keep the edges incident to the most connected user. This gives us a better understanding of the type of communication between the most connected user and the users it is connected to by a single node. 

## plot_ego_graph
# The function `plot_ego_graph` creates an ego graph from a given node and plots its graph. It is best to use the network that does not contain joins or leaves, since those messages get sent to everyone. The ego is colored light blue while the rest of the nodes are black. 
# This function calls plot_network and hence requires the frequency boolean that states whether we want the frequency of edges or all edges (see plot_network). 

# INPUTS: Network object, string ego, int order, boolean frequency (default = FALSE), boolean to_channel(default = FALSE), string channel_name,  OUTPUT: Graph of ego Network
plot_ego_graph <- function(picup_network_channel, ego, order, frequency = FALSE, to_channel = FALSE, channel_name, savedirectory) {
    # create ego graph
    ego_graph <- make_ego_graph(picup_network_channel, order = order, nodes = ego , mode = c("all"))[[1]]
    # if node given
    is_node <- V(ego_graph)$name == ego  
    # plot the graph
    plot_ego_graph <- plot_network(ego_graph, frequency, to_channel, channel_name, savedirectory) + geom_node_point(aes(size = degree, fill = is_node), color = "black", pch = 21) + scale_fill_manual(name = "Given Node", values = c("TRUE" = "lightsteelblue2", "FALSE" = "honeydew2"), labels = c("Node", "Other Nodes")) + ggtitle(paste("Ego Network of User", ego, " \n from", channel_name))

    ggsave(paste(savedirectory, "EgoGraph.png", sep=""),plot_ego_graph)
    return(plot_ego_graph)
}


## plot_ego_graph_comparedegreeties
# The function `plot_ego_graph_comparedegreeties` is similar to the original compare ties graph, but it is just for the ego provided.

# INPUTS: Network object, string ego, int order, boolean frequency (default = FALSE), boolean to_channel (default = FALSE), string channel_name, OUTPUT: Graph of ego Network with ties and nodes highlighted
plot_ego_graph_comparedegreeties <- function(picup_network_channel, ego, order, frequency = FALSE, to_channel = FALSE, channel_name, savedirectory) {
    # create ego graph
    ego_graph <- make_ego_graph(picup_network_channel, order = order, nodes = ego , mode = c("all"))[[1]]
    # plot the graph
    plot_ego_graph_comparedegreeties <- plot_compare_degreeties(ego_graph, frequency, to_channel, channel_name, savedirectory)
   
    ggsave(paste(savedirectory, "EgoGraphCompareTies.png", sep=""),plot_ego_graph_comparedegreeties)
    return(plot_ego_graph_comparedegreeties)
}

## plot_ego_graph_bypostcat
# The function `plot_ego_graph_bypostcat` is similar to the original plot_bypostcat graph, just for the ego provided.

# INPUTS: Network object, string ego, int order, boolean frequency, boolean to_channel, string channel_name, OUTPUT: Grid of networks
plot_ego_graph_bypostcat <- function(picup_network_channel, ego, order, frequency = FALSE, to_channel = FALSE, channel_name, savedirectory) {
    # create ego graph
    ego_graph <- make_ego_graph(picup_network_channel, order = order, nodes = ego , mode = c("all"))[[1]]
    # plot the graph
    plot_ego_graph_bypostcat <- plot_channel_bypostcat(ego_graph, channel_name, savedirectory)

    ggsave(paste(savedirectory, "EgoGraphByPostCat.png", sep=""),plot_ego_graph_bypostcat)
    return(plot_ego_graph_bypostcat)
}

## plot_ego_graph_postcat
# The function `plot_ego_graph_postcat` is similar to the original ego graph, but the ties are colored by post category.

# INPUTS: Network object, string ego, int order, boolean post_cat_count (default = FALSE), boolean to_channel (default = FALSE), string channel_name, OUTPUT: Graph of ego network with ties as post category
plot_ego_graph_postcat <- function(picup_network_channel, ego, order, post_cat_count=FALSE, to_channel = FALSE, channel_name) {
    # create ego graph
    ego_graph <- make_ego_graph(picup_network_channel, order = order, nodes = ego , mode = c("all"))[[1]]
    # plot the graph
    plot_ego_graph_postcat <- plot_postcat(ego_graph, post_cat_count, to_channel, channel_name, savedirectory)
    
    ggsave(paste(savedirectory, "EgoGraphPostCat.png", sep=""),plot_ego_graph_postcat)
    return(plot_ego_graph_postcat)
}

## get_MostConnected_User
# The function `get_MostConnected_User` finds the index and the name of the user with the highest degree in the network. 

# Input: Network object, Output: List of index and name of user with highest degree
get_MostConnected_User <- function(picup_network_channel){
    # get index of most connected user
    mcu_index <- which.max(V(picup_network_channel)$degree)
    # get name of most connected user
    mcu_name <- V(picup_network_channel)$name[mcu_index]  
    mcu_list <- c(mcu_index, mcu_name)
    return(mcu_list)
}

## get_Incident_edges
# The function `get_Incident_edges` returns three graph objects containing only the edges directly connected to the user provided (incident edge of vertex), rather than all the edges in the ego graph. 

# Input: Network object, string userid, Output: List of three subgraphs: all incident edges, incident edges towards node, incident edges away from node
get_Incident_edges <- function(picup_network_channel, userid){
    # gets all incident edges, edges in, edges out
    all_edges <- incident(picup_network_channel, userid)
    in_edges <- incident(picup_network_channel, userid, mode = c("in"))
    out_edges <- incident(picup_network_channel, userid, mode = c("out"))

    # create graph objects only those edges
    all_graph <- subgraph.edges(picup_network_channel, all_edges)  
    in_graph <- subgraph.edges(picup_network_channel, in_edges)  
    out_graph <- subgraph.edges(picup_network_channel, out_edges)  

    graph_list <- list(all_graph, in_graph, out_graph)

    return(graph_list)
}

## plot_incident_withcolor
# The function `plot_incident_withcolor` gets the incident edges of the user provided. Then, plots the network with the node size as degree and colors the given user/ego.
# The frequency boolean and to_channel boolean are required as we use the plot_network function (see plot_network). 

# INPUTS: Network object, string userid, boolean frequncy (default = FALSE), boolean to_channel(default = FALSE), string channel_name,  OUTPUT: Plot of incident edges
plot_incident_withcolor <- function(picup_network_channel, userid, frequency=FALSE, to_channel=FALSE, channel_name, savedirectory){
    # get all incident edges
        # before or after frequency should be the same, since the edge of 1 exists
    network_incident_edges <- get_Incident_edges(picup_network_channel, userid)[[1]]
    # set boolean for the userid provided
    is_node <- V(network_incident_edges)$name == userid  

    if (frequency == FALSE && to_channel == FALSE){
        picup_network_channel_incident_wcolor <- plot_network(network_incident_edges, frequency,   to_channel, channel_name, savedirectory) +
            geom_node_point(aes(size = degree, fill = is_node), color = "black", pch = 21) +
            scale_fill_manual(name = "Given Node", values = c("TRUE" = "lightsteelblue2", "FALSE" = "honeydew2"), labels = c("Node", "Other Nodes")) +
            ggtitle(paste("Network of All Incident Edges of \n User", userid, "from \n", channel_name))
    } else if (frequency == TRUE && to_channel == FALSE){
        # if considering edge counts
         picup_network_channel_incident_wcolor <- plot_network(network_incident_edges, frequency, to_channel, channel_name) +
            geom_node_point(aes(size = degree, fill = is_node), color = "black", pch = 21) +
            scale_fill_manual(name = "Given Node", values = c("TRUE" = "lightsteelblue2", "FALSE" = "honeydew2"), labels = c("Node", "Other Nodes")) +
            ggtitle(paste("Network with Weighted Incident Edges of \n User", userid, "from \n", channel_name))
    } else if (frequency == FALSE && to_channel == TRUE){
        picup_network_channel_incident_wcolor <- plot_network(network_incident_edges, frequency,   to_channel, channel_name) +
            geom_node_point(aes(size = degree, fill = is_node), color = "black", pch = 21) +
            scale_fill_manual(name = "Given Node", values = c("TRUE" = "lightsteelblue2", "FALSE" = "honeydew2"), labels = c("Node", "Other Nodes")) +
            ggtitle(paste("Network of All Incident Edges \n with To Channel Denoted of User", userid, "from \n", channel_name))
    } else if (frequency == TRUE && to_channel == TRUE){
        # if showing all edges
        picup_network_channel_incident_wcolor <- plot_network(network_incident_edges, frequency, to_channel, channel_name) +
            geom_node_point(aes(size = degree, fill = is_node), color = "black", pch = 21) +
            scale_fill_manual(name = "Given Node", values = c("TRUE" = "lightsteelblue2", "FALSE" = "honeydew2"), labels = c("Node", "Other Nodes")) +
            ggtitle(paste("Network of Weighted Incident Edges & \n To Channel Denoted of User", userid, "\n from ", channel_name)) +
            guides(fill = guide_legend(override.aes = list(size=4, stroke=.5))) 
    }
    ggsave(paste(savedirectory, "IncidentEdgesPlot.png", sep=""),picup_network_channel_incident_wcolor)
    return (picup_network_channel_incident_wcolor)
}

## plot_incident_withcolor_withlabels
# The function `plot_incident_withcolor_withlabels` plots the incident graph of the user provided using the `plot_incident_withcolor` function, but also add labels to the nodes. The username boolean specifies whether to user the username or the userid as the label. 

# INPUTS:  Network object, string userid, boolean frequency (default = FALSE), boolean to_channel (default = FALSE), boolean username (default = FALSE), string channel_name, OUTPUT: Plot of incident edges with labels on nodes

plot_incident_withcolor_withlabels <- function (picup_network_channel, userid, frequency=FALSE, to_channel=FALSE, username = FALSE, channel_name, savedirectory){
    incident_withcolor <- plot_incident_withcolor(picup_network_channel, userid, frequency, to_channel, channel_name)
    if (username == TRUE){
        plot_incident_withcolor_withlabels <- incident_withcolor + geom_node_text(aes(label = username), nudge_x = incident_withcolor$data$x * 0.1, nudge_y = incident_withcolor$data$y * 0.2)
    } else if (username == FALSE){
        plot_incident_withcolor_withlabels <- incident_withcolor + geom_node_text(aes(label = name), nudge_x = incident_withcolor$data$x * 0.1, nudge_y = incident_withcolor$data$y * 0.2)
    }
    
    ggsave(paste(savedirectory, "IncidentEdgesPlotwLabels.png", sep=""),plot_incident_withcolor_withlabels)
    return(plot_incident_withcolor_withlabels)
}


### METRICS

## DEGREE 

## get_Degree_Dataframe
# The function `get_Degree_Dataframe` takes in a network and a node list and returns a dataframe containing a user's id, name, degree, in-degree, and out-degree. 

# INPUT: network object, filename of list of userid w/ username, OUTPUT: Dataframe of userid, username, degree, in-degree, out-degree. 

get_Degree_Dataframe <- function (picup_network_channel, picup_nodelist_dataframe){
    degree_df <- data.frame(matrix(NA, nrow = length(V(picup_network_channel)$name), ncol = 5))
    names(degree_df) <- c("Userid", "Username", "Degree", "In_Degree", "Out_Degree" )
    # for each user in network
    for (user in 1:length(V(picup_network_channel))){
        degree_df[user, "Userid"] <- V(picup_network_channel)[user]$name
        degree_df[user, "Degree"] <- V(picup_network_channel)[user]$degree
        degree_df[user, "In_Degree"] <- V(picup_network_channel)[user]$in_degree
        degree_df[user, "Out_Degree"] <- V(picup_network_channel)[user]$out_degree

        if( toString(V(picup_network_channel)[user]$name) == "NaN"){
            degree_df[user, "Username"] <- "NA"
        } else {
            degree_df[user, "Username"] <- get_Name(picup_nodelist_dataframe, toString(V(picup_network_channel)[user]$name))
        }
        # degree_df[user, "Username"] <- get_Name(picup_nodelist_dataframe, toString(V(picup_network_channel)[user]$name))
    }
    degree_df_sorted <- degree_df[order(degree_df[,"Degree"], decreasing = TRUE),]
    return (kable(degree_df_sorted))
}

## degree_centrality_matrix
# This function creates an adjacency matrix from the matrix. It then computes the row sum for the In Degree, column sum for the Out Degree, Average Out Degree, and Average In Degree for each node in the network. The degree is calculates as the sum of the In Degree and Out Degree. For the average calculations, we use the total number of edges. 

# Note: We are unable to compute a normalized value for any of the metrics, since our values are weighted. 

# INPUT: Network, OUTPUTS: Dataframe of Degre, Average Degree, Out Degree, In Degree, Average Out Degree, Average In Degree
degree_centrality_matrix <- function(picup_network_channel){
    adjacency_matrix <- as.matrix(as_adjacency_matrix(picup_network_channel, attr = "count"))

    degree_df <- data.frame(matrix(NA, nrow = length(V(picup_network_channel)), ncol = 6))
    colnames(degree_df) <- c("Userid", "Sum: Degree", "Sum: Outdegree", "Average Per Node: Outdegree", "Sum: Indegree", "Average Per Node: Indegree")

    for (i in 1:length(V(picup_network_channel))){
        degree_df[i,"Userid"] <- row.names(adjacency_matrix)[i]
        degree_df[i,"Sum: Degree"] <- sum(adjacency_matrix[i,]) + sum(adjacency_matrix[,i])
        # degree_df[i,"Average Per Node: Degree"] <- (sum(adjacency_matrix[i,]) + sum(adjacency_matrix[,i]))/length(E(picup_network_all_edges))

        degree_df[i,"Sum: Outdegree"] <- sum(adjacency_matrix[i,])
        degree_df[i,"Average Per Node: Outdegree"] <- sum(adjacency_matrix[i,])/length(E(picup_network_channel))

        degree_df[i,"Sum: Indegree"] <- sum(adjacency_matrix[,i])
        degree_df[i,"Average Per Node: Indegree"] <- sum(adjacency_matrix[,i])/length(E(picup_network_channel))
    }
    degree_df_sorted <- degree_df[order(degree_df[,"Sum: Degree"], decreasing = TRUE),]
    # M1[order(M1[,1],decreasing=FALSE),]
   return(kable(degree_df_sorted))
}

# NOTE THE ABOVE TWO DEGREE FUNCTIONS CALCULATE THE SAME VALUE FOR DEGREE, IN DEGREE, AND OUT DEGREE. THE SECOND INCLUDES THE AVERAGE INFORMATION

## average_tie_strength
# The function `average_tie_strength` is calculates the average tie strength for the given network. The function requires a network with edge attributes or an adjacency matrix. We set the diagonals to 0 and calculate the average tie strength as the total weight divided by the possible connections in the network. This is the average amount of values ties a node has in the given network. The boolean `network` specifies whether the network (default = TRUE) or the adjacency_matrix (FALSE) is supplied

# INPUT: Network/Adjacency Matrix, boolean network, OUTPUT: average tie strength 
average_tie_strength <- function (picup_network_channel, network = TRUE){
    if(network == TRUE){
        # Convert to Matrix
        picup_network_channel_adj <- as.matrix(as_adjacency_matrix(picup_network_channel, attr = "count"))
    } else if (network == FALSE){
        picup_network_channel_adj <- picup_network_channel
    }

    # All diags 0, we want to omit them in calculation
    diag(picup_network_channel_adj) <- 0
    # Average tie strength, omitting diagonals
    return (sum(picup_network_channel_adj)/ (dim(picup_network_channel_adj)[1] * dim(picup_network_channel_adj)[2] - dim(picup_network_channel_adj)[1]))
}

## average_degree
# The function `average_degree` is calculates the average degree across for the given network. It is a slightly different calculation from the average tie strength. The function returns the total sum of edges over the total number of nodes. Note, for directed data this calculation is not super helpful. The directionality is an important part of the interpretation of the calculation, however the value for the average in degree, average out degree, and average degree will all be the same, based on summation properties. 

# INPUT: Network/Adjacency Matrix, OUTPUT: average degree
average_degree <- function (network_with_all_edges){

    adjacency_matrix <- as.matrix(as_adjacency_matrix(network_with_all_edges))
   return (sum(adjacency_matrix)/(length(V(network_with_all_edges))))
}

## DENSITY
# Built-in function to igraph that calculates edge density: edge_density


## RECIPROCITY

## recripocity - built in function to igraph that calculates basic reciprocity

## reciprocated_adjmatrix
# The `reciprocated_adjmatrix` function takes in a network with edges as attributes and returns a symmetric matrix. The smaller value of matrix[i,j] and matrix [j,i] is used as symmetric_matrix[i,j] = symmetric_matrix[j,i]. The boolean `network` specifies whether a network is passed (default = TRUE) or an adjacency matrix (FALSE). 

# INPUT: Network with all edges/Adjacency Matrix, OUTPUT: Symmetric Adjacency Matrix
reciprocated_adjmatrix <- function(picup_network_channel, network = TRUE){
    if (network == TRUE){
        adj_matrix <- as.matrix(as_adjacency_matrix(picup_network_channel, attr = "count"))
    } else if (network == FALSE){
        adj_matrix <- picup_network_channel
    }

    symmetric_adj <- matrix(data = NA, nrow = dim(adj_matrix)[1], ncol = dim(adj_matrix)[2])
    for (i in 1:dim(adj_matrix)[1]){
        for (j in 1:dim(adj_matrix)[2]){
            # print(network_with_all_edges_adj[i,j])
            if (adj_matrix[i,j] <= adj_matrix[j,i]){
                symmetric_adj[i,j] = adj_matrix[i,j]
                symmetric_adj[j,i] = adj_matrix[i,j]
            }else{
                symmetric_adj[i,j] = adj_matrix[j,i]
                symmetric_adj[j,i] = adj_matrix[j,i]
            }
        }
    }
    return (symmetric_adj)
    }


## weighted_reciprocity
# The `weighted_reciprocity` function takes a network with edges as attributes and a symmetric matrix from that network and calculates the weighted reciprocity, which we have defined as the sum of the values in the symmetric matrix divided by the sum of the values in the original matrix. The user can also provide the adjacency matrix of the original graph (`network` = FALSE) instead of the original network (`network` = TRUE, default). 

# INPUT: original network, symmetric matrix, boolean network, OUTPUT: weighted reciprocity
weighted_reciprocity <- function(picup_network_channel, symmetric_matrix, network = TRUE){
    if (network == TRUE){
        picup_network_channel_adj <- as.matrix(as_adjacency_matrix(picup_network_channel, attr = "count"))
    } else if (network == FALSE){
        picup_network_channel_adj <- picup_network_channel
    }
    
    diag(picup_network_channel_adj) <- 0
    # print(sum(symmetric_matrix))
    # print(sum(picup_network_channel_adj))
    return(sum(symmetric_matrix)/sum(picup_network_channel_adj))

}

## CLUSTERING COEFFICIENTS

## TRANSITIVITY
# Built-in function to igraph that calculates transitivity - transitivity

## GLOBAL CLUSTERING COEFFICIENTS 
# Using tnet's built in function for weighted clustering metric by Opshal. Function below converts igraph object to tnet object to use the clustering function. Must use network with count or frequency attribute

# INPUT: network, OUTPUT: Global Clustering Coefficient 
weightedglobal_clusteringcoef <- function(network){
     # Create format to convert to tnet object 
        # edge pairs with weight attribute as the third column 
    tnet_graph <- cbind(get.edgelist(network, names=FALSE), E(network)$count)     
    # Convert to a tnet object 
    tnet_graph <- as.tnet(tnet_graph, type="weighted one-mode tnet")
    # Get weighted and directed GCC using geometric mean 
    return(clustering_w(tnet_graph, measure="gm"))
}

## Non-Weighted GCC
    # Make the network binary (changes the postive weighted to 1)
    # Calculate the clustering coefficient in the same was as before. We know that the weighted version is equivalent to the non-weighted version, when dealing with a binary network, since all the triplet values are 1 always. 
        # This is done with the "bi" for binary measure 
            # returns same value if you changed all the edge weighted to 1 by hand
global_clusteringcoefficient <- function(network){
    # Create format to convert to tnet object 
        # edge pairs with weight attribute as the third column 
    tnet_graph <- cbind(get.edgelist(network, names=FALSE), E(network)$count) 
    # Convert to a tnet object 
    tnet_graph <- as.tnet(tnet_graph, type="weighted one-mode tnet")
    # Get weighted and directed GCC using geometric mean 
    return(clustering_w(tnet_graph, measure="bi"))

}

## LOCAL CLUSTERING COEFFICIENT
# Using Clemente and Grassi's function for weighted and directed local clustering coefficient. The function reports the results in a table. 
# Use 'Global' attributes for the average clustering coefficient. Use 'In' (two arrows in) and 'Out' (two arrows out) attributes to separate total clustering coefficient 

# INPUT: Network, OUTPUT: LCC for each node in table
local_clusteringcoef <- function(network){
    # Get local clustering coefficient 
    lcc_advt <- ClustBCG(as.matrix(as_adjacency_matrix(network, attr = "count")), type = "directed", isolates = "zero")

    totalcc_advt <- as.matrix(lcc_advt$totalCC)
    totalcc_advt <- cbind(rownames(totalcc_advt), totalcc_advt)
    rownames(totalcc_advt) <- 1:nrow(totalcc_advt)

    totalcc_advt[,2] <- as.numeric(as.character(totalcc_advt[,2]))  
    return(kable(totalcc_advt[order(totalcc_advt[,2], decreasing = TRUE),], col.names = c("Users", "LCC")))
}


## EIGENVECTOR CENTRALITY
# Use built-in igraph function for eigenvector centrality. The below function just displays the information nicely in a table. 
# INPUT: Network with frequency counts, OUTPUT: Eigenvector centrality for all nodes

eigenvector_centrality <- function(network){
    return (kable(sort(eigen_centrality(network, weights = E(network)$counts, scale = FALSE)$vector, decreasing = TRUE), col.names = c("EC")))
}

## BETA CENTRALITY
# Use built-in igraph function for beta/power/Bonacich centrality. The below function just displays the information nicely in a table. This function needs the network to contain all the edges. So, for consistency with the howtorun_metrics function below, we convert the graph with weights as an edge attribute to an adjacancy matrix and then convert that to a graph object with multiple edges (not as an edge attribute)

# INPUT: Network with frequency counts, beta value to use,  OUTPUT: Beta centrality for all nodes

beta_centrality <- function(network_wedge_attr, beta){
    network_wedge_attr_adj <- as.matrix(as_adjacency_matrix(network_wedge_attr, attr = "count"))
    network <- graph_from_adjacency_matrix(network_wedge_attr_adj)
    return (kable(sort(power_centrality(network, exponent = beta), decreasing = TRUE), col.names = c("Beta Centrality")))
}


## NETWORK RANDOMIZATION
# OVERALL GOAL: Using the metric functions for creating the network,  (done in howtorun_randomization)
                # calculate the metric for the original graph, (done in howtorun_randomization)
                # generate a set amount of random graphs, (done below and called in howtorun_randomization)
                # calculate the metric for all the random graphs, (done in howtorun_randomization)
                # create a histogram to see the shape of the metric values (ggplot) (done below potentially and called in howtorun_randomization)
                # run the statistical analysis (done below and called in howtorun_randomization )
  

    # Create function that takes in a igraph network object, returns a given number of random graphs as a list (or whatever object makes sense to you)

perm_edgeset <- function(network){
    vertices <- seq(1, length(V(network)))
    edges <- combn(vertices, 2, simplify=FALSE)

    edge_bothways <- matrix(0, nrow = length(edges)*2, ncol = 2)
    
    for (i in 1:length(edges)){
        edge_bothways[i,] <- c(edges[[i]][1], edges[[i]][2])
        edge_bothways[i+length(edges),] <- c(edges[[i]][2], edges[[i]][1]) # add the opposing edge
    }
    return(edge_bothways) 
}


generate_random_graphs <- function(network){
    # Calculating threshold probability
    vlen <- length(V(network))
    sum_edgeweights <- sum(E(network)$count)
    denom <- (vlen*(vlen-1)) + 2*sum_edgeweights
    p <- (2*sum_edgeweights)/denom

    bothways_edge <- perm_edgeset(network)

    # Create empty graph
    ran_net <- graph.empty(directed = TRUE)
    vertices <- seq(1, length(V(network)))    
    ran_net <- ran_net + vertices(vertices)

    for (edge_inx in 1:nrow(bothways_edge)){
        weight_val <- 0 # reset for each edge       
        # Continually update weight value until threshold is not met
        while (runif(1) < p){
            weight_val <- weight_val + 1
        }
        if (weight_val != 0){# if change in value, add the edge with the correct weight 
            ran_net <- add_edges(ran_net, bothways_edge[edge_inx, ], count = weight_val)
        }  
    }     
    return(ran_net)
}

### CONFIGURATION MODEL
    # Keep the same in/out degree distribution for the network, essentially swapping the edges
# # Existence of edge
edge_existence <- function(edgelist, tailnode, headnode){
    # edgelist: Tail, Head, Count
    has_edge = FALSE
    edge_true = NA
    for (edge in 1:nrow(edgelist)){
        # if the heads and tails of each pair are equivalent
        if(edgelist[edge,1] == tailnode & edgelist[edge,2] == headnode ){
            has_edge = TRUE
            edge_true = edge
        }
    }
    return (c(has_edge, edge_true))
}

# Add an edge to the graph 
    # If not already there, include it
    # If there, increase weight
add_one_edge <- function(edgelist, tailnode, headnode){
     # edgelist: Tail, Head, Count
    does_edgeexist <- edge_existence(edgelist, tailnode, headnode)
    has_edge <- does_edgeexist[1]
    edge_true <- does_edgeexist[2]

    if (has_edge == TRUE){
        edgelist[edge_true,3] = edgelist[edge_true,3] + 1 
    } else{
        edgelist <- rbind(edgelist, c(tailnode, headnode, 1)) 
    }
    return(edgelist)
}

# Delete an edge from the graph
    # if it has edge weight 1, remove it 
    # if it has edge weight greater than 1, decrement it
delete_one_edge <- function(edgelist, tailnode, headnode){
    does_edgeexist <- edge_existence(edgelist, tailnode, headnode)
    has_edge <- does_edgeexist[1]
    edge_true <- does_edgeexist[2]

    if (has_edge == TRUE && edgelist[edge_true,3] > 1){
        edgelist[edge_true,3] <- edgelist[edge_true,3] - 1 
    } else if (has_edge == TRUE && edgelist[edge_true,3] == 1) {
        edgelist <- edgelist[-c(edge_true),]
    }
    return(edgelist)

}

# create simple graph with self loop variations and see if they get removed. 

# Generate a new network
    # shuffle out degrees on nodes option
generation_configuration_model <- function(og_network, shuffle = FALSE){
    # Get the in, out degree of the network
    heads = V(og_network)$in_degree
    tails = V(og_network)$out_degree

    # print('pre-shuffle')
    # print(heads)
    # print(tails)

    # if want to shuffle out degree of nodes
    if (shuffle == TRUE){
        tails <- sample(tails)
    }

    # print('post-shuffle')
    # print(heads)
    # print(tails)

    # # creating empty graph
    # result = make_empty_graph(n=length(V(og_network)), directed = TRUE)
    # V(result)$name <- V(og_network)$name # keeping the same nodes
    # print(V(result)$name)
    # V(result)$in_degree <- heads # setting new indegree
    # V(result)$out_degree <- tails # setting new outdegree

    # Check that heads and tails length is the same
    if (sum(heads) == sum(tails)){
        i = 1
       # while we still have edges to add
        while (sum(heads) > 0){
            # Get heads, tails probability 
                # if value in heads, tails list is 0, we set the probability of picking it to be 0
            tails_probs = c()
            for (t in 1:length(tails)){
                if (tails[t]==  0){
                    tails_probs[t] <- 0
                } else{
                    tails_probs[t] <- 1
                }
            }

            heads_probs = c()
            for (h in 1:length(heads)){
                if (heads[h]==  0){
                    heads_probs[h] <- 0
                } else{
                    heads_probs[h] <- 1
                }
            }
       
            # Randomly choose a head index and tail index to create an edge between 
            tail_index = sample(1:length(tails), 1, prob = tails_probs/sum(tails_probs))
            head_index = sample(1:length(heads), 1, prob = heads_probs/sum(heads_probs))
            # print(tail_index)
            # print(head_index)

            # Decrement the value of degree at the chosen index
            tails[tail_index] <- tails[tail_index] - 1
            heads[head_index] <- heads[head_index] - 1
            # print(tails)
            # print(heads)
           
            if (i == 1){
                # if first iteration, add a row 
                result_edgelist <-  matrix(c(tail_index, head_index, 1), nrow = 1, byrow = TRUE)
                # print(result_edgelist)

            }else{
                #cbind(get.edgelist(result, names=FALSE), as.integer(E(result)$count))
                # Add one edge here 
                result_edgelist <- add_one_edge(result_edgelist, tail_index, head_index)
                # print(result_edgelist)
            }
            # print(i)
            i = i + 1
        }
        
        # Undo any self loops that were created 
            # We do this by preserving in,out degree distributions
        # for all the nodes (checking for 1,1; 2,2; etc )
        for (i in 1:length(heads)){
            # while the self loop exists for that index
                # ie fix 4 4 2, twice 
            while (edge_existence(result_edgelist, i, i)[1] == TRUE){
                # print(paste0("i: ", i))
                # print(result_edgelist)
                # Create a matrix without any instances of said node
                    # Remove row that contains it in column 1, then 2 
                no_i_matrix <- result_edgelist[result_edgelist[,1] != i,  ]
                no_i_matrix <- no_i_matrix[no_i_matrix[,2] != i, ]
                # print(no_i_matrix)

                # Randomly pick an edge to do the swap of edges
                    # Randomly pick an index
                replace_index = sample(1:nrow(no_i_matrix), 1)
                row_selected = no_i_matrix[replace_index,]
                # print(replace_index)
                # print(row_selected)

                    # crisscross swap 
                        # remove ii edge and remove chosen edge
                result_edgelist = delete_one_edge(result_edgelist, i, i)
                result_edgelist = delete_one_edge(result_edgelist, row_selected[1], row_selected[2])

                # print(replace_index)
                # print("Deleted Edges")
                # print(result_edgelist)

                        # add back in the new edge
                result_edgelist = add_one_edge(result_edgelist, i, row_selected[2])
                result_edgelist = add_one_edge(result_edgelist, row_selected[1], i)

                # print("Added Edges")
                # print(result_edgelist)
                # print(nrow(result_edgelist))
 
            }

        }
        
    }
    # Convert edgelist to graph 
    config <- graph_from_edgelist(result_edgelist[,1:2], directed=TRUE) #col 1 = tail, col 2 = head
    E(config)$count <- result_edgelist[,3] # set the weights 
    V(config)$name <- V(og_network)$name # reset the names

    # Calculate the in, out, degree of the new config. 
    config <- set_Node_Attributes(config, frequency = TRUE)

    # Just checking our work:
        # if shuffle equals FALSE, the in, out, total degree distributions should be the exact same up to shuffle 
    if (shuffle == FALSE){
        if (all(sort(V(config)$degree) == sort(V(og_network)$degree)) && all(sort(V(config)$in_degree) == sort(V(og_network)$in_degree)) && all(sort(V(config)$out_degree) == sort(V(og_network)$out_degree))){
            print("All forms of degree match")
        }else{
            print("Something went wrong ")
        }
    } # if shuffle equals TRUE, the in and out degree should be the same up to shuffle, but the total degree distrbution will be different. However the total sum of edges should be the same
    else if(shuffle == TRUE) {
        if (sum(E(config)$count) == sum(E(og_network)$count) && all(sort(V(config)$in_degree) == sort(V(og_network)$in_degree)) && all(sort(V(config)$out_degree) == sort(V(og_network)$out_degree))){
            print("All forms of degree match")
        }else{
            print("Something went wrong ")
        }

    }
    # Note: since we are swapping the edges, the weights of each of the edges could be different (ie the lists themselves could have different values and lengths). The number of edges themselves might be different as well. BUT the sum of the edges should be the same.  
        # print(all(sort(E(config)$count) == sort(E(og_network)$count)) should FALSE
        # print(sum(E(config)$count)== sum(E(og_network)$count)) should be TRUE
   
   
    # print(sort(V(config)$degree))
    # print(sort(V(og_network)$degree))
    # print(sort(V(config)$in_degree))
    # print(sort(V(og_network)$in_degree))
    # print(sort(V(config)$out_degree))
    # print(sort(V(og_network)$out_degree))


    return (config)
}

### PRICE VARIANT MODEL
calculate_indegree <- function(curr_node, edge_matrix){
    # Calculate indegree
    indegree <- 0
    for (i in 1:length(edge_matrix[,1])){
        if (curr_node == edge_matrix[i,2]){
            indegree <- indegree + edge_matrix[i,3]
        }
    }
    return(indegree)
}   
calculate_outdegree <- function(curr_node, edge_matrix){
    # Calculate outdegree
    outdegree <- 0
    for (i in 1:length(edge_matrix[,1])){
        if (curr_node == edge_matrix[i,1]){
            outdegree <- outdegree + edge_matrix[i,3]
        }
    }
    return(outdegree)
}
calculate_indegree_list <- function(v_list, edge_matrix){
    # Calculate indegree
    indegree_list <- rep(0, length(v_list))
    for (i in 1:length(v_list)){
        indegree_list[i] <- calculate_indegree(v_list[i], edge_matrix)
    }
    return(indegree_list)
}

calculate_outdegree_list <- function(v_list, edge_matrix){
    # Calculate indegree
    outdegree_list <- rep(0, length(v_list))
    for (i in 1:length(v_list)){
        outdegree_list[i] <- calculate_outdegree(v_list[i], edge_matrix)
    }
    return(outdegree_list)
}  
check_edge_already_here <- function(edge_matrix, from, to){
    # Check if edge already exists
    edge_exists <- FALSE
    if (is.na(edge_matrix[1,1])){
        edge_exists <- FALSE
    }else{
        for (i in 1:length(edge_matrix[,1])){
            if (edge_matrix[i,1] == from & edge_matrix[i,2] == to){
                edge_exists <- TRUE
            }
        }
    }
    return(edge_exists)
}
price_initialconditions <- function(og_network){
    # Setting up the initial graph
    og_total_vertices <- seq(1, length(V(og_network)))
    m_0 <- ceiling(length(og_total_vertices)/6) 
    w_0 <- 1 

    # Make a fully connected graph with m0 subset
    starter_graph <- make_full_graph(m_0, directed = TRUE, loops = FALSE)
    edge_matrix <- cbind(as_edgelist(starter_graph, names = TRUE), 1) 
    # From, To, EdgeWeight

    # Vertex list
    curr_v_list <- seq(1, m_0)
    remaining_v_list <- setdiff(og_total_vertices, curr_v_list)

    # In/Out degrees
    in_degrees <- calculate_indegree_list(curr_v_list, edge_matrix)
    out_degrees <- calculate_outdegree_list(curr_v_list, edge_matrix)

    initial_conditions <- list("edge_matrix" = edge_matrix, "curr_vertex_list" = curr_v_list, "remain_v" = remaining_v_list, "in_degrees" = in_degrees, "out_degrees" = out_degrees, "w0" = w_0, "m0" = m_0)
    return(initial_conditions)
}
price_preferentialattachment <- function(curr_node, in_degrees, out_degrees){
    # Out
    s_i_out <- out_degrees[curr_node]
    out_prob <- s_i_out/sum(out_degrees)
        # This is threshold probability for out
    
    # In
    s_i_in <- in_degrees[curr_node]
    in_prob <- s_i_in/sum(in_degrees)
        # This is threshold probability for in

    pa_probabilities <- list("out_prob" = out_prob, "in_prob" = in_prob)
    return (pa_probabilities)
}
price_growth <- function(edge_matrix, curr_v_list, in_degrees, out_degrees, w0, t){
    # print(curr_v_list)
    # Capture m1 edges in matrix (so easier for weight updates)
    m1 <- matrix(data = NA, nrow = 1, ncol = 3)

    # Adding edges between t and all other nodes in network - m1 edges
    for (curr in 1:(length(curr_v_list))){
        # Choose outgoing node      
        if (runif(1) < price_preferentialattachment(curr, in_degrees, out_degrees)$out_prob){
            # Add outgoing edge
            if (is.na(m1[1,1])){
                m1[1,] <- c(curr, t, w0)
            }else{
                m1 <- rbind(m1, c(curr, t, w0))
            }
        }
        # Choose incoming node
        if (runif(1) < price_preferentialattachment(curr, in_degrees, out_degrees)$in_prob){
            # Add incoming edge
            if (is.na(m1[1,1])){
                m1[1,] <- c(t, curr, w0)
            }else{
                m1 <- rbind(m1, c(t, curr, w0))
            }
        }
        # print(m1)
    }


    # Capture m2 edges in matrix (so easier for weight updates)
    m2 <- matrix(data = NA, nrow = 1, ncol = 3)
    # Nested loop to potentially add edges between edge i and j for all combos i, j  - m2 edges
        # ONLY ADD NEW EDGES. IF EDGE EXISTS IN EDGE LIST DO NOT ADD
    for (curr_1 in 1:(length(curr_v_list))){
        for (curr_2 in 1:(length(curr_v_list))){
            if (curr_1 != curr_2) {# removing self-loops
                if(check_edge_already_here(edge_matrix, curr_2, curr_1) == FALSE && check_edge_already_here(m1, curr_2, curr_1) == FALSE && check_edge_already_here(m2, curr_2, curr_1) == FALSE){ 
                    if (runif(1) < price_preferentialattachment(curr_2, in_degrees, out_degrees)$out_prob){
                        # Add outgoing edge
                        if (is.na(m2[1,1])){
                            m2[1,] <- c(curr_2, curr_1, w0)
                        }else{
                            m2 <- rbind(m2, c(curr_2, curr_1, w0))
                        }
                    }
                }
                if(check_edge_already_here(edge_matrix, curr_1, curr_2) == FALSE && check_edge_already_here(m1, curr_1, curr_2) == FALSE && check_edge_already_here(m2, curr_1, curr_2) == FALSE){
                    # Choose incoming node
                    if (runif(1) < price_preferentialattachment(curr_2, in_degrees, out_degrees)$in_prob){
                        # Add incoming edge
                        if (is.na(m2[1,1])){
                            m2[1,] <- c(curr_1, curr_2, w0)
                        }else{
                            m2 <- rbind(m2, c(curr_1, curr_2, w0))
                        }
                    }
                }
            }
        }
    }
    
    # ANY EDGE ADDED HERE HAS WEIGHT w0
    m1_m2 <- list("m1" = m1, "m2" = m2)
    return (m1_m2)
}

 # We keep const/bigdelt to 1 based on verification of thereotical results through simulation for increasing network sizes
price_weightevolution <- function(m1, m2, edge_matrix, in_degrees, out_degrees, w_0, curr_v_list){
    # Weights of all new edges added are w0

    # Update strength of existing nodes
    # Case 1: Update edge between existing node i and new node t
        # If node i is an outgoing nodes in m1, updates the weights of outgoing edges from node i
    if (!is.na(m1[1,1])){ # if outgoing edges added 
        # print("M1 OUT DEGREE")
        for (i in 1:length(m1[,1])){
            for(j in 1:length(edge_matrix[,1])){
                # think this needs to be a while they are equal
                if (m1[i,1] == edge_matrix[j,1]){ # while newly outgoing node is equal to existing outgoing node
                    constant <- 1
                    del_w_ij <- constant * (edge_matrix[j,3]/out_degrees[m1[i,1]]) # update weight of edge
                    edge_matrix[j,3] <- edge_matrix[j,3] + del_w_ij
                    # out_degrees[m1[i,1]] <- out_degrees[m1[i,1]] + w_0 + constant # THiS IS NOT I, i is the length of the m1 matrix, we want the value at m1[i,1]
                    out_degrees[m1[i,1]] <- calculate_outdegree(m1[i,1], edge_matrix)
                }
            }
        }
        # Recalculate in/out degree of all nodes based on change 
        in_degrees <- calculate_indegree_list(curr_v_list, edge_matrix)
        out_degrees <- calculate_outdegree_list(curr_v_list, edge_matrix)

            # If node i is an incoming node in m1
        # print("M1 IN DEGREE")
        for (i in 1:length(m1[,2])){
            for(j in 1:length(edge_matrix[,2])){
                if (m1[i,2] == edge_matrix[j,2]){
                    constant <- 1
                    del_w_ij <- constant * (edge_matrix[j,3]/in_degrees[m1[i,2]])
                    edge_matrix[j,3] <- edge_matrix[j,3] + del_w_ij
                    # in_degrees[m1[i,2]] <- in_degrees[m1[i,2]] + w_0 + constant
                    in_degrees[m1[i,2]] <- calculate_indegree(m1[i,2], edge_matrix)
                }
            }
        }
          # Recalculate in/out degree of all nodes based on change 
        in_degrees <- calculate_indegree_list(curr_v_list, edge_matrix)
        out_degrees <- calculate_outdegree_list(curr_v_list, edge_matrix)
    }
   

    if (!is.na(m2[1,1])){ # if outgoing edges added 
        # Case 2: Update edge between node i and node j
        # print("M2 OUT DEGREE")
        for (r in 1:length(m2[,1])){
            for(j in 1:length(edge_matrix[,1])){
                if (m2[r,1] == edge_matrix[j,1]){
                    constant <- 1
                    del_w_rj <- constant * (edge_matrix[j,3]/out_degrees[m2[r,1]])
                    edge_matrix[j,3] <- edge_matrix[j,3] + del_w_rj
                    # out_degrees[m2[r,1]] <- out_degrees[m2[r,1]] + w_0 + constant
                    out_degrees[m2[r,1]] <- calculate_outdegree(m2[r,1], edge_matrix)
                }
            }
        }
          # Recalculate in/out degree of all nodes based on change 
        in_degrees <- calculate_indegree_list(curr_v_list, edge_matrix)
        out_degrees <- calculate_outdegree_list(curr_v_list, edge_matrix)
 
        for (r in 1:length(m2[,2])){
            for(j in 1:length(edge_matrix[,2])){
                if (m2[r,2] == edge_matrix[j,2]){
                    constant <- 1
                    del_w_rj <- constant * (edge_matrix[j,3]/in_degrees[m2[r,2]])
                    edge_matrix[j,3] <- edge_matrix[j,3] + del_w_rj
                    # in_degrees[m2[r,2]] <- in_degrees[m2[r,2]] + w_0 + constant
                    in_degrees[m2[r,2]] <- calculate_indegree(m2[r,2], edge_matrix)
                }
            }
        }
          # Recalculate in/out degree of all nodes based on change 
        in_degrees <- calculate_indegree_list(curr_v_list, edge_matrix)
        out_degrees <- calculate_outdegree_list(curr_v_list, edge_matrix)
    }

    # If weight values doesn't change, it means that is was not connected to the new node t
    weight_update <- list("edge_matrix" = edge_matrix, "in_degrees" = in_degrees, "out_degrees" = out_degrees)
    return(weight_update)

}

generate_price_variant <- function(og_network){
    # 1. Set up initial conditions
    init_cond <- price_initialconditions(og_network)
    edge_matrix <- init_cond$edge_matrix
    curr_v_list <- init_cond$curr_vertex_list   
    in_degrees <- init_cond$in_degrees
    out_degrees <- init_cond$out_degrees
    N <- length(V(og_network))

    # print(edge_matrix)
    # print(curr_v_list)
    # print(in_degrees)
    # print(out_degrees)

    # 2. Add new node t, and m = m1 + m2 edges
    for (t in init_cond$remain_v){
        # print(t)
        # Find new edges to add
        m1_m2 <- price_growth(edge_matrix, curr_v_list, in_degrees, out_degrees, init_cond$w0, t)
        
        # Update weights of existing edges
        weight_update <- price_weightevolution(m1_m2$m1, m1_m2$m2, edge_matrix, in_degrees, out_degrees, init_cond$w0, curr_v_list)
     
        # After weight evolution of old edges, add new edges from m1 and m2 matrices in growth stage
        edge_matrix <- weight_update$edge_matrix
        if (is.na(m1_m2$m1[1,1]) == FALSE){ # If edges were added
            # Add m1 to edge matrix
            edge_matrix <- rbind(edge_matrix, m1_m2$m1) 
        }
        if (is.na(m1_m2$m2[1,1]) == FALSE){ # If edges were added
            # Add m2 to edge matrix
            edge_matrix <- rbind(edge_matrix, m1_m2$m2)
                
        }

        # Add t to curr nodes
        curr_v_list <- c(curr_v_list, t)
        
        # Recalculate in/out degrees of all existing nodes after weight evolution and adding m1, m2 
        in_degrees <- calculate_indegree_list(curr_v_list, edge_matrix)
        out_degrees <- calculate_outdegree_list(curr_v_list, edge_matrix)

           }

    # Convert matrix back into graph object with edge weights
        # SO WE CAN CALCULATE METRIC VALUES
    price_graph <- graph_from_edgelist(edge_matrix[,1:2], directed=TRUE) #col 1 = tail, col 2 = head
    E(price_graph)$count <- edge_matrix[,3] # set the weights 
    price_graph <- set_Node_Attributes(price_graph, NA, TRUE)

    # edge_matrix_ordered <- edge_matrix[order(edge_matrix[,1],decreasing=FALSE),]
    # print(edge_matrix_ordered)

    return(price_graph)
}





## DEGREE DISTRIBUTIONS
    # Create a plot for the total degree distribution of a given network
    # Creat a heatmap for the in/out degree distribution of a given network

plot_degreedistrib <- function(input, network, channel_name, args, binwidth=5){
    # Calculate the frequency of each degree
    if (network == TRUE){
        totaldegree <- V(input)$degree
        indegree <- V(input)$in_degree
        outdegree <- V(input)$out_degree

        degree_df <- data.frame(TotalDegree = totaldegree, InDegree = indegree, OutDegree = outdegree)
        input <- degree_df
        print(input)
    }
    mean_TOTDeg <- mean(input$TotalDegree)
    mean_InDeg <- mean(input$InDegree)
    mean_OutDeg <- mean(input$OutDegree)

    # Histogram Degree Distribution - FOR WEIGHTED NETWORKS THESE ARE REALLY STRENGTH DISTIRBUTIONS
    total_hist <- ggplot(input, aes(x=TotalDegree)) + 
                geom_histogram(binwidth = binwidth, color="black", fill="lightsteelblue2") + 
                geom_vline(xintercept = mean_TOTDeg, linetype="dashed", colour="black") + 
                ggtitle(paste("Strength Distribution \n", channel_name)) + xlab("Total Strength") + ylab("Frequency")  + 
                theme_classic() +
                theme(plot.title = element_text( hjust = 0.5, size = 15, face = "bold")) 
                
    in_hist <- ggplot(input, aes(x=InDegree)) +
                geom_histogram(binwidth = binwidth, color="black", fill="lightsteelblue2") + 
                geom_vline(xintercept = mean_InDeg, linetype="dashed", colour="black") + 
                ggtitle(paste("In Strength Distribution \n", channel_name)) + xlab("In Strength") + ylab("Frequency")  + 
                theme_classic() +
                theme(plot.title = element_text( hjust = 0.5, size = 15, face = "bold")) 

    out_hist <- ggplot(input, aes(x=OutDegree)) +
                geom_histogram(binwidth = binwidth, color="black", fill="lightsteelblue2") + 
                geom_vline(xintercept = mean_OutDeg, linetype="dashed", colour="black") + 
                ggtitle(paste("Out Strength Distribution \n", channel_name)) + xlab("Out Strength") + ylab("Frequency")  + 
                theme_classic() +
                theme(plot.title = element_text( hjust = 0.5, size = 15, face = "bold")) 

    save_str_full = paste( args$which, args$channelname, args$metric, "FULLTotalStrengthDistribution.png", sep="_")
    ggsave(gsub(" ", "", paste(args$sd, save_str_full)), total_hist)
    save_str_in = paste( args$which, args$channelname,args$metric, "FULLInStrengthDistribution.png", sep="_")
    ggsave(gsub(" ", "", paste(args$sd, save_str_in)), in_hist)
    save_str_out = paste( args$which, args$channelname,args$metric, "FULLOutStrengthDistribution.png", sep="_")
    ggsave(gsub(" ", "", paste(args$sd, save_str_out)), out_hist)

    # Heat Map Degree Distribution 
    # unique_degredist <- input %>% group_by(InDegree, OutDegree) %>% summarize(Count = n())

    # hm_degree<- ggplot(unique_degredist, aes(x = InDegree, y = OutDegree, fill = Count)) + geom_tile()

    # ggsave(paste(savedirectory,"FULLDegreeHeatmap.png", sep=""), hm_degree) + theme_bw() + theme(plot.title = element_text(hjust = 0.5)) 
}

stat_histogram_for_rand <- function(list_metrics, og_metric, total_runs, prob_value, args, type){
    stat_hist <- ggplot(data.frame(list_metrics), aes(x=list_metrics)) +
        geom_histogram(aes(y=after_stat(density)), binwidth=0.01, colour="black", fill="lightsteelblue2") +
        geom_vline(aes(xintercept = og_metric, color = "Original")) +
        geom_vline(aes(xintercept = mean(list_metrics), color="Mean")) +
        scale_color_manual("Metric Value", values = c(Original = "red", Mean = "black")) +
        ggtitle(paste("Distribution of Random Network Metric Values")) +
        labs( title = (paste("Distribution of Random Network Metric Values")), caption = paste("\nChannel Name:", args$channelname, "\nGeneration Type:", type, "\nMetric: ", args$metric, "\nNumber of Runs:", total_runs, "\nProbability: ", prob_value) ) + 
        xlab("Metric Value") + 
        ylab("Frequency") + 
        theme_classic() +
        theme(plot.title = element_text( hjust = 0.5, size = 15, face = "bold"), legend.title = element_text( size = 10), legend.text = element_text(size=8), legend.key.size = unit(.45, "cm"), legend.key.width = unit(1.75,"cm")) + 
        guides(fill = guide_legend(override.aes = list(size=2, stroke=.5)))
    
    save_str = paste( args$which, args$channelname,args$metric, "DistRandom.png", sep="_")
    ggsave(gsub(" ", "", paste(args$sd,save_str)), stat_hist)
}



### ARGPARSER
# Note R has an argparse library which is a wrapper to Python's argparse
# Argparser is a separate library with no Python dependency

# Create a parser
pars <- arg_parser("SNA")
# FILE NAME for edge data
pars <- add_argument(pars, "edge_filename", help="Directory to File", type="character")
# FILE NAME for node data
pars <- add_argument(pars, "node_filename", help="Directory to File", type="character")
pars <- add_argument(pars, "--which", help="('met', 'plot', 'both', 'randomization', 'configuration', 'price')", type="character") 
pars <- add_argument(pars, "--joinleave", help="Include Join/Leave Messages", flag = TRUE)  # If this option is not supplied, the value of it will be FALSE

# PLOTTING OPTIONS
pars <- add_argument(pars, "--plot", help="Choice of plot", type="character")
pars <- add_argument(pars, "--channelname", help="Channel Name for plots", type="character")
pars <- add_argument(pars, "--sd", help = "Save Directory for the Figure", type = "character")
pars <- add_argument(pars, "--rantype", help = "Random Graph Generation Type for Example Plotting", type = "character")
pars <- add_argument(pars, "--numrand", help="Number of Random Networkings for Degree Distribution Plotting", type="numeric") # only used in ran_degreedistribution plot
pars <- add_argument(pars, "--freq", help="Frequency as Edge Attribute", flag = TRUE)
pars <- add_argument(pars, "--tochannel", help="To Channel Noted", flag = TRUE)
pars <- add_argument(pars, "--labels", help="Use Labels in Some Plots", flag = TRUE) # for mutual ties and plot_postcat
pars <- add_argument(pars, "--username", help="Use Username or User ID for Labels", flag = TRUE) # for mutual ties and plot_postcat


# OPTIONS (function): postcatgrid (plot_channel_bypostcat), postcat(plot_postcat) - need joinleave flag, freq flag, tochannel flag, label flag, username flag  
                    # mutualties(plot_compare_degreeties) - need joinleave flag, freq flag, tochannel flag, label flag, username flag  
                    # og_degreedistribution (plot_degreedistrib) - need joinleave flag, freq flag, tochannel flag
                    # ran_degreedistribution (plot_degreedistrib)- need joinleave flag, freq flag, tochannel flag, numrand

# METRIC OPTIONS
# OPTIONS (function): degree (degree_centrality_matrix), avgtiestr (average_tie_strength), density (edge_density), recip (recriprocity), weightedrecip (weighted_reciprocity w/ reciprocated_adjmatrix, if needed), transit (transitivity), wgcc (weightedglobal_clusteringcoef), gcc (global_clusteringcoef) lcc (local_clusteringcoef), eigencentral (eigenvector_centrality), betacentral (beta_centrality)
    # also needs --joinleave 
pars <- add_argument(pars, "--metrics", help="Choice of metric", type="character", nargs = Inf)
pars <- add_argument(pars, "--beta", help="Choice of Beta Value for Beta Centrality", type="numeric")

# Parse the command line arguments
args_pars <- parse_args(pars)


# RUNNING THE METRICs
    # Building the correct network for the calculation (this is separated for the randomization part)
howtobuild_formetrics <-function(args){
    # Uses _freq: all metrics
    if (args_pars$metrics == "degree" || args$metrics == "density" || args$metrics == "recip" || args$metrics == "transit" || args$metrics == "wgcc" || args$metrics == "gcc" || args$metrics == "eigencentral" || args_pars$metrics == "weightedrecip" || args_pars$metrics == "avgtiestr" || args_pars$metrics == "lcc" || args_pars$metrics == "betacentral" ){
        # Load in Network in the correct way
            # Creates a network with the number of interactions between the two users as an edge attribute. This does not combine reciprocated ties.
        picup_network_channel_freq <- fulldata_To_GraphwCount(args$edge_filename, args$joinleave, to_channel = FALSE)
        picup_network_channel_freq  <- set_Node_Attributes(picup_network_channel_freq, args$node_filename,TRUE)
        
        return(picup_network_channel_freq)
    }
}

    # Running the metric for a given calculation 
howtorun_metrics <- function(args, network){
        # Basically, if we are using GraphwCount/picup_network_channel_freq, freq of edges between two users is edge attribute
    if ("count" %in% edge_attr_names(network)){
        if (args$metrics == "degree"){
            # return(degree_centrality_matrix(picup_network_channel))
            return(get_Degree_Dataframe(network,args$node_filename ))
        } else if (args$metrics == "density"){
            return(edge_density(network))
        } else if (args$metrics == "recip"){
            return(reciprocity(network))
        } else if (args$metrics == "transit"){
            return(transitivity(network, type = "global", weights = NULL))
        } else if (args$metrics == "wgcc"){
            return(weightedglobal_clusteringcoef(network))
        } else if (args$metrics == "gcc"){
            return(global_clusteringcoefficient(network))
        } else if (args$metrics == "eigencentral"){
            return(eigenvector_centrality(network))
        } else if (args$metrics == "weightedrecip"){
            return(weighted_reciprocity(network, reciprocated_adjmatrix(network)))
        } else if (args$metrics == "avgtiestr"){
            return(average_tie_strength(network, TRUE))  
        } else if (args$metrics == "lcc"){
            return(local_clusteringcoef(network))
        } else  if (args$metrics == "betacentral"){
            return(beta_centrality(network, args$beta))
        }
    }
}


# RUNNING THE PLOTS
howtorun_plots<-function(args){
    if(args$plot == "postcatgrid"){
        if(args$freq == TRUE){
            # USE fulldata_To_GraphwPostCatCount
                # Flags in here join/leave, tochannel
            picup_network_channel_pcfreq <- fulldata_To_GraphwPostCatCount(args$edge_filename, join_leave = args$joinleave, to_channel = args$tochannel)
            picup_network_channel_pcfreq  <- set_Node_Attributes(picup_network_channel_pcfreq, args$node_filename, freq = args$freq)

            plot_channel_bypostcat(picup_network_channel_pcfreq, frequency = args$freq, to_channel = args$tochannel, channel_name = paste(args$channelname, "j/l= ", args$joinleave), savedirectory = args$sd)
        } else if (args$freq == FALSE){
            picup_network_channel <- fulldata_To_noPostCatCount(args$edge_filename, join_leave = args$joinleave) 
            picup_network_channel <- set_Node_Attributes(picup_network_channel, args$node_filename, freq = args$freq)

            plot_channel_bypostcat(picup_network_channel, frequency = args$freq, to_channel = TRUE, channel_name = paste(args$channelname, "j/l= ", args$joinleave), savedirectory = args$sd)
        }
    }else if(args$plot == "postcat"){
        if(args$freq == TRUE){
            # USE fulldata_To_GraphwPostCatCount
                # Flags in here join/leave, tochannel
            picup_network_channel_pcfreq <- fulldata_To_GraphwPostCatCount(args$edge_filename, join_leave = args$joinleave, to_channel = args$tochannel)
            picup_network_channel_pcfreq  <- set_Node_Attributes(picup_network_channel_pcfreq, args$node_filename, freq = args$freq)

            if (args$labels == TRUE){
                plot_postcat_wlabels(picup_network_channel_pcfreq, post_cat_count = args$freq, to_channel = args$tochannel, username = args$username, channel_name = paste(args$channelname, "j/l= ", args$joinleave), savedirectory = args$sd)
            }else{
                plot_postcat(picup_network_channel_pcfreq, post_cat_count = args$freq, to_channel = args$tochannel, channel_name = paste(args$channelname, "j/l= ", args$joinleave), savedirectory = args$sd)
            }
        } else if (args$freq == FALSE){
            picup_network_channel <- fulldata_To_noPostCatCount(args$edge_filename, join_leave = args$joinleave) 
            picup_network_channel <- set_Node_Attributes(picup_network_channel, args$node_filename, freq = args$freq)

           if (args$labels == TRUE){
                plot_postcat_wlabels(picup_network_channel, post_cat_count = args$freq, to_channel = TRUE, username = args$username, channel_name = paste(args$channelname, "j/l= ", args$joinleave), savedirectory = args$sd)
            }else{
                plot_postcat(picup_network_channel, post_cat_count = args$freq, to_channel = TRUE, channel_name = paste(args$channelname, "j/l= ", args$joinleave), savedirectory = args$sd)
            }
        }
    }else if(args$plot == "plotnetwork"){
        if(args$freq == TRUE){
            picup_network_channel_freq <- fulldata_To_GraphwCount(args$edge_filename, join_leave = args$joinleave, to_channel = args$tochannel)
            picup_network_channel_freq <- set_Node_Attributes(picup_network_channel_freq, args$node_filename, freq = args$freq)
            plot_network(picup_network_channel, frequency = args$freq,  to_channel = args$tochannel, directed = TRUE, channel_name = paste(args$channelname, "j/l= ", args$joinleave), savedirectory = args$sd)
        }else if(args$freq == FALSE){
            picup_network_channel <- fulldata_To_noPostCatCount(args$edge_filename, join_leave = args$joinleave)
            picup_network_channel <- set_Node_Attributes(picup_network_channel, args$node_filename, freq = args$freq)
            plot_network(picup_network_channel, frequency = args$freq,  to_channel = args$tochannel,  directed = TRUE, channel_name = paste(args$channelname, "j/l= ", args$joinleave), savedirectory = args$sd)
        }
    }else if(args$plot == "mutualties"){
        if(args$freq == TRUE){
            picup_network_channel_freq <- fulldata_To_GraphwCount(args$edge_filename, join_leave = args$joinleave, to_channel = args$tochannel)
            picup_network_channel_freq <- set_Node_Attributes(picup_network_channel_freq, args$node_filename, freq = args$freq)
            

            if (args$labels == TRUE){
                plot_compare_degreeties_wlabels(picup_network_channel_freq, frequency = args$freq,  to_channel = args$tochannel, username = args$username, channel_name = paste(args$channelname, "j/l= ", args$joinleave), savedirectory = args$sd)

            }else{
                plot_compare_degreeties(picup_network_channel_freq, frequency = args$freq,  to_channel = args$tochannel, channel_name = paste(args$channelname, "j/l= ", args$joinleave), args)
            }

        }else if(args$freq == FALSE){
            picup_network_channel <- fulldata_To_noPostCatCount(args$edge_filename, join_leave = args$joinleave)
            picup_network_channel <- set_Node_Attributes(picup_network_channel, args$node_filename, freq = args$freq)

            if (args$labels == TRUE){
                plot_compare_degreeties_wlabels(picup_network_channel, frequency = args$freq,  to_channel = args$tochannel, username = args$username, channel_name = paste(args$channelname, "j/l= ", args$joinleave), savedirectory = args$sd)

            }else{
                plot_compare_degreeties(picup_network_channel, frequency = args$freq,  to_channel = args$tochannel, channel_name = paste(args$channelname, "j/l= ", args$joinleave), args)
            }
        }
    } ### CHANNEL SPECIFIC FOLDERS NEED TO BE SET UP PRIOR, LOOK AT GLOWSCRIPT AS EXAMPLE
    else if(args$plot == "og_degreedistribution"){
        print("reached here")
        if(args$freq == TRUE){
            picup_network_channel_freq <- fulldata_To_GraphwCount(args$edge_filename, join_leave = args$joinleave, to_channel = args$tochannel)
            print(V(picup_network_channel_freq))
            picup_network_channel_freq <- set_Node_Attributes(picup_network_channel_freq, args$node_filename, freq = args$freq)
            print("reached here")

            plot_degreedistrib(input = picup_network_channel_freq, network = TRUE, channel_name = args$channelname, args = args)

        }else if(args$freq == FALSE){
            picup_network_channel <- fulldata_To_noPostCatCount(args$edge_filename, join_leave = args$joinleave)
            picup_network_channel <- set_Node_Attributes(picup_network_channel, args$node_filename, freq = args$freq)

            plot_degreedistrib(input = picup_network_channel_freq, network = TRUE, channel_name = args$channelname,  savedirectory = args$sd)
         }

    } 
}




# RUNNING THE RANDOMIZATION
howtorun_randomization <- function(args){
    # first need to build the network properly for the metric specified
    picup_network_channel <- howtobuild_formetrics(args)
    # then run the metric for that network
    og_metric_value <- howtorun_metrics(args, picup_network_channel)
    # print(og_metric_value)

    # Call function that generates a set number of random graphs that look similar to picup_network_channel
    total_runs <- 3000
    list_rand_net <- c()
    ran_net_metrics <- rep(0, total_runs)

    start_time <- Sys.time()
    for(i in 1:total_runs){
        if (i %% 100 == 0){
            print(i)
        }
        ran_network <- generate_random_graphs(picup_network_channel)
        list_rand_net <- append(list_rand_net,ran_network)
        ran_net_metrics[i] <- howtorun_metrics(args, ran_network)
    }
    end_time <- Sys.time()
    # print(end_time-start_time)
  
    # Add attr to the last random network for plotting
    ran_net_attr <- set_Node_Attributes(ran_network, NA, TRUE)
    print(V(ran_net_attr)$degree)

    # Plot random network  - NEED DW in some format as paper
    plot_compare_degreeties(ran_net_attr, TRUE, FALSE, "Random Network", args)

    # Plot associated degree distribution 
    plot_degreedistrib(ran_net_attr, TRUE, "Random Network", args)

    prob_value = length(which(ran_net_metrics < og_metric_value))/total_runs

    ret_list <- list("ran_met_list" = ran_net_metrics, "og_value" = og_metric_value, "total_runs" = total_runs, "probability" = prob_value)
    return (ret_list)
  }

howtorun_configuration <- function(args){
    # first need to build the network properly for the metric specified
    picup_network_channel <- howtobuild_formetrics(args)
    # then run the metric for that network
    og_metric_value <- howtorun_metrics(args, picup_network_channel)

    # Call function that generates a set number of random graphs that look similar to picup_network_channel
    total_runs <- 3000
    list_rand_net <- c()
    ran_net_metrics <- rep(0, total_runs)

    start_time <- Sys.time()
    for(i in 1:total_runs){
        if (i %% 100 == 0){
            print(i)
        }
        ran_network <- generation_configuration_model(picup_network_channel, shuffle = FALSE)
        list_rand_net <- append(list_rand_net,ran_network)
        ran_net_metrics[i] <- howtorun_metrics(args, ran_network)
    }
    end_time <- Sys.time()
    # print(end_time-start_time)
  
    # Add attr to the last random network for plotting
    ran_net_attr <- set_Node_Attributes(ran_network, NA, TRUE)
    print(V(ran_net_attr)$degree)

    # Plot random network  - NEED DW in some format as paper
    plot_compare_degreeties(ran_net_attr, TRUE, FALSE, "Random Network", args)

    # Plot associated degree distribution 
    plot_degreedistrib(ran_net_attr, TRUE, "Random Network", args)

    prob_value = length(which(ran_net_metrics < og_metric_value))/total_runs

    ret_list <- list("ran_met_list" = ran_net_metrics, "og_value" = og_metric_value, "total_runs" = total_runs, "probability" = prob_value)
    return (ret_list)
}


howtorun_price <- function(args){
    # first need to build the network properly for the metric specified
    picup_network_channel <- howtobuild_formetrics(args)
    # then run the metric for that network
    og_metric_value <- howtorun_metrics(args, picup_network_channel)

    # Call function that generates a set number of random graphs that look similar to picup_network_channel
    total_runs <- 500
    list_rand_net <- c()
    ran_net_metrics <- rep(0, total_runs)

    for(i in 1:total_runs){
        start_time <- Sys.time()
        ran_network <- generate_price_variant(picup_network_channel)
        # print(ran_network)
        list_rand_net <- append(list_rand_net,ran_network)
        # print(howtorun_metrics(args, ran_network))
        ran_net_metrics[i] <- howtorun_metrics(args, ran_network)
        
        end_time <- Sys.time()
        
        if (i %% 10 == 0){
            print(i)
            print(end_time-start_time)
        }
    }
  
    # Add attr to the last random network for plotting
    ran_net_attr <- set_Node_Attributes(ran_network, NA, TRUE)
    # print(V(ran_net_attr)$degree)

    # Plot random network  - NEED DW in some format as paper
    plot_compare_degreeties(ran_net_attr, TRUE, FALSE, "Random Network", args)

    # Plot associated degree distribution 
    plot_degreedistrib(ran_net_attr, TRUE, "Random Network", args)

    prob_value = length(which(ran_net_metrics < og_metric_value))/total_runs

    ret_list <- list("ran_met_list" = ran_net_metrics, "og_value" = og_metric_value, "total_runs" = total_runs, "probability" = prob_value)
    return (ret_list)
}


main <- function(args){
    if (args$which == "met"){
        print(howtorun_metrics(args, howtobuild_formetrics(args)))
        # print(howtorun_metrics(args))
    }else if(args$which == "plot"){
        howtorun_plots(args)
    }else if(args$which == "met" && args$which == "plot"){
        print(howtorun_metrics(args, howtobuild_formetrics(args)))
        howtorun_plots(args)
    }else if (args$which == "randomization"){
        return_list <- howtorun_randomization(args)
        stat_histogram_for_rand(return_list$ran_met_list, return_list$og_val, return_list$total_runs,  return_list$probability, args, "Erdos Renyi")
        print(paste("Probability of getting a value less than the original value is: ", return_list$probability))
        print(return_list$ran_met_list)
    }else if (args$which == "configuration"){
        return_list <- howtorun_configuration(args)
        stat_histogram_for_rand(return_list$ran_met_list, return_list$og_val, return_list$total_runs,  return_list$probability, args, "Configuration")
        print(paste("Probability of getting a value less than the original value is: ", return_list$probability))
        print(return_list$ran_met_list)
    }else if (args$which == "price"){
        return_list <- howtorun_price(args)
        stat_histogram_for_rand(return_list$ran_met_list, return_list$og_val, return_list$total_runs,  return_list$probability, args, "Price Variant")
        print(paste("Probability of getting a value less than the original value is: ", return_list$probability))
        print(return_list$ran_met_list)
    }
}

main(args_pars)



# FLAGS: JOIN/LEAVE, FREQUENCY (POST CAT OR NORMAL), TO CHANNEL (NOPOSTCATCOUNT HAS AUTOMATICALLY), LABELS Flag
# ### GETTING THE NETWORKS 
# * picup_network_channel -> fulldata_To_noPostCatCount(args$edge_filename, join_leave = TRUE) -> join/leave, post category as edge attr, no post count, to channel flag (automatically added)

# * picup_network_channel_nojoin OR picup_network_channel_nojoin_C ->  fulldata_To_noPostCatCount(args$edge_filename, join_leave = FALSE) -> no join/leave, post category as edge attr, no post count, to channel flag (automatically added)


# * picup_network_channel_pcfreq -> fulldata_To_GraphwPostCatCount(args$edge_filename, join_leave = TRUE, to_channel = FALSE)-> join/leave, post categories and count as edge attribute, no to_channel flag
# * picup_network_channel_pcfreq_C -> fulldata_To_GraphwPostCatCount(args$edge_filename, join_leave = TRUE, to_channel = TRUE)-> join/leave, post categories and count as edge attribute, to_channel flag

# * picup_network_channel_nojoin_pcfreq -> fulldata_To_GraphwPostCatCount(args$edge_filename, join_leave = FALSE, to_channel = FALSE)-> no join/leave, post categories and count as edge attribute, no to_channel flag
# * picup_network_channel_nojoin_pcfreq_C -> fulldata_To_GraphwPostCatCount(args$edge_filename, join_leave = FALSE, to_channel = TRUE)-> no join/leave, post categories and count as edge attribute, to_channel flag



# ### PLOT CHANNEL BY POSTCAT IN GRID
# * plot_channel_bypostcat w/ picup_network_channel - plot_postcat(picup_network_channel, post_cat_count = FALSE, to_channel = FALSE, channel_name = args$channelname, savedirectory = args$sd)
# * plot_channel_bypostcat w/ picup_network_channel - plot_postcat(picup_network_channel, post_cat_count = FALSE, to_channel = TRUE, channel_name = args$channelname, savedirectory = args$sd) -> FOR TOCHANNEL EDGES
                
# * plot_channel_bypostcat w/ picup_network_channel_nojoin - plot_postcat(picup_network_channel_nojoin, post_cat_count = FALSE, to_channel = FALSE, channel_name = paste(args$channelname, "w/o j/l"), savedirectory = args$sd)
# * plot_channel_bypostcat w/ picup_network_channel_nojoin - plot_postcat(picup_network_channel_nojoin, post_cat_count = FALSE, to_channel = TRUE, channel_name = paste(args$channelname, "w/o j/l"), savedirectory = args$sd)-> FOR TOCHANNEL EDGES

# * plot_channel_bypostcat w/ picup_network_channel_pcfreq - plot_postcat(picup_network_channel_pcfreq, post_cat_count = TRUE, to_channel = FALSE, channel_name = args$channelname), savedirectory = args$sd)
# * plot_channel_bypostcat w/ picup_network_channel_pcfreq_C - plot_postcat(picup_network_channel_pcfreq_C, post_cat_count = TRUE, to_channel = TRUE, channel_name = args$channelname), savedirectory = args$sd)

# * plot_channel_bypostcat w/ picup_network_channel_nojoin_pcfreq - plot_postcat(picup_network_channel_nojoin_pcfreq, post_cat_count = TRUE, to_channel = FALSE, channel_name = paste(args$channelname, "w/o j/l"), savedirectory = args$sd)
# * plot_channel_bypostcat w/ picup_network_channel_nojoin_pcfreq_C - plot_postcat(picup_network_channel_nojoin_pcfreq_C, post_cat_count = TRUE, to_channel = TRUE, channel_name = paste(args$channelname, "w/o j/l"), savedirectory = args$sd)



### PLOT CHANNEL BY POSTCAT IN GRID
# Same situation as above 

# ### PLOT MUTUAL TIES

#  picup_network_channel <- fulldata_To_noPostCatCount(args$edge_filename, join_leave = TRUE) 
#  picup_network_channel_nojoin <- fulldata_To_noPostCatCount(args$edge_filename, join_leave = FALSE) 

# * picup_network_channel_freq -> fulldata_To_GraphwCount(args$edge_filename, join_leave = TRUE, to_channel = FALSE) -> join/leave, count as edge attribute, no to channel
# * picup_network_channel_nojoin_freq -> fulldata_To_GraphwCount(args$edge_filename, join_leave = FALSE, to_channel = FALSE) -> no join/leave, count as edge attribute, no to channel

# * picup_network_glow_freq_C <- fulldata_To_GraphwCount(args$edge_filename, join_leave = TRUE, to_channel = TRUE) -> no join/leave, to_channel flag
# * picup_network_glow_nojoin_freq_C <- fulldata_To_GraphwCount(args$edge_filename, join_leave = TRUE, to_channel = TRUE) -> no join/leave, to_channel flag
  

# TESTING for Github 