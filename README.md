# PICUP_Publication_Codeshare

This Github Repository hosts the code used for the submitted journal article ``Characterizing Faculty Online Learning Community Interactions Using Social Network Analysis" by Emily Bolger, Marius Nwobi, and Dr. Marcos (Danny) Caballero. 

# The Data 
For this code, a coder will need a node list and a edge list. For our Slack data, this looks like a list of the users that have joined each of the channels ('DataforPublication/FULLnodelist_cleaned.csv') and a spreadsheet of the messages for a single channel. We have attached the edgelists for the three channels that we mention in the paper - Advanced Thermodynamics ('Publication_FULLpicup_nochannels_advthermal.csv'), Jupyter ('Publication_FULLpicup_nochannels_jupyter.csv'), and Classroom Pedagogy ('Publication_FULLpicup_nochannels_classroompedagogy,csv'). As a note, we have deidentified our data, so that it can be shared with the community safely.

There are a few steps to get the data into this format. If you plan on working with Slack data, an example process for converting the exported information to a csv file can be seen in the `DataPreparation` directory. The information for the Workspace can be exported in a series of json files. Each channel has a folder which contains json files for each day there are Slack messages sent in that channel. An deidentified example of what the json file could look like is shown in `Example_SlackMessages.json`. One of the json files exported is a file called `users.json` which contains all the information Slack stores about the users in the network. `UserProfile_Dictionary.py` reads in that json file and returns a pkl file with relevant user information. `JsontoCSV.py` converts the messages from the json files to a single csv file with the user information attached. The `PICUP_data_info.txt`file contains information on how this process works and what is accounted for.

In order to use this information for SNA, we need to do an extra step of creating all the edges from the messages in the Slack channel. This process can be seen in the `PICUP_RemoveChannel_NodeListClean_FULLDATA.R` file. For computational speed and efficiency in making the networks, this file exported from this file is only for single channel. 

# The SNA Code
There are many to read in the data and create networks from it. This choice affects the type of figures and analysis that can be done later. There functionality for various stages of directedness and weightedness is accounted throughout the code base.

At the base level, for each message in the dataset, there is a edge between the user that sent the message and the user that received the message. Based on the function various other edge attributes can be added like frequency of the edge and category of the message. We also add node attributes for each of the users like their name as well as some node specific metrics like degree. See the functions for specifics on what is accounted for. 

# Pieces of the Code 

The code base of this project is expansive, especially in one file. For ease of use, I encourage coders to extract functions that are most relevant to them and adjust them for their use case, rather than try to use this code base directly. 

## Plotting Functions
There are plots for both the whole network as well as ego networks (focused on a ,,specific user). Not all the plots in this code base were used for figures in the final paper, but they should still all run correctly. Some of the plots have subfunctions to perform calculations on the data for plot (e.g. Mutual Ties). These subfunctions should all appear right before the plotting function.

Further, nearly all of the plots have if/else statements based on the whether frequency and directedness are accounted for. The structure is similar across all plot types.

## Metric Functions
Some of the metric functions are just wrappers on functions built into various network packages in R. Most were put in a function to account for some formatting that needed to be set up. 

## Randomization Functions
The implementations for each of the random network generation techniques we used are in the code base as well. They are based on the papers cited in our paper. Some network packages contain randomization generation functions, however, it is important to make sure the function is running the exact methodology you want as well as accounts for all the features in your network that you want it to. 


# Example Figures
In the `Example_Figures` directory contains (almost all of the figures) one can generate from the `PICUP_Full_Code.R` file. The file name of the plot is identical (or very close to) the name of the function that creates it. There are often different variations of the plot - including directionality and weightedness of the edges. 

# How to Run the Code
If you do want to run the code directly, there are a series of arguments you can pass which are explained in lines 1766-1788.
 
Here are a few examples of the code that one can run
- Plotting Example:
  -  Rscript PICUP_Full_Code.R "directory/to/edgelist" "directory/to/nodelist" --which "plot" --plot "postcat" --channelname "Glowscript" --sd "Figs/Glowscript/Normal/Full/" --freq --tochannel
-  Randomization Example for Erdos Method:
   - Rscript PICUP_Full_Code.R "directory/to/edgelist" "directory/to/nodelist" --which "randomization" --channelname "AdvThermal" --freq --tochannel --sd "Figs/AdvThermal/Random/Erdos/Graphs/" --metric "weightedrecip"
- Metrics Example:
  -  Rscript PICUP_Full_Code.R "directory/to/edgelist" "directory/to/nodelist" --which "met" --metrics "degree"
    




