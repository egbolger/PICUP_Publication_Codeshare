import numpy as np
import pandas as pd
import time
import os


path = r'/Users/'

users_df = pd.read_pickle('users.pkl')
print(users_df)

df_out = pd.DataFrame(columns=['Date','Time','User_handle', 'Display_name', 'Message','@', '@_disp_name', 'Communication_type','Channel_name', 'Reply_flag', 'Reply_count', 'Reply_list'])
    # print(df_out['Message'])
for root, directories, files in os.walk(path, topdown=True):
    # SORTS files by oldest to newest within the subdirectory
    files.sort(reverse=False)
    for name in files:
        
        # KEEPING just json files
        if not name.endswith('.DS_Store') and not name.startswith("channels") and not name.startswith("integration") and not name.startswith("users") and name.endswith(".json"):
            #print(os.path.join(root, name))
            # CREATES path directory
            df_cur = os.path.join(root, name)
            df_in = pd.read_json(df_cur)
            
            # LOOPS through each entry in the file
            for i in range(0,len(df_in)):
                print(df_cur)
                # pulls the entry in the df
                entry = df_in.iloc[i]

                # GENERATES time and date
                epoch_time = entry['ts']
                theTime = time.localtime(epoch_time)
                formattedDate = time.strftime('%Y-%m-%d', theTime)
                formattedTime = time.strftime('%H:%M:%S', theTime)

                # ADDS message
                message = entry['text']
                
                #ADDS file name and title to the message, if the file contains one
                if 'files' in entry and np.all(pd.notna(entry['files']) == True):
                    # pd.notna: entry not na returns T, o/w F
                    # np.all: makes sure all the entries are T - ie there are (multiple) files to add
                    message_addon = ''
                    for file_iter in range(len(entry['files'])):
                        message_addon_name = entry['files'][file_iter]['name']  
                        message_addon_title = entry['files'][file_iter]['title']  
                        message_addon_total = ' The ' +  str(file_iter)  + ' th file name and title is ' + message_addon_name + '; ' + message_addon_title + '.'
                        message_addon += message_addon_total
                    message += message_addon
                
                # IF a message is blank and has an attachment, add the attachment name to the text of the message (eg. some giphy, shared messages)
                    # messages that are not blank and have an attachment already have the url in the text of the message
                if entry['text'] == '':
                    if 'attachments' in entry and np.all(pd.notna(entry['attachments']) == True):
                        message_attach_on = ''
                        for attach_iter in range(len(entry['attachments'])):
                            message_attachment = entry['attachments'][attach_iter]['fallback']
                            #print("message_attachment", message_attachment)
                            message_attach_on += message_attachment
                    message += message_attach_on

                # ADDS user id
                try:
                    if 'user' in entry and pd.isnull(entry['user']) == False:
                        user_handle = entry['user']
                    elif 'user' in entry['comment'] and pd.isnull(entry['comment']['user']) == False:
                        user_handle = entry['comment']['user']
                except:
                    user_handle = "NaN"
                
                # ADDS display name for user
                    # loops through the users_df to find the display name (or real name) - for join, leave, mentions, replies, emojis
                def get_Display_Name(users_df, correct_user):
                    for id_iter in range(len(users_df['id'])):
                        # if the user id equals current id 
                        if users_df['id'][id_iter] == correct_user:
                            # if display at that location is not "NaN"
                            if users_df['display_name'][id_iter] != "NaN":
                                display_name = users_df['display_name'][id_iter]
                            # if display_name is NaN use real_name
                            elif users_df['display_name'][id_iter] == "NaN":
                                display_name = users_df['real_name'][id_iter]
                    return display_name

                try:
                    # we try to pull all the info we can from the message itself first
                        # if user_profile exists and not null 
                    if 'user_profile' in entry and pd.isnull(entry['user_profile']) == False:
                        # if display name exists and not empty
                        if 'display_name' in entry['user_profile'] and entry['user_profile']['display_name'] != "":
                            disp_name = entry['user_profile']['display_name']
                            # else use real_name from the message
                        elif 'display_name' in entry['user_profile'] and entry['user_profile']['display_name'] == "":
                            disp_name = entry['user_profile']['real_name']

                    # now look through the user_df based on id and find the display_name or real_name - this is for cases like 'join','leave', 'comment', etc
                    else: #ie no user_profile
                        if 'user' in entry and pd.isnull(entry['user']) == False:
                            disp_name = get_Display_Name(users_df, entry['user'])
                        elif 'user' in entry['comment'] and pd.isnull(entry['comment']['user']) == False:
                            disp_name = get_Display_Name(users_df, entry['comment']['user'])
                except:
                    disp_name = "NaN"

                # ADDs some communication types
                if 'subtype' in entry and pd.isnull(entry['subtype']) == False:
                    if entry['subtype'] == 'channel_join':
                        comm_type = 'joined'
                    elif entry['subtype'] == 'channel_leave':
                        comm_type = 'left'
                    elif entry['subtype'] == 'channel_purpose' or entry['subtype'] == 'channel_name' or entry['subtype'] == 'channel_topic' or entry['subtype'] == 'channel_archive' or entry['subtype'] == 'channel_unarchive':
                        comm_type = 'norm'
                    # channel purpose -> channel description, channel nane -> renamed channel, channel topic -> channel topic, channel (un)archive
                    else: 
                        comm_type = ''
                else:
                    comm_type = ''

                # ADDS channel name 
                channel_name = root.split("2021/", 1)[1]

                # ADDs number of replies
                # ADDS list of userids that replied
                rep_list = []
                if 'reply_count' in entry and pd.isnull(entry['reply_count']) == False:
                    reply_count = int(entry['reply_count'])
                    for rep_name in range(0, int(len(entry["replies"]))):
                        rep_list.append(entry["replies"][rep_name]['user']) 
                    # if a user replies to own message flag it
                    for rep_name in rep_list:
                        if rep_name == user_handle:
                            rep_flag = True
                            break
                        else:
                            rep_flag = ''
                else:
                    reply_count = "0"
                    rep_list = ""
                    rep_flag = ""
                
                # REPLIES to a message using parent id 
                if 'parent_user_id' in entry and pd.isnull(entry['parent_user_id']) == False:
                    replied_to_user = entry['parent_user_id']
                else:
                    replied_to_user = "NaN"
                
                # HANDLING thread-broadcast
                    # when slack decides a thread message should be to channel, not to original message
                    # this only occurred a few times 
                if 'subtype' in entry and pd.isnull(entry['subtype']) == False:
                    if entry['subtype'] == 'thread_broadcast':
                        replied_to_user = entry['root']['user']

                # MENTIONS another user in a message
                in_at = False
                mentioned_users = []
                mentioned_users_remove = []
                prevchar = ""
                # look through all the characters in a message
                for char in message:

                    if in_at == False:
                        # continue until we find '@' (or ! with previous < for !channel) and add new entry in list
                        if char == "@" or (char == "!" and prevchar == "<"):
                            mentioned_users.append("")
                            in_at = True
                    else:
                        # once find @ continue looking through char until find > 
                        # if char in ">|., ":
                        if char == ">" or char == "|" or char == "," or char == "." or char == " ":
                            in_at = False
                        else:
                            # add the new user to the list, character by character
                            mentioned_users[-1] = mentioned_users[-1] + char
                    prevchar = char
                #print(mentioned_users)

                # KEEPS each mentioned user only once 
                mentioned_users = np.unique(mentioned_users).tolist()

                # CHECKS entries in mentioned users to make sure it is actually a user
                for user in mentioned_users:
                    if user == "":
                        mentioned_users_remove.append(user)  
                    elif user == "channel":
                        continue
                    elif user[0] != "U":
                        # if the user does not appear in the display or real name section, remve it
                        if user not in list(users_df['display_name']) and user not in list(users_df['real_name']): 
                            mentioned_users_remove.append(user)
                # REMOVE all users put in remove list
                for each in mentioned_users_remove:
                    mentioned_users.remove(each)
    
                # CHECKS if message should go to channel 
                # JOIN. LEFT logic - return "join", etc -- bool to str of kind of message
                def is_generic_message(replied_to, mentioned, user):
                    #If messgae is not a reply and has no mentions or if there is one mention and it is the user - '@' == "channel"
                    return (replied_to == "NaN" and len(mentioned) == 0) or ( len(mentioned) == 1 and mentioned[-1] == user)

                # if it's generic message, create new entry and set '@' to channel 
                if is_generic_message(replied_to_user, mentioned_users, user_handle):
                    # THE UPDATE TO COMMUNICATION TYPE WILL BE HERE (IF SUBTYPE IN ENTRY EQUAL THIS - HERE IS THE COMMUNICATION TYPE ELSE, ITS NULL)

                    new_row = {'Date': formattedDate, 'Time': formattedTime, 'User_handle': user_handle, 'Display_name': disp_name, 'Message': message, '@': "channel", '@_disp_name' : "channel", 'Communication_type': comm_type, 'Channel_name': channel_name, 'Reply_flag': rep_flag, 'Reply_count': reply_count, 'Reply_list': rep_list}
                    df_out = df_out.append(new_row, ignore_index=True)
                else:
                    # there was a mention or reply
                    reply_accounted_for = False
                    # for all the users in list of mentioned users
                    for at_user in mentioned_users:
                        # FIND display name
                        if at_user == "channel":
                            mention_display_name = "channel"
                        elif at_user in list(users_df['display_name']) or at_user in list(users_df['real_name']):
                            mention_display_name = at_user
                        else:
                            mention_display_name = get_Display_Name(users_df, at_user)

                        # accounts for if users @ themselves (mostly for user comments on user's file)
                        if len(mentioned_users) > 1 and at_user == user_handle:
                            continue #place at next user/skip this iter
                        # if a mentioned user was one replied to from the parent_id section 
                            #this eliminates a double count of '@' the user in a reply thread
                        if at_user == replied_to_user:
                            reply_accounted_for = True
                        # regardless, add a new entry with the replied or mentioned user
                        new_row = {'Date': formattedDate, 'Time': formattedTime, 'User_handle': user_handle, 'Display_name': disp_name, 'Message': message, '@': at_user, '@_disp_name': mention_display_name, 'Communication_type': comm_type, 'Channel_name': channel_name, 'Reply_flag': rep_flag, 'Reply_count': reply_count, 'Reply_list': rep_list}
                        df_out = df_out.append(new_row, ignore_index=True)

                    # if the user was not mentioned, but is a reply, add new entry 
                    if (reply_accounted_for == False and replied_to_user != "NaN"):
                        # FIND display name
                        at_display_name = get_Display_Name(users_df, replied_to_user)
                    
                        # ADD new row
                        new_row = {'Date': formattedDate, 'Time': formattedTime, 'User_handle': user_handle, 'Display_name': disp_name, 'Message': message,'@': replied_to_user, '@_disp_name': at_display_name,  'Communication_type': comm_type, 'Channel_name': channel_name, 'Reply_flag': rep_flag,'Reply_count': reply_count, 'Reply_list': rep_list}
                        df_out = df_out.append(new_row, ignore_index=True)



                # CREATES new row for emojis/ reactions to message
                try: 
                    # loop through all the reactions
                    for s in range(len(entry["reactions"])):
                        # get emoji name 
                        emoji_name = entry["reactions"][s]['name']  
                        comm_type = 'emoji'
                        # loop through all the users of that emoji 
                        for user in range(len(entry["reactions"][s]['users'])): 
                            # get user name/id 
                            emo_user = entry["reactions"][s]['users'][user]
                            # get the display name for the emoji user
                            emoji_display_name = get_Display_Name(users_df, emo_user)

                            # ADD new entry
                            new_emoji_row = {'Date':formattedDate, 'Time':formattedTime, 'User_handle' : emo_user, 'Display_name': emoji_display_name, 'Message': emoji_name, '@': user_handle, '@_disp_name': disp_name,  'Communication_type': comm_type, 'Channel_name': channel_name, 'Reply_flag': '', 'Reply_count': 0, 'Reply_list': ""}
                            df_out = df_out.append(new_emoji_row, ignore_index=True)  
                except: 
                    pass

# CONVERT to csv file
df_out.to_csv(path + 'slack_PICUP.csv')
#print(csv_test)

