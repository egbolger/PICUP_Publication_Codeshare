import numpy as np
import pandas as pd
import time
import os
import math

df_in = pd.read_json('users.json')
users_df = pd.DataFrame(columns=['id', 'display_name', 'real_name'])

# LOOP through all entries in df. Add id, real_name, and display_name if available. 
for i in range(0, len(df_in)):
    entry = df_in.iloc[i]
    id_val= entry['id']
    real_name = entry['profile']['real_name']
    if entry['profile']['display_name'] != "":
        display_name = entry['profile']['display_name']
    else: 
        display_name = "NaN"
    new_row = {'id' : id_val, 'display_name' : display_name, 'real_name' : real_name}
    users_df = users_df.append(new_row, ignore_index=True)


users_df.to_pickle('users.pkl')

