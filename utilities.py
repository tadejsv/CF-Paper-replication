import pandas as pd
import numpy as np

from scipy.stats.mstats import winsorize

# Winsorize some vars (note: we are limiting them to [0.02, 0.97th] percentile)
def winsor(x, l = 0.02, u = 0.03):
    return  winsorize(x, (l, u))
    
# Function that mimics Stata's rowtotal
def rowtotal(data, cols, signs = None):
    if signs == None:
        signs = np.repeat(1, len(cols))
    else:
        signs = np.array(signs)

    result = (data[cols] * signs).sum(axis = 1, min_count = 1)
            
    return result
    
# Assumes data is sorted from  largest to smallest
def top_x_share(data, x = 5):
    return data.iloc[0:x].sum()/data.sum()

# Weighted mean function, takes care of NAs
def wt_mean(x, weights):
    prod = x * weights
    return ((x * weights) / weights[prod.notna()].sum()).sum()

# Helper function to take space-delimited list of vars and transform to list
def str_to_list(string):
    return [x.strip() for x in string.split(' ') if len(x.strip()) > 0]

# Helper function to transform stata rename commands to a dictionary to be used
# for pandas renaming
def rename_str_to_dict(string):
    command_list = string.replace('rename', '').split('\n')
    command_list = list(map(str_to_list, command_list))
    command_list = [x for x in command_list if len(x) == 2]
    
    rename_dict = {}
    for pair in command_list:
        rename_dict[pair[0]] = pair[1]

    return rename_dict
