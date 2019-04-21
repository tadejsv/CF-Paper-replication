from io import BytesIO
from zipfile import ZipFile

import requests

import pandas as pd

class FFCategory(object):
    '''
    An Fama-French category converter (sic -> category). Works by
    reading a classification file from French's website, which it stores
    as a dictionary, and can later conver and pandas Series of sic numbers
    to these categories. Should work generally with any number of categories.
    '''
    
    def __init__(self, url):
        # Initialize empty cat. dict
        ff_dict = {}
        index = ''

        # Download, unzip file
        content = requests.get(url)
        f = ZipFile(BytesIO(content.content))

        # Get a dictionary of num ranges
        with f.open(f.namelist()[0]) as file:
            for line in file:
                line = line.decode("utf-8") 
                # Ignore empty (\n only) lines
                if len(line) > 2:
                    # Category lines start with category name (number)
                    if line[1].isdigit():
                        index = line.strip().split(' ')[0]
                        ff_dict[index] = []
                    # Lists of SIC numbers
                    else:
                        num_range = line.strip().split(' ')[0]
                        nums = num_range.split('-')
                
                        # Add range to dict
                        ff_dict[index].append(nums) 
        
        ff_dict.pop(index)
        self.other = int(index)
        
        # Set this dict as property
        self.ff_dict = ff_dict
                
    def assign_ff(self, sic_nums):
        # Set the default (other)
        ff_category = pd.Series(self.other, index = sic_nums.index)

        # Loop over all categories
        for index, ranges in self.ff_dict.items():
            isin = pd.Series(False, index = sic_nums.index)
            
            # Check if value is in any sic range for category
            for r in ranges:
                isin = isin | sic_nums.between(int(r[0]), int(r[1]))
    
            ff_category[isin] = int(index)
    
        # Make sure that Nones stay that way
        ff_category[sic_nums.isna()] = None
    
        return ff_category