import requests
import json

import pandas as pd

class BEADownloader(object):
    '''
    This is a helper class for downloading tables from BEA's api.
    It has to be initialized with your unique BEA key.
    
    Its main method is `get_table`, which, for a specified database 
    and table, downloads the table, filters columns (if specified),
    formats and sorts date and returns a Pandas dataframe.
    
    There is also a helper function to diplay available
    databases and tables for each database.
    '''
    
    def __init__(self, api_key):
        self.key_bea = api_key
        
    @property
    def _base_url_bea(self):
        return 'http://apps.bea.gov/api/data?UserID=' + self.key_bea
    
    def _get_url(self, table_name, frequency='Q', database='NIPA'):
        url = self._base_url_bea + '&method=GetData' +'&DataSetName=' + database + \
              '&TableName=' + table_name + '&Frequency=' + frequency + '&Year=X'+ \
              '&ResultFormat=JSON'
    
        return url

    def _filter_data(self, results, col_names_in=None, year_begin=1960):
        # If we want just a single column, put it in a list
        if type(col_names_in != list) and col_names_in:
            col_names_in = [col_names_in]
            
        data = pd.read_json(json.dumps(results))
        data['TimePeriod'] = pd.to_datetime(data['TimePeriod'].astype(str))
    
        # Filter to the columns we need
        if col_names_in:
            data = data.query('SeriesCode.isin(@col_names_in)')
        
        data = data[['DataValue', 'TimePeriod', 'SeriesCode', 'LineDescription']]
    
        data['TimePeriod'] = pd.to_datetime(data['TimePeriod'])
        data.rename(columns = {'TimePeriod': 'date', 'DataValue': 'value'}, inplace = True)
        data = data.loc[data['date'] >= (str(year_begin) + '-01-01')]\
                .set_index('date').sort_values('date')

        # Commas are a thousands separator, pandas somehow doesn't get it - fix this manually
        if not pd.api.types.is_numeric_dtype(data['value']):
            data['value'] = pd.to_numeric(data['value'].str.replace(',', ''))
    
        return data

    def get_table(self, table_name, col_names_in = None, frequency='Q', 
                  database='NIPA'):

        r = requests.get(self._get_url(table_name, frequency, database))
        data = self._filter_data(r.json()['BEAAPI']['Results']['Data'], col_names_in)
        
        return data    
    
    def list_databases(self):
        r = requests.get(self._base_url_bea + '&method=GetDataSetList' + '&ResultFormat=JSON')
        json_r = r.json()['BEAAPI']['Results']['Dataset']
        
        databases = pd.read_json(json.dumps(json_r))
        return databases
    
    def list_tables(self, database, param_name = 'TableName'):
        r = requests.get(self._base_url_bea + '&method=GetParameterValues' +\
                         '&datasetname=' + database + '&ParameterName=' + param_name +\
                         '&ResultFormat=JSON')
        json_r = r.json()['BEAAPI']['Results']['ParamValue']

        tables = pd.read_json(json.dumps(json_r))
        return tables
    
    def param_list(self, database):
        r = requests.get(self._base_url_bea + '&method=GetParameterList' +\
                         '&datasetname=' + database + '&ResultFormat=JSON')
    
        json_r = r.json()['BEAAPI']['Results']['Parameter']

        param_list = json.dumps(json_r)
        return param_list
