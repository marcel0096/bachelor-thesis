""" Adapted from a different student project, original code sent by thesis advisor """

import json
from datetime import date
import pandas as pd
import requests

url: str = 'https://spot-bid-advisor.s3.amazonaws.com/spot-advisor-data.json'

try:
    response = requests.get(url=url)
    frequencies: dict = json.loads(response.text)['spot_advisor']['us-east-1']['Linux']

except requests.exceptions.RequestException or requests.exceptions.ConnectionError as ex:
    print('Request exception: ', ex)
    exit(1)

interruption_frequencies: dict = {
    0: '<5%',
    1: '5-10%',
    2: '10-15%',
    3: '15-20%',
    4: '>20%'
}

name = 'rates_' + str(date.today()) + '.csv'

df: pd.DataFrame = pd.DataFrame.from_dict(
    frequencies,
    orient='index',
)
df['instanceType'] = frequencies.keys()
df.reset_index(drop=True, inplace=True)
df['r'] = df['r'].map(interruption_frequencies)
df.rename(columns={'s': 'savingsOverOnDemand', 'r': 'interruptionFrequency'}, inplace=True)
df.to_csv('~/bachelor-thesis/aws_raw_data/spot_interruption_freq_' + str(date.today()) + '.csv', index=False)

exit(1)
