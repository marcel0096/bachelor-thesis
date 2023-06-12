""" Adapted from a different student project, original code sent by thesis advisor """

import boto3
import datetime
from datetime import date
import pandas as pd


# get all entries with calling func of AWS API
def get_json(func, **kwargs):
    response = func(**kwargs)
    nextToken = kwargs['NextToken'] = response['NextToken']
    result = response['SpotPriceHistory']
    if not nextToken:
        return []
    result += get_json(func, **kwargs)
    return result


# Connecting to client
ec2 = boto3.client('ec2', region_name='us-east-1')

# Create filters
availabilityZone1a = 'us-east-1a'
availabilityZone1b = 'us-east-1b'
availabilityZone1c = 'us-east-1c'
filters = [{'Name': 'product-description', 'Values': ['Linux/UNIX']}]

search_rates_1a = get_json(ec2.describe_spot_price_history,
                           AvailabilityZone=availabilityZone1a,
                           Filters=filters)

search_rates_1b = get_json(ec2.describe_spot_price_history,
                           AvailabilityZone=availabilityZone1b,
                           Filters=filters)

search_rates_1c = get_json(ec2.describe_spot_price_history,
                           AvailabilityZone=availabilityZone1c,
                           Filters=filters)

path = '~/bachelor-thesis/aws_raw_data/'

df1 = pd.json_normalize(search_rates_1a)
name = 'Spot_rates_useast1a_' + str(date.today()) + '.csv'
df1.to_csv(path + name, index=False)

df2 = pd.json_normalize(search_rates_1b)
name = 'Spot_rates_useast1b_' + str(date.today()) + '.csv'
df2.to_csv(path + name, index=False)

df3 = pd.json_normalize(search_rates_1c)
name = 'Spot_rates_useast1c_' + str(date.today()) + '.csv'
df3.to_csv(path + name, index=False)

exit(1)
