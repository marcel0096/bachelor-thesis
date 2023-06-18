""" Adapted from a different student project, original code sent by thesis advisor """

import boto3
from datetime import date
import pandas as pd


# get all entries with calling func of AWS API
def get_json(func, **kwargs):
    response = func(**kwargs)
    nextToken = kwargs['NextToken'] = response.get('NextToken')
    result = response['ReservedInstancesOfferings']
    if not nextToken:
        return []
    result += get_json(func, **kwargs)
    return result


# Connecting to client, use pre-configured keys
ec2 = boto3.client('ec2', region_name='us-east-1')

# Create filters
availabilityZone1a = 'us-east-1a'
availabilityZone1b = 'us-east-1b'
availabilityZone1c = 'us-east-1c'
includeMarketplace = False
instanceTenancy = 'default'
filters = [{'Name': 'product-description', 'Values': ['Linux/UNIX']}]

search_rates_1a = get_json(ec2.describe_reserved_instances_offerings,
                           AvailabilityZone=availabilityZone1a,
                           IncludeMarketplace=includeMarketplace,
                           InstanceTenancy=instanceTenancy,
                           Filters=filters)

search_rates_1b = get_json(ec2.describe_reserved_instances_offerings,
                           AvailabilityZone=availabilityZone1b,
                           IncludeMarketplace=includeMarketplace,
                           InstanceTenancy=instanceTenancy,
                           Filters=filters)

search_rates_1c = get_json(ec2.describe_reserved_instances_offerings,
                           AvailabilityZone=availabilityZone1c,
                           IncludeMarketplace=includeMarketplace,
                           InstanceTenancy=instanceTenancy,
                           Filters=filters)

# convert to pandas dataframe and save as csv
path = '~/bachelor-thesis/aws_raw_data/'

df1 = pd.json_normalize(search_rates_1a)
name = 'RI_rates_useast1a_' + str(date.today()) + '.csv'
df1.to_csv(path + name, index=False)

df2 = pd.json_normalize(search_rates_1b)
name = 'RI_rates_useast1b_' + str(date.today()) + '.csv'
df2.to_csv(path + name, index=False)

df3 = pd.json_normalize(search_rates_1c)
name = 'RI_rates_useast1c_' + str(date.today()) + '.csv'
df3.to_csv(path + name, index=False)

exit(1)
