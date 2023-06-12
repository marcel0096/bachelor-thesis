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
availabilityZone = 'us-east-1a'  # representative for region us-east-1 as prices do not differ between AZs in one region
includeMarketplace = False
instanceTenancy = 'default'
filters = [{'Name': 'product-description', 'Values': ['Linux/UNIX']}]

search_rates = get_json(ec2.describe_reserved_instances_offerings,
                        AvailabilityZone=availabilityZone,
                        IncludeMarketplace=includeMarketplace,
                        InstanceTenancy=instanceTenancy,
                        Filters=filters)

# convert to pandas dataframe and save as csv
df = pd.json_normalize(search_rates)
path = '~/bachelor-thesis/aws_raw_data/'
name = 'RI_rates_' + str(date.today()) + '.csv'
df.to_csv(path + name, index=False)

exit(1)
