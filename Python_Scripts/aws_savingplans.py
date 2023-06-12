""" Adapted from a different student project, original code sent by thesis advisor """

import boto3
from datetime import date
import pandas as pd


# get all entries with calling func of AWS API
def get_json(func, **kwargs):
    response = func(**kwargs)
    nextToken = kwargs['nextToken'] = response['nextToken']
    result = response['searchResults']
    if not nextToken:
        return []
    result += get_json(func, **kwargs)
    return result


# Connecting to client, use pre-configured keys
ec2 = boto3.client('savingsplans', region_name='us-east-1')

# Create filters
products = ['EC2']
savingsPlanTypes = ['Compute', 'EC2Instance']
serviceCodes = ['AmazonEC2']
filters = [{'name': 'region', 'values': ['us-east-1']},
           {'name': 'productDescription', 'values': ['Linux/UNIX']},
           {'name': 'tenancy', 'values': ['dedicated', 'shared']}]

search_rates = get_json(ec2.describe_savings_plans_offering_rates,
                         savingsPlanTypes=savingsPlanTypes,
                         products=products,
                         serviceCodes=serviceCodes,
                         filters=filters)

# extract properties as attributes of rates
organized_rates = []
for rate in search_rates:
    rate.pop('productType')
    rate.pop('unit')
    properties = rate.pop('properties', None)
    for property in properties:
        rate[property['name']] = property['value']
    organized_rates += [rate]

# convert to pandas dataframe and save as csv
df = pd.json_normalize(organized_rates)
path = '~/bachelor-thesis/aws_raw_data/'
name = 'SP_rates_' + str(date.today()) + '.csv'
df.to_csv(path + name, index=False)

exit(1)
