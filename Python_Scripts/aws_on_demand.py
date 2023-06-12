""" Adapted from a different student project, original code sent by thesis advisor """

import boto3
from datetime import date
import pandas as pd
import csv
import json


# get all entries with calling func of AWS API
def get_json(func, **kwargs):
    response = func(**kwargs)
    nextToken = kwargs['NextToken'] = response.get('NextToken')
    result = response['PriceList']
    if not nextToken:
        return []
    result += get_json(func, **kwargs)
    return result


# Connecting to client, use pre-configured keys
ec2 = boto3.client('pricing', region_name='us-east-1')

# Create filters
serviceCode = 'AmazonEC2'
filters = Filters = [
    {'Type': 'TERM_MATCH', 'Field': 'termType', 'Value': 'OnDemand'},
    {'Type': 'TERM_MATCH', 'Field': 'location', 'Value': 'US East (N. Virginia)'},
    {'Type': 'TERM_MATCH', 'Field': 'operatingSystem', 'Value': 'Linux'},
    {'Type': 'TERM_MATCH', 'Field': 'tenancy', 'Value': 'Shared'},
    {'Type': 'TERM_MATCH', 'Field': 'preInstalledSw', 'Value': 'NA'}
]

search_rates = get_json(ec2.get_products,
                        ServiceCode=serviceCode,
                        Filters=filters)

instance_types = []
hourly_prices = []
regions = []
vCpus = []
memories = []
networks = []

# Parse JSON data
for json_data in search_rates:

    data = json.loads(json_data)

    # Extract required information
    instance_type = data['product']['attributes']['instanceType']
    region = data['product']['attributes']['location']
    vCpu = data['product']['attributes']['vcpu']
    memory = data['product']['attributes']['memory']
    network = data['product']['attributes']['networkPerformance']
    for on_demand in data['terms']['OnDemand'].values():
        for price_dimensions in on_demand['priceDimensions'].values():
            hourly_price = price_dimensions['pricePerUnit']['USD']

    # Append the extracted information to the respective lists
    instance_types.append(instance_type)
    hourly_prices.append(hourly_price)
    regions.append(region)
    vCpus.append(vCpu)
    memories.append(memory)
    networks.append(network)

df = pd.DataFrame({
    'Instance Type': instance_types,
    'Region': regions,
    'vCpus': vCpus,
    'Memory': memories,
    'Network': networks,
    'Hourly Price': hourly_prices,
})
path = '~/bachelor-thesis/aws_raw_data/'
name = 'OD_rates_' + str(date.today()) + '.csv'
df.to_csv(path + name, index=False)

exit(1)
