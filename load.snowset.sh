#!/bin/bash

# download the raw dataset as csv

echo "Downloading csv datasets..." # out=csv-download.log

wget http://www.cs.cornell.edu/~midhul/snowset/snowset-main.csv.gz

# wait for downloads to finish

wait

echo "Download successful. Please now extract the dataset to the aws_raw_data/ directory."
