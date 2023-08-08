# On automated price model choice in the cloud - Model Implementation

The implementation for the cost model introduced in the bachelor's thesis "On automated price model choice in the cloud" written at the Chair of Decentralized Information Systems and Data Management at TUM.

## Running

This version of the model is implemented in *R*. 
As such you need to have R installed on your computer, see [here](https://www.r-project.org/).
I also depend on various R packages that are not part of the basic installation.
These should install automatically when running any file. 
If they don't, please look at the list in `util.R` and install them manually.

To retrieve the AWS EC2 data please execute the Python scripts in the `Python_Scripts` directory.
To run the AWS API boto3, a Python version >= 3.8 is required.

To generate the plots used in the paper, please execute the respective code blocks in the `plots.R` file.

## Project Structure

Files of interest:
| Path | Description 
|-|-
| aws.prices.R | Code for loading and manipulating AWS instance and pricing data retrieved vai boto3. 
| model.R | The main model implementation as described in the thesis.
| szenarios.snow.R | Evaluation of the model with Snowflake data. 
| plots.r | `ggplot` sources for the plots shown in the thesis.

The main algorithms relating to the model section in the paper are implemented in the functions `create.config.dataset`, `get.base.workload`, and `find.cheapest.instance.final` in `model.R`.

Note that this is exploratory code for research purposes; code quality and maintainability are secondary.
