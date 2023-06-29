# Thesis structure - first draft

## Introduction

- Mehr Motivation -> viele instanzen -> viele preispläne
- Führt dazu dass einfach irgendwas geholt wird -> muss besser gehen

- One or two introductory sentences about cloud computing (pay-as-you-go, flexibility to scale up and out, high reliability, ...)
- Gegenbewegung gibt es auch -> zeigt dass es oft nicht gezielt durchgerechnet wurde -> Bedarf für automatisierte Lösung
- What and why are there so many different services and price plans, particulary for VM services (as the foundation fo cloud services)

- Show Examples from Google, Azure and AWS -> see first Presentation
- Show how much money can be saved by not just buying some random option on demand, but by carefully selecting a suitable VM with an appropriate price plan. Show that this can be optimized by splitting the VM selection across multiple instances with multiple price plans (e.g., Reserved Instance + Spot Instance)
- Show that this is very hard and tedious to do manually, and thus real money is wasted (and that there is, to this point, no automated way of doing this) -> i.e. relevance of this thesis (maybe reference to another paper?)

- Use "The Cloud Computing Book – The Future Of Computing Explained, Douglas Comer, 2021" for explaining e.g. migration between instances.


## Terms and Definitions

- Explain all terms used -> put in Background (danach muss man die Arbeit verstehen können)


## Background and Related Work

- Explain what the different price plans mean? -> How detailed? -> Maybe high level which then applies to all cloud providers
- Maybe put it behind approach and evaluation as there is not too much?
- Further development of the idea of the paper "towards cost-optimal query processing in the cloud" -> cloud services far from being cost optimal
- Paper that explored the optimal instance configuartion for cloud workloads (e.g. Bilal et al., Kaulakiene et al.)
- Paper Maxi



## Approach

- Basic Idea: Get aws EC2 pricing data and write an algorithm that uses this data and a target workload as an input parameter to determine the price optimal instance configuration (i.e. which instance type and how many of them)
- Iterativen Schritte beschreiben -> leicht anfangen -> fluctuations einfügen -> etc.

- Algorithm: Central Assumption -> Only vCPUs matter
	- Input parameter: Required 
		- CPU hours per hour: Base Workload (integer)
		- CPU hours per hour: Fluct Workload (array of integer), 24 values for 24 hours of one day
		- CPU hours per hour: Migration costs (integer) -> how much CPU time is needed to migrate a workload from one instance (or instance cluster) to another
	- Input parameter: Optional (provided with default value)
		- amdahl.param  (percentage in decimal), default value: 0.95 -> the percentage of the workload to be parallelized
		- amdahl.max (integer) -> Maximum number of instances possible to parallelize before amdahls formula goes to infinity
		- plan (string) -> valid values are OD, RI and SP, default values: all
		- type (string) -> valid values are standard, convertible, Compute, EC2Instance, default values: all 
                - duration (string) -> 1, 3, default values: all
		- payment (string) ->  All, Partial, No, default values: all
	- Output: On terminal and as data frame
		- Base workload: Name of the instance, total costs per hour, number of instances, information about plan, type, duration and payment
		- Fluct workload: Name of the instance for each hour, total costs per hour, number of instances, plan, migration costs (in $), migration time (in CPU hours)	
	- How it works:
		- Base workload gets calculated by using the CPU hour requirements for the base workload, amdahl, plan, type, duration and payment
		- A working data set is created, in which the adapted spot prices and instances required after the application of amdahls law are calculated
		- Outer loop goes through each value in the array of fluct workloads
		- Inner loop goes through each row of the working data set to determine cheapest instance for the current value in the array
		-> Stats of the cheapest instance configuration are saved in variables to use after the inner loop
		- After the inner loop, the goal of the logic is to check whether it makes sense to migrate to another instance or to stay at the previously used, even if it may not be the cheapest determined by the inner loop. This can be the case due to the migration costs that are added to the "normal" spot price if one has to migrate. 

- Data: retrieved 18.06.2023
	- Which data aka which price plans should be included in the algorithm? -> On demand, Reserved Instances (shared tenancy: standard and convertible), Savings Plans (shared tenancy: EC2Instance and compute), Spot Instances 
	- Data fetched via AWS API "boto3" for aws region us-east-1 (North Virginia)
	- As Reserved Instances are AZ specific, prices for AZs 1a, 1b and 1c were merged together by using the average price for one particular instance over all three AZs 
	- For spot instances, the prices for the same three AZs as for RIs where averaged over the course of the last three months (also one data set with minimum value of last three months)
	- To also model that spot instances can be interrupted and that in the case of an interuption, workloads have to be migrated to another instance, spot prices were adapted by their interruption frequency (corrected for one hour) and the migration costs put into the algorithm -> show formula?   
	- Burstable instances were excluded due to their specific way they work 
	- Only instances that have data on all pricing options possible were included, resulting in a working data set of 566 different instances



## Evaluation

- Evaluating algorithm using different input based on szenarios that aim to cover different company sizes
- It shows that even with the basic version of the algorithm, money can be saved compared to similar on prem solutions
- Using the full algorithm, even more money can be saved by perfectly covering spikes in the workload with the best spot instances -> this effect is best when the base workload covers workloads up to the minimum of the spikes, and the rest is covered by the fluctuating workload
- If one sets the base workload even lower, costs begin to raise again as spot prices can't compete with e.g. 3 year reserved instances of a certain size and type
- The best savings on base loads can be achieved by being flexible between RI and SP options and commiting to at least 3 years



## Future Work

- everything that can be optimized but was too much for this thesis
- e.g. not only use vCPUs as metric, but also other stats like memory and network



## Conclusion





