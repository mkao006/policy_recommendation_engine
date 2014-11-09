# Policy Recommendation Engine


## About this project

This is the repository for the policy recommendation engine. The aim
is to assist countries lacking statistical and analytical capacity to
identify policies and areas which are in most need.

## Why do we need this engine

There are 210 countries and territories in the world, of which at
least a third of countries has an undernourishment greater than the
accepted threshold of 5%.

Hunger as we know it, is a complex issue. Ranging from the
availability of production and acceesibility by both physical and
monetary means, to the utilization of resources and
vulnerability. Yet, often this is not the only problem countries face;
thus agencies working towards eradicating hunger often face contraints
in budget and resources to achieve its goal. In particular, skilled
statistician and policy analysts are in high demand to provide support
to decision making.

The goal of this recommendation engine is to bring together the data
and policies to identify key shortage in order to eradicate hunger and
also recommend appropriate policies based on similar established
policies and practices.


## What gap does the engine fill in?

### Imputation:

The first problem encountered by countries when dealing with data is
the presense of missing values. Often adhoc and statistically unsound
imputation methodology were used to fill in the missing values. The
use of invalid imputatino procedures can and often result in
undesirable result such as self-fulfilling and invalid inference.

The missing values in the engine are imputed using the Amelia II
package with well tuned parameters and priors for the best result. The
data is also multiplicatively impute several times in order for sound
valid statistical inference. 

### Multivariate analysis:

As stated above, hunger is a complex issue involving
multi-disciplinary knowledge. The complex problem involves tens and
maybe hundreds of indicators to be analyzed. Solving problem requires
focus of resources and energies in the most burning areas, the engine
will identify the key areas which the countries may focus and result
in improvements in the situation of the country.

To know where to go, you need to know where you are and where the
remaining of the world rests. The engine uses multivaraite analysis
such as nearest neighbourgh to locate countries which faced similar
situations in order for analyst and policy makers to decide how they
would tackle the problem

### Recommendation:

Knowing where you are is the first step, the next is where to go. The
key component of the engine is to identify policies and efforts which
has deemed to be successful in the past for potential starting point
for the country.

## How does the engine work


## Project Timeline

### Phase (1)

* A global network portraying the current state of the food insecurity
  in order to identify the current position of the country.

* A panel which identify dimensions in which countries are performing
  poor and well with respect to countries which has achieved zero
  hunger.

* Recommendation page which identifies countries of similar profile
  and also overview of countries commitment and action towards
  eradicating hunger. (Research from SOFI and other paper).

* Initial setup of the featured project page with a draft of the "food
  reach" project.


Deliverable:

1. Home page depicting the state network and performance.
2. Recommendation with country overview
3. Featured analysis page with Food reach project.

Deadline: 10th of December

### Phase (2)

* A log-in mechanism in order to record information of the user, location, affiliation etc.

* A data base in which relevant research working paper and policies can be uploaded and saved.

* A document harvestor and repository to harveset policy paper from
  other policy data bases and governmental websites.

* Integration of the policy database to the recommendation engine page.

* A designer to design the home page.

Deadline: 1st of June

### Phase (3) 

* Promotion of the engine to FAO and countries of interest.

* Integration of crowd voting mechanism for identification of successful policies.