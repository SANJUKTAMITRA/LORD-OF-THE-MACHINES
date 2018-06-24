# LORD-OF-THE-MACHINES
AN analytic vidhya hackathon

https://datahack.analyticsvidhya.com/contest/lord-of-the-machines/

Rank : 220 in public leaderboard and 101 rank in private leader board

Problem Statement : 

Email Marketing is still the most successful marketing channel and the essential element of any digital marketing strategy
Marketers spend a lot of time in writing that perfect email, labouring over each word, catchy layouts on multiple devices to get them best in-industry open rates & click rates.

Problem statement is to predict the click probability of links inside a mailer for email campaigns from January 2018 to March 2018.
DATASET DESCRIPTION
campaign_data.csv

Contains the features related to 52 email Campaigns

Variable

Definition

campaign_id

Email campaign ID

communication_type

Email agenda

total_links

Total links inside the email

no_of_internal_links

Total internal links inside the email (redirecting to analyticsvidhya.com)

no_of_images

Number of images inside the email

no_of_sections

Number of sections inside the email

email_body

Email Text

subject

Email Subject

email_url

Email URL

 

train.csv

Contains the click and open information for each user corresponding to given campaign id (Jul 17 - Dec 17)

Variable

Definition

id

Unique ID for email session

user_id

User ID

campaign_id

Email Campaign ID

send_date

Time stamp for email sent

is_open

Did not open - 0, Opened -1

is_click

Did not click - 0, clicked - 1

 
Approach:
Started with Data preparation steps, derived numerous variables and did random sampling as the ratio of negative to positive data in the training file was 98%:02% ratio

With logistic regression in R I have determined the key variables that are critical in predicting the target variable is click and 
used those variables in the Naive Bayes Algorithm and to achieve over 80% sensitivity
