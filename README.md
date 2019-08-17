# Loan_Prediction
Predict the amount of loan repaid and the status of the loan as of the 360th day 

Attached alongside this file is a data file named: `Data Science Assessment.csv`. Each row in this file consists of a single loan. Each loan has two outcome variables (explained in the following section): `AmountPaid360` and `LoanStatus360`. The first of these variables is a continuous numerical value, the second is a categorical variable. Your objective is:

- build a regression model to predict `AmountPaid360` and a classification model to predict `LoanStatus360` OR both (your choice) 
- evaluate the performance of your model(s)
- briefly communicate your process for model building, your model results, any limitations that you faced, and what next steps you might take with additional data access


## Dataset Structure

### Input Features

- `Product`: The product type purchased by the customer
- `CustomerGender`: Self-explanatory
- `Location`: The location where the loan was issued. (Also includes a long-tail of legacy locations)
- `Region`: The sales region where the device was purchased. Includes many regions which have since been changed. The current set of main sales regions are Regions 1-7.
- `TotalPrice`: The total amount to-be-paid by the customer over the course of the loan (including deposit and daily payments)
- `StartDate`: The day on which the customer's loan became active
- `Deposit`: The initial deposit required from the customer prior to loan activation
- `DailyRate`: The amount that the customer must pay daily in order to continue to use their device
- `TotalDays`: The number of days for which the customer is expected to continue paying the daily rate
- `AmountPaid30`: The amount that the customer had repaid as of the 30th day of their loan
- `AmountPaid60`: The amount that the customer had repaid as of the 60th day of their loan

### Outcome Variables

- `AmountPaid360`: The amount that the customer had repaid as of the 360th day of their loan
- `LoanStatus360`: The status of the loan as of day 360:
  - `Active`: The customer is still actively repaying their loan.
  - `Blocked`: The customer has had their account blocked after more than 30 days without a payment.
  - `Finished Payment`: The customer has successfully repaid their loan.

