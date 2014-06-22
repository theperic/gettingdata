There are two core functions to product the two tidy data set, recombining the test and training data.

The first, dataset1() creates the first data set by
-searching for fields continig mean or std data
-isolating these columns in the calculated data
-writing the combined data to a file
-inserting the key data (activities) back into the set

The second, dataset2, reads in each raw data file per variable and reduces it to a mean vector whish is added to a reault set.  This process is performed for the test data, then the training data.  next the two sets are combined and select with group by aggrates the data by user and activity.


Activity is the description of the Actions

Subject is the ID of the subjects involved

Avg(variable() provides the mean value for each variable.