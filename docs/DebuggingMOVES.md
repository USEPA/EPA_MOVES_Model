# Debugging MOVES

MOVES is a model with many interdependent components. Sometimes a given set of inputs results in unexpected output from a MOVES run. When you encounter results that suggest a problem with the model, there are several things that you can do to isolate the potential bug. This document provides some suggestions for isolating potential bugs in MOVES, which will be helpful even if you are just trying to communicate with the MOVES team. It also notes some tools in the code that can aid code testers in identifying the source of a confirmed bug.

## Isolating MOVES bugs

The first step to diagnose a potential bug is to identify the conditions that cause it to occur. In general this process involves confirming that the model inputs are valid, and then defining the scope of the potential issue.

### Check the RunSpec and input data

The vast majority of unexpected outputs from MOVES arise from inconsistencies in the RunSpec, the input dataset(s), or in the post-processing of the MOVES output. It is generally a good practice to review the [MOVES training](https://www.epa.gov/moves/moves-training-sessions) materials to avoid making simple mistakes during RunSpec and input database creation.

The most common mistakes in input databases have to do with allocation tables, which are generally those that end in `fraction` or `distribution`. MOVES importers will check to make sure values in these tables sum to 1 as they should, so that total activity is conserved. However, values that are set to 0 *must be included*. For example, the two examples of *roadtypedistribution* imply the same activity allocation:

| sourceTypeID | roadTypeID | roadTypeVMTFraction |
| ------------ | ---------- | ------------------- |
| 21           | 2          | 0.45                |
| 21           | 3          | 0.25                |
| 21           | 4          | 0.3                 |

| sourceTypeID | roadTypeID | roadTypeVMTFraction |
| ------------ | ---------- | ------------------- |
| 21           | 2          | 0.45                |
| 21           | 3          | 0.25                |
| 21           | 4          | 0.3                 |
| 21           | 5          | 0.0                 |

However, MOVES requires an explicit 0 where the user does not want any activity. The first example will result in incorrect MOVES output.

Another common mistake in input databases is using the incorrect units, or incorrect interpretation of the tables definitions. For example, MOVES has two types of "daily" inputs that can be confusing. The first is a true daily input, which corresponds to an average day for a given dayID - for dayID 5, that would be an average weekday. However, MOVES can also interpret an input for dayID 5 as total activity across all weekdays, which it refers to as "portion of week" inputs. Consider the following *startsperday* table:

| dayID | sourceTypeID | startsPerDay |
| ----- | ------------ | ------------ |
| 5     | 21           | 40           |
| 2     | 21           | 10           |

If the units are *per average day*, then MOVES will calculate 220 total starts in a week. However, if the units are *portion of week*, MOVES will only calculate 50 total starts in a week. **All MOVES input tables use units of *per average day***. However, either of these units can be selected for a MOVES output database in the RunSpec. If MOVES outputs by day look inconsistent, double check the units selected in the RunSpec make sure it matches how you are using the data in the output database.

### Create the smallest set of runs that demonstrate the issue

Testing MOVES is typically an iterative process. If a potential bug occurs in a large run, testing small changes in inputs could potentially take hours or days. Large input databases can be complicated to tweak without making mistakes, and large output datasets can be tricky to process and interpret. For these reasons it is helpful to try to find the most minimal RunSpec and set of inputs that will reproduce the issue. Ideally, a test run in MOVES should only take a few minutes to complete, and the outputs should provide enough diagnostic information to demonstrate the scope of the issue. Below are some suggestions for how to reduce the scope of a MOVES run.

#### Limit time span

Select the smallest time span that captures the unexpected output. Ideally this is one year, one month, one day type, and one hour. Some issues however may require an expanded scope. For example, calculating evaporative emissions require all hours. Likewise, capturing temperature effects often requires multiple months. In that case it is best to select months that would show the largest difference in temperature effects like January and July.

#### Limit source types

Select the fewest and most simple source types that capture the unexpected output. Motorcycles and passenger cars for example contain one regulatory class each, whereas combination long-haul trucks contain multiple regulatory classes. Identifying which source types are involved in generating unexpected results can be very helpful for troubleshooting.

#### Limit pollutants and processes

Try to select the most limited set of pollutants and processes that generate the unexpected behavior. If possible, select pollutants that have few if any prerequisites. Likewise only select the processes that are required to reproduce the behavior. Eliminating processes like evaporative or refueling emissions can significantly reduce the complexity of the MOVES run and make it easier to identify the source of the unexpected behavior.

### Create a simple test to confirm suspect behavior

Once you have a minimal RunSpec that produces unexpected results, create a simple test to let you identify the suspected bug in your MOVES output. Typically, the easiest way to do this is to write a SQL query to demonstrate the issue. The goal is for this test to be fast, easy to apply, and clear in its results so that you can demonstrate the issue to others, and enable them to reproduce the issue.

## Saving MOVES intermediate outputs

When debugging MOVES code and algorithms it can be helpful to look at the intermediate data that MOVES calculates on its way to generating a final output. Generally MOVES discards this data once it is done with it. However, there are settings in the source code that let you keep this data. Once you make these code changes you must recompile MOVES in order to keep the intermediate data.

### Save the execution database

**Source File:** 

gov\epa\otaq\moves\master\framework\OutputProcessor.java

```java
 keepDebugData = true;
```

This will keep the execution database from getting cleared. Note that it will only have the latest information in it though. You may have to limit your RunSpecs to make sure it contains what you need.

### Save the worker bundles

**Source File:**

gov\epa\otaq\moves\worker\framework\RemoteEmissionsCalculator.java

```java
isTest = true;
```

This will save to .\WorkerFolder\WorkerTempXX. Look at the BundleManifest.xml to see if that folder has the correct information in it. These are not necessarily run in the same order every time.

### Save the generator files

**Source File:**

gov\epa\otaq\moves\master\framework\Generator.java

```java
KEEP_EXTERNAL_GENERATOR_FILES = true;
```

This will save to .\MOVESTemporary\GeneratorTempXX. Look at the BundleManifest.xml to see if that folder has the correct information in it. The generators tend to run in the same order every time.
