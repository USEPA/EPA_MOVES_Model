/*
Checks for the MOVESDefault schema.
*/
SELECT
	stateID,
	stateName
FROM State
LIMIT 1;

SELECT
	countyID,
	countyName,
	stateID
FROM County
LIMIT 1;
