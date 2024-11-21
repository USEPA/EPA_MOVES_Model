-- Version 2006-08-05
-- Updates pre-Task 220 output databases to be Task 220-compliant

ALTER TABLE MOVESRun
	ADD COLUMN minutesDuration		FLOAT NOT NULL DEFAULT 0,
	ADD COLUMN defaultDatabaseUsed	VARCHAR(200) NULL,
	ADD COLUMN masterVersionDate	CHAR(10) NULL,
	ADD COLUMN masterComputerID	 	VARCHAR(20) NULL
;

CREATE TABLE MOVESWorkersUsed (
	MOVESRunID			SMALLINT UNSIGNED NOT NULL,
	workerVersion		CHAR(10) NOT NULL,
	workerComputerID	VARCHAR(20) NOT NULL,
	workerID			VARCHAR(10),
	
	bundleCount			INTEGER UNSIGNED NOT NULL DEFAULT 0,
	failedBundleCount	INTEGER UNSIGNED NOT NULL DEFAULT 0
) Engine=MyISAM DEFAULT CHARSET='utf8mb4' COLLATE 'utf8mb4_unicode_ci';
