-- Version 2008-05-22
-- Create tables for tracking imports and database changes.

create table if not exists auditLog (
	whenHappened datetime not null,
	importerName varchar(100) not null,
	briefDescription varchar(100) null,
	fullDescription varchar(4096) null,
	key logByDate (whenHappened),
	key logByImporter (importerName)
);
