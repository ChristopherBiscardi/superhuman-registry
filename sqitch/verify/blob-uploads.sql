-- Verify sr:blob-uploads on pg

BEGIN;

SELECT * from sr.blob_uploads;

ROLLBACK;
