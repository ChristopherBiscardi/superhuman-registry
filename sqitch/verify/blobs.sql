-- Verify sr:blobs on pg

BEGIN;

SELECT * from sr.blobs;
SELECT * from sr.repo_blobs;

ROLLBACK;
