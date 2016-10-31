-- Revert sr:blobs from pg

BEGIN;

DROP TABLE sr.blobs;
DROP TABLE sr.repo_blobs;
DROP TRIGGER insert_blobs_createtime;

COMMIT;
