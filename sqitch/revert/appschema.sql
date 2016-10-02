-- Revert sr:appschema from pg

BEGIN;

DROP SCHEMA sr;

COMMIT;
