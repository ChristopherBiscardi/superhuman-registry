-- Verify sr:appschema on pg

BEGIN;

SELECT pg_catalog.has_schema_privilege('sr', 'usage');

ROLLBACK;
