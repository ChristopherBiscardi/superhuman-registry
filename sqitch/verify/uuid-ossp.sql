-- Verify sr:uuid-ossp on pg

BEGIN;

SELECT * FROM pg_extension WHERE extname LIKE 'uuid-ossp';

ROLLBACK;
