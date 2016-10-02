-- Deploy sr:uuid-ossp to pg
-- requires: appschema

BEGIN;

CREATE EXTENSION "uuid-ossp";

COMMIT;
