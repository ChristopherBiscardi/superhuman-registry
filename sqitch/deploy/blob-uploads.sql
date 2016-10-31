-- Deploy sr:blob-uploads to pg
-- requires: uuid-ossp

BEGIN;

-- Table
CREATE TABLE sr.blob_uploads (
  id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
  lo_id OID,
  repo_name VARCHAR(256) NOT NULL,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL,
  modified_at TIMESTAMP WITH TIME ZONE NOT NULL
);

-- Automatically update modified_at
CREATE OR REPLACE FUNCTION update_modified_column()
RETURNS TRIGGER AS $$
BEGIN
    NEW.modified_at = now();
    -- Force created_at to never change
    NEW.created_at = OLD.created_at;
    RETURN NEW;
END;
$$ language 'plpgsql';

CREATE TRIGGER update_blob_upload_modtime
BEFORE UPDATE ON sr.blob_uploads
FOR EACH ROW EXECUTE PROCEDURE update_modified_column();

-- | Automatically populate created_at
-- Use a trigger so it's impossible to override on insert
CREATE OR REPLACE FUNCTION populate_create_mod_columns()
RETURNS TRIGGER AS $$
BEGIN
    NEW.created_at = now();
    NEW.modified_at = now();
    RETURN NEW;	
END;
$$ language 'plpgsql';

CREATE TRIGGER insert_blob_upload_createtime
BEFORE INSERT ON sr.blob_uploads
FOR EACH ROW EXECUTE PROCEDURE populate_create_mod_columns();

COMMIT;
