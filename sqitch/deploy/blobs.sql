-- Deploy sr:blobs to pg
-- requires: appschema

BEGIN;

-- Table
CREATE TABLE sr.blobs (
  -- id is a sha256 digest hash
  -- TODO: limit this to sha256 length?
  blob_digest TEXT PRIMARY KEY,
  -- id of the hashed blob
  lo_id OID NOT NULL,
  -- Since we need to calculate the size when finalizing an upload
  -- and blobs are content-adressable we can store the size at that
  -- time, avoiding having to recalculate the value during HEAD requests
  blob_size INT NOT NULL,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL
);

-- | Automatically populate created_at
-- Use a trigger so it's impossible to override on insert
CREATE OR REPLACE FUNCTION populate_create_column()
RETURNS TRIGGER AS $$
BEGIN
    NEW.created_at = now();
    RETURN NEW;	
END;
$$ language 'plpgsql';

-- Triggers to automatically handle created_at time
CREATE TRIGGER insert_blobs_createtime
BEFORE INSERT ON sr.blobs
FOR EACH ROW EXECUTE PROCEDURE populate_create_column();

-- A Repo is a "bag of blobs" which may or may not be referenced by
-- a tag.
-- When uploading, a blob is uploaded to a repo and can be a memeber
-- of multiple repos.
CREATE TABLE sr.repo_blobs (
  -- blob_digest is a sha256 digest hash
  -- TODO: limit this to sha256 length?
  -- Unceremoniously deleting a blob from the store can cause
  -- images to become incomplete. Therefore we RESTRICT the deletion
  -- and request that the user delete all referencing images in the
  -- registry.
  blob_digest TEXT REFERENCES sr.blobs ON DELETE RESTRICT NOT NULL,
  repo_name VARCHAR(256) NOT NULL,
  PRIMARY KEY (repo_name, blob_digest)
);

COMMIT;
