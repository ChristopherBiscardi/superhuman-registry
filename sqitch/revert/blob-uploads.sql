-- Revert sr:blob-uploads from pg

BEGIN;

DROP TABLE sr.blob_uploads;

DROP FUNCTION update_modified_column;
DROP TRIGGER update_blob_upload_modtime;

DROP FUNCTION populate_create_mod_columns;
DROP TRIGGER insert_blob_upload_createtime;

COMMIT;
