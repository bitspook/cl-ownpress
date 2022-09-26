-- id: 20220913162648
-- direction: UP
-- description: initial_schema
PRAGMA foreign_keys = ON;
--;;
CREATE TABLE IF NOT EXISTS provided_content (
  id TEXT PRIMARY KEY NOT NULL,
  provider TEXT NOT NULL,
  metadata TEXT NOT NULL,
  body TEXT,
  type TEXT NOT NULL,           -- e.g org, md
  created_at TEXT NOT NULL default current_timestamp,
  updated_at TEXT NOT NULL default current_timestamp
) STRICT, WITHOUT ROWID;
--;;
CREATE TABLE IF NOT EXISTS processed_content (
  prov_cont_id TEXT NOT NULL,
  type TEXT NOT NULL,           --e.g html, org
  processor TEXT NOT NULL,
  body TEXT,
  created_at TEXT NOT NULL default current_timestamp,
  updated_at TEXT NOT NULL default current_timestamp,
  FOREIGN KEY(prov_cont_id) REFERENCES provided_content(id) ON DELETE CASCADE,
  PRIMARY KEY (prov_cont_id, type, processor)
) STRICT, WITHOUT ROWID;
--;;
