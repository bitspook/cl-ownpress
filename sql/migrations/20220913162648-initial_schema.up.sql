-- id: 20220913162648
-- direction: UP
-- description: initial_schema

CREATE TABLE IF NOT EXISTS provided_content (
  id TEXT PRIMARY KEY NOT NULL,
  provider TEXT NOT NULL,
  metadata JSON NOT NULL,
  body TEXT,
  type TEXT NOT NULL,           -- e.g org, md
  created_at DATETIME NOT NULL default datetime,
  updated_at DATETIME NOT NULL default datetime
) WITHOUT ROWID;
--;;
CREATE TABLE IF NOT EXISTS processed_content (
  prov_cont_id TEXT NOT NULL,
  type TEXT NOT NULL,           --e.g html, org
  processor TEXT NOT NULL,
  body TEXT,
  created_at DATETIME NOT NULL default datetime,
  updated_at DATETIME NOT NULL default datetime,
  FOREIGN KEY(prov_cont_id) REFERENCES provided_content(id),
  PRIMARY KEY (prov_cont_id, type, processor)
) WITHOUT ROWID;
--;;
