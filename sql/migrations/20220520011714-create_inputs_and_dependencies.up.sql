-- id: 20220520011714
-- direction: UP
-- description: create_inputs_and_dependencies

CREATE TABLE IF NOT EXISTS inputs (
  slug TEXT PRIMARY KEY NOT NULL,
  title TEXT,
  published_at TEXT,
  provider TEXT NOT NULL,
  is_deleted BOOLEAN,
  metadata JSON,
  content NULL                  -- Content of *some* inputs
);
--;;
CREATE TABLE IF NOT EXISTS dependencies (
  depended_by TEXT NOT NULL,
  dependent_on TEXT NOT NULL,
  PRIMARY KEY (depended_by, dependent_on)
) WITHOUT ROWID;

