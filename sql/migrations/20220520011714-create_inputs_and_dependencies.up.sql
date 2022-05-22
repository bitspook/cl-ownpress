-- id: 20220520011714
-- direction: UP
-- description: create_inputs_and_dependencies

CREATE TABLE IF NOT EXISTS inputs (
  id TEXT PRIMARY KEY NOT NULL, -- Uniquely identify an input, in case we want to allow changing slugs in future
  slug TEXT UNIQUE NOT NULL,
  provider TEXT NOT NULL,
  title TEXT,
  tags JSON,                     -- An array
  content TEXT,                  -- Content of *some* inputs
  published_at TEXT
);
--;;
CREATE TABLE IF NOT EXISTS dependencies (
  depended_by TEXT NOT NULL,
  dependent_on TEXT NOT NULL,
  PRIMARY KEY (depended_by, dependent_on)
) WITHOUT ROWID;

