-- id: 20220520011714
-- direction: UP
-- description: create_inputs_and_dependencies

CREATE TABLE IF NOT EXISTS inputs (
  id TEXT PRIMARY KEY NOT NULL, -- Uniquely identify an input, in case we want to allow changing slugs in future
  slug TEXT UNIQUE NOT NULL,
  provider TEXT NOT NULL,
  title TEXT,
  tags JSON,                     -- An array
  metadata JSON,
  content TEXT,                  -- Content of *some* inputs
  published_at DATETIME
) WITHOUT ROWID;
--;;
CREATE TABLE IF NOT EXISTS dependencies (
  input_id TEXT NOT NULL,
  depends_on TEXT NOT NULL,
  PRIMARY KEY (input_id, depends_on),
  FOREIGN KEY(input_id) REFERENCES inputs(id),
  FOREIGN KEY(depends_on) REFERENCES inputs(id)
) WITHOUT ROWID;
--;;
CREATE TABLE IF NOT EXISTS outputs (
  id INTEGER PRIMARY KEY NOT NULL,
  input_id TEXT NOT NULL,
  path TEXT NOT NULL UNIQUE,
  created_at TEXT NOT NULL DEFAULT datetime,
  updated_at TEXT NOT NULL DEFAULT datetime,
  FOREIGN KEY(input_id) REFERENCES inputs(id)
);
