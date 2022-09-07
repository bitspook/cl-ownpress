-- id: 20220520011714
-- direction: UP
-- description: create_inputs_and_dependencies

CREATE TABLE IF NOT EXISTS inputs (
  id TEXT PRIMARY KEY NOT NULL, -- Uniquely identify an input, in case we want to allow changing slugs in future
  slug TEXT UNIQUE NOT NULL,
  provider TEXT NOT NULL,
  title TEXT,
  tags JSON,                     -- An array
  category TEXT,
  metadata JSON,
  content_raw TEXT,                  -- Content of *some* inputs
  content_html TEXT,                 -- Content in HTML formfor *some* inputs
  published_at DATETIME,
  out_path TEXT         -- Path where the input is published
) WITHOUT ROWID;
--;;
