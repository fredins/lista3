CREATE TABLE Todo(
  id UUID,
  text TEXT,
  completed BOOLEAN NOT NULL,
  marked BOOLEAN NOT NULL,
  PRIMARY KEY (id)
);
