BEGIN;

CREATE TABLE  World (
  id integer NOT NULL,
  randomNumber integer NOT NULL default 0,
  PRIMARY KEY  (id)
);

INSERT INTO World (id, randomnumber)
SELECT x.id, floor(random() * 10000 + 1) FROM generate_series(1,10000) as x(id);

COMMIT;
