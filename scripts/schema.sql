DROP TABLE IF EXISTS users;

CREATE TABLE users (
    usr_id INTEGER PRIMARY KEY
  , usr_email VARCHAR(64) UNIQUE NOT NULL
  , usr_username VARCHAR(64) UNIQUE NOT NULL
  , usr_password VARCHAR(64) NOT NULL
  , usr_bio TEXT
  , usr_image TEXT
);

DROP TABLE IF EXISTS follows;

CREATE TABLE follows (
    fws_usr_id INTEGER
  , fws_follows_usr_id INTEGER

  , FOREIGN KEY (fws_usr_id) REFERENCES users (usr_id)
  , FOREIGN KEY (fws_follows_usr_id) REFERENCES users (usr_id)
  , UNIQUE (fws_usr_id, fws_follows_usr_id)
);
