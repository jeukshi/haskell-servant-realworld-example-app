drop table if exists users;

create table users (
    usr_id INTEGER PRIMARY KEY
  , usr_email VARCHAR(64) UNIQUE NOT NULL
  , usr_username VARCHAR(64) UNIQUE NOT NULL
  , usr_password VARCHAR(64) NOT NULL
  , usr_bio TEXT
  , usr_image VARCHAR(64)
);
