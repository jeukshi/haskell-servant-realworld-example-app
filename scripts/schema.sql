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

DROP TABLE IF EXISTS articles;

-- TODO trigger for updatedAt
CREATE TABLE articles (
    art_id INTEGER PRIMARY KEY
  , art_slug VARCHAR(256) UNIQUE NOT NULL
  , art_title VARCHAR(256) NOT NULL
  , art_description VARCHAR(256) NOT NULL
  , art_body TEXT NOT NULL
  , art_createdAt DATETIME DEFAULT CURRENT_TIMESTAMP
  , art_updatedAt DATETIME DEFAULT CURRENT_TIMESTAMP
  , art_usr_id INTEGER NOT NULL

  , FOREIGN KEY (art_usr_id) REFERENCES users (usr_id)
);

DROP TABLE IF EXISTS tags;

CREATE TABLE tags (
    tag_id INTEGER PRIMARY KEY
  , tag_text VARCHAR(32) UNIQUE
);

DROP TABLE IF EXISTS tagged;

CREATE TABLE tagged (
    tgd_art_id INTEGER
  , tgd_tag_id INTEGER

  , FOREIGN KEY (tgd_tag_id) REFERENCES tags (tag_id)
  , FOREIGN KEY (tgd_art_id) REFERENCES articles (art_id)
  , UNIQUE (tgd_tag_id, tgd_art_id)
);

DROP TABLE IF EXISTS favourited;

CREATE TABLE favorited (
    fav_usr_id INTEGER
  , fav_art_id INTEGER

  , FOREIGN KEY (fav_usr_id) REFERENCES users (usr_id)
  , FOREIGN KEY (fav_art_id) REFERENCES articles (art_id)
  , UNIQUE (fav_usr_id, fav_art_id)
);
