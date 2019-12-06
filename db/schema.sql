CREATE TABLE IF NOT EXISTS users (
       id          int PRIMARY KEY,
       username    varchar(255) UNIQUE,
       email       varchar(255) UNIQUE,
       password    text,
       uuid        text UNIQUE NOT NULL,
       created_at  timestamp NOT NULL,
       updated_at  timestamp NOT NULL
);
--CREATE UNIQUE INDEX IF NOT EXISTS users_username_idx ON users (username);


CREATE TABLE IF NOT EXISTS messages (
       id           int PRIMARY KEY,
       sender_id    int NOT NULL,
       recipient_id int NOT NULL,
       reply_id     int NOT NULL,
       body         text NOT NULL,
       created_at   timestamp NOT NULL,
       updated_at   timestamp NOT NULL
);
