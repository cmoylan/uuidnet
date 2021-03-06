CREATE TABLE IF NOT EXISTS users (
       id          serial PRIMARY KEY,
       username    varchar(255) UNIQUE NOT NULL,
       email       varchar(255) UNIQUE,
       password    text,
       uuid        text UNIQUE NOT NULL,
       created_at  timestamp NOT NULL,
       updated_at  timestamp NOT NULL
);
--CREATE UNIQUE INDEX IF NOT EXISTS users_username_idx ON users (username);


CREATE TABLE IF NOT EXISTS messages (
       id           serial PRIMARY KEY,
       sender_id    int NOT NULL,
       recipient_id int NOT NULL,
       reply_id     int NOT NULL DEFAULT 0,
       body         text NOT NULL,
       created_at   timestamp NOT NULL,
       updated_at   timestamp NOT NULL
);

