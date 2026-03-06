CREATE TABLE IF NOT EXISTS users (
    id SERIAL PRIMARY KEY,
    username TEXT NOT NULL UNIQUE,
    password_hash TEXT NOT NULL,
    role TEXT NOT NULL CHECK (role IN ('admin', 'laborleitung', 'colleague', 'leading_peer', 'co_peer')),
    registered_at TIMESTAMP DEFAULT NOW(),
    activated BOOLEAN DEFAULT FALSE,
    registration_token TEXT
);