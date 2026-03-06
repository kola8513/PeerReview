CREATE TABLE IF NOT EXISTS invitation_codes (
    id SERIAL PRIMARY KEY,
    code TEXT UNIQUE NOT NULL,
    role TEXT NOT NULL CHECK (role IN ('admin', 'laborleitung', 'colleague', 'leading_peer', 'co_peer')),
    assessment_type TEXT,
    generated_by INTEGER REFERENCES users(id),
    project_id INTEGER REFERENCES projects(id),
    used_by_user_id INTEGER REFERENCES users(id),
    used_at TIMESTAMP,
    generated_at TIMESTAMP DEFAULT NOW()
);