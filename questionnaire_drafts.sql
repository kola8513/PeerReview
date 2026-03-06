-- Drop and recreate with correct types
DROP TABLE IF EXISTS questionnaire_drafts CASCADE;

CREATE TABLE questionnaire_drafts (
    id SERIAL PRIMARY KEY,
    user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    project_id INTEGER REFERENCES projects(id) ON DELETE CASCADE,
    questionnaire_id TEXT NOT NULL,
    answers JSONB,
    status TEXT DEFAULT 'draft',
    last_saved TIMESTAMP DEFAULT NOW(),
    UNIQUE(user_id, project_id, questionnaire_id)
);

-- Create indexes for performance
CREATE INDEX idx_drafts_user ON questionnaire_drafts(user_id);
CREATE INDEX idx_drafts_project ON questionnaire_drafts(project_id);
CREATE INDEX idx_drafts_last_saved ON questionnaire_drafts(last_saved DESC);