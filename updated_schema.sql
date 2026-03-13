-- ============================================================================
  -- UPDATED SCHEMA WITH ASSESSMENT_TYPE
-- ============================================================================
  
  -- 1. ALTER responses table to add assessment_type column
ALTER TABLE responses 
ADD COLUMN IF NOT EXISTS assessment_type TEXT CHECK (assessment_type IN ('selbstbewertung', 'fremdbewertung'));

-- 2. ALTER users table to add assessment_type column
ALTER TABLE users 
ADD COLUMN IF NOT EXISTS assessment_type TEXT CHECK (assessment_type IN ('selbstbewertung', 'fremdbewertung'));

-- 3. Set default values for existing data
-- For users: based on role
UPDATE users 
SET assessment_type = CASE 
WHEN role IN ('laborleitung', 'colleague') THEN 'selbstbewertung'
WHEN role IN ('leading_peer', 'co_peer') THEN 'fremdbewertung'
ELSE 'selbstbewertung'
END
WHERE assessment_type IS NULL;

-- For responses: based on user role
UPDATE responses 
SET assessment_type = (
  SELECT CASE 
  WHEN u.role IN ('laborleitung', 'colleague') THEN 'selbstbewertung'
  WHEN u.role IN ('leading_peer', 'co_peer') THEN 'fremdbewertung'
  ELSE 'selbstbewertung'
  END
  FROM users u
  WHERE u.id = responses.user_id
)
WHERE assessment_type IS NULL;

-- 4. Make assessment_type NOT NULL after setting defaults
ALTER TABLE responses 
ALTER COLUMN assessment_type SET NOT NULL;

ALTER TABLE users 
ALTER COLUMN assessment_type SET NOT NULL;

-- ============================================================================
  -- COMPLETE UPDATED SCHEMA (For reference or fresh database)
-- ============================================================================
  
  -- Users table with assessment_type
DROP TABLE IF EXISTS users CASCADE;
CREATE TABLE users (
  id SERIAL PRIMARY KEY,
  username TEXT NOT NULL UNIQUE,
  password_hash TEXT NOT NULL,
  role TEXT NOT NULL CHECK (role IN ('admin', 'laborleitung', 'colleague', 'leading_peer', 'co_peer')),
  assessment_type TEXT NOT NULL CHECK (assessment_type IN ('selbstbewertung', 'fremdbewertung')),
  registered_at TIMESTAMP DEFAULT NOW(),
  activated BOOLEAN DEFAULT FALSE,
  registration_token TEXT,
  project_id INTEGER REFERENCES projects(id),
  has_submitted BOOLEAN DEFAULT FALSE
);

-- Responses table with assessment_type
DROP TABLE IF EXISTS responses CASCADE;
CREATE TABLE responses (
  id SERIAL PRIMARY KEY,
  user_id INTEGER NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  question_id TEXT NOT NULL,
  response_value TEXT,
  text_value TEXT,
  assessment_type TEXT NOT NULL CHECK (assessment_type IN ('selbstbewertung', 'fremdbewertung')),
  tag TEXT,
  project_id INTEGER REFERENCES projects(id),
  created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP
);

-- Create indexes for performance
CREATE INDEX IF NOT EXISTS idx_responses_user_id ON responses(user_id);
CREATE INDEX IF NOT EXISTS idx_responses_project_id ON responses(project_id);
CREATE INDEX IF NOT EXISTS idx_responses_assessment_type ON responses(assessment_type);
CREATE INDEX IF NOT EXISTS idx_users_project_id ON users(project_id);
CREATE INDEX IF NOT EXISTS idx_users_assessment_type ON users(assessment_type);