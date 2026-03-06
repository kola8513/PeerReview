CREATE TABLE IF NOT EXISTS lab_info (
    id SERIAL PRIMARY KEY,
    project_id INTEGER UNIQUE NOT NULL REFERENCES projects(id) ON DELETE CASCADE,
    created_by INTEGER REFERENCES users(id),
    
    -- Grundlagen und Organisation
    name TEXT,
    internetadresse TEXT,
    trag TEXT,
    zug TEXT,
    ansprechpartner TEXT,
    email TEXT,
    telefonnummer TEXT,
    oeffnungzeiten TEXT,
    versorgung TEXT,
    sonstiges_versorgung TEXT,
    laborbereiche TEXT,
    sonstiges_laborbereiche TEXT,
    leistungsspektrum TEXT,
    sonstiges_leistungsspektrum TEXT,
    ausstattung TEXT,
    hauptlieferanten TEXT,
    question_binary_1 TEXT,
    farbcodesystem TEXT,
    question_binary_2 TEXT,
    auftragsgenerierung TEXT,
    praeanalytik TEXT,
    befunduebermittlung TEXT,
    beratungsleistungen TEXT,
    question_binary_3 TEXT,
    organigramm_filename TEXT,
    ergaenzung TEXT,
    
    -- Personal
    anzahl_total INTEGER,
    anzahl_mit INTEGER,
    davon_fachaerzte INTEGER,
    davon_weiterbildung INTEGER,
    anzahl_planstellen INTEGER,
    davon_unbesetzt INTEGER,
    anzahl_tech INTEGER,
    anzahl_tplanstellen INTEGER,
    davon_tunbesetzt INTEGER,
    anzahl_natur INTEGER,
    anzahl_nplanstellen INTEGER,
    davon_nunbesetzt INTEGER,
    anzahl_it INTEGER,
    anzahl_iplanstellen INTEGER,
    davon_iunbesetzt INTEGER,
    beschreibung_it TEXT,
    weitereinfo_personal TEXT,
    
    -- EDV
    anbieterinfo TEXT,
    anbieterorder TEXT,
    anbietermiddleware TEXT,
    weitereit TEXT,
    
    -- Kennzahlen
    angaben TEXT,
    laufendenjahres TEXT,
    vorjahres TEXT,
    kompetenzschwerpunkte TEXT,
    
    -- ? Add this column
    anmerkungen JSONB,
    
    created_at TIMESTAMP DEFAULT NOW(),
    updated_at TIMESTAMP DEFAULT NOW()
);