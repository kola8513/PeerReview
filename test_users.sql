-- =========================================================
-- TEST USERS
-- Password = username (bcrypt hashes generated in R)
-- Safe to run multiple times
-- =========================================================

INSERT INTO public.users (username, password_hash, role, registered_at, activated)
VALUES
  -- Admin
  ('kola',
   '$2a$12$IwJi0a7R94fcyli4a4pAK.Ev69uQHa9EQlNMuqPNa4X1TcBENU6SW',
   'admin', NOW(), TRUE),

  -- Laborleitung
  ('test-laborleitung',
   '$2a$12$3u29e8MQoFhBT30I7ehhHeG.jTkkEUj.75azJdPgXMU5WD4/qEqgG',
   'laborleitung', NOW(), TRUE),

  -- Colleague
  ('test-kollege',
   '$2a$12$Q9XpXiFRWcmLLeZI.y34tepJIMFiv6d4bCWV3qEDV0H/PpxfHH6Bu',
   'colleague', NOW(), TRUE),

  -- Leading Peer
  ('test-leadingpeer',
   '$2a$12$t345ptms4yT1AddUbgocluDMjp72w7ZoedDe.tvzoCd24f17X1ine',
   'leading_peer', NOW(), TRUE),

  -- Co-Peer
  ('test-peer',
   '$2a$12$inys5jIsv9S4ClRAgd7.zuLP2f70OtAFjUBsbFtUniOT2sd1N6dza',
   'co_peer', NOW(), TRUE)

ON CONFLICT (username) DO UPDATE
SET
  password_hash = EXCLUDED.password_hash,
  role          = EXCLUDED.role,
  activated     = TRUE;
