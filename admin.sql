INSERT INTO users 
  (username, password_hash, role, registered_at, activated, registration_token)
VALUES 
  ('kola', '$2a$12$IwJi0a7R94fcyli4a4pAK.Ev69uQHa9EQlNMuqPNa4X1TcBENU6SW', 'admin', NOW(), TRUE, NULL);