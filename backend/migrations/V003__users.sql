CREATE TABLE users (
  id               BIGSERIAL PRIMARY KEY,
  provider         TEXT NOT NULL,           -- 'google', 'facebook', …
  provider_subject TEXT NOT NULL,           -- OIDC `sub` (stable per provider)
  email            TEXT NOT NULL,
  name             TEXT NOT NULL DEFAULT '',
  picture          TEXT,
  created_at       TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  UNIQUE (provider, provider_subject)
);

CREATE TABLE user_sessions (
  token      TEXT PRIMARY KEY,
  user_id    BIGINT NOT NULL REFERENCES users(id),
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

ALTER TABLE orders ADD COLUMN user_id BIGINT REFERENCES users(id);
CREATE INDEX orders_user_idx ON orders (user_id, created_at DESC);
