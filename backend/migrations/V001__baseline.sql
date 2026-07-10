CREATE TABLE products (
  id            BIGSERIAL PRIMARY KEY,
  sku           TEXT UNIQUE NOT NULL,
  name          TEXT NOT NULL,
  description   TEXT NOT NULL DEFAULT '',
  category      TEXT NOT NULL,
  brand         TEXT NOT NULL DEFAULT '',
  price_cents   INTEGER NOT NULL CHECK (price_cents >= 0),
  stock         INTEGER NOT NULL DEFAULT 0 CHECK (stock >= 0),
  weight_grams  INTEGER,
  image_url     TEXT,
  active        BOOLEAN NOT NULL DEFAULT TRUE,
  created_at    TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at    TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE SEQUENCE order_ref_seq;

CREATE TABLE orders (
  id              BIGSERIAL PRIMARY KEY,
  order_ref       TEXT UNIQUE NOT NULL,
  -- created | pending_payment | paid | shipped | completed | cancelled
  status          TEXT NOT NULL DEFAULT 'created',
  customer_name   TEXT NOT NULL,
  customer_email  TEXT NOT NULL,
  customer_phone  TEXT NOT NULL,
  calle           TEXT NOT NULL,
  numero          TEXT NOT NULL,
  colonia         TEXT NOT NULL,
  codigo_postal   TEXT NOT NULL,
  ciudad          TEXT NOT NULL,
  estado          TEXT NOT NULL,
  referencias     TEXT,
  total_cents     INTEGER NOT NULL,
  mp_preference_id TEXT,
  mp_payment_id   TEXT,
  mp_status       TEXT,
  mp_last_check   TIMESTAMPTZ,
  dhl_waybill     TEXT,
  created_at      TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  updated_at      TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TABLE order_items (
  order_id         BIGINT NOT NULL REFERENCES orders(id),
  product_id       BIGINT NOT NULL REFERENCES products(id),
  quantity         INTEGER NOT NULL CHECK (quantity > 0),
  unit_price_cents INTEGER NOT NULL,   -- price snapshot at purchase time
  product_name     TEXT NOT NULL,
  product_sku      TEXT NOT NULL,
  weight_grams     INTEGER,
  PRIMARY KEY (order_id, product_id)
);

CREATE TABLE order_events (
  id          BIGSERIAL PRIMARY KEY,
  order_id    BIGINT NOT NULL REFERENCES orders(id),
  from_status TEXT,
  to_status   TEXT NOT NULL,
  note        TEXT,
  created_at  TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TABLE admin_sessions (
  token      TEXT PRIMARY KEY,
  created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX orders_status_idx ON orders (status, created_at DESC);
