-- Second payment provider (Stripe hosted Checkout, for Apple/Google Pay).
-- payment_provider: 'mercadopago' | 'stripe' | NULL (offline / dev order).
ALTER TABLE orders
  ADD COLUMN payment_provider  TEXT,
  ADD COLUMN stripe_session_id TEXT;

-- Orders that already carry a Mercado Pago preference were MP orders.
UPDATE orders SET payment_provider = 'mercadopago' WHERE mp_preference_id IS NOT NULL;
