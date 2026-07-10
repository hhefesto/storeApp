-- Messages from the public "Contacto" form. Read state is admin-side only.
CREATE TABLE contact_messages
  ( id         BIGSERIAL PRIMARY KEY
  , name       TEXT        NOT NULL
  , email      TEXT        NOT NULL
  , phone      TEXT        NOT NULL DEFAULT ''
  , message    TEXT        NOT NULL
  , created_at TIMESTAMPTZ NOT NULL DEFAULT now()
  , read_at    TIMESTAMPTZ
  );

CREATE INDEX contact_messages_unread_idx ON contact_messages (created_at DESC)
  WHERE read_at IS NULL;
