from __future__ import annotations

from contextlib import contextmanager

from sqlmodel import Session, SQLModel, create_engine

from .config import get_settings

_engine = None


def get_engine():
    global _engine
    if _engine is None:
        settings = get_settings()
        _engine = create_engine(settings.database_url, echo=False, future=True)
    return _engine


def init_db() -> None:
    """Create database tables."""

    engine = get_engine()
    SQLModel.metadata.create_all(engine)


@contextmanager
def session_scope():
    engine = get_engine()
    with Session(engine) as session:
        yield session
