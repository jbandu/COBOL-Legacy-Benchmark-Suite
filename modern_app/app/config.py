from __future__ import annotations

from pydantic_settings import BaseSettings, SettingsConfigDict


class Settings(BaseSettings):
    """Application configuration derived from environment variables."""

    database_url: str = "sqlite:///./portfolio.db"
    access_token: str = "secret-token"

    model_config = SettingsConfigDict(env_prefix="PORTAPP_", extra="ignore")


def get_settings() -> Settings:
    """Return cached settings instance for dependency injection."""

    return Settings()
