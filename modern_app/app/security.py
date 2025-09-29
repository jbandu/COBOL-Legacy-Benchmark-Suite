from __future__ import annotations

from fastapi import Depends, HTTPException, Security
from fastapi.security import APIKeyHeader

from .config import get_settings

api_key_scheme = APIKeyHeader(name="X-Access-Token", auto_error=False)


def authorize(token: str | None = Security(api_key_scheme)) -> None:
    settings = get_settings()
    if token != settings.access_token:
        raise HTTPException(status_code=401, detail="Invalid access token")


def get_token_header(token: str = Depends(authorize)) -> None:  # pragma: no cover
    """FastAPI dependency stub to align with router usage."""

    return None
