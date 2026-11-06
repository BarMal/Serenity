"""Keeps a test's stdout out of the CI log, where a `::error::` line becomes a false annotation (#2557)."""

from __future__ import annotations

import contextlib
import io
import unittest


def capture_stdout(test: unittest.TestCase) -> io.StringIO:
    """Redirects stdout into the returned buffer until the test ends, so the test can assert on what was printed."""
    buffer = io.StringIO()
    redirect = contextlib.redirect_stdout(buffer)
    redirect.__enter__()
    test.addCleanup(redirect.__exit__, None, None, None)
    return buffer
