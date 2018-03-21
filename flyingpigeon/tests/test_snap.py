import pytest

# run tests only if snappy is available
snappy = pytest.importorskip("snappy")


def test_snappy():
    import snappy
    from snappy import (ProductIO, ProductUtils, ProgressMonitor, jpy)
