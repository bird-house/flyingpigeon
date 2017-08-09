import os
from pywps.app.Service import Service

from flyingpigeon.processes import processes


application = Service(processes, [
    os.path.join(os.path.dirname(__file__), 'default.cfg'),
    os.environ.get('PYWPS_CFG', '')])
