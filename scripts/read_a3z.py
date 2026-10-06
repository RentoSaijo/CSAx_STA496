"""Export public A3Z workbook extract for R preparation."""

import argparse
import csv
import os
from pathlib import Path
import tempfile
import zipfile

from tableauhyperapi import Connection, CreateMode, HyperProcess, Telemetry


parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('workbook', type=Path)
parser.add_argument('output', type=Path)
args = parser.parse_args()
workbook = args.workbook.resolve()
output = args.output.resolve()

# Read downloaded extract without modifying workbook.
with tempfile.TemporaryDirectory(prefix='a3z-') as directory:
    os.chdir(directory)
    with zipfile.ZipFile(workbook) as archive:
        extracts = [name for name in archive.namelist() if name.endswith('.hyper')]
        if len(extracts) != 1:
            raise ValueError('Workbook must contain exactly one extract')
        source = Path(directory) / 'source.hyper'
        source.write_bytes(archive.read(extracts[0]))
    with HyperProcess(Telemetry.DO_NOT_SEND_USAGE_DATA_TO_TABLEAU,
                      parameters={'log_config': ''}) as process:
        with Connection(process.endpoint, str(source), CreateMode.NONE) as connection:
            tables = connection.catalog.get_table_names('Extract')
            if len(tables) != 1:
                raise ValueError('Extract must contain exactly one table')
            table = tables[0]
            columns = connection.catalog.get_table_definition(table).columns
            with output.open('w', newline='') as handle:
                writer = csv.writer(handle)
                writer.writerow(column.name.unescaped for column in columns)
                with connection.execute_query(f'SELECT * FROM {table}') as rows:
                    writer.writerows(rows)
