"""
Take fields listed on the "L.A. County Park Needs Assessment - Demographics"
website, and process them so we can refer to the fields in the lessons.

https://geohub.lacity.org/datasets/lacounty::l-a-county-park-needs-assessment-demographics/about
"""

import csv

def extract_fields(text_file_path):
    fields = []

    with open(text_file_path) as reader:
        lines = list(reader)
        for idx, line in enumerate(lines):
            if line == 'Display name\n':
                fields.append([lines[idx+3].strip(), lines[idx+1].strip()])

    return fields

def create_fields_csv(fields, csv_path):
    with open(csv_path, mode='w') as file:
        writer = csv.writer(file, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
        writer.writerow(['long field', 'description'])
        for field in fields:
            writer.writerow(field)

def create_field_table(fields):
    html = """
<table>
    <tr>
        <th>field</th><th>description</th>
    </tr>"""

    for field in fields:
        html += f"""
    <tr>
        <td>{field[0]}</td><td>{field[1]}</td>
    </tr>"""
    html += """
</table>"""
    return html


fields = extract_fields('PNA_fields.txt')
# create_fields_csv(fields, 'PNA_fields.csv')
table = create_field_table(fields)
print(table)


