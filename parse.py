import re

lines = [
'---',
'up:',
'  - "[[Efforts]]"',
'related:'
'  - "[[Academic Efforts]]"',
'created: 2023-02-01',
'aceWhoInst:',
'  - Yale',
'aceWhoTeam:',
'  - Alex',
'  - "[[Biniam Garomsa]]"',
'aceLinksCode:',
'  - https://github.com/alexjakubow/lgll-student-survey',
'aceLinksData:',
'  - https://osf.io/rbya9/',
'aceSummary: Assist with the implementation and analysis of the annual student survey of library services.',
'aceStatus: In progress',
'databytes: true',
'aceUpdate: Codebase revised and working on next iteration of the dashboard',
'updated: 2024-10-25',
'---'
]

text = '\n'.join(lines)

def is_property_line(x):
    re.search("^---$", x)




def extract_properties(file_path):
    """Returns properties header from an Obsidian note"""

    with open(file_path, 'r') as file:
        text = file.readlines()


def extract_key_value_pairs(file_path):
    """Extracts key-value pairs from a text file with multiline values."""

    with open(file_path, 'r') as file:
        text = file.read()

    pairs = {}
    matches = re.findall(r'(\w+):\s*([\s\S]*?)(?=\w+:|$)', text)

    for key, value in matches:
        pairs[key] = value.strip()

    return pairs


print(text)

# if __name__ == '__main__':
#     file_path = 'your_text_file.txt'  # Replace with your file path
#     result = extract_key_value_pairs(file_path)
#     print(result)


