def read_to_string(path):
    with open(path, 'r') as f:
        text = f.read()
    return text.strip()