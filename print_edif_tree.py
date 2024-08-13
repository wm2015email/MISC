import re
import argparse

class Cell:
    def __init__(self, name):
        self.name = name
        self.children = []

    def add_child(self, child):
        self.children.append(child)

    def __repr__(self, level=0):
        ret = "\t" * level + f"{self.name}\n"
        for child in self.children:
            ret += child.__repr__(level + 1)
        return ret

def parse_edif(file_path):
    cells = {}
    current_cell = None

    # This pattern matches both cell declarations and instance references.
    pattern = re.compile(r"\(cell\s+(\S+)|\(instance\s+(\S+)\s+\(viewref\s+\S+\s+\(cellref\s+(\S+)\)\)\)")

    with open(file_path, 'r') as file:
        for line in file:
            match = pattern.search(line)
            if match:
                if match.group(1):  # This group matches the cell definition
                    current_cell_name = match.group(1)
                    current_cell = cells.get(current_cell_name, Cell(current_cell_name))
                    cells[current_cell_name] = current_cell
                elif match.group(2) and current_cell:  # These groups match the instance within a cell
                    instance_name = match.group(2)
                    child_cell_name = match.group(3)
                    
                    if child_cell_name not in cells:
                        cells[child_cell_name] = Cell(child_cell_name)
                    
                    current_cell.add_child(cells[child_cell_name])

    # Assuming the top-level cell is the one without any parents
    top_level_cells = [cell for cell in cells.values() if not any(cell in c.children for c in cells.values())]
    return top_level_cells

def main():
    parser = argparse.ArgumentParser(description='Parse an EDIF netlist file and print the hierarchy of cells.')
    parser.add_argument('file_path', type=str, help='The path to the EDIF netlist file')
    args = parser.parse_args()

    top_level_cells = parse_edif(args.file_path)
    
    if top_level_cells:
        print("Hierarchical Structure of Cells:")
        for cell in top_level_cells:
            print(cell)
    else:
        print("No top-level cells found in the EDIF netlist file.")

if __name__ == "__main__":
    main()

