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
    instance_pattern = re.compile(r"\(instance\s+(\S+)\s+\(viewref\s+\S+\s+\(cellref\s+(\S+)\)\)\)")
    cell_pattern = re.compile(r"\(cell\s+(\S+)")

    with open(file_path, 'r') as file:
        for line in file:
            # Check if the line defines a new cell
            cell_match = cell_pattern.search(line)
            if cell_match:
                current_cell_name = cell_match.group(1)
                current_cell = cells.get(current_cell_name, Cell(current_cell_name))
                cells[current_cell_name] = current_cell
                continue

            # Check if the line defines an instance within the current cell
            instance_match = instance_pattern.search(line)
            if instance_match and current_cell:
                instance_name = instance_match.group(1)
                child_cell_name = instance_match.group(2)
                
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

