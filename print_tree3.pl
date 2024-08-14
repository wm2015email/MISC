use strict;
use warnings;

# Function to add a child cell to a parent cell
sub add_child {
    my ($parent, $child) = @_;
    push @{$parent->{children}}, $child;
}

# Function to print the cell hierarchy with indentation
sub print_hierarchy {
    my ($cell, $indent_level) = @_;
    
    # If no indent level is provided, start with 0 (top level)
    $indent_level ||= 0;

    # Print the current cell's name with indentation
    print '    ' x $indent_level, $cell->{name}, "\n";

    # Recursively print each child with increased indentation
    foreach my $child (@{$cell->{children}}) {
        print_hierarchy($child, $indent_level + 1);
    }
}

# Function to check if a cell has a specific child or descendant
sub has_descendant {
    my ($cell, $child_name) = @_;

    # Check each child to see if it's the one we're looking for
    foreach my $child (@{$cell->{children}}) {
        return 1 if $child->{name} eq $child_name || has_descendant($child, $child_name);
    }
    return 0;
}

# Function to read the file and build the cell hierarchy
sub parse_file {
    my ($file_path) = @_;

    # Hash to store all cells by their names
    my %cells;
    my $current_cell;

    # Open the file for reading
    open my $fh, '<', $file_path or die "Cannot open file: $!";

    # Process the file line by line
    while (<$fh>) {
        chomp;  # Remove newline at the end of the line

        # If the line starts with "ENTITY", we're defining a new cell
        if (/^ENTITY\s+(\S+)/) {
            my $cell_name = $1;

            # If the cell doesn't exist yet, create it with an empty list of children
            $cells{$cell_name} ||= { name => $cell_name, children => [] };

            # Set this cell as the current one we're working on
            $current_cell = $cells{$cell_name};
        }
        # If the line starts with "CINST", we're adding a child instance to the current cell
        elsif (/^CINST\s+(\S+)/ && defined $current_cell) {
            my $child_name = $1;

            # If the child cell doesn't exist yet, create it
            $cells{$child_name} ||= { name => $child_name, children => [] };

            # Add this child cell to the current cell's list of children
            add_child($current_cell, $cells{$child_name});
        }
    }

    close $fh;

    # Find and return top-level cells (those that are not children of any other cell)
    my @top_level_cells = grep {
        my $cell_name = $_->{name};
        not grep { has_descendant($_, $cell_name) } values %cells;
    } values %cells;

    return @top_level_cells;
}

# Main function to control the script flow
sub main {
    my $file_path = 'file.vsf'; 
	
    # Parse the file to get the top-level cells
    my @top_level_cells = parse_file($file_path);

    # Print the cell hierarchy if we have top-level cells
    if (@top_level_cells) {
        print "Hierarchical Structure of Cells:\n";
        foreach my $cell (@top_level_cells) {
            print_hierarchy($cell);
        }
    } else {
        print "No top-level cells found in the file.\n";
    }
}

# Start the script by calling the main function
main();
