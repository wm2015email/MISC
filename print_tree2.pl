use strict;
use warnings;

# Define the Cell class
package Cell;
sub new {
    my ($class, $name) = @_;
    my $self = {
        name => $name,
        children => []
    };
    bless $self, $class;
    return $self;
}

sub add_child {
    my ($self, $child) = @_;
    push @{$self->{children}}, $child;
}

sub repr {
    my ($self, $level) = @_;
    $level ||= 0;
    my $ret = ('    ' x $level) . $self->{name} . "\n";
    for my $child (@{$self->{children}}) {
        $ret .= $child->repr($level + 1);
    }
    return $ret;
}

package main;

# Parsing function
sub parse_file {
    my ($file_path) = @_;
    my %cells;
    my $current_cell = undef;

    open my $fh, '<', $file_path or die "Cannot open file: $!";

    while (<$fh>) {
        chomp;
        # Match ENTITY lines
        if (/^ENTITY\s+(\S+)/) {
            my $cell_name = $1;
            $current_cell = $cells{$cell_name} ||= Cell->new($cell_name);
        }
        # Match CINST lines
        elsif (/^CINST\s+(\S+)/ && defined $current_cell) {
            my $instance_name = $1;
            $current_cell->add_child(Cell->new($instance_name));
        }
    }

    close $fh;

    # Find top-level cells (cells that are not children of any other cell)
    my @top_level_cells = grep {
        my $cell_name = $_;
        not grep { $_->repr() =~ /$cell_name/ } values %cells;
    } values %cells;

    return @top_level_cells;
}

# Main function
sub main {
    my $file_path = 'your_file.txt';  # Replace with the path to your file
    my @top_level_cells = parse_file($file_path);

    if (@top_level_cells) {
        print "Hierarchical Structure of Cells:\n";
        for my $cell (@top_level_cells) {
            print $cell->repr();
        }
    } else {
        print "No top-level cells found in the file.\n";
    }
}

main();

