use strict;
use warnings;

# A hash to store the hierarchy
my %tree;

# Read the file line by line
while (<>) {
    chomp;
    
    # Check for ENTITY or CINST keywords
    if (/^(ENTITY|CINST)\s+(\S+)/) {
        my ($type, $name) = ($1, $2);
        
        if ($type eq 'ENTITY') {
            # Add ENTITY as a key in the tree hash
            $tree{$name} = {};
        } elsif ($type eq 'CINST') {
            # Add CINST as a child under the last ENTITY seen
            my $last_entity = (keys %tree)[-1];
            $tree{$last_entity}{$name} = {};
        }
    }
}

# Print the tree structure
sub print_tree {
    my ($hash, $indent) = @_;
    $indent //= '';
    foreach my $key (keys %$hash) {
        print "$indent$key\n";
        print_tree($hash->{$key}, $indent . '  ');
    }
}

print_tree(\%tree);
