use strict;
use lib qw(.);
use Math::Group::Thompson;
use Getopt::Std;

my %args;
getopts("r:v:",\%args);

# Chequear radio
if(!exists $args{r}) {
  die "\nNo indico un radio\n\n";
} elsif( $args{r} =~ /\D/) {
  die "\nEl radio solo puede ser un numero entero\n\n";
}


# Chequear verbose
my $verbose = 0;
if(exists $args{v}) {
  if($args{v} == 1 || $args{v} == 2) {
    $verbose = $args{v};
  }
}

# Chequear el elemento pasado
my $g;
if(!defined $ARGV[0]) {
  $g = '';
} else {
  $g = $ARGV[0];
  # Readecuar el radio para obtener el size de gB(n)
  $args{r} += length($g);
}

# Crear objeto Grupo de Thompson
my $F = Math::Thompson->new( VERBOSE => $verbose );
print "\nCardinalidad de la bola de radio $args{r} : ".$F->cardBn($args{r},$g)."\n";
