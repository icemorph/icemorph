use strict;
use warnings;

use Algorithm::Viterbi;
use DBI;
use Set::CrossProduct;
use Data::Dumper;
use Getopt::Long;


$|++;

my $dbh = DBI->connect( "DBI:mysql:icemorph;host=localhost", "esa", "esa" ) or die $!;

my $max_nr_poscs = 1000;

my $options = GetOptions( "max_nr_poscs=s" => \$max_nr_poscs );


# get EXPERT and GOLD test data
my $test_data;
my $nr_test_data = 0;
open IN, "/var/icemorph/data/hand_tagged_corpora" or die $!;
while ( my $line = <IN> ) {
   my @lmnts = split /\t/, $line;
   $nr_test_data++;
   #next if $nr_test_data > 190;
   #next if $nr_test_data < 761;
   $test_data->{$lmnts[0]}->{type} = $lmnts[1];
   $test_data->{$lmnts[0]}->{env} = $lmnts[2];
   $test_data->{$lmnts[0]}->{pos} = $lmnts[3];
   $test_data->{$lmnts[0]}->{ms} = $lmnts[4];
}
close IN;


# get POSC to POS mapping
my $posc_to_pos;
eval {
   my $sql = "SELECT DISTINCT posc FROM lexeme";
   my $sth = $dbh->prepare($sql);
   $sth->execute();
   while ( my $r = $sth->fetchrow_hashref() ) {
      my $pos = $r->{posc};
      next if ! defined $pos || $pos eq '';
      $pos =~ s/ .+$//;
      $posc_to_pos->{$r->{posc}} = $pos;
   }
};
die $@ if $@;
eval {
   my $sql = "SELECT * FROM decl_info";
   my $sth = $dbh->prepare($sql);
   $sth->execute();
   while ( my $r = $sth->fetchrow_hashref() ) {
      $posc_to_pos->{$r->{decl_info}} = $r->{pos};
   }
};
die $@ if $@;


# Get expert feedback from LEXEME table (this is for P forms)
my $form_to_posc;
my $already_processed;
eval {
   my $sql = "SELECT c.decl_info, c.form_norm, l.posc FROM corpus c, lexeme l " .
             "WHERE c.dict_id = l.lexeme_id AND c.confirmed_by = 'Expert Feedback'";
   my $sth = $dbh->prepare($sql);
   $sth->execute();
   while ( my $r = $sth->fetchrow_hashref() ) {
      my $di = $r->{decl_info} && $r->{decl_info} ne '' && $r->{decl_info} ne 'not defined' ? $r->{decl_info} : $r->{posc};
$di =~ s/_(?:nom|dat|gen|acc)_(?:sg|pl)// if $di =~ /^prespar/;
      next if ! $di || $di eq '' || $di eq 'not defined';
      my $value = $r->{form_norm};
      next if $already_processed->{$value}->{$di};
      push @{$form_to_posc->{$value}}, $di;
      $already_processed->{$value}->{$di} = 1;
   }   
};
die $@ if $@;


eval {
   my $sql = "SELECT declension, lexeme_id FROM lexeme WHERE declension IS NOT NULL AND declension != ''";
   my $sth = $dbh->prepare($sql);
   $sth->execute();
   while ( my @row = $sth->fetchrow_array() ) { 
      if ( $row[0] ) { 
         while ( $row[0] =~ /\"([^\"]+)\":\"([^\"]+)\"/g ) { 
            my $key = $1; 
            my $value = lc($2);
$key =~ s/_(?:nom|dat|gen|acc)_(?:sg|pl)// if $key =~ /^prespar/;
            next if $already_processed->{$value}->{$key};
            push @{$form_to_posc->{$value}}, $key;
            $already_processed->{$value}->{$key} = 1;
         }   
      }   
   }   
};
die $@ if $@;


# create training data
open TRAIN, ">train.data" or die $!;
binmode TRAIN, ":utf8";
eval {
   my $sql = "SELECT corpus_id, form_text, form_norm FROM corpus";
   my $sth = $dbh->prepare($sql);
   $sth->execute();
   my $possible_ms;
   my $form_norms;
   my $bos = 0;
   while ( my $r = $sth->fetchrow_hashref() ) {
      my @poscs = $form_to_posc->{$r->{form_norm}} ? @{$form_to_posc->{$r->{form_norm}}} : ();
      @poscs = @poscs[0..$max_nr_poscs-1] if $max_nr_poscs-1 < $#poscs;
      @poscs = () if $test_data->{$r->{corpus_id}}; # THIS IS FOR SPLITTING TEST DATA
      if ( $#poscs == -1 || $bos ) {
         my $it = Set::CrossProduct->new( $possible_ms );
         if ( defined $it ) {
#            print "corpus_id = " . $r->{corpus_id} . "\n";
#            my $f = join ' ', @$form_norms;
#            print "segment = $f\n";
#            print "nr words in segment = " . $#$possible_ms . ", nr combinations = " . $it->cardinality . "\n";
            while ( my $tuple = $it->get ) {
               foreach my $i ( 0..$#{$tuple} ) {
                  print TRAIN $form_norms->[$i] . "\t" . $tuple->[$i] . "\t" . $tuple->[$i] . "\n";
               }
               print TRAIN "\n";
            }
         }
         undef $possible_ms;
         undef $form_norms;
         $bos = 0;
      } else {
         push @$possible_ms, \@poscs;
         push @$form_norms, $r->{form_norm};
         $bos = 1 if $r->{form_text} =~ /[\.\?\!\;\,]/;
      }
   }
};
die $@ if $@;
close TRAIN;


print "creating test.data file ...\n";
open TEST, ">test.data" or die $!;
binmode TEST, ":utf8";
eval {
   my $sql = "SELECT corpus_id, form_text, form_norm FROM corpus";
   my $sth = $dbh->prepare($sql);
   $sth->execute();
   my @permutations = ();
   my $contains_test_form = 0;
   while ( my $r = $sth->fetchrow_hashref() ) {
      $contains_test_form = 1 if $test_data->{$r->{corpus_id}};
      my $posc = $test_data->{$r->{corpus_id}} 
                 ? $test_data->{$r->{corpus_id}}->{pos} . "\t" . $test_data->{$r->{corpus_id}}->{ms} . "\t" . $test_data->{$r->{corpus_id}}->{type} 
                 : "xxx\txxx";
      push @permutations, $r->{form_norm} . "\t" . $posc;
      if ( $r->{form_text} =~ /[\.\?\!\;\,]/ ) {
         my $out = join "\n", @permutations;
         print TEST $out . "\n\n" if $contains_test_form;
         @permutations = ();
         $contains_test_form = 0;
      }
   }
};
die $@ if $@;
close TEST;
      
