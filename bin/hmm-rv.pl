use strict;
use warnings;

use Algorithm::Viterbi;
use DBI;
use Getopt::Long;
use Data::Dumper;

my $test_set = "all";
my $options = GetOptions( "test_set=s" => \$test_set );


$|++;

my $dbh = DBI->connect( "DBI:mysql:icemorph;host=localhost", "esa", "esa" ) or die $!;

my $decl_info;
my $start;
my $emission;
my $transition;

my $nr_starts = 0;
my $nr_emissions;
my $nr_transitions;



# get EXPERT and GOLD test data
my $test_data;
my $count_test_data = 0;
open IN, "/var/icemorph/data/hand_tagged_corpora" or die $!;
while ( my $line = <IN> ) {
   next unless $line =~ /^\d/;
   my @lmnts = split /\t/, $line;
   $count_test_data++;
   next if $test_set eq 'first20' && $count_test_data > 190;
   next if $test_set eq 'last20' && $count_test_data < 760;
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
            next if $already_processed->{$value}->{$key};
            push @{$form_to_posc->{$value}}, $key;
            $already_processed->{$value}->{$key} = 1;
         }   
      }   
   }   
};
die $@ if $@;


# add edit distance hits for forms that have no posc associated with them yet
my $distance_max = 1.4;
my $edit_distance;
eval {
   my $sql = "select di.decl_info, lp.form, ld.distance " . 
             "FROM lexeme_prototype lp, decl_info di, lexeme_distance ld " . 
             "WHERE ld.lexeme_prototype_id = lp.lexeme_prototype_id AND lp.declinfo_id = di.id " . 
             "AND ld.distance <= ? ORDER BY ld.distance ASC";
   my $sth = $dbh->prepare($sql);
   $sth->execute( $distance_max );
   my $current_distance = undef;
   my $previous_di = undef;
   while ( my $r = $sth->fetchrow_hashref() ) { 
      my $key = $r->{decl_info};
      my $value = $r->{form};
      next if $already_processed->{$value};
      if ( ! defined $previous_di || $key ne $previous_di ) {
         $current_distance = $r->{distance};
         $previous_di = $key;
      }
      if ( $r->{distance} == $current_distance ) { 
         push @{$edit_distance->{$value}}, $key;
      }   
   }   
};
die $@ if $@;

#print "form_to_posc:\n\n" . Dumper($form_to_posc) . "\n\n";
#print "edit_distance:\n\n" . Dumper($edit_distance) . "\n\n";
#die;



# create HMM
eval {
   my $sql = "SELECT c.corpus_id, c.decl_info, c.confirmed_by, c.form_text, c.form_norm, l.posc FROM corpus c, lexeme l " .
             "WHERE c.dict_id = l.lexeme_id";
   my $sth = $dbh->prepare($sql);
   $sth->execute();
   
   my $count = 0;
   my $bos = 1;
   my $previous_states = undef;
   while ( my $r = $sth->fetchrow_hashref() ) {
      my @poscs = $form_to_posc->{$r->{form_norm}} ? @{$form_to_posc->{$r->{form_norm}}} : ();
      @poscs = () if $test_data->{$r->{corpus_id}};
      foreach my $p ( @poscs ) {
         $decl_info->{$p} = 1;
      }
      my $is_allowed = $#poscs > -1 ? 1 : 0;
      if ( $is_allowed && defined $previous_states && ! $bos ) {
         foreach my $ps ( @$previous_states ) {
            foreach my $p ( @poscs ) {
               $transition->{$ps}->{$p}++;
               $nr_transitions->{$ps}++;
            }
         }
      }
      if ( $bos ) {
         foreach my $p ( @poscs ) {
            $start->{$p}++ if $is_allowed;
            $nr_starts++ if $is_allowed;
         }
         $bos = 0;
      }
      if ( $r->{form_text} =~ /[\.\?\!]/ ) {
         $bos = 1;
      }
      if ( $is_allowed ) {
         foreach my $p ( @poscs ) {
            $emission->{$r->{form_norm}}->{$p}++;
            $nr_emissions->{$r->{form_norm}}++;
         }
      }
      $previous_states = $is_allowed ? \@poscs : undef;
      $count++;
      #print "processed $count\n" if $count % 10000 == 0;
   }
};

# normalize probabilities
foreach my $k ( keys %$decl_info ) {
   $start->{$k} = $start->{$k} ? $start->{$k} / $nr_starts : 0;
}

foreach my $k ( keys %$transition ) { 
   foreach my $l ( keys %{$transition->{$k}} ) { 
      $transition->{$k}->{$l} = $transition->{$k}->{$l} / $nr_transitions->{$k};
   }   
}

foreach my $k ( keys %$emission ) { 
   foreach my $l ( keys %{$emission->{$k}} ) { 
      $emission->{$k}->{$l} = $emission->{$k}->{$l} / $nr_emissions->{$k};
   }   
}


#print "START\n\n" . Dumper($start) . "\n\n";
#print "TRANS\n\n" . Dumper($transition) . "\n\n";
#print "EMISS\n\n" . Dumper($emission) . "\n\n";
#die;


# initiate Viterbi
my $vit = Algorithm::Viterbi->new(unknown_emission_prob => 1, unknown_transition_prob => 1e-100);
$vit->start($start);
$vit->transition($transition);
$vit->emission($emission);

my $stats;
eval {
   my $sql = "SELECT * from corpus";
   my $sth = $dbh->prepare($sql);
   $sth->execute();
   my $observations;
   my $bos = 1;
   my $word_index = 1;
   my $word_index_start = 1;
   while ( my $r = $sth->fetchrow_hashref() ) {
      if ( $bos ) {
         if ($observations) {
            my $do_test = 0;
            my $word_index_end = $word_index_start + $#{$observations} + 1;
            foreach my $k ( $word_index_start..$word_index_end ) {
               if ( $test_data->{$k} ) {
                  $do_test = 1;
                  last;
               }
            }
            if ( $do_test ) {
               print "now evaluating " . $r->{corpus_id} . "\n";
               my ($prob, $v_path, $v_prob) = $vit->forward_viterbi($observations, $form_to_posc, $edit_distance);
               # USE THE FOLLOWING CODE TO TEST PERFORMANCE IF FORM-TO-POSC OR EDIT DISTANCE DATA IS NOT AVAILABLE
               #my %no_form_to_posc;
               #my %no_edit_dist;
               #my ($prob, $v_path, $v_prob) = $vit->forward_viterbi($observations, \%no_form_to_posc, $edit_distance);
               foreach my $k ( 0..$#{$observations} ) {
                  if ( $test_data->{$word_index} ) {
                     my $pos_ok = $posc_to_pos->{$v_path->[$k]} eq $test_data->{$word_index}->{pos} ? 1 : 0;
                     my $ms_ok = $v_path->[$k] eq $test_data->{$word_index}->{ms} ? 1 : 0;
                     my $type = $test_data->{$word_index}->{type};
                     if ( $test_data->{$word_index}->{pos} ) {
                        $stats->{$type}->{pos}->{all}++;
                        $stats->{$type}->{pos}->{cor}++ if $pos_ok;
                     }
                     if ( $test_data->{$word_index}->{ms} ) {
                        $stats->{$type}->{ms}->{all}++;
                        $stats->{$type}->{ms}->{cor}++ if $ms_ok;
                     }
                  }
                  $word_index++;
               }
            } else {
               $word_index += $#{$observations}+1;
            }
            undef $observations;
         }
         $bos = 0;
         $word_index_start = $word_index;
      }
      if ( $r->{form_text} =~ /[\.\?\!\;\,]/ ) {
         $bos = 1;
      }
      push @$observations, $r->{form_norm};
   }
};
die $@ if $@;


print "RESULTS\n\n";
eval { print "EXPERT POS = " . ( $stats->{expert}->{pos}->{cor} / $stats->{expert}->{pos}->{all} ) . "\n"; };
eval { print "EXPERT MS  = " . ( $stats->{expert}->{ms}->{cor} / $stats->{expert}->{ms}->{all} ) . "\n"; };
eval { print "GOLD POS   = " . ( $stats->{gold}->{pos}->{cor} / $stats->{gold}->{pos}->{all} ) . "\n"; };
eval { print "GOLD MS    = " . ( $stats->{gold}->{ms}->{cor} / $stats->{gold}->{ms}->{all} ) . "\n"; };

