package Games::Sudoku::Win32;

use strict;
use vars;
use Win32::GUI;
use Pod::Usage;
use File::Basename;

use constant OUT_TT_PRECIS => 4;
use constant FF_SWISS => 32;
my ($W, $Board, $CandWin, $Menu, $Nextstep, $Verbosity, %opts);

=pod

=head1 NAME

Games::Sudoku::Win32

=head1 DESCRIPTION

Solve and play a Soduko game, with optional Win32 GUI

=head1 FUNCTIONS

=over

=cut

# inarray ignoring 0
sub inarray ($@) {
  my $n = shift;
  for (@_) {
    next unless $_;
    return 1 if $_ == $n;
  }
  0
}

# inarray including 0
sub inarray_0 ($@) {
  my $n = shift;
  for (@_) {
    return 1 if $_ == $n;
  }
  0
}

sub count {
  my $count = 0;
  for (@_) {
    $count++ if $_;
  }
  $count;
}

sub count_of ($@) {
  my $n = shift;
  my $count = 0;
  for (@_) {
    $count++ if $_ and $_ == $n;
  }
  $count;
}

=pod

=item which_col(i) => col

0 => 0, 9 => 0

=item which_row(i) => row

  0..8  => 0
  9..17 => 1

=item which_squ(i) => squ

0 1 2 9 10 11 18 19 20 => 0

=cut

# i => col
# 0 => 0, 9 => 0
sub which_col ($) {
  my $i = shift;
  return $i % 9;
}
sub which_row ($) {
  my $i = shift;
  return int($i / 9);
}
# 0 1 2 9 10 11 18 19 20 => 0
sub which_squ ($) {
  my $i = shift;
  my $col = which_col($i);
  my $row = which_row($i);
  return int($col / 3) + (int($row / 3) * 3);
}

# helper 60 => "60 (row 0, col 2)"
# helper 60 => "60 (1/3)"
sub at_i ($) {
  my $i = shift;
  # return "$i (row ".which_row($i).", col ".which_col($i).")";
  return "$i (".(which_row($i)+1)."/".(which_col($i)+1).")";
}

=pod

=item row_i(i) => list of indices

row_i(0) => 0..8

=cut

sub row_i {
  my $i = shift;
  my $j = which_row($i);
  return (($j*9)+0 .. ($j*9)+8);
}

sub col_i {
  my $n = shift;
  my $i = which_col($n);
  return ($i+0,$i+9,$i+18,$i+27,$i+36,$i+45,$i+54,$i+63,$i+72);
}

# squ(0) => 0,1,2,9,10,11,...
# squ(40) => 30,31,32,39,40,41,48,49,50
sub squ_i {
  my $n = shift;
  my $i = int(which_col($n) / 3) * 3; # 012 => 0, 345 => 3, 678 => 6
  my $j = int(which_row($n) / 3) * 3;
  my $x = $i + ($j * 9);
  #return int($col / 3) + (int($row / 3) * 3);
  return ($x+0,$x+1,$x+2,$x+9,$x+10,$x+11,$x+18,$x+19,$x+20);
}

# the reverse: row to index
sub i_row {
  my $r = shift;
  return (($r*9)+0 .. ($r*9)+8);
}
sub i_col {
  my $r = shift;
  return ($r+0,$r+9,$r+18,$r+27,$r+36,$r+45,$r+54,$r+63,$r+72);
}
# i_squ(0) => 0,1,2 9,10,11 18,19,20, 1=> 345..., 2=> 678..., 3=> 21,22,23
sub i_squ {
  my $r = shift;
  return squ_i(int($r / 3) * 27 + (($r % 3) * 3));
}

# return the list of missing values from 1..9
# (1,5,2,3,6)
# => (4,7,8,9)
sub missing (@) {
  my (%h, @r);
  for (@_) { $h{$_}++ }
  for (1..9) { push @r,($_) unless $h{$_} }
  @r;
}

=pod

=back

=head1 METHODS

=over

=item Games::Sudoku::Win32->new(1,2,3,4,5,6,7,8,9,0,0,0,0,...81);

=cut

sub new {
  my $s = shift;
  my @board;
  for my $i (0..80) { $board[$i] = 0; }
  my $args = { board      => [@board],
               candidates => [],
               rules      => [qw(one_missing one_candidate single_candidate
                                 naked_pairs hidden_pairs hidden_triples
                                 locked_candidate_1 locked_candidate_2
                                )], #
               init       => count(@board),
             };
  my $obj = bless $args, $s;
  $Verbosity = 1 unless defined $Verbosity;
  # print "init candidates:\n" unless $W;
  if (@_) {
    $obj->open(@_);
  } else {
    $obj->init_candidates();
  }
  $obj;
}

=pod

=item ->init_candidates

For simplicity we use ->found for all board positions to init the list of candidate values,
starting with [1..9] we remove the found candidates.

{init} is a boolean marker not to print the found positions at startup.

=cut

# with [1..9]
sub init_candidates {
  my $s = shift;
  $s->{init}++;
  my @board = @{$s->{board}};
  my $candidates = [];
  for my $i (0..80) {
    $s->{candidates}->[$i] = $board[$i] ? [] : [1..9];
  }
  for my $i (0..80) {
    if ($board[$i]) {
      $s = $s->found($i, $board[$i]);
    }
  }
  delete $s->{init};
  $s;
}

sub status($;@);

=pod

=item ->row(i) => list of board values at row with index i

row(5) => $board[0..8]

=cut

sub row {
  my $s = shift;
  my $i = shift;
  my @board = @{$s->{board}};
  return @board[row_i($i)];
}

# ->col(0)
# => 0,9,18,27,...81
sub col {
  my $s = shift;
  my $i = shift;
  my @board = @{$s->{board}};
  return @board[col_i($i)];
}

sub squ ($$) {
  my $s = shift;
  my $n = shift;
  my @board = @{$s->{board}};
  return @board[squ_i($n)];
}

=pod

=item ->board [i [value]]

On i return board value at i

On i and value set board value at i

Without arguments return board list

=cut

sub board ($$;$) {
  my $s = shift;
  if (@_) {
    if (@_ == 2) { $s->{board}->[$_[0]] = $_[1]; }
    return $s->{board}->[$_[0]];
  } else {
    return @{$s->{board}};
  }
}


sub validate ($) {
  my $s = shift;
  my @board = @{$s->{board}};
  status "validate" if $Verbosity >= 2;
  for my $row (qw(row col squ)) {
    for my $r (0..8) {
      no strict 'refs';
      my @j = &{"i_".$row}($r); # indices
      my $j = @j[0];
      my @c = @{$s->{candidates}->[$j]};
      my @l = &{$row}($s,$j);   # values
      if ($board[$j]) {
        (count(@l) > 1 and count(@l) <= 9) or die "VALIDATE: 1<@l<=9 $row $r at ".at_i($j);
        inarray($board[$j],@l) or die "VALIDATE: $board[$j] $row at ".at_i($j);
      } else {
        count(@c) >= 1 
          or warn "VALIDATE: no candidates [".join("",@c)."] $row $r at empty ".at_i($j);
      }
      @l == 9 or die "VALIDATE: @l<>9 at $j";
      for (1..9) { count_of($_,@l) < 2
                     or die "VALIDATE: count($_,@l) $row $r at ".at_i($j); }
      count(@l) <= 9 or die "VALIDATE: count(@l) $row $r at ".at_i($j);
      # each value 1..9 is either in the group or in the candidates
      for my $v (1..9) {
        my @cands = map{ @{$s->{candidates}->[$_]} } @j;
        inarray($v,@l)
          or inarray($v,@cands)
            or die "VALIDATE: $v not in [ @l ] nor cand [ @cands ]\n  $row $r at ".at_i($j);
      }
    }
  }
}

=pod

=item ->found(i,b) => move b from all candidates in the groups to the board at i

=cut

sub found ($$$) {
  my $s = shift;
  my $i = shift;
  my $f = shift;
  die "do not overwrite ".$s->board($i)." with $f at $i" if $s->board($i) and !$s->{init};
  # now also remove $f from the neighbor candidates, same col,row,squ
  for my $row (qw(row col squ)) {
    no strict 'refs';
    for my $j (&{$row."_i"}($i)) {
      my @l = &{$row}($s,$i);
      if ($j>80 or $j<0) {print "ERROR: $row $i=>$j\n"; next;}
      if (inarray($f,@l) and !$s->{init}) {
        $s->Show();
        die "$f already in $row at ".at_i($i)." [".join("",@l)."]";
      }
      my @c = @{$s->{candidates}->[$j]};
      if (@c and inarray($f,@c)) {
        $s->{candidates}->[$j] = [ grep { $f != $_ } @c ];
        if (!$s->{init} and $Verbosity > 1) {
          status "remove candidate $f from $row at ".at_i($j)
            .($Verbosity > 2 ? ", remaining [".join("",@{$s->{candidates}->[$j]})."]" : "");
        }
      }
    }
  }
  # and finally place the board
  $s->{board}->[$i] = $f;
  # remove the candidate $f at i
  $s->{candidates}->[$i] = []; #[ grep { $f != $_ } @{$s->{candidates}->[$i]} ];
  $s->validate if $Verbosity >= 3 and !$s->{init};
  $s->Show if $Verbosity >= 2 and !$s->{init};
  #status "found $f at $i" unless $s->{init} and $Verbosity < 2;
  $s;
}

=pod

=item RULE ->one_candidate

If only one potential candidate, L<found>() it i.e. move this value
from the candidates (in all groups) to the board.

=cut

sub one_candidate {
  my $s = shift;
  my $found = 1;
  while ($found) {
    $found = 0;
    for my $i (0..80) {
      my @c = @{$s->{candidates}->[$i]};
      if (@c == 1) {
        my $v = $c[0];
        status "found one_candidate $v at ".at_i($i) if $Verbosity >= 1;
        $s = $s->found($i,$v);
        $found++;
      }
    }
  }
  $s;
}

=pod

=item RULE ->one_missing

If in a row,col or square 8 are known, the missing ninth is also known.

=cut

sub one_missing {
  my $s = shift;
  my @board = @{$s->{board}};
  for my $row (qw(row col squ)) {
    ROW:
    for my $r (0..8) {
      no strict 'refs';
      my @j = &{"i_".$row}($r); # indices
      my $j = @j[0];
      my @l = &{$row}($s,$j);   # values
      if (count(@l) == 8) {
        for my $i (@j) {
          if (!$board[$i]) { # Fill the first missing from @l - only one
            my @v = missing(@l);
            if (@v != 1) {
              $s->Show();
              die "internal error at ".at_i($i)." in missing(".join(",",@l).")=>".
                join(",",@v);
            }
            status "found one_missing $v[0] in $row $r at ".at_i($i) if $Verbosity >= 1;
            $s = $s->found($i,$v[0]);
            next ROW;
          }
        }
      }
    }
  }
  $s;
}

sub unique{
  my $s = shift;
  my %h;
  for my $i (@_) {
    for my $c (@{$s->{candidates}->[$i]}) {
      $h{$c}->{i} = $i;
      $h{$c}->{c}++;
    }
  }
  for my $c (keys %h) {
    return ($c, $h{$c}->{i}) if $h{$c}->{c} == 1;
  }
  ();
}

=pod

=item RULE ->single_candidate

For each row, col and square look for a single candidate in the whole group
i.e. the list of candidates is of length 1.
Also called "hidden_single"

=cut

sub single_candidate {
  my $s = shift;
  my @board = @{$s->{board}};
  for my $j (0..80) {
    my ($v,$i);
    for my $row (qw(row col squ)) {
      no strict 'refs';
      if (($v,$i) = $s->unique(&{$row."_i"}($j))) {
        my $r = &{"which_".$row}($j);
        status "found single_candidate $v in $row $r at ".at_i($i) if $Verbosity >= 1;
        $s = $s->found($i,$v);
      }
    }
  }
  $s;
}

sub pairs {
  my $s = shift;
  my %h;
  for my $i (@_) {
    for my $c (@{$s->{candidates}->[$i]}) {
      $h{$c}->{i} = $i;
      $h{$c}->{c}++;
    }
  }
  for my $c (keys %h) {
    return ($c, $h{$c}->{i}) if $h{$c}->{c} == 2;
  }
  ();
}

=pod

=item RULE ->naked_pairs

If two cells in a group contain an identical pair of
candidates and only those two candidates, then no other
cells in that group could be those values.
L<http://www.angusj.com/sudoku/hints.php>

=cut

sub naked_pairs {
  my $s = shift;
  my @board = @{$s->{board}};
  for my $j (0..80) {
    NEXT:
    for my $row (qw(row col squ)) {
      no strict 'refs';
      my (%h, @pair);
      for my $i (&{$row."_i"}($j)) {
        my @c = @{$s->{candidates}->[$i]};  # always sorted
        next unless @c == 2;
        push @pair, ({i=>$i,b1=>$c[0],b2=>$c[1]});
      }
      if (@pair == 2) {
        my $pair_i1 = $pair[0]->{i};
        my $pair_i2 = $pair[1]->{i};
        next NEXT if $pair_i1 == $pair_i2;
        next NEXT if $pair[0]->{b1} != $pair[1]->{b1};
        next NEXT if $pair[0]->{b2} != $pair[1]->{b2};
        my $pair_b1 = $pair[0]->{b1};
        my $pair_b2 = $pair[0]->{b2};
        my $r = &{"which_".$row}($j);
        next NEXT if $s->{naked}->{$pair_i1}->{$pair_i2};
        status "found two naked_pairs ($pair_b1,$pair_b2) in $row $r at ".at_i($pair_i1).", ".at_i($pair_i2)
          if $Verbosity >= 1;
        $s->{naked}->{$pair_i1}->{$pair_i2}++;
        # delete it from the other candidates
        for my $i (&{$row."_i"}($j)) {
          if ($i != $pair_i1 and $i != $pair_i2) {
            $s->{candidates}->[$i] = [ grep { $pair_b1 != $_ } @{$s->{candidates}->[$i]} ];
            $s->{candidates}->[$i] = [ grep { $pair_b2 != $_ } @{$s->{candidates}->[$i]} ];
            status "remove candidates ($pair_b1,$pair_b2) from $row($r) at ".at_i($i)
              if $Verbosity >= 2;
          }
        }
      }
    }
  }
  $s;
}

=pod

=item RULE ->hidden_pairs

If two cells in a group contain a pair of candidates
(hidden amongst other candidates) that are not found in
any other cells in that group, then other candidates in
those two cells can be excluded safely.
L<http://www.angusj.com/sudoku/hints.php>

=cut

sub unique_c{
  my $s = shift;
  my (%h,@pair);
  for my $i (@_) {
    for my $c (@{$s->{candidates}->[$i]}) {
      $h{$c}->{i} = $i;
      $h{$c}->{c}++;
    }
  }
  for my $c (keys %h) {
    push @pair, ([$c, $h{$c}->{i}]) if $h{$c}->{c} == 1;
  }
  @pair;
}

sub hidden_pairs {
  my $s = shift;
  my @board = @{$s->{board}};
  for my $j (0..80) {
    NEXT:
    for my $row (qw(row col squ)) {
      no strict 'refs';
      my (%h, @pair);
      # check candidates for count==1 in the whole group. if 2 we have a hidden pair
      @pair = $s->unique_c(&{$row."_i"}($j));
      if (@pair == 2) {
        my $pair_i1 = $pair[0]->[1];
        my $pair_i2 = $pair[1]->[1];
        die if $pair_i1 == $pair_i2;
        my $pair_c1 = $pair[0]->[0];
        my $pair_c2 = $pair[1]->[0];
        my $r = &{"which_".$row}($j);
        die "$pair_c1 == $pair_c2 in $row $r at ".at_i($pair_i1).", ".at_i($pair_i2) if $pair_c1 == $pair_c2;
        status "found two hidden_pairs ($pair_c1,$pair_c2) in $row $r at ".at_i($pair_i1).", ".at_i($pair_i2)
          if $Verbosity >= 1;
        # delete other candidates from these two cells
        for my $i (&{$row."_i"}($j)) {
          if ($i != $pair_i1 and $i != $pair_i2) {
            $s->{candidates}->[$i] = [ grep { $pair_c1 != $_ } @{$s->{candidates}->[$i]} ];
            $s->{candidates}->[$i] = [ grep { $pair_c2 != $_ } @{$s->{candidates}->[$i]} ];
            status "remove candidates unless ($pair_c1,$pair_c2) from $row($r) at ".at_i($i)
              if $Verbosity >= 2;
          }
        }
      }
    }
  }
  $s;
}

=pod

=item RULE ->hidden_triples

If three cells in a group contain a triple of candidates
(hidden amongst other candidates) that are not found in
any other cells in that group, then other candidates in
those three cells can be excluded safely.
L<http://www.angusj.com/sudoku/hints.php>

=cut

sub hidden_triples {
  my $s = shift;
  my @board = @{$s->{board}};
  for my $j (0..80) {
    NEXT:
    for my $row (qw(row col squ)) {
      no strict 'refs';
      my (%h, @pair);
      # check candidates for count==1 in the whole group. if 2 we have a hidden pair
      @pair = $s->unique_c(&{$row."_i"}($j));
      if (@pair == 3) {
        my $pair_i1 = $pair[0]->[1];
        my $pair_i2 = $pair[1]->[1];
        my $pair_i3 = $pair[2]->[1];
        die if $pair_i1 == $pair_i2 or $pair_i1 == $pair_i3;
        my $pair_c1 = $pair[0]->[0];
        my $pair_c2 = $pair[1]->[0];
        my $pair_c3 = $pair[2]->[0];
        my $r = &{"which_".$row}($j);
        status "found three hidden_triples ($pair_c1,$pair_c2,$pair_c3) in $row $r at ".at_i($pair_i1).", ".at_i($pair_i2)
          if $Verbosity >= 1;
        # delete other candidates from these cells
        for my $i (&{$row."_i"}($j)) {
          if ($i != $pair_i1 and $i != $pair_i2 and $i != $pair_i3) {
            $s->{candidates}->[$i] = [ grep { $pair_c1 != $_ } @{$s->{candidates}->[$i]} ];
            $s->{candidates}->[$i] = [ grep { $pair_c2 != $_ } @{$s->{candidates}->[$i]} ];
            $s->{candidates}->[$i] = [ grep { $pair_c3 != $_ } @{$s->{candidates}->[$i]} ];
            status "remove candidates unless ($pair_c1,$pair_c2,$pair_c3) from $row($r) at ".at_i($i)
              if $Verbosity >= 2;
          }
        }
      }
    }
  }
  $s;
}


=pod

=item RULE ->locked_candidate_1

Sometimes a candidate within a square is restricted to one
row or column. Since one of these cells must contain that
specific candidate, the candidate can safely be excluded
from the remaining cells in that row or column outside of
the box.

See L<http://www.angusj.com/sudoku/hints.php>

=cut

# returns 1 if the list consists only of unique values, otherwise 0.
# return 1 on length 0 or 1
sub uniq (@) {
  return 1 if @_ < 2;
  my $prev = shift;
  for my $i (@_) {
    return 0 if $i != $prev;
  }
  return 1;
}

sub locked_candidate_1 {
  my $s = shift;
  my @board = @{$s->{board}};
  for my $r (0..8) {
   SQUARE:
    my %h;
    for my $i (i_squ($r)) {
      my @c = @{$s->{candidates}->[$i]};
      # count candidates per square
      for my $c (@c) {
        # $h{$c}->{i} = [];
        push @{$h{$c}->{i}}, ($i);
        push @{$h{$c}->{row}}, (which_row($i));
        push @{$h{$c}->{col}}, (which_col($i));
        $h{$c}->{c}++;
      }
    }
    for my $row (qw(row col)) {
      my $found;
      # check candidates to be only in a single row or col
      for my $v (1..9) {
        next unless defined $h{$v}->{$row};
        next unless uniq(@{$h{$v}->{$row}});
        my $r1 = $h{$v}->{$row}->[0];
        next if $s->{locked}->{$v}->{$r}->{$row}->{$r1};
        no strict 'refs';
        for my $j (&{"i_".$row}($r1)) {
          if (!inarray_0($j, @{$h{$v}->{i}})
              and !inarray_0($j, i_squ($r))
              and inarray_0($v, @{$s->{candidates}->[$j]}))
          {
            status "found locked_candidate $v at $row $r1 in square $r"
              if !$s->{locked}->{$v}->{$r}->{$row}->{$r1} and $Verbosity >= 1;
            $s->{locked}->{$v}->{$r}->{$row}->{$r1}++;
            # remove from other rows in other squares
            status "remove locked_candidate $v at $row $r1 outside square $r at ".at_i($j)
              . " (square ".which_squ($j).")"
              if $Verbosity >= 2;
            $s->{candidates}->[$j] = [ grep { $v != $_ } @{$s->{candidates}->[$j]} ];
          }
        }
      }
    }
  }
  $s;
}

=pod

=item RULE ->locked_candidate_2

Same if a candidate within a row or column is restricted
to one box. Since one of these cells must contain that
specific candidate, the candidate can safely be excluded
from the remaining cells in the box.
See L<http://www.angusj.com/sudoku/hints.php>

FIXME!

=cut

sub locked_candidate_2 {
  my $s = shift;
  my @board = @{$s->{board}};
  for my $r (0..8) {
   ROW:
    my %h;
    for my $row (qw(row col)) {
      no strict 'refs';
      for my $i (&{"i_".$row}($r)) {
        my @c = @{$s->{candidates}->[$i]};
        # count candidates per square
        for my $c (@c) {
          push @{$h{$c}->{i}}, ($i);
          push @{$h{$c}->{squ}}, (which_squ($i));
          $h{$c}->{c}++;
        }
      }
      {
        use strict 'refs';
        my $found;
        # check candidates to be only in square r
        for my $v (1..9) {
          next unless defined $h{$v}->{squ};
          next unless uniq(@{$h{$v}->{squ}});

          my $r1 = $h{$v}->{squ}->[0];
          next if $s->{locked}->{$v}->{$r}->{squ}->{$r1};
          for my $j (i_squ($r1)) {
            no strict 'refs';
            if (!inarray_0($j, @{$h{$v}->{i}})
                and !inarray_0($j, &{"i_".$row}($r))
                and inarray_0($v, @{$s->{candidates}->[$j]}))
            {
              status "found locked_candidate $v at square $r1 in $row $r"
                if !$s->{locked}->{$v}->{$r}->{squ}->{$r1} and $Verbosity >= 1;
              $s->{locked}->{$v}->{$r}->{squ}->{$r1}++;
              # remove from other rows/cols in this square
              status "remove locked_candidate $v at square $r1 outside $row $r at ".at_i($j)
                if $Verbosity >= 2;
              $s->{candidates}->[$j] = [ grep { $v != $_ } @{$s->{candidates}->[$j]} ];
            }
          }
        }
      }
    }
  }
  $s;
}

=pod

=item solve

Apply all rules until the board is filled => SUCCESS,
or for a FAILURE the number of candidates is the same as
in the previous round, and the number of found numbers
on the board stayed the same.

On failure print the list of candidates for all board positions then
and ask for more rules to be implemented.

=cut

sub solve {
  my $s = shift;
  my $step = shift;
  my @candidates = @{$s->{candidates}};
  my $solved = count(@{$s->{board}});
  my $cands = grep { @{$_} } @candidates;
  my $lasttry;
  my @rules = @{$s->{rules}};
  while ($solved < 81) {
    my $newcands;
    # TODO: 1st rule until no progress, then 2+1+2, then 3+1+2+3, ...
  RULES:
    for my $m (0..$#rules) {
      my $meth = $rules[$m];
      next if $step and $step ne $meth;
      status "rule $meth";
      $s = $s->$meth();
      if ($step) {
        $s->Show;
        $s->validate();
        return $m==$#rules?$rules[0]:$rules[$m+1];
      }
      $s->validate() if $Verbosity >= 2;
      $newcands = grep { @{$_} } @{$s->{candidates}};
      if (count(@{$s->{board}}) > $solved
          or $cands != $newcands)
      {
        if (count(@{$s->{board}}) > $solved) {
          status "found ".(count(@{$s->{board}})-$solved).", restart at $rules[0]"
            if $Verbosity > 0;
        } elsif ($cands != $newcands) {
          status "removed ".($cands-$newcands)." candidates, restart at $rules[0]"
            if $Verbosity > 1;
        }
        $solved = count(@{$s->{board}});
        $cands = $newcands;
        goto RULES; # restart
      }
    }
    $s->Show unless $opts{nogui};
    #if ($lasttry and $solved == count(@{$s->{board}}) and $newcands == $cands) {
    #  print "failed to solve Sudoku! Need more solver rules.\n",$s->{init}," init, ",
    #    $solved, " solved, ", 81-$solved, " left, $newcands candidates\n";
    #  $s->ShowCands if $Verbosity >= 2;
    #  return $s;
    #}
    #if ($solved == count(@{$s->{board}}) and $newcands == $cands) {
    #  $lasttry++;
    #}
    #$solved = count(@{$s->{board}});
    #$cands = $newcands;
    if (!$cands) {
      print "all candidates solved";
      print ", find the rest" if $solved < 81;
      print "\n";
      $lasttry++;
    }
    status ($solved, " solved, ", 81-$solved, " left, $cands candidates");
  }
  $s;
}

=pod

=item gui_init ( { size => 50 } )

=cut

sub gui_init {
  my $s = shift;
  my $props = shift;
  my $ts = 24;
  my $framestyle = 0; # 'etched'; # 0
  my $size = $props->{size} || 9*$ts;
  if ($framestyle eq 'etched') {
    $size += 9*2 - 5;
  }
  $Menu = Win32::GUI::MakeMenu
    (
     "&File" => "&File",
     "> &Clear"    => {-name => 'Clear',  -onClick => \&File_Clear},
     "> C&reate"   => {-name => 'Create',},
     "> > &Easy"   => {-name => 'Easy',  -onClick => \&File_Create_Easy},
     "> > &Medium" => {-name => 'Medium',-onClick => \&File_Create_Medium},
     "> > &Hard"   => {-name => 'Hard',  -onClick => \&File_Create_Hard},
     "> &Open..."  => {-name => 'Open', -onClick => \&File_Open},
     "> &Save"     => {-name => 'Save', -onClick => \&File_Save},
     "> &Quit"     => {-name => 'Quit', -onClick => sub { return -1; }},
     "&Options" => "&Options",
     "> &Cheat" => => {-name => 'Cheat', -checked => $s->{cheating},
                       -onClick => sub {
                         $Menu->{"Cheat"}->Checked($Menu->{"Cheat"}->Checked() ? 0 : 1);
                         my $ud = $W->UserData();
                         my $G = $ud->{'game'};
                         $G->{cheating} = $Menu->{"Cheat"}->Checked();
                         $ud->{'game'} = $G;
                         $W->UserData($ud);
                         $Menu->Redraw(1);
                       }},
     "> &Verbosity" => "&Verbosity",
     "> > &Rules only"  => {-name => 'Silent',  -onClick => sub { $Verbosity = 0; 1; }},
     "> > &Board"       => {-name => 'Medium',  -onClick => sub { $Verbosity = 1; 1; }},
     "> > &Candidates"  => {-name => 'Full',    -onClick => sub { $Verbosity = 2; 1; }},
     "&Help"    => "&Help",
     "> &Help"     => {-name => 'Help',  -onClick => \&Help_Help},
     "> &About"    => {-name => 'About', -onClick => \&Help_About},
    );
  my $Accel = new Win32::GUI::AcceleratorTable
    (
     #"Ctrl-C"   => "Edit_Copy_Click",
     #"Ctrl-F"   => "Edit_Find_Click",
     "S"   	=> "Solve",
     "T"   	=> "Step",
     "Ctrl-S"   => "Main_Save",
#     "Left"	=> "List_PrevErr",
#     "Right"	=> "List_NextErr",
#     "Ctrl-Left" => "List_PrevErr",
#     "Ctrl-Right"=> "List_NextErr",
#     "Alt-A"	=> "List_PrevErr",
#     "Alt-S"	=> "List_NextErr",
    );
  $W = new Win32::GUI::Window
    (
     -name        => 'Main'
     -title       => "Sudoku",
     -size        => [$size + 100+5, $size+50+20+5],
     -minsize     => [$size + 100+5, $size+50+20+5],
     -maxsize     => [$size + 100+5, $size+50+20+5],
     -onTerminate => sub { return -1; },
     -menu        => $Menu,
#    -accel       => $Accel,
     -dialogui    => 1,
    );
  $W->UserData({'game' => $s});
  my $font = Win32::GUI::Font->new
    (
     -name => "Verdana",
     -outputprecision => OUT_TT_PRECIS,
     -family => FF_SWISS,
     -size   => $framestyle eq 'etched' ? ($ts/2)-6 : $ts/2,
    );
  $Board = new Win32::GUI::Window
    (
     -parent      => $W,
     -name        => 'Board'
     -pos         => [0, 0],
     -size        => [$size, $size],
     -popstyle    => WS_CAPTION | WS_SIZEBOX,
     -pushstyle   => WS_CHILD | WS_CLIPCHILDREN,
     -pushexstyle => WS_EX_CLIENTEDGE,
     -background  => 0xFFFFFF,
     -font        => $font,
    );
  my $cs = 12;
  my $font1 = Win32::GUI::Font->new
    (
     -outputprecision => OUT_TT_PRECIS,
     -family => FF_SWISS,
     -size   => $cs,
     -weight => 100,
    );
  $CandWin = new Win32::GUI::Window
    (
     -name        => 'Candidates'
     -title       => "Candidates",
     -pos         => [$W->Left + $W->Width, $W->Top],
     -size        => [10 + $cs * 27, 10 + $cs * 27],
     -background  => 0xFFFFFF,
     -onTerminate => sub { shift->Hide; },
     -font        => $font1,
    );
  #$Board->SetBkColor(0xFFFFFF);
  $Board->ScaleHeight;
  my $sb = $W->AddStatusBar(-name => "Status");

  for my $x (0..8) {
    for my $y (0..8) {
      my $i = $x + $y*9;
      $Board->AddLabel
        (-frame => $framestyle,
         -name  => "Label_$i",
         -pos   => [5+$x*$ts, 5+$y*$ts],
         -size  => [$ts,$ts],
         -notify => 1, #enable Click
         #-font  => $font,
         -background     => 0xFFFFFF,
         #-tip     => sub {
         #  my $ud = $W->UserData();
         #  my $G = $ud->{'game'};
         #  join(",",@{$s->{candidates}->[$i]})
         #},
         -onDblClick => sub {
           my $ud = $W->UserData();
           my $G = $ud->{'game'};
           if ($G->{cheating} and @{$G->{candidates}->[$i]}) {
             $W->Status->Text("candidates: ".join(",",@{$G->{candidates}->[$i]}));
             $W->Status->Update;
           }
         },
         -onClick => sub {
           my $ud = $W->UserData();
           my $G = $ud->{'game'};
           $G->Choose($i);
           $ud->{'game'} = $G;
           $W->UserData($ud);
         },

        );
      for my $j (0..8) {
        $CandWin->AddLabel
          (
           -name  => "Label_{$i}_{$j}",
           -pos   => [5+($x*$cs*9)+($j%3)*$cs*3, 5+($y*$cs*9)+int($j/3)*$cs*3],
           -size  => [$cs, $cs],
           #-font  => $font1,
           -background => 0xFFFFFF,
           #-tip     => sub { join(",",$s->{candidates}->[$i]) },
           #-onClick => sub { status(join(",",$s->{candidates}->[$i])) }
          );
      }
    }
  }
  if (1) { # FIXME: draw lines
    my $DC = $Board->GetDC;
    my $width   = $Board->ScaleWidth;
    my $height  = $Board->ScaleHeight;
    $DC->PaintRgn(CreateRectRgn Win32::GUI::Region(0,0,$width,$height));
    # $DC->BackColor(0xFFFFFF);
    my $Pen = new Win32::GUI::Pen(-color => [ 0x000000 ], -width => 3);
    my $oldP = $DC->SelectObject($Pen);
    $DC->BeginPath();
    for my $x (0..2) {
      $DC->MoveTo(0,      3*$x*$ts);
      $DC->LineTo($width, 3*$x*$ts); #hor
      $DC->MoveTo(3*$x*$ts, 0);
      $DC->LineTo(3*$x*$ts, $height); #ver
      #$DC->Line(0,3*$x*$ts,$width,3*$x*$ts);  #hor
      #$DC->Line(3*$x*$ts,0,3*$x*$ts,$height); #ver
    }
    $DC->EndPath();
    $DC->StrokePath();
    $DC->SelectObject($oldP) if defined $oldP;
    $Board->InvalidateRect(1);
  }
  $W->AddButton(
                -pos   => [9*$ts + 20, $ts],
                -width => 65,
                -name  => 'solve',
                -title  => '&Solve',
                -onClick => sub { $s->solve; 1}
                );
  $Nextstep = $s->{rules}->[0];
  $W->AddButton(
                -pos   => [9*$ts + 20, $ts + 32],
                -width => 65,
                -name  => 'step',
                -title  => 'S&tep',
                -onClick => sub { $Nextstep = $s->solve($Nextstep); $s->Show; 2}
                );
  if ($s->{cheating}) {
    $W->AddButton(
                -pos   => [9*$ts + 20, $ts + 32 + 32],
                -width => 65,
                -name  => 'candidates',
                -title  => 'Candidates',
                -onClick => sub { $s->{showcands} = !$s->{showcands}; $s->ShowCands; 3}
                );
  }
  $s->Show();
  $W;
}

sub status($;@) {
  if ($W) {
    $W->Status->Text(join("",@_));
    $W->Status->Update;
    print join("",@_),"\n";
  } else {
    print join("",@_),"\n";
  }
  1;
}

# draw numbers
# clean old numbers?
sub Show {
  my $s = shift;
  my @board = @{$s->{board}};
  print "\nboard:" unless $W;
  for my $i (0 .. 80) {
    my $b = $board[$i] ? $board[$i] : ".";
    unless ($W) { # ascii art
      unless ($i % 27) {
        print "|" if $i;
        print "\n+---+---+---+\n";
      } else {
        print "|\n" unless $i % 9;
      }
      print "|" unless $i % 3;
      print $b;
    } else { # update gui for board
      $Board->{"Label_$i"}->Text("$b");
    }
  }
  print "|\n+---+---+---+\n" unless $W;
  $Board->Show() if $Board;
  $W->Show() if $W;
  $W->Update() if $W;
  $W->SetForegroundWindow() if $W;
  1;
}

sub ShowCands {
  my $s = shift;
  print "\ncandidates:" unless $W;
  my %max;
  for my $r (0..8) {
    for (i_col($r)) {
      my $c = count(@{$s->{candidates}->[$_]});
      $max{$r} = $c > $max{$r} ? $c : $max{$r};
    }
  }
  for my $i (0 .. 80) {
    my @c = @{$s->{candidates}->[$i]};
    if ($W) {
      for my $j (0..8) {
        my $c = $j < @c ? $c[$j] : " ";
        $CandWin->{"Label_{$i}_{$j}"}->Text("$c");
      }
    }
    if (1) { # ascii art
      #indent by max length per col
      print "\n\t------------------------" unless $i % 27;
      print "\n" unless $i % 9;
      print "\t|" unless $i % 3;
      my $in = $max{which_col($i)} - @c;
      print "[",join("",@c),"] "." "x$in;
    }
  }
  print "\n\t------------------------";
  if ($W and $s->{cheating}) {
    # flip status
    if ($s->{showcands}) {
      $CandWin->Update;
      $CandWin->Show;
      $CandWin->SetForegroundWindow;
    } else {
      #while ($CandWin->DoEvents() != -1) {;}
      $CandWin->Hide;
      $W->SetForegroundWindow;
    }
  }
  if (1) {
    my $old_fh = select(OUTPUT_HANDLE);
    $| = 1;
    select($old_fh);
    print "\n";
  }
  1;
}

# on empty position
#   display 1..9 chooser and let user select which number to fill in
# on filled position no take back?
sub Choose ($$) {
  my $s = shift;
  my $i = shift;
  unless ($s->board($i)) {
    status "Choose a value from 1 - 9";
  }
}

sub Solve {
  my $ud = $W->UserData();
  my $G = $ud->{'game'};
  $G->solve;
  $ud->{'game'} = $G;
  $W->UserData($ud);
  1
}
sub Step {
  my $ud = $W->UserData();
  my $G = $ud->{'game'};
  $Nextstep = $G->solve($Nextstep);
  $G->Show;
  $ud->{'game'} = $G;
  $W->UserData($ud);
  2
}
sub File_Clear {
  my $ud = $W->UserData();
  my $G = $ud->{'game'};
  for my $i (0..80) {
    $G->{board}->[$i] = 0;
  }
  $G->{init}++;
  $G->init_candidates();
  delete $G->{init};
  $ud->{'game'} = $G;
  $W->UserData($ud);
  $G->Show();
}

sub File_Open {
  my $ud = $W->UserData();
  my $G = $ud->{'game'};
  my @file = $W->GetOpenFileName
    (
     -title  => "Load Sudoku board ...",
     -filter => [ 'Sudoko - *.sudoku', '*.sudoku',
                  'Sudoko - *.ss', '*.ss' ],
    );

  $G->open_file($file[0]);

  $ud->{'game'} = $G;
  $W->UserData($ud);
  $G->Show();
}

sub open {
  my $G = shift;
  my @board = @_; # array or string
  my $in = join ("", @board);
  $in =~ s/\./0/g;
  $in =~ s/[^0-9]//g;
  status "Invalid sudoku board: ".length($in)." of 81 numbers"
    if length($in) != 81;
  for my $i (0..80) {
    my $ch = substr($in,$i,1);
    $G->{board}->[$i] = substr($in,$i,1);
  }
  $G->{init}++;
  $G->init_candidates();
  delete $G->{init};
  $G;
}

sub open_file {
  my $G = shift;
  my $file = shift;
  my @in;
  open F, "< $file" or die "missing file $file";
  while (<F>) {
    chomp;
    push @in, ($_) unless /^[;#]/;
  }
  close F;
  my $in = join ("", @in);
  $in =~ s/\./0/g;
  $in =~ s/[^0-9]//g;
  status "Load sudoku file $file";
  status "Invalid sudoku file $file: ".length($in)." of 81 numbers"
    if length($in) != 81;
  $G->{file} = $file;
  $W->Change("-title", "Sudoku - ".basename($file)) if $W;
  $G->open(@in);
}

sub File_Save {
  my $ud = $W->UserData();
  my $G = $ud->{'game'};
  my @file = $W->GetOpenFileName
    (
     -default=> dirname($G->{file})."/".basename($G->{file}).".solution",
     -title  => "Save Sudoku board ...",
     -filter => [ 'Sudoko Problem - *.sudoku', '*.sudoku',
                  'Sudoko Problem - *.ss', '*.ss',
                  'Solution - *.solution', '*.solution']);
  $G->save($file[0]);
  1;
}

sub save {
  my $G = shift;
  my $file = shift;
  open F, "> $file";
  for my $i (0..80) {
    my $b = $G->{board}->[$i];
    $b =~ s/0/./;
    unless ($i % 27) {
      print F "|\n" if $i;
      print F "+---+---+---+\n";
    } else {
      print F "|\n" unless $i % 9;
    }
    print F "|" unless $i % 3;
    print F $b;
  }
  print F "|\n+---+---+---+\n";
  close F;
  status "Sudoku file $file written";
}

sub Help_Resize {
  $W->Help->HelpText->Width($W->Help->ScaleWidth);
  $W->Help->HelpText->Height($W->Help->ScaleHeight);
}

sub Help_Help {
    my $help = new Win32::GUI::Window
      (-name   => 'Help',
       -pos    => [$W->Left + $W->Width, $W->Top],
       -width  => 400, -height => 600,
       -title  => "Sudoku HELP",
       -onTerminate => sub { return -1; },
       -parent => $W,
       -dialogui => 1,
      );
    $help->AddListView
      (-name   =>'HelpText',
       -list   => 0,
       -report => 1,
       -height => $help->ScaleHeight,
       -width  => $help->ScaleWidth,
       -fullrowselect => 1,
       -onresize => \&Help_Resize,
      );
    $help->Hide();

    my $f = "_tmp.pod";
    my $s;
    pod2usage({-verbose => 2, -output => $f, -exitval => 'NOEXIT'});
    open F, "< $f";
    while (<F>) {
      chomp;
      $s .= ($_ . "\r\n");
    }
    close F;
    unlink $f;

    map { $W->Help->HelpText->InsertItem(-text => $_) } split /\r\n/, $s;
    # $W->Hide();

    if ($PerlApp::TOOL) {
        my $chwnd = Win32::GUI::GetPerlWindow();
        Win32::GUI::Show($chwnd) unless $W->{-handle} == $chwnd;
    }
    $W->Help->Show();
    while ($W->Help->DoEvents() != -1) {;}
    $W->Help->Hide();
    $W->Help->HelpText->DeleteAllItems();
    1;
}

=pod

=back

=cut

# Example if not loaded as Module
# perl Sudoku.pm --v=2 --cheat 1.sudoku --solve --save=1.solution
package main;
use Getopt::Long;

$W = 0;
GetOptions(\%opts,
           "verbose|v=i",
           "cheat!",
           "nogui!",
           "solve!",
           "save=s",
          );
$Verbosity = defined $opts{verbose} ? $opts{verbose} : 1;
my $G = Games::Sudoku::Win32->new();
$G->{cheating}=1 if $opts{cheat};
my $board1 = <<eod;
.29 ..3 .4.
... 647 ..2
4.. ..8 .7.
... 8.. 3.7
... .6. ...
5.7 ..9 ...
.1. 3.. ..6
2.. 481 ...
.7. 5.. 12.
eod
#$G->open(board1);
if (@ARGV) {
  $G->open_file(shift);
} else {
  $G->open_file("blott.sudoku");
}
$W = $G->gui_init unless $opts{nogui};
$G->Show();
$G->solve() if (!$W or $opts{solve});
$G->Show() if (!$W or $opts{solve});
$G->save($opts{save}) if $opts{save} and $opts{solve};
$W->Dialog() if $W;
