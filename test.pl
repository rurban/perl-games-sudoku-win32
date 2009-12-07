use strict;
use vars;
use Win32::GUI;

use lib 'lib';
use Games::Sudoku::Win32;

use Getopt::Long;
# my ($W, $Board, $Menu, $Nextstep, %opts);
my (%opts);

sub gui_init {
  my $s = shift;
  my $ts = 24;
  my $framestyle = 0; #'etched'; # 0
  my $size = 9*$ts;
  if ($framestyle eq 'etched') {
    $size += 9*2 - 5;
  }
  my $Menu = Win32::GUI::MakeMenu
    (
     "&File" => "&File",
     "> &Clear"    => {-name => 'Clear',  -onClick => \&Games::Sudoku::Win32::File_Clear},
     "> C&reate"   => {-name => 'Create',},
     "> > &Easy"   => {-name => 'Easy',  -onClick => \&Games::Sudoku::Win32::File_Create_Easy},
     "> > &Medium" => {-name => 'Medium',-onClick => \&Games::Sudoku::Win32::File_Create_Medium},
     "> > &Hard"   => {-name => 'Hard',  -onClick => \&Games::Sudoku::Win32::File_Create_Hard},
     "> &Open..."  => {-name => 'Open', -onClick => \&Games::Sudoku::Win32::File_Open},
     "> &Save"     => {-name => 'Save', -onClick => \&Games::Sudoku::Win32::File_Save},
     "> &Quit"     => {-name => 'Quit', -onClick => sub { return -1; }},
     "&Options" => "&Options",
     "> &Cheat" => => {-name => 'Cheat', -checked => $s->{opts}->{cheat},
                       -onClick => sub {
                         $Menu->{"Cheat"}->Checked($Menu->{"Cheat"}->Checked() ? 0 : 1);
                         my $ud = $W->UserData();
                         my $G = $ud->{'game'};
                         $G->{opts}->{cheat} = $Menu->{"Cheat"}->Checked();
                         $ud->{'game'} = $G;
                         $W->UserData($ud);
                         $Menu->Redraw(1);
                       }},
     "> &Verbosity" => "&Verbosity",
     "> > &Rules only"  => {-name => 'Silent',  -onClick => sub { $Verbosity = 0; 1; }},
     "> > &Board"       => {-name => 'Medium',  -onClick => sub { $Verbosity = 1; 1; }},
     "> > &Candidates"  => {-name => 'Full',    -onClick => sub { $Verbosity = 2; 1; }},
     "&Help"    => "&Help",
     "> &Help"     => {-name => 'Help',  -onClick => \&Games::Sudoku::Win32::Help_Help},
     "> &About"    => {-name => 'About', -onClick => \&Games::Sudoku::Win32::Help_About},
    );
  my $Accel = new Win32::GUI::AcceleratorTable
    (
     #"Ctrl-C"   => "Edit_Copy",
     "S"   	=> "Solve",
     "T"   	=> "Step",
     "Ctrl-S"   => "Main_Save",
    );
  my $W = new Win32::GUI::Window
    (
     -name        => 'Main'
     -title       => "Sudoku",
     -size        => [$size + 100+10, $size+50+20+10],
     -minsize     => [$size + 100+10, $size+50+20+10],
     -maxsize     => [$size + 100+10, $size+50+20+10],
     -onTerminate => sub { return -1; },
     -onKeyDown   => \&Games::Sudoku::Win32::OnChoose,
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
     -size   => $framestyle eq 'etched' ? $ts/2-6 : $ts/2,
     -bold => 1,
    );
  my $Board = new Win32::GUI::Window
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
  my $cs = int($ts/3);
  my $font1 = Win32::GUI::Font->new
    (
     -outputprecision => OUT_TT_PRECIS,
     -family => FF_SWISS,
     -size   => $cs,
     -weight => 100,
    );
  #$CandWin = new Win32::GUI::Window
  #  (
  #   -name        => 'Candidates'
  #   -title       => "Candidates",
  #   -pos         => [$W->Left + $W->Width, $W->Top],
  #   -size        => [10 + $cs * 27, 10 + $cs * 27],
  #   -background  => 0xFFFFFF,
  #   -onTerminate => sub { shift->Hide; },
  #   -font        => $font1,
  #   -dialogui    => 1,
  #  );
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
         -align => 'center',
         -notify => 1, #enable Click
         #-font  => $font,
         -background => which_squ($i) % 2 ? 0xEEEEEE : 0xFFFFFF,
         #-tip     => sub {
         #  my $ud = $W->UserData();
         #  my $G = $ud->{'game'};
         #  join(",",@{$s->{candidates}->[$i]})
         #},
         -onDblClick => sub {
           my $ud = $W->UserData();
           my $G = $ud->{'game'};
           if ($G->{opts}->{cheat} and @{$G->{candidates}->[$i]}) {
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
        $Board->AddLabel
          (
           -name  => "Cand_{$i}_{$j}",
           -pos   => [5+($x*$cs*9)+($j%3)*$cs*3, 5+($y*$cs*9)+int($j/3)*$cs*3],
           -size  => [$cs, $cs],
           -notify => 1, #enable Click
           -font  => $font1,
           -background => which_squ($i) % 2 ? 0xEEEEEE : 0xFFFFFF,
           -onClick => sub {
             my $ud = $W->UserData();
             my $G = $ud->{'game'};
             $G->found($i, $_[0]->Text());
             $G->Show();
             $ud->{'game'} = $G;
             $W->UserData($ud);
           },
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
  $W->AddButton(
                -pos   => [9*$ts + 20, $ts + 32 + 32],
                -width => 65,
                -name  => 'candidates',
                -title  => 'Candidates',
                -onClick => sub { $s->{showcands} = !$s->{showcands}; $s->ShowCands; 3}
                );
  $W->candidates->Hide unless $s->{opts}->{cheat};
  $W->{Board} = $Board;

  $Board->Show();
  $W->Show();
  $W->Update();
  $W->SetForegroundWindow();

  for my $i (0 .. 80) {
    my @board = @{$s->{board}};
    my $b = $board[$i] ? $board[$i] : ".";
    $Board->{"Label_$i"}->Text("$b");
    if ($s->{opts}->{cheat}) {
      if (!$board[$i]) {
        for my $j (0..8) {
          $Board->{"Cand_{$i}_{$j}"}->Text($j+1);
        }
      }
    }
  }
  $W
}

###################################################################

GetOptions(\%opts,
           "verbose|v=i",
           "cheat!",
           "nogui!",
           "solve!",
           "save=s",
          );
# $Games::Sudoku::Win32::Verbosity = defined $opts{verbose} ? $opts{verbose} : 1;
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
my $G = Games::Sudoku::Win32->new(\%opts);
if (@ARGV) {
  $G->open_file(shift);
} else {
  $G->open($board1);
}
# $s->{cheating}=1 if $opts{cheat};
$Games::Sudoku::Win32::W = gui_init($G) unless $opts{nogui};
$G->Show();
my $ret = $G->solve() if (!$W or $opts{solve});
$G->Show() if (!$W or $opts{solve});
$G->save($opts{save}) if $opts{save} and $opts{solve};
$W->Dialog() if $W;
exit $ret;
