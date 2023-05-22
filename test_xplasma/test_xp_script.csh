#! /bin/csh -f
#
  set ztest = `which tek2ps`
  if ( $#ztest != 1 ) then
     echo " ?? sample_run.csh: 'tek2ps' program not found in PATH:"
     which tek2ps
     exit 1
  endif

  echo " test_xplasma script running... "

  rm -f foo.ps
  rm -f foo.txt

  setenv PLOT "foo.plt-ps"

  time test_xplasma @script > foo.txt

  mv foo.plt_ps foo.ps

  echo " SGLIB plot output in foo.ps"
  echo "    (reference plots in script_output.ps)"

  time test_xplasma d 0.1 x 11114P06 q q >> foo.txt

  echo " text output in foo.txt"
  echo "    (reference text in script_output.txt)"

