#! /bin/csh -f
#
  set ztest = `which tek2ps`
  if ( $#ztest != 1 ) then
     echo " ?? sample_run.csh: 'tek2ps' program not found in PATH:"
     which tek2ps
     exit 1
  endif

  echo " geqxpl script running ... "

  rm -f foo.ps
  rm -f foo.txt

  setenv PLOT "foo.plt-ps"

  time geqxpl @nstx_geqdsk > foo.txt

  mv foo.plt_ps foo.ps

  echo " SGLIB plot output in foo.ps"
  echo "    (reference plots in geqxpl_output.ps)"
  echo " text output in foo.txt (compare to reference: geqxpl_output.txt)"
