#! /bin/csh -f
#
#  run python generator to update plasma state code
#
#  > cd plasma_state
#  > ./update_ps.csh test
#
#     ... causes all the outputs to be written into plasma_state/tmp
#     ... so you can examine them before having them distributed to their
#     ... respective source directories
#
#  > ./update_ps.csh nocopy
#
#     ... tells you what generated sources are modified from their current
#     ... versions, but does not actually modify the current versions
#     ... (the tmp directory & source file copies are not saved).
#

  set ps_target = $cwd
  set ps_kernel = $cwd/../plasma_state_kernel
  set ps_mdescr = $cwd/../mdescr
  set ccps_targ = $cwd/../ccps_lib
#
  cd $ps_target
#
#  execute in clean tmp subdirectory
#
  if ( -d tmp ) then
    rm -rf tmp
  endif

  mkdir tmp
  cp *.py tmp
  cp plasma_state_spec.dat tmp

  cd tmp
#
#  run the generator
#
  python3 plasma_state_gen.py
  set pstat = $status
#
  if ( $pstat != 0 ) then
    echo " ?? update_ps.csh: python error."
    exit 99
  endif
#
  if ( $1 == "debug" || $1 == "test" || $1 == "tmp" ) then
    echo " update_ps.csh: test or debug switch detected."
    echo " cwd = $cwd"
    echo " exiting..."
    exit 0
  endif
#
  rm -rf __pycache__
  rm -f *.pyc
  rm -f *.py
  rm -f plasma_state_spec.dat
#
#--------------------------------------
#  mdescr routines

  set mlist = ( mdescr_data.f90 mdescr_init.f90 mdescr_read.f90 )
#
  @ count = 0
  @ num = $#mlist
  @ update_mdescr = 0
#
  while ( $count < $num )
    @ count++
    set file = $mlist[$count]
#
    if ( ! -d $ps_mdescr ) then
      echo "(OK, removed $file -- no mdescr target directory)"
      rm $file
#
    else if ( ! -f $ps_mdescr/$file ) then

#  apparent new output file

      @ update_mdescr++
      echo " --> added: $file"
      if ( $1 == "nocopy" ) then
        echo "(skipped copy to $ps_mdescr)"
        rm $file
      else
        mv $file $ps_mdescr
      endif

    else
#
#  compare...
#
      diff $file $ps_mdescr/$file >& /dev/null
      set dstat = $status
      if ( $dstat == 1 ) then
        @ update_mdescr++
        echo " --> updated: $file"
        if ( $1 == "nocopy" ) then
          echo "(skipped copy to $ps_mdescr)"
          rm $file
        else
          mv $file $ps_mdescr
        endif
      else
        rm $file
      endif

    endif
  end
#
#--------------------------------------
#  check the kernel items first
#
#   *** if this list changes, Makefile change needed in SWIM distribution ***
#
  set flist = ( plasma_state_definition_mod.f90 \
                plasma_state_constants_mod.f90 \
                plasma_state_util_mod.f90 \
		plasma_state_clear_profs.f90 \
		plasma_state_free.f90 \
                ps_contents_alloc.f90 \
                plasma_state_hash_addr.f90 \
                plasma_state_hash_code.f90 \
                plasma_state_hash_diff.f90 \
                ps_mdescr_namelist_write.f90 \
                ps_sconfig_namelist_write.f90 \
                ps_mdescr_namelist_read.f90 \
                ps_update_report_exec.f90 \
                ps_print_category.f90 \
		)
#
#  filename prefixes for subroutine sets destined for plasma_state_kernel...
#
  set prefix_flist = ( plasma_state_merge \
                plasma_state_copy \
                plasma_state_empty_alloc \
		)
#
# note: ps_sconfig_namelist_read.f90 needs to be in plasma_state.a
#--------------------------------------
#
  @ count = 0
  @ num = $#flist
  @ update_kernel = 0
#
  while ( $count < $num )
    @ count++
    set file = $flist[$count]
#
    if ( ! -f $ps_kernel/$file ) then

#  apparent new output file

      @ update_kernel++
      echo " --> added: $file"
      if ( $1 == "nocopy" ) then
        echo "(skipped copy to $ps_kernel)"
        rm $file
      else
        mv $file $ps_kernel
      endif

    else
#
#  compare...
#
      diff $file $ps_kernel/$file >& /dev/null
      set dstat = $status
      if ( $dstat == 1 ) then
        @ update_kernel++
        echo " --> updated: $file"
        if ( $1 == "nocopy" ) then
          echo "(skipped copy to $ps_kernel)"
          rm $file
        else
          mv $file $ps_kernel
        endif
      else
        rm $file
      endif

    endif
  end
#
#  remaining files are the generator output
#  for the plasma_state library...
#
  set flist = `ls`
#
  @ count = 0
  @ num = $#flist
  @ update = 0
  @ update_ccps = 0
#
#  only copy files that changed...
#
  while ( $count < $num )
    @ count++
    set file = $flist[$count]
    @ imatch = 0
    foreach prefix ( $prefix_flist )
      set ftest = `echo $file | sed "s/^$prefix//"`
      if ( "$ftest" != "$file" ) then
        @ imatch++
      endif
    end

    if ( $imatch == 0 ) then
      set ftest = `echo $file | sed 's:^ccps::' | sed 's:^cxxps::'`
    else
      set ftest = "kernel_match"
    endif

    if ( "$ftest" == "$file" ) then
      set dtarg = $ps_target
      set ftarg = $ps_target/$file
    else if ( $imatch > 0 ) then
      set dtarg = $ps_kernel
      set ftarg = $ps_kernel/$file
    else
      set dtarg = $ccps_targ
      set ftarg = $ccps_targ/$file
    endif

    if ( ! -f $ftarg ) then

#  apparent new generator output file

      if ( "$dtarg" == "$ps_target" ) then
        @ update++
      else if ( "$dtarg" == "$ps_kernel" ) then
        @ update_kernel++
      else
        @ update_ccps++
      endif

      echo " --> added: $file"
      if ( $1 == "nocopy" ) then
        echo "(skipped copy to $dtarg)"
        rm $file
      else
        mv $file $ftarg
      endif

    else
#
#  compare...
#
      diff $file $ftarg >& /dev/null
      set dstat = $status
      if ( $dstat == 1 ) then

        if ( "$dtarg" == "$ps_target" ) then
          @ update++
        else if ( "$dtarg" == "$ps_kernel" ) then
          @ update_kernel++
        else
          @ update_ccps++
        endif

        echo " --> updated: $file"
        if ( $1 == "nocopy" ) then
          echo "(skipped copy to $dtarg)"
          rm $file
        else
          mv $file $ftarg
        endif
      else
        rm $file
      endif

    endif
  end
#
#  how many files?
#
  @ update_total = $update_kernel + $update + $update_ccps + $update_mdescr
  echo " ==> $update_total files updated."
#
  cd ..
  rm -rf tmp
#
  if ( $1 == "nocopy" ) then
    echo "nocopy exit"
    exit
  endif

#---------------------------------------
