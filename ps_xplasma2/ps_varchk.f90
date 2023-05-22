subroutine ps_varchk(ss,varname,icdf,varid,iout)

  !  hand coded variable check
  !  this routine is invoked e.g. for variables whose units definitions
  !  have changed over the history of the definition of the Plasma State

  use plasma_state_definition_mod
  use netcdf
  implicit NONE

  !---------------------------------------
  !  passed arguments:

  type (plasma_state) :: ss
  character*(*), intent(in) :: varname

  integer, intent(in) :: icdf   ! NetCDF file ID, open file
  integer, intent(in) :: varid  ! NetCDF variable ID
  integer, intent(in) :: iout   ! LUN for messages

  !---------------------------------------
  !  local:
  character*40 att_value
  integer :: istat
  !-----------------------

  if((varname.eq.'freq_ec').or.(varname.eq.'freq_ic').or. &
       (varname.eq.'freq_lh')) then
     att_value=' '
     istat=nf90_get_att(icdf,varid,"units",att_value)

     if(trim(att_value).eq.'MHz') then
        write(iout,*) ' %ps_varchk: '//trim(varname)// &
             ' element converted, MHz -> Hz.'
        if(varname.eq.'freq_ec') then
           ss%freq_ec = ss%freq_ec * 1.0d6
        else if(varname.eq.'freq_ic') then
           ss%freq_ic = ss%freq_ic * 1.0d6
        else if(varname.eq.'freq_lh') then
           ss%freq_lh = ss%freq_lh * 1.0d6
        endif

     else if(trim(att_value).eq.'GHz') then
        write(iout,*) ' %ps_varchk: '//trim(varname)// &
             ' element converted, GHz -> Hz.'
        if(varname.eq.'freq_ec') then
           ss%freq_ec = ss%freq_ec * 1.0d9
        else if(varname.eq.'freq_ic') then
           ss%freq_ic = ss%freq_ic * 1.0d9
        else if(varname.eq.'freq_lh') then
           ss%freq_lh = ss%freq_lh * 1.0d9
        endif

     else if(trim(att_value).eq.'Hz') then
        continue

     else
        if(istat.ne.NF90_NOERR) then
           write(iout,*) ' %ps_varchk: nf90_get_att error.'
        else
           write(iout,*) ' %ps_varchk: unexpected '//trim(varname)// &
                ' element units: "'// &
                trim(att_value)//'".'
        endif
     endif

  else
     write(iout,*) ' %ps_varchk: unexpected call, varname = "'// &
          trim(varname)//'".'
  endif

end subroutine ps_varchk
