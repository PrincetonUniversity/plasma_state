module cstate_lib_mod

  use plasma_state_mod
  implicit NONE

  public
  save

  !------------------------------------------------------------------
  ! module to contain data items shared amongst cstate_lib routines

  character*60 :: cur_components = '*' ! current components -- this specifies 
  ! the FSP component for which data is being examined, or, it has the value
  ! "*" (wildcard) to specify all components.

  integer :: ncur_components(ps_ccount) = 1 ! current components -- represented
  ! as integer.  0 means absent, 1 means present.

  ! data windows-- gather data on states and/or state comparisons
  type (ps_list_window), dimension(:), allocatable :: ww,wwnorm

  ! data windows for display
  type (ps_list_window), dimension(:), allocatable :: wdisp

  ! windows w/o variation from state to state (e.g. list of dimensions)
  type (ps_list_window) :: w1,wmerge,wgrid,wtmp

  !----------------------------------------
  ! utility variables for displays

  character*70 :: rline = &
       '----------------------------------------------------------------------'

  character*60 c2lab  ! col. 2 label
  character*60 c3lab  ! col. 3 label
  character*60 c4lab  ! col. 4 label

  character*256 :: cbuf
  
  !------------------------------------------------------------------
  ! old style sglib/trgraf plot labels

  character*75 mnlab(3)
  character*17 kylab(5)

  !------------------------------------------------------------------
  ! list of grid dimensions

  character*32, dimension(:), allocatable :: grid_dims

  !------------------------------------------------------------------
  ! list of empty (unallocated) dimensions

  character*32, dimension(:), allocatable :: empty_dims
  logical :: omit_empty = .TRUE.
  logical :: is_empty

  !------------------------------------------------------------------
  ! list dimension statistics block

  integer :: nldims   ! number of dimensions not tied to physical coordinates

  ! dimension names & integer lower limits of dimension as declared
  !   "0:nspec_th" -> list_lolim(j)=0; "nrfmin" -> list_lolim(j)=1

  character*32, dimension(:), allocatable :: list_dims  ! (dim#)
  integer, dimension(:), allocatable :: list_lolim      ! (dim#)
  !  (species lists, lists of beams, antennas, etc.)

  integer :: maxnl    ! max length of any list dimension in any single run.
  integer :: maxml    ! max length of any list dimension, with all runs merged.

  integer, dimension(:,:), allocatable :: dimsizes  ! list dim sizes
  !            (state#,dim#)

  integer, dimension(:), allocatable :: mrgsizes    ! merged list dim sizes

  !  mapping data for ordered merge of datasets from different states
  integer, dimension(:,:,:), allocatable :: mrg_addr_map
  !            (state#,merge#,dim#)

  !  name of species or list element in merged list
  character*32, dimension(:,:), allocatable :: mrg_name
  !                   (merge#,dim#)

  !  original ordering in each state's list dimensions
  integer, dimension(:,:,:), allocatable :: orig_order
  !            (index#,state#,dim#)

  integer, parameter :: nmax_rsum = 40  ! max number of sums reduced to...
  !  choose a safe upper limit

  character*10, dimension(:,:), allocatable :: sumlbl
  !                  (sum#,dim#)
  ! list dimension reduction summation label
  !    e.g. "BEAM" to sum over beam ion species

  integer, dimension(:,:), allocatable :: sumterms
  ! no. of terms in summations.  If 0 or 1, summation is suppressed.
  ! dimensioning: same as sumlbl

  integer, dimension(:,:,:), allocatable :: sumindx,sumctrl
  !               (index#,state#,dim#)
  ! index to list dimension reduction sum, if applicable
  ! index to reduction control information, if applicable

  !------------------------------------------------------------------
  ! enumeration dimension information

  integer :: nedims
  character*32, dimension(:), allocatable :: enum_dims  ! scalar name
  integer, dimension(:), allocatable :: enum_max        ! max size seen
  character*80, dimension(:), allocatable :: enum_decl  ! declaration
  integer, dimension(:), allocatable :: enum_ldim_ref   ! reference to list dim

  !------------------------------------------------------------------
  ! floating point display precision (#digits after decimal pt):

  integer :: nprec = 4   ! will be kept btw 2 and 16

  !------------------------------------------------------------------
  ! long character string display option
  ! these are only used for partial display of strings with lengths
  ! exceeding 32 characters...

  integer :: ntail = 0   ! 0: head of string; -1: tail of string;
  !  <positive value> -> start display at indicated charactor position

  !------------------------------------------------------------------
  ! suggested enumeration data length limit
  ! this is a hint for display of enumeration quantities such as (R,Z)
  ! limiter contours

  integer :: nenum = 6   ! will be kept btw 5 and 50

  !------------------------------------------------------------------
  ! rezone grid size control
  !   if =0, use the minimum size found in the state data

  integer :: nrezon_rho = 0

  !------------------------------------------------------------------
  ! list dimension reduction controls
  !   there are reductions for species lists and a few other things
  !   **maintenance note** -- as plasma state definition evolves
  !   (in particular if new species lists are added) this section
  !   will need to evolve in parallel).

  integer, parameter :: nreduce_opt = 14
  logical :: reduce_active(nreduce_opt) = (/ &
       .FALSE., .FALSE., .FALSE., .FALSE., &
       .FALSE., .FALSE., .FALSE., .FALSE., &
       .FALSE., .FALSE., .FALSE., .FALSE., &
       .FALSE., .FALSE. /)

  ! These should be all in LOWER CASE letters:
  character*12 :: reduce_dims(nreduce_opt) = (/ &
       'nspec_th    ', 'nspec_all   ', 'nspec_tha   ', 'nspec_alla  ', &
       'nspec_beam  ', 'nspec_fusion', 'nspec_rfmin ', 'nspec_gas   ', &
       'nspec_imp0  ', 'nspec_impi  ', &
       'nbeam       ', 'nicrf_src   ', 'nlhrf_src   ', 'necrf_src   ' /)

  ! Some scalar reductions are power weighted averages instead of straight
  ! sums.  List these here...
  integer, parameter :: nreduce_avg = 4

  ! All in UPPER CASE letters:
  character*10 :: avg_var(nreduce_avg) = (/ &
       'KVOLT_NBI ', 'FREQ_IC   ', 'FREQ_LH   ', 'FREQ_EC   ' /)
  character*10 :: avg_wgt(nreduce_avg) = (/ &
       'POWER_NBI ', 'POWER_IC  ', 'POWER_LH  ', 'POWER_EC  ' /)

  !---------------------------
  ! flags to track user-modified state labels

  logical :: user_label(nsss_max)

end module cstate_lib_mod
