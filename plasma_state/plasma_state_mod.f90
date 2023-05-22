module plasma_state_mod

  ! Version 0.0 7/16/2006 (Batchelor)
  ! Version 0.1 9/21/2006 (McCune)
  ! Version 0.2 10/8/2006 (McCune)

  ! Version 1.0 4/02/2007 (McCune)
  !    version ID tag added to file & version ID access methods in module
  !       character*32 parameter:  ps_version_id  (plasma_state_constants_mod)
  !    partial backwards compatibility to earlier state definitions 

  ! Version 2.0 12/10/2007 (McCune)
  !    to go with Version 2.0 generated code -- self-describing NetCDF I/O;
  !    fast species split by type; abridged species list, etc.

  !---------------------------------------------------------------------------
  ! D. McCune -- public interface to plasma_state
  !    built over code generated system:
  !       plasma_state_definition_mod.f90
  !       i/o routines
  !    Specification of contents:  plasma_state_spec.dat
  !       (this is the input to the python code generator)
  !
  !---------------------------------------------------------------------------
    
  use plasma_state_definition_mod
  !--------------------------------------------------------------------------
  !
  !   Data declarations
  !
  !--------------------------------------------------------------------------
    
  IMPLICIT NONE
  SAVE
  PUBLIC

  ! initialization flag (cf ps_init_tag)
  logical, private :: plasma_state_mod_init = .FALSE.

  TYPE (plasma_state), target :: ps    ! current plasma state (being updated)
  TYPE (plasma_state), target :: psp   ! previous (committed) plasma state
  TYPE (plasma_state), target :: ps_next  ! future state (being updated)
                                       ! (for updates not yet wanted in ps).

  TYPE (plasma_state), target :: saw0  ! state at start of sawtooth
  TYPE (plasma_state), target :: saw1  ! state at end of sawtooth

  TYPE (plasma_state), target :: pel0  ! state at start of pellet injection
  TYPE (plasma_state), target :: pel1  ! state at end of pellet injection

  TYPE (plasma_state), target :: output ! state for time dependent output
  TYPE (plasma_state), target :: aux   ! auxilliary state

  TYPE (plasma_state), target :: cs    ! "client-server" state 
  !                                      (used by TSC//TRANSP)

  ! path to state files (default values assigned) (modification permitted).
  CHARACTER*256 :: state_path = ' '

  !-------------------------------------------------------------------------
  !  information for comparison of states...

  integer, parameter :: nsss_max = 5
  integer :: nsss_cur = 0

  TYPE (plasma_state), target :: sss(nsss_max)  ! array of states to compare

  character*256 :: sss_path(nsss_max)  ! sss state file paths
  character*32  :: sss_label(nsss_max) ! sss state display labels

  !--------------------------------------------------------------------------
  ! used internally to suppress file i/o...
  CHARACTER*1, parameter, private :: no_file = ' '

  !--------------------------------------------------------------------------
  !  public routines
  !  likely to be used during time advance loop:
  !
  !    PS_GET_PLASMA_STATE -- read plasma state from file or URL.
  ! 
  !    PS_STORE_PLASMA_STATE -- save full updated plasma state to file.
  !        (this includes a PS_STATE_MEMORY_UPDATE operation).
  !
  !    PS_STATE_MEMORY_UPDATE -- check/update internal state interpolation
  !                             data and hash tables(no file is written).
  !
  !    PS_COMMIT_PLASMA_STATE -- copy current state to prior (previous) state
  !                              (ps -> psp) and write to file.
  !                              (note: ps_next is not touched or copied).
  !
  !    PS_COPY_PLASMA_STATE -- copy a plasma state; the entire state may be
  !                            copied, or, grids and profiles of some 
  !                            components may be omitted.
  ! 
  !    PS_MERGE_PLASMA_STATE-- merge data from 2 state objects into a 3rd
  !                            object-- this can be used e.g. to time
  !                            interpolate state data
  !
  !    PS_WRITE_UPDATE_FILE -- write file containing updated quantities only,
  !                            i.e. quantities modified since the state was
  !                            last read or updated.
  !
  !    PS_READ_UPDATE_FILE  -- read file containing updated quantities; apply
  !                            updates to indicated state object.
  !
  !    PS_UPDATE_REPORT     -- write a report on what state quantities have
  !                            been updated since the last hash table update
  !                            (i.e. usually since the last ps_get_plasma_state
  !                            or ps_store_plasma_state or 
  !                            ps_state_memory_update call).  There is also
  !                            an option to instead write the list of what
  !                            has not changed.
  ! 
  !    PS_DEFAULT_FILEPATH --  generate a default filename for a specified 
  !                            instance of a plasma state object (full state
  !                            file or partial update file).  This is generally
  !                            used by  module routines such as
  !                            PS_GET_PLASMA_STATE to define a default filename
  !                            which can be overridden by an optional argument.
  !                            However, the routine is publicly available.
  ! 
  !  updating the MHD equilibrium representation--
  !  the plasma state can contain both a free boundary equilibrium: Psi(R,Z)
  !  and a core flux coordinate equilibrium representation: [R,Z](rho,theta)
  !  and profile g(rho)=(R*B_phi), Psi(rho) and q(rho), toroidal flux phit(rho)
  !  and pressure P(rho).  The radial coordinate rho covers range [0,1] and
  !  satisfies rho = sqrt(Phit(rho)/Phit(1)); Phit(rho=1) the total toroidal
  !  flux (Wb) enclosed by the boundary [R,Z](rho=1,theta).
  !
  !  The free boundary equilibrium representation need not be present, but
  !  if it is present it should be maintained current as the equilibrium is
  !  evolved in the plasmas state object.
  !
  !  The "easiest" way to set up the equilibrium is to read it in from an
  !  EFIT (or EFIT compatible) G-eqdsk file or MDS+ data source.  Use:
  !
  !    PS_UPDATE_EQUILIBRIUM -- update entire MHD equilibrium set {Psi(R,Z),
  !                             [R,Z](rho,theta), q(rho), g(rho), P(rho),
  !                             Psi(rho), Tor. flux. vs. (rho), and 
  !                             representations of the magnetic field}
  !                             from a G-eqdsk file.
  !
  !  This routine also causes profiles of enclosed volume, enclosed
  !  cross-sectional area, area-of-flux-surface toroid, and poloidal
  !  path length to be computed: ps%vol(:), ps%area(:), ps%surf(:), and
  !  ps%lpol(:).  However, other flux surface averaged
  !  metric and field quantities are not updated by this call-- see
  !  PS_MHDEQ_DERIVE subroutine, below.
  !
  !  It is also possible to maintain the equilibrium directly by setting
  !  elements of the state as defined over grids ps%rho_eq(:) and ps%th_eq(:)
  !  for the core equilibrium, and ps%R_grid(:) and ps%Z_grid(:) for the 
  !  free boundary Psi(R,Z).  If the equilibrium is maintained this way,
  !  self-consistency is the responsibility of the driving code, although
  !  some routines are available to help.
  !
  !  The core equilibrium flux surface geometry [R,Z](rho,theta) are 
  !  specified in bicubic Hermite representation by the state elements:
  !      ps%R_geo(:,:)  (all dimensioned (1:ps%nrho_eq,1:ps%nth_eq)
  !      ps%dR_geo_drho(:,:)
  !      ps%dR_geo_dth(:,:)
  !      ps%d2R_geo_drhodth(:,:)
  !      ps%Z_geo(:,:)  (all dimensioned (1:ps%nrho_eq,1:ps%nth_eq)
  !      ps%dZ_geo_drho(:,:)
  !      ps%dZ_geo_dth(:,:)
  !      ps%d2Z_geo_drhodth(:,:)
  !
  !  The following routine can load these profiles from input data, computing
  !  a C2 bicubic spline which is then converted to the bicubic Hermite
  !  representation (but still continuously twice differentiable in all
  !  coordinates):
  !
  !    PS_UPDATE_RZSURF -- update MHD equilibrium geometry from data:
  !                        {R(rho,theta),Z(rho,theta)} or
  !                        {R(theta,rho),Z(theta,rho)}; spline boundary 
  !                        conditions may be specified as optional arguments
  !                        Note: the state's rho_eq and th_eq grids have to
  !                        have been defined and their sizes must match the
  !                        dimensions of the 2d array data provided.
  !
  !  This routine also causes profiles of enclosed volume, enclosed
  !  cross-sectional area, area-of-flux-surface toroid, and poloidal
  !  path length to be computed: ps%vol(:), ps%area(:), ps%surf(:), and
  !  ps%Lpol(:).
  !
  !  The core equilibrium specification (and associated field) is completed
  !  by the field and current direction specifiers ps%kccw_Bphi and 
  !  ps%kccw_Jphi (which are part of shot configuration, set once during
  !  initialization) and a small set of 1d profiles.  For convenience during
  !  use of equilibrium data, there is some redundancy in the representation;
  !  if the profiles are set directly by driver code, this code assumes 
  !  responsibility for self-consistency.  The routine PS_MHDEQ_VERIFY can
  !  be used to check and/or recompute specified profiles.
  !
  !  Here are are the equilibrium radial profiles (for state instance ps)
  !  with units and sign convention:
  !      0) defined over grid ps%rho(1:ps%nrho_eq)
  !      1) ps%g_eq(1:ps%nrho_eq) = R*|B_phi|, always POSITIVE, T*m
  !      2) ps%q_eq(1:ps%nrho_eq) = q profile, always POSITIVE, dimensionless.
  !      3) ps%P_eq(1:ps%nrho_eq) = Pressure profile, non-negative, Pa.
  !      4) ps%Psipol(1:ps%nrho_eq) = Psi(rho), Wb/rad.
  !            Psi(0)=0; d(Psi)/drho = rho*[Phit(rho=1)]/(pi*q(rho))
  !                      d(Psi)/drho POSITIVE for rho > 0.
  !      5) ps%Phit(1:ps%nrho_eq) = Phit(rho), Wb, always POSITIVE
  !            Phit(rho)=integral[0 to rho]{drho*g(rho)*dVol/drho*<1/R^2>}
  !            This is the enclosed toroidal flux.
  !
  !  Several updating schemes are possible:
  !      a) driver updates all profiles and maintains self-consistency;
  !      b) driver updates P_eq, g_eq, and q_eq, and uses PS_MHDEQ_VERIFY
  !         to set Psipol and to set/check Phit;
  !      c) driver updates P_eq, g_eq, and Psi_eq, and uses PS_MHDEQ_VERIFY
  !         to set q_eq and to set/check Phit.
  !
  !    PS_MHDEQ_VERIFY  -- perform updates and/or checks on 1d MHD equilibrium
  !                        profiles, according to specification of an ACTION
  !                        variable.  Note that when Phit is computed by
  !                        integration, it must come close to satisfying
  !                        (for j_rho = 1:ps%nrho_eq)
  !                           ps%phit(j_rho)=
  !                              ps%rho_eq(j_rho)**2 * ps%phit(ps%nrho_eq)
  !                        If it does not, it indicates an inconcistency 
  !                        among ps%g_eq(:), ps%R_geo(:,:), and ps%Z_geo(:,:),
  !                        and an error condition is set.
  !                        IF THERE IS NO ERROR, then, update internal field
  !                        splines needed for field averages and NCLASS flux
  !                        surface averages.  Profiles of enclosed volume
  !                        (ps%vol(:), etc.) are updated as well.
  !
  !    PS_MHDEQ_VERIFY takes the total plasma current as an optional argument.
  !       If this is provided, it udpates ps%curt(:) and ps%jdotb(:): the
  !       enclosed toroidal current and <J.B>.
  !
  !  Whether the equilibrium is updated directly, or updated from the read of
  !  an EFIT G-eqdsk file, there are numerous "flux surface averaged metrics"
  !  associated with any core equilibrium.  These metrics and their time
  !  variation enter into transport equations and neoclassical modeling of
  !  tokamak plasmas.  The following routine is provided to manage computation
  !  of these derived metrics and field averages after completion of the 
  !  update of the core equilibrium:
  !
  !  The flux surface averaged quantities are stored in the state on the
  !  grid ps%rho_eq_geo(:)-- so, a different grid than the one used for
  !  the primary equilibrium quantities can be used, if desired.
  !
  !  The grid on which metric quantities are computed can also be set
  !  independently-- it is sometimes sufficient (and desirable for
  !  performance reasons) to compute metrics on a somewhat coarser grid
  !  and then fill in intermediate points by interpolation.  If this is
  !  done, it is recommended that the computation grid and the target grid
  !  ps%rho_eq_geo(:) match for several points adjacent to the magnetic 
  !  axis and the plasma boundary.
  !
  !    PS_MHDEQ_DERIVE --  compute sets of derived profiles after equilibrium
  !                        is updated by {PS_UPDATE_RZSURF,PS_MHDEQ_VERIFY} or
  !                        by PS_UPDATE_EQUILIBRIUM (from g-file).  Set ACTION
  !                        variable to select... "<,>" indicates differential
  !                        volume flux surface averages... for more information
  !                        on specific profiles see plasma_state_spec.dat.
  !                        Actions marked with asterisk "*" depend only on
  !                        [R,Z](rho,theta) and so can be executed after 
  !                        PS_UPDATE_RZSURF and before PS_MHDEQ_VERIFY, if
  !                        desired:
  !
  !                          "All" or "Everything" -- all of these:
  !
  !                          "grho2r2i"* -- compute <|grad(rho)|^2/R^2>
  !                                        and <1/R^2>.  This fills out
  !                                        elements ps%grho2ri and ps%gr2i
  !
  !                          "metric_avg"* -- compute all other flux surface
  !                                        averages of products of powers
  !                                        of R and grad(rho): ps%grho1,
  !                                        ps%grho2, ps%gr1, ps%gr2, ps%gri,
  !                                        ps%gr3i, ps%grho2r3i, ps%gr2rho2.
  !
  !                          "field_avg" or "NCLASS" -- compute field average
  !                                        quantites <B>, <B^2>, <1/B^2>, and
  !                                        also compute field averages used
  !                                        by NCLASS neoclassical model:
  !                                        ps%gbr2, ps%gb1, ps%gb2, ps%gb2i,
  !                                        ps%grho2b2i, ps%gamma_nc,
  !                                        ps%ngradb2_av, ps%psmom_nc,
  !                                        ps%psmom_errck
  !                                        IF dimension npsmom has not been
  !                                        set at time of call, a default
  !                                        value of 16 is chosen!
  !
  !                          "NC_aux" -- compute mildly singular flux surface
  !                                        averages involving sqrt(1-B/Bmax):
  !                                        ps%gncfh, ps%gncfb2h.
  !
  !                          "surf_param"* -- compute elongation and triang-
  !                                        ularity profiles ps%elong, 
  !                                        ps%triang, ps%triangU, ps%triangL
  !                                        AND minmax values:
  !                                        ps%R_surfMin, ps%R_surfMax,
  !                                        ps%Z_surfMin, ps%Z_surfMax,
  !                                        AND approximate midplane through
  !                                        each flux surface: ps%Z_midp,
  !                                        ps%R_midp_in, ps%R_midp_out.
  !
  !                          "Bminmax" --  Bmin and Bmax of each flux surface
  !                                        ps%B_surfMin, ps%B_surfMax
  ! 
  !  used only during initialization:
  !
  !    PS_INIT_USER_STATE -- initialize user defined state object (i.e.
  !                          an instance of a state object declared outside
  !                          this module).  A unique name must be supplied.
  !
  !    PS_UPDATE_HASHCODES --update hash codes.  Hash codes are a mechanism
  !                          for detecting state changes; they are used e.g.
  !                          to enforce locking of machine description, shot
  !                          configuration, and simulation initialization
  !                          sections of the state, after initialization is
  !                          complete.  Optional arguments to this routine
  !                          can be used to set section locks.
  !
  !    PS_MDESCR_READ --     read machine description namelist or partial
  !                          namelist.  Option to provide G-eqdsk file
  !                          to override limiter and (R,Z) grid limits.
  !                          Namelist file can contain comments delimited 
  !                          by exclamation point ("!") characters.
  !                          For an existing state, PS_MDESCR_WRITE can be
  !                          used to write a commented machine description
  !                          namelist.  NOTE: the routine assumes by default
  !                          that the Plasma State may already contain a
  !                          partial machine description.  Use the optional
  !                          argument ACTION='INIT' or ACTION='NEW' to
  !                          explicitly clear machine description variables
  !                          to their default values prior to the read (or
  !                          simply use a newly created or empty Plasma State 
  !                          object).  The default is ACTION='ADD' which
  !                          allows the machine description namelist to be 
  !                          subdivided into several files that are read 
  !                          separately.
  !
  !    PS_SCONFIG_READ --    read shot configuration namelist; lock the shot
  !                          configuration.  Namelist file can contain 
  !                          comments delimited by exclamation point ("!")
  !                          characters.  For an existing state, 
  !                          PS_SCONFIG_WRITE can be used to write a 
  !                          commented shot configuration namelist.
  !                          NOTE: no partial read capability.  The entire
  !                          shot configuration must be in a single namelist.
  !
  !    PS_COPY_DIMS --       copy array dimensions from one state to another
  !                          but do not allocate arrays (an aid to creating
  !                          e.g. custom light weight state objects: some 
  !                          dimensions can be zeroed, and then allocate and
  !                          use PS_MERGE_PLASMA_STATE to copy only the
  !                          allocated parts.
  ! 
  !
  !    PS_ALLOC_PLASMA_STATE -- incrementally allocate plasma state (at start
  !                             of simulation).  Once arrays are allocated,
  !                             they cannot be reallocated, i.e. dynamic 
  !                             growth of arrays is not currently supported;
  !                             a future version of the software will allow
  !                             this.
  !
  !    PS_SPECIES_CONVERT -- convert integer species data (atomic number,
  !                          charge number, mass in integer AMU) to floating
  !                          point (charges in C, mass in kg).
  !
  !    PS_SET_BEAM_SPECIES -- construct beam species list (SNBI) from 
  !                        nbion(1:nbeam), i.e. from the list of species
  !                        injected by each neutral beam.  This routine
  !                        will be called automatically by PS_LABEL_SPECIES,
  !                        if it detects that size(SNBI)=0 but size(NBION)>0.
  !
  !    PS_LABEL_SPECIES -- generate species labels for the following species
  !                        lists: thermal species "s"; beam species "snbi";
  !                        fusion product species "sfus"; RF minority 
  !                        species "rfmin".
  !
  !    PS_MERGE_SPECIES_LISTS -- combine thermal and non-thermal species lists.
  !                        This should only be called ONCE in the history of
  !                        a state; it forms the species lists:
  !                          "sa": reduced thermal species list: electrons,
  !                                fully stripped H & He isotopes, and just 2
  !                                Z>2 impurity species at most; subset of "s".
  !                          "all": union of "s", "snbi", "rfmin", "sfus".
  !                          "alla": union of "sa", "snbi", "rfmin", "sfus".
  !
  !    PS_NEUTRAL_SPECIES -- form a list of neutral species: all isotopes
  !                          with Z<=2 (He and H isotopes); optional arguments
  !                          allow adjustment of this selection.  This should
  !                          only be called ONCE in the history of a state, to
  !                          create the list of recycling neutral species.
  !
  !
  !  interpolation:
  !    These are interfaces which support scalar or vector evaluations
  !    and evaluation of a single profile or of a group of profiles which
  !    are similar in the sense of being defined over a common grid and 
  !    using a common interpolation representation.  For profiles with
  !    interpolation representation other than step function, derivative
  !    evaluation is also available.
  !
  !    PS_INTRP_1D -- interpolation on profiles defined over a single
  !                   coordinate grid.
  !
  !    PS_INTRP_2D -- interpolation on profiles defined over two
  !                   coordinate grids.
  !
  !    PS_RZ2RHOTH -- coordinate transformation:  (R,Z) -> (rho,theta)
  !    PS_RHOTH2RZ -- coordinate transformation:  (rho,theta) -> (R,Z)
  !
  !    PS_USER_1DINTRP -- interpolation of user proved 1d profile arrays
  !
  !    PS_RZ_EVAL  -- another interface to (rho,theta) -> (R,Z) with 
  !                   capability for derivatives, and with choice between
  !                   the bicubic spline and the Fourier spline 
  !                   representations of [R,Z](rho,theta) core flux
  !                   surfaces.
  !
  !  rezoning:
  !    Rezoning of profiles (over certain supported coordinate
  !    combinations) with preservation of normalization-- volume integral
  !    for densities, cross-sectional area integration for current densities.
  !
  !    The input profiles can employ any interpolation, but, rezoning of
  !    step functions is thought to be the normally used functionality.
  !
  !    PS_RHO_REZONE -- rezone profile over a new rho grid.  Input profile
  !                   can be defined over (rho) or (rho,theta).
  !
  !    PS_RHOTH_REZONE -- rezone 2d profile to new 2d combination of rho
  !                   and theta grids; input profiles defined over (rho,theta).
  !                   -- NOT CURRENTLY IMPLEMENTED --
  !
  !    If input profiles are some kind of density (e.g W/m^3), the output
  !    by default is in the same units.  But, an optional switch nonorm=.TRUE.
  !    can be set to cause the data to be returned without the density 
  !    normalization (e.g W/zone).
  !
  !    The routine PS_RHO_REZONE is used to rezone a state profile.  It is
  !    also possible to rezone user profiles (not in the state):
  !
  !    PS_USER_REZONE -- conservative rezone of user profile(s) of form f(rho).
  !
  !  Specialized data retrieval or interpolation calls for thermal ions:
  !
  !    PS_THA_FETCH -- fetch densities, temperatures, and/or <vpar>s for all
  !                    elements of the reduced thermal ion species lists.  No
  !                    interpolation; dimensioning of arrays to receive the
  !                    information must match the radial grid : ps%nrho - 1 
  !                    (zonal data) and species list dimension ps%nspec_tha
  !                    (for state object ps).  Electron data is not fetched.
  !
  !    PS_THA_REZONE-- rezone densities, temperatures, and/or <vpar>s for all
  !                    elements of the reduced thermal ion species lists.  The 
  !                    output profiles are conservatively rezoned to the 
  !                    user specified grid.  The data returned are zone
  !                    oriented: dimensioned size(rho) - 1; the number of
  !                    ion species is ps%nspec_tha (for state object ps).
  !                    Electron data is not returned in the output.
  !
  !    PS_TI_FETCH --  fetch a total density, averaged temperature, <vpar>
  !                    or toroidal angular velocity, by summing over all 
  !                    thermal ion species (optionally constrained by a 
  !                    range of atomic numbers).  No interpolation; output
  !                    arrays must be dimensioned: ps%nrho - 1, to receive
  !                    zonal data.
  !
  !    PS_TI_REZONE -- rezone a total density, averaged temperature, <vpar>
  !                    or toroidal angular velocity, by summing over all 
  !                    thermal ion species (optionally constrained by a 
  !                    range of atomic numbers).  The output profiles are
  !                    conservatively rezoned to the user specified rho grid.
  !                    The data returned are zone oriented: dimensioned 
  !                    size(rho) - 1.
  !
  !  Also available:
  !
  !    PS_PSIMAP -- given a set of Psi values find corresponding Rho values
  !                 e.g. as a preparatory step for interpolation
  ! 
  !    PS_RHO_METRIC -- return flux surface averaged metrics over a specified
  !                     rho grid...
  !                     NOTE: common flux surface averages are also defined
  !                     as profile elements of the state (and can be rapidly
  !                     interpolated to any grid).  See description of 
  !                     PS_MHDEQ_DERIVE, above.
  ! 
  !
  !  And:
  !    PS_SAVE_HASH_CODES -- copy hash_codes => saved_hash_codes
  !                     (returning warning status if prior saved_hash_codes
  !                     values are overwritten).
  !    PS_RESTORE_HASH_CODES -- restore saved_hash_codes => hash_codes
  !                     (return error status if there was no prior call to
  !                     ps_save_hash_codes for the referenced plasma state
  !                     object.
  !
  !       PS_SAVE_HASH_CODES and PS_RESTORE_HASH_CODES can be used to control
  !       the hash state to use for determining the output written by a call to
  !       PS_WRITE_UPDATE_FILE; this can be useful if a component needs to 
  !       make intermediate calls which affect hash codes, such as calls to
  !       PS_STATE_MEMORY_UPDATE(...)
  !
  !       NOTE: saved_hash_codes are NEVER written to or read from state files
  !       nor are they copied in a PS_COPY_PLASMA_STATE call.
  !--------------------------------------------------------------------------
  ! Cleanup
  !    PS_FREE_USER_STATE -- free memory in state and underlying XPLASMA object
  !
  !--------------------------------------------------------------------------
  ! Additional methods in the module -- these are used by the cstate program
  ! For more information see the comments accompanying the method code below
  !
  ! These are public for access from cstate_lib but are not documented for
  ! for general use; interfaces may change in the future.
  !
  !   ps_get_prof_label
  !   ps_get_grid_by_name
  !   pscomp_grids
  !   ps_cclist_add
  !   ps_cclist_remove
  !   psw_components
  !   psw_contents
  !   psw_slist
  !   psw_ilist
  !   psw_dlist
  !   psw_plist
  !   psw_merge
  !--------------------------------------------------------------------------

  !---------------------------------------
  ! interpolation generic interfaces

  interface ps_intrp_1d
     module procedure &
          ps_intrp_1d_sc, ps_intrp_1d_vec, &
          ps_intrp_1d_sc_arr1, ps_intrp_1d_vec_arr1, &
          ps_intrp_1d_sc_arr2, ps_intrp_1d_vec_arr2
  end interface

  interface ps_intrp_2d
     module procedure &
          ps_intrp_2d_sc, ps_intrp_2d_vec, &
          ps_intrp_2d_sc_arr1, ps_intrp_2d_vec_arr1, &
          ps_intrp_2d_sc_arr2, ps_intrp_2d_vec_arr2
  end interface

  interface ps_rz2rhoth
     module procedure &
          ps_rz2rhoth_sc, ps_rz2rhoth_vec
  end interface

  interface ps_rhoth2rz
     module procedure &
          ps_rhoth2rz_sc, ps_rhoth2rz_vec
  end interface

  interface ps_rz_eval
     module procedure &
          ps_rz_eval_sc, ps_rz_eval_vec
  end interface

  interface ps_rho_rezone
     module procedure ps_rho_rezone1,ps_rho_rezone_arr1,ps_rho_rezone_arr2
  end interface

  interface ps_user_rezone
     module procedure ps_user_rezone1,ps_user_rezone_arr1,ps_user_rezone_arr2
  end interface

  interface ps_user_1dintrp
     module procedure ps_user_1dintrp_sc,ps_user_1dintrp_vec
     module procedure ps_user_1dintrp_sc_arr1,ps_user_1dintrp_vec_arr1
  end interface

  !---------------------------------------
  ! private routines & data
  !   SCHK -- vector size compare with mismatch error message write
  !   RINGCHK -- spline ring check/correct (values only, not derivatives).

  private :: schk,ringchk

  ! private PSW routines
  private :: psw_order,psw_ordersub,psw_ordersub2

  ! species labeling routines now PRIVATE:
  private :: ps_label_spectype, ps_label_species_1

  ! misc internal error checking:
  private :: tha_gridchk,tha_dimchk,chksize_ti

  ! initialization check
  private :: chk_ini

  ! user 1d interpolation (internal)

  private :: chk_xorder,interp_1d

  ! impurity species
  private :: ps_imp_species

  !---------------------------------------
  ! the following for species labeling...
  ! --- parameters ---
  !
  ! RGA June 2013 -- Table represents isotopic average AMU in (u).  Separate subroutine
  ! atom_amu_to_proton() to convert to proton mass while accounting for charge state.
  ! Copied from comput/periodic_table.f90 but use character*3 for ISO symbol and populate
  !        for He and Li isotopes.
  ! **************************
  !
  ! --- a structure which describes an element ---
  !
  logical, parameter, private :: ignore_user_AMU = .true.      ! .true. -> old behaviour, ignore user's AMU when the element does not
                                                               !           have known isotopes (Z>3)
                                                               ! .false.-> allow user AMU for Z>3

  logical, parameter, private :: ignore_electron_AMU = .true.  ! .true. to NOT adjust neutral atom AMU by electron weight
                                                               ! why does this matter to toric?

  type, private :: ELM
     character*2 :: symbol   ! symbol for the element
     integer     :: number   ! atomic number of the element
     real(rspec) :: weight   ! isotopic average atomic weight of the element
     integer     :: isoindex ! index of first isotope in isotope list or 0 if none
  end type ELM
 
  !
  ! --- a structure which describes an isotope ---
  !
  type, private :: ISO
     character*3  :: symbol   ! symbol for the isotope or blank for element symbol
     character*13 :: name     ! name for the isotope or blank for element name
     integer      :: number   ! atomic number of the isotope
     real(rspec)  :: weight   ! atomic weight of the isotope
  end type ISO

  !
  ! --- parameters ---
  ! http://physics.nist.gov/cuu/Constants/Table/allascii.txt
  !
  real(rspec), parameter, private :: electronCharge = 1.602176565e-19_rspec    ! (C), also J/eV
  real(rspec), parameter, private :: proton_amu     = 1.007276466812_rspec     ! (u)
  real(rspec), parameter, private :: electron_amu   = 0.000548579909_rspec     ! (u)
  real(rspec), parameter, private :: neutron_amu    = 1.00866491600_rspec      ! (u)
  real(rspec), parameter, private :: speedLight     = 2.99792458e8_rspec       ! (m/s)
  real(rspec), parameter, private :: protonMass     = 1.672621777e-27_rspec    ! (kg)

  integer, parameter,private  :: max_elements = 109 ! number of elements

  !
  ! weights are isotopic average in AMU for neutral atom
  !
  type (elm), dimension(max_elements), parameter, private :: elex = (/ &
       elm('Ac', 89,227.0000_rspec,0) , elm('Ag', 47,107.8680_rspec,0) , elm('Al', 13, 26.9815_rspec,0) ,  &
       elm('Am', 95,243.0000_rspec,0) , elm('Ar', 18, 39.9480_rspec,0) , elm('As', 33, 74.9216_rspec,0) ,  &
       elm('At', 85,210.0000_rspec,0) , elm('Au', 79,196.9665_rspec,0) , elm('B ',  5, 10.8100_rspec,0) ,  &
       elm('Ba', 56,137.3400_rspec,0) , elm('Be',  4,  9.0122_rspec,0) , elm('Bh',107,262.0000_rspec,0) ,  &
       elm('Bi', 83,208.9804_rspec,0) , elm('Bk', 97,247.0000_rspec,0) , elm('Br', 35, 79.9040_rspec,0) ,  &
       elm('C ',  6, 12.0110_rspec,0) , elm('Ca', 20, 40.0800_rspec,0) , elm('Cd', 48,112.4000_rspec,0) ,  &
       elm('Ce', 58,140.1200_rspec,0) , elm('Cf', 98,251.0000_rspec,0) , elm('Cl', 17, 35.4530_rspec,0) ,  &
       elm('Cm', 96,247.0000_rspec,0) , elm('Co', 27, 58.9332_rspec,0) , elm('Cr', 24, 51.9960_rspec,0) ,  &
       elm('Cs', 55,132.9054_rspec,0) , elm('Cu', 29, 63.5460_rspec,0) , elm('Db',105,262.0000_rspec,0) ,  &
       elm('Dy', 66,162.5000_rspec,0) , elm('Er', 68,167.2600_rspec,0) , elm('Es', 99,252.0000_rspec,0) ,  &
       elm('Eu', 63,151.9600_rspec,0) , elm('F ',  9, 18.9984_rspec,0) , elm('Fe', 26, 55.8470_rspec,0) ,  &
       elm('Fm',100,257.0000_rspec,0) , elm('Fr', 87,223.0000_rspec,0) , elm('Ga', 31, 69.7200_rspec,0) ,  &
       elm('Gd', 64,157.2500_rspec,0) , elm('Ge', 32, 72.5900_rspec,0) , elm('H ',  1,  1.0079_rspec,2) ,  &
       elm('He',  2,  4.0026_rspec,5) , elm('Hf', 72,178.4900_rspec,0) , elm('Hg', 80,200.5900_rspec,0) ,  &
       elm('Ho', 67,164.9304_rspec,0) , elm('Hs',108,265.0000_rspec,0) , elm('I ', 53,126.9045_rspec,0) ,  &
       elm('In', 49,114.8200_rspec,0) , elm('Ir', 77,192.2200_rspec,0) , elm('K ', 19, 39.0980_rspec,0) ,  &
       elm('Kr', 36, 83.8000_rspec,0) , elm('La', 57,138.9055_rspec,0) , elm('Li',  3,  6.9410_rspec,7) ,  &
       elm('Lr',103,262.0000_rspec,0) , elm('Lu', 71,174.9700_rspec,0) , elm('Md',101,258.0000_rspec,0) ,  &
       elm('Mg', 12, 24.3050_rspec,0) , elm('Mn', 25, 54.9380_rspec,0) , elm('Mo', 42, 95.9400_rspec,0) ,  &
       elm('Mt',109,265.0000_rspec,0) , elm('N ',  7, 14.0067_rspec,0) , elm('Na', 11, 22.9898_rspec,0) ,  &
       elm('Nb', 41, 92.9064_rspec,0) , elm('Nd', 60,144.2400_rspec,0) , elm('Ne', 10, 20.1790_rspec,0) ,  &
       elm('Ni', 28, 58.7000_rspec,0) , elm('No',102,259.0000_rspec,0) , elm('Np', 93,237.0482_rspec,0) ,  &
       elm('O ',  8, 15.9994_rspec,0) , elm('Os', 76,190.2000_rspec,0) , elm('P ', 15, 30.9738_rspec,0) ,  &
       elm('Pa', 91,231.0359_rspec,0) , elm('Pb', 82,207.2000_rspec,0) , elm('Pd', 46,106.4000_rspec,0) ,  &
       elm('Pm', 61,145.0000_rspec,0) , elm('Po', 84,209.0000_rspec,0) , elm('Pr', 59,140.9077_rspec,0) ,  &
       elm('Pt', 78,195.0900_rspec,0) , elm('Pu', 94,244.0000_rspec,0) , elm('Ra', 88,226.0254_rspec,0) ,  &
       elm('Rb', 37, 85.4678_rspec,0) , elm('Re', 75,186.2070_rspec,0) , elm('Rf',104,261.0000_rspec,0) ,  &
       elm('Rh', 45,102.9055_rspec,0) , elm('Rn', 86,222.0000_rspec,0) , elm('Ru', 44,101.0700_rspec,0) ,  &
       elm('S ', 16, 32.0600_rspec,0) , elm('Sb', 51,121.7500_rspec,0) , elm('Sc', 21, 44.9559_rspec,0) ,  &
       elm('Se', 34, 78.9600_rspec,0) , elm('Sg',106,263.0000_rspec,0) , elm('Si', 14, 28.0860_rspec,0) ,  &
       elm('Sm', 62,150.4000_rspec,0) , elm('Sn', 50,118.6900_rspec,0) , elm('Sr', 38, 87.6200_rspec,0) ,  &
       elm('Ta', 73,180.9479_rspec,0) , elm('Tb', 65,158.9254_rspec,0) , elm('Tc', 43, 97.0000_rspec,0) ,  &
       elm('Te', 52,127.6000_rspec,0) , elm('Th', 90,232.0381_rspec,0) , elm('Ti', 22, 47.9000_rspec,0) ,  &
       elm('Tl', 81,204.3700_rspec,0) , elm('Tm', 69,168.9342_rspec,0) , elm('U ', 92,238.0290_rspec,0) ,  &
       elm('V ', 23, 50.9414_rspec,0) , elm('W ', 74,183.5000_rspec,0) , elm('Xe', 54,131.3000_rspec,0) ,  &
       elm('Y ', 39, 88.9059_rspec,0) , elm('Yb', 70,173.0400_rspec,0) , elm('Zn', 30, 65.3800_rspec,0) ,  &
       elm('Zr', 40, 91.2200_rspec,0) &
       /)
  !
  ! known isotopes of select elements and electron
  ! weights are in AMU for neutral atom
  ! https://en.wikipedia.org/wiki/Isotopes_of_hydrogen
  !
  integer, parameter, private :: max_isotopes = 8

  type (iso), dimension(max_isotopes), parameter, private :: isox = (/ &
       iso('e  ', 'Electron', -1, 0.000548579909_rspec), &   ! 1
       iso('H  ', 'Hydrogen',  1, 1.00782503207_rspec),  &   ! 2
       iso('D  ', 'Deuterium', 1, 2.0141017778_rspec),   &   ! 3
       iso('T  ', 'Tritium',   1, 3.0160492777_rspec),   &   ! 4
       iso('He3',  '',          2, 3.016029_rspec),      &   ! 5  He3
       iso('He4',  '',          2, 4.002602_rspec),      &   ! 6  He4
       iso('Li6',  '',          3, 6.015122_rspec),      &   ! 7  Li6
       iso('Li7',  '',          3, 7.016004_rspec)       &   ! 8  Li7
       /)
  ! 
  ! element* arrays indexed by atomic number
  !
  integer, dimension(max_elements), private :: element_index = (/ &  ! index into elex array by atomic number
       39,  40,  &
       51,  11,   9,  16,  59,  67,  32,  63,  &
       60,  55,   3,  90,  69,  85,  21,   5,  &
       48,  17,  87,  99, 103,  24,  56,  33,  &
       23,  64,  26, 108,  36,  38,   6,  88,  15,  49,  &
       79,  93, 106, 109,  61,  57,  96,  84,  &
       82,  72,   2,  18,  46,  92,  86,  97,  45, 105,  &
       25,  10,  50,  19,  75,  62,  73,  91,  &
       31,  37,  95,  28,  43,  29, 101, 107,  &
       53,  41,  94, 104,  80,  68,  47,  76,   8,  42, &
      100,  71,  13,  74,   7,  83,  &
       35,  78,   1,  98,  70, 102,  66,  77,  &
        4,  22,  14,  20,  30,  34,  54,  65,  &
       52,  81,  27,  89,  12,  44,  58 &
       /)
  !

  private :: elindex, standard_amu_rspec, use_amu, atom_amu_to_proton

CONTAINS
    
  !==========================================================================
  SUBROUTINE ps_get_plasma_state(ierr, filename, state)

    !-------------------------------------------------------------------
    !
    ! PS_GET_PLASMA_STATE
    !
    ! This subroutine reads a plasma state file and puts the data into the
    ! semi-public variables declared in plasma_state_definitions_mod
    !
    ! PS_GET_PLASMA_STATE is to be called from GET_PLASMA_STATE_<COMP> 
    ! routines
    ! 
    !-------------------------------------------------------------------

    !----------------------------------------------------------------------
    !
    !   Declare local variables
    !
    !----------------------------------------------------------------------
    
    INTEGER, INTENT(out) :: ierr  ! exit status code (0=OK)
     
    !  the default name of file to read will be constructed
    !  but, the caller can override this by passing a
    !  filename:

    CHARACTER(len=*), intent(in), optional :: filename  ! full path

    !  if this is set, the state named by (filename) is read; otherwise
    !  the filename is constructed based on the state instance being read.

    !  if the filename starts with "http://" the code will attempt to
    !  download a copy of the file for reading.

    TYPE (plasma_state), optional :: state

    !  read file data into THIS state, if argument is present
    !  otherwise read into "ps" state.

    !----------------------------------------------------------------------

    character(len=256) :: sfile,zcmd
    character(len=8) :: http_test
    integer :: ii,ilen,iout,istat

    !----------------------------------------------------------------------
    !
    !   Read Plasma State data file
    !
    !----------------------------------------------------------------------

    iout = SWIM_err_out_file

    CALL ps_init_tag
    if(present(state)) call chk_ini(state)

    if(present(filename)) then
       ! optional argument detected...
       sfile = filename
       ierr = 0

       ! check for URL...
       ilen=len(trim(filename))
       if(ilen.gt.7) then
          http_test=filename(1:8)
          call uupper(http_test)
          if((http_test(1:7).eq.'HTTP://').and.(http_test(8:8).ne.'/')) then
             write(iout,*) ' '
             write(iout,*) ' %ps_get_plasma_state: attempting URL download: ',&
                  filename(1:ilen)
             do
                if(filename(ilen:ilen).eq.'/') then
                   ilen=ilen-1
                else
                   exit
                endif
             enddo
             do ii=ilen-1,1,-1
                if(filename(ii:ii).eq.'/') exit
             enddo
             sfile = filename(ii+1:ilen)
             zcmd = '/usr/bin/curl '//trim(filename)//' > ./'//trim(sfile)
             write(iout,*) trim(zcmd)
             call execute_command_line(trim(zcmd),exitstat=istat)
             if(istat.ne.0) then
                ierr=11
                write(iout,*)  ' download failed! '
                return
             else
                write(iout,*)  ' download to: '//trim(sfile)//' -- complete.'

                sfile = filename(ii+1:ilen)
             endif
          endif
       endif

    else
       ! no filename or URL passed argument

       if(present(state)) then
          call ps_default_filepath(state, sfile, ierr)
       else
          call ps_default_filepath(ps, sfile, ierr)
       endif

    endif
    if(ierr.ne.0) then
      write(iout,*) "Error reading state file: ", present(state)
      call flush(iout)
      return
    end if

    if(present(state)) then
       CALL plasma_state_get(trim(sfile),state,ierr)
       if(ierr.eq.0) then
          if((state%nspec_th.gt.0).AND.(state%nspec_impi.eq.0)) then
             call ps_imp_species(state,ierr)
             call ps_state_memory_update(ierr,state=state)
          endif
       endif
    else
       CALL plasma_state_get(trim(sfile),ps,ierr)
       if(ierr.eq.0) then
          if((ps%nspec_th.gt.0).AND.(ps%nspec_impi.eq.0)) then
             call ps_imp_species(ps,ierr)
             call ps_state_memory_update(ierr,state=ps)
          endif
       endif
    endif

    IF(ierr.NE.0) THEN
       CALL SWIM_error('state_file_read','PS_GET_PLASMA_STATE')
       write(iout,*) 'failed to read state file: ',trim(sfile)
       return
    ENDIF

  END SUBROUTINE ps_get_plasma_state

  !==========================================================================
  SUBROUTINE ps_store_plasma_state(ierr, filename, state, eqcheck, eqcheck_RZ)
   
    !-------------------------------------------------------------------
    !
    ! PS_STORE_PLASMA_STATE
    !
    ! This subroutine updates the internal state representation and
    ! writes a plasma state file.
    !
    ! PS_STORE_PLASMA_STATE is to be called from PUT_PLASMA_STATE_<COMP> 
    ! routines.  These must re-allocate the output arrays of semi-public data
    ! and load the semi-public data from the output of the component code
    !
    !-------------------------------------------------------------------
    
    !----------------------------------------------------------------------
    !
    !   Declare local variables
    !
    !----------------------------------------------------------------------
    
    INTEGER, INTENT(out) :: ierr  ! exit status code (0=OK)

    !  the default name of file to write will be constructed
    !  but, the caller can override this by passing a
    !  filename:

    CHARACTER*(*), intent(in), optional :: filename  ! full path

    !  if this is set, the state is written to (filename); otherwise
    !  the filename is constructed based on the state instance being read.

    TYPE (plasma_state), optional, target :: state

    !  write file data from THIS state, if argument is present
    !  otherwise write from "ps" state.

    LOGICAL, intent(in), optional :: eqcheck

    !  .TRUE. (default) to check/update equilibrium representation
    !  .FALSE. to skip this check

    LOGICAL, intent(IN), OPTIONAL :: eqcheck_RZ  ! .FALSE. to suppress [R,Z]
                                                 ! free bdy equilibrium check
                                                 ! (ignored if eqcheck=.FALSE.)
                                                 ! (if eqcheck=.TRUE., the
                                                 ! default is to do the check)

    !----------------------------------------------------------------------

    TYPE (plasma_state), pointer :: ss
    character*256 sfile
    integer :: icheck

    !----------------------------------------------------------------------
    !
    !   Write plasma state file
    !
    !----------------------------------------------------------------------

    CALL ps_init_tag

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    icheck = 1
    if(present(eqcheck)) then
       if(.not.eqcheck) icheck = 0
    endif
    if(icheck.eq.1) then
       if(present(eqcheck_RZ)) then
          if(.not.eqcheck_RZ) icheck=2
       endif
    endif

    if(present(filename)) then
       sfile = filename
       ierr = 0
    else
       call ps_default_filepath(ss, sfile, ierr)
    endif
    if(ierr.ne.0) return

    CALL plasma_state_store(trim(sfile),ss,icheck,ierr)

    IF(ierr.NE.0) THEN
       CALL SWIM_error('state_file_write','PS_STORE_PLASMA_STATE')
       write(SWIM_err_out_file,*) 'failed to write state to file: ',trim(sfile)
       return
    ENDIF

  END SUBROUTINE PS_STORE_PLASMA_STATE

  !==========================================================================
  SUBROUTINE ps_update_report(ierr, state, report_filename, cclist, &
       section_control, reverse)

    !-------------------------------------------------------------------
    !
    ! PS_UPDATE_REPORT
    !
    ! This subroutine writes an ascii report of what quantities have changed
    ! in the plasma state since the last hash update (i.e., usually, since
    ! the last state I/O or memory_update operation or).
    !
    ! The report can be written to stdout (the default) or to a file.  It
    ! can optionally be restricted to elements in a subset of components.
    ! It can optionally report instead on what has NOT been updated.
    !
    ! This routine has no side effects, i.e. the Plasma State hash table
    ! contents are preserved.
    !-------------------------------------------------------------------
    
    !----------------------------------------------------------------------
    !
    !   Declare local variables
    !
    !----------------------------------------------------------------------
    
    INTEGER, INTENT(out) :: ierr  ! exit status code (0=OK)

    !--------------------------
    !  optional arguments:

    TYPE (plasma_state), optional, target :: state

    !  write based on data from THIS state, if argument is present;
    !  otherwise write based on data from "ps" state.

    CHARACTER*(*), optional, intent(in) :: report_filename

    !  write report to THIS file, if argument is present;
    !  otherwise write report to stdout.

    INTEGER, intent(in), optional :: cclist(ps_ccount)

    !  (default: report on changed items from all components)
    !  set cclist(ps_cnum_<component_name>) to 0 to suppress writes on
    !  a per component basis.  See methods ps_cclist_add, ps_cclist_remove
    !  to generate cclist control arrays.

    INTEGER, intent(in), optional :: section_control

    !  (default: 2) choose the sections on which to report:
    !   1:  State_Profiles only;
    !   2:  State_Profiles and State_Data only;
    !   3:  All sections.

    LOGICAL, intent(in), optional :: reverse

    !  Set this to .TRUE. to report on state elements NOT updated,
    !  I.e. output list of unchanged elements, instead of a list of
    !  changed elements.

    !----------------------------------------------------------------------

    TYPE (plasma_state), pointer :: ss

    integer :: loc_cclist(ps_ccount)
    integer :: jwarn,ii,iout,ilun,idum,isect
    integer :: ireverse

    !----------------------------------------------------------------------
    !
    !   Write plasma state update report
    !
    !----------------------------------------------------------------------

    CALL ps_init_tag

    iout = SWIM_err_out_file

    !  check for whether to report changes or non-changes...

    ireverse=0
    if(present(reverse)) then
       if(reverse) ireverse=1
    endif

    isect=2
    if(present(section_control)) then
       isect=max(1,min(3,section_control))
    endif
    !  choice of state object...

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    !  option for restriction to subset of component data members...

    if(present(cclist)) then
       loc_cclist = cclist
    else
       loc_cclist = ps_cc_all
    endif

    !  check partial-state-update (subset) controls

    ierr = 0
    do ii=1,ps_ccount
       if((loc_cclist(ii).lt.0).or.(loc_cclist(ii).gt.1)) then
          if(ierr.eq.0) &
               CALL SWIM_error('cclist_check','PS_UPDATE_REPORT')
          ierr=ierr + 1
          write(iout,*) ' cclist(',ii,')=',cclist(ii),'; should be 0 or 1.'
          write(iout,*) ' index refers to component: ',trim(ps_cnames(ii))
       endif
    enddo
    if(ierr.gt.0) then
       ierr=1
       return
    endif

    !  check output file...

    ilun = iout
    if(present(report_filename)) then
       call find_io_unit(ilun)
       open(unit=ilun,file=report_filename,status='unknown',iostat=ierr)
       if(ierr.ne.0) then
          write(iout,*) ' ?ps_update_report: report file open failure: '// &
               trim(report_filename)
          return
       endif
    endif

    !  generate the report (new_hash_codes(...) are updated).

    call ps_update_report_exec(ss,loc_cclist,isect,ireverse,ilun,ierr)

    if(present(report_filename)) close(unit=ilun)

  end SUBROUTINE ps_update_report

  !==========================================================================
  SUBROUTINE ps_write_update_file(filename, ierr, state, hash_update, &
       cclist, cclist_errchk, iwarn)
   
    !-------------------------------------------------------------------
    !
    ! PS_WRITE_UPDATE_FILE
    !
    ! This subroutine updates the internal state representation and
    ! writes a file containing *updates* to the plasma state
    !    This consists of quantities whose hash codes have changed since
    ! the last time the state hash codes were updated-- usually when the
    ! full state was last read or written.
    !
    !-------------------------------------------------------------------
    
    !----------------------------------------------------------------------
    !
    !   Declare local variables
    !
    !----------------------------------------------------------------------

    CHARACTER*(*), intent(in) :: filename   ! name of update file to be written
    
    INTEGER, INTENT(out) :: ierr  ! exit status code (0=OK)

    !--------------------------
    !  optional arguments:

    TYPE (plasma_state), optional, target :: state

    !  write file data from THIS state, if argument is present
    !  otherwise write from "ps" state.

    LOGICAL, intent(in), optional :: hash_update
    !  (default: TRUE) flag to signal update of hash codes after file is
    !  written -- in which case an immediate subsequent call to this routine
    !  would find nothing to write.

    INTEGER, intent(in), optional :: cclist(ps_ccount)
    !  (default: write changed items from all components)
    !  set cclist(ps_cnum_<component_name>) to 0 to suppress writes on
    !  a per component basis.  See methods ps_cclist_add, ps_cclist_remove
    !  to generate cclist control arrays.

    LOGICAL, intent(in), optional :: cclist_errchk ! if .FALSE. do not set
    !  ierr if iwarn>0; .TRUE. is default.

    INTEGER, intent(out), optional :: iwarn  ! number of items not written
    !  due to cclist control settings

    !----------------------------------------------------------------------

    TYPE (plasma_state), pointer :: ss
    logical :: ihash_update,ilcheck

    integer :: loc_cclist(ps_ccount)
    integer :: jwarn,ii,iout

    !----------------------------------------------------------------------
    !
    !   Write plasma state update file
    !
    !----------------------------------------------------------------------

    CALL ps_init_tag

    iout = SWIM_err_out_file

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    ihash_update = .TRUE.
    if(present(hash_update)) ihash_update = hash_update

    ilcheck=.TRUE.
    if(present(cclist_errchk)) ilcheck = cclist_errchk

    if(present(cclist)) then
       loc_cclist = cclist
    else
       loc_cclist = ps_cc_all
    endif

    !  check partial-state-copy controls

    ierr = 0
    do ii=1,ps_ccount
       if((loc_cclist(ii).lt.0).or.(loc_cclist(ii).gt.1)) then
          if(ierr.eq.0) &
               CALL SWIM_error('cclist_check','PS_WRITE_UPDATE_FILE')
          ierr=ierr + 1
          write(iout,*) ' cclist(',ii,')=',cclist(ii),'; should be 0 or 1.'
          write(iout,*) ' index refers to component: ',trim(ps_cnames(ii))
       endif
    enddo
    if(ierr.gt.0) then
       ierr=1
       return
    endif

    CALL ps_update_file_write(filename,ss,loc_cclist,jwarn,ierr)

    if(present(iwarn)) iwarn = jwarn

    IF(ierr.NE.0) THEN
       CALL SWIM_error('ps_update_file_write','PS_WRITE_UPDATE_FILE')
       write(iout,*) 'failed to write updates to file: ',trim(filename)
       return
    ELSE IF(ilcheck.AND.(jwarn.GT.0)) THEN
       CALL SWIM_error('ps_update_file_write','PS_WRITE_UPDATE_FILE')
       write(iout,*) 'non-cclist compliant updated elements detected, '// &
            'file is: '//trim(filename)
       write(iout,*) '(plasma state contains updated elements not in the '// &
            'optional cclist component list)'
       ierr = 1
    ENDIF

    if(ihash_update) ss%hash_codes = ss%new_hash_codes

  END SUBROUTINE PS_WRITE_UPDATE_FILE

  !==========================================================================
  SUBROUTINE ps_read_update_file(filename, ierr, state, update_complete)
   
    !-------------------------------------------------------------------
    !
    ! PS_READ_UPDATE_FILE
    !
    ! This subroutine reads state updates from the specified file.
    !
    !-------------------------------------------------------------------
    
    !----------------------------------------------------------------------
    !
    !   Declare local variables
    !
    !----------------------------------------------------------------------

    CHARACTER*(*), intent(in) :: filename     ! name of file to read
    
    INTEGER, INTENT(out) :: ierr  ! exit status code (0=OK)

    !--------------------------
    !  optional arguments:

    TYPE (plasma_state), optional, target :: state

    !  read file data into THIS state, if argument is present
    !  otherwise read into "ps" state.

    LOGICAL, intent(in), optional :: update_complete
    !  (default: TRUE) flag to update interpolation data and hash codes
    !  if a series of updates are to be read, set this to .FALSE. on all
    !  but the last of the ps_read_update_file(...) calls.
    !----------------------------------------------------------------------

    TYPE (plasma_state), pointer :: ss
    logical :: iupdate

    !----------------------------------------------------------------------
    !
    !   Read plasma state update file
    !
    !----------------------------------------------------------------------

    CALL ps_init_tag

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    iupdate = .TRUE.
    if(present(update_complete)) iupdate = update_complete

    CALL ps_update_file_read(filename,ss,ierr)

    IF(ierr.NE.0) THEN
       CALL SWIM_error('state_file_read','PS_READ_UPDATE_FILE')
       write(SWIM_err_out_file,*) 'failed to read updates from file: ',trim(filename)
       return
    ENDIF

    IF(iupdate) THEN
       CALL ps_state_memory_update(ierr, state=ss)
    ENDIF

  END SUBROUTINE PS_READ_UPDATE_FILE

  !==========================================================================
  SUBROUTINE ps_state_memory_update(ierr, state, eqcheck, eqcheck_RZ)
   
    !-------------------------------------------------------------------
    !
    ! PS_STATE_MEMORY_UPDATE
    !
    ! This subroutine updates the internal state representation but does
    ! NOT write a state file-- better performance when a state update is
    ! required for cooperation of components that are co-resident in memory
    ! e.g. within a single process.
    !
    !-------------------------------------------------------------------
    
    !----------------------------------------------------------------------
    !
    !   Declare local variables
    !
    !----------------------------------------------------------------------
    
    INTEGER, INTENT(out) :: ierr

    TYPE (plasma_state), optional, target :: state

    !  update data in THIS state, if argument is present
    !  otherwise update "ps" state.

    LOGICAL, intent(in), optional :: eqcheck

    !  .TRUE. (default) to check/update equilibrium representation
    !  .FALSE. to skip this check

    LOGICAL, intent(IN), OPTIONAL :: eqcheck_RZ  ! .FALSE. to suppress [R,Z]
                                                 ! free bdy equilibrium check
                                                 ! (ignored if eqcheck=.FALSE.)
                                                 ! (if eqcheck=.TRUE., the
                                                 ! default is to do the check)

    !----------------------------------------------------------------------

    TYPE (plasma_state), pointer :: ss
    integer :: icheck

    !----------------------------------------------------------------------
    !
    !   Update plasma state
    !
    !----------------------------------------------------------------------

    CALL ps_init_tag

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    icheck = 1
    if(present(eqcheck)) then
       if(.not.eqcheck) icheck = 0
    endif
    if(icheck.eq.1) then
       if(present(eqcheck_RZ)) then
          if(.not.eqcheck_RZ) icheck=2
       endif
    endif

    CALL plasma_state_store(no_file,ss,icheck,ierr)

    IF(ierr.NE.0) THEN
       CALL SWIM_error('state_file_update','PS_STATE_MEMORY_UPDATE')
       write(SWIM_err_out_file,*) 'failed to update state.'
       return
    ENDIF

  END SUBROUTINE PS_STATE_MEMORY_UPDATE

  !==========================================================================
  SUBROUTINE PS_COMMIT_PLASMA_STATE(ierr, filename)

    !-------------------------------------------------------------------
    !
    ! PS_COMMIT_PLASMA_STATE
    !
    ! make sure   ps   state is up-to-date; then copy to state   psp   and
    !   save in file (filename = "prev_state.cdf" by default)
    !
    !-------------------------------------------------------------------
    
    !----------------------------------------------------------------------
    !
    !   Declare local variables
    !
    !----------------------------------------------------------------------

    integer, intent(out) :: ierr  ! exit status code (0=OK)

    CHARACTER*(*), intent(in), optional :: filename  ! full path

    !  if this is set, the committed state is written to (filename); otherwise
    !  the filename is constructed based on the psp state instance (prior
    !  state object).

    integer :: iout

    character*256 sfile
    !----------------------------------------------------------------------
    !
    !   Commit plasma state
    !
    !----------------------------------------------------------------------

    CALL ps_init_tag

    do
       !  (1) copy the state  ps --> psp

       CALL ps_copy_plasma_state(ps, psp, ierr)
       IF(ierr.NE.0) EXIT

       !  (2) write psp (same as ps now) to file

       if(present(filename)) then
          sfile = filename
          ierr = 0
       else
          call ps_default_filepath(psp, sfile, ierr)
       endif
       IF(ierr.NE.0) EXIT

       CALL plasma_state_store(trim(sfile), psp, 1, ierr)
       
       IF(ierr.NE.0) EXIT

       EXIT
    enddo

    IF(ierr.NE.0) THEN
       CALL SWIM_error('state_file_commit','PS_COMMIT_PLASMA_STATE')
       write(SWIM_err_out_file,*) 'failed to write (commit) plasma state.'
       return
    ENDIF

  END SUBROUTINE PS_COMMIT_PLASMA_STATE

  SUBROUTINE PS_DEFAULT_FILEPATH(state, sfile, ierr, component_str)

    !-------------------------------------------------------------------
    !
    ! PS_DEFAULT_FILEPATH
    !
    !  Generate the default full path to a state file, for a given
    !  state object
    !
    !-------------------------------------------------------------------
    
    !----------------------------------------------------------------------
    !
    !   Declare local variables
    !
    !----------------------------------------------------------------------

    type (plasma_state) :: state         ! state object for which file path
    !                                      is desired...

    character*(*), intent(out) :: sfile  ! file path returned

    integer, intent(out) :: ierr  ! exit status code (0=OK)

    character*(*), intent(in), OPTIONAL :: component_str  ! component string
    !  if this argument is present, the filename is extended by the
    !  string '_'//trim(component_str).  This is typically used to form
    !  a component update file for the state.  The argument generally
    !  refers to a component providing the data update, but is treated
    !  here simply as a character string.

    !  EXAMPLES: (if state_path module variable has value ' ')

    !    CALL ps_default_filepath(ps, sfile, ierr) 
    !                      -> sfile = "cur_state.cdf"

    !    CALL ps_default_filepath(psp, sfile, ierr) 
    !                      -> sfile = "prev_state.cdf"

    !    CALL ps_default_filepath(ps, sfile, ierr, component_str="LH") 
    !                      -> sfile = "cur_state_LH.cdf"

    !    CALL ps_default_filepath(ps, sfile, ierr, component_str="LH_update") 
    !                      -> sfile = "cur_state_LH_update.cdf"

    !  EXAMPLE: (if state_path module variable has value '/u/myname')

    !    CALL ps_default_filepath(ps, sfile, ierr) 
    !                      -> sfile = "/u/myname/cur_state.cdf"
    !----------------------------------------------------------------------

    CALL ps_init_tag
    call chk_ini(state)

    ierr = 0

    if(present(component_str)) then
       call ps_filename(state%chtag, state_path, sfile, component_str)
    else
       call ps_filename(state%chtag, state_path, sfile, ' ')
    endif

  end SUBROUTINE PS_DEFAULT_FILEPATH

  SUBROUTINE PS_INIT_USER_STATE(state, tagname, ierr)

    !  Initialize a state object provided by the user.

    !  This should only be called once per object; attempt to re-initialize
    !  an already-initialized state object is considered an error.

    !  State objects contained in plasma_state_mod are not to be initialized
    !  with this subroutine.

    TYPE (plasma_state) :: state          ! state object to initialize...
    character*(*), intent(in) :: tagname  ! short tag name for state
    !  converted to lowercase; only alphanumeric characters and "_" allowed;
    !  first character should not be a digit.

    integer, intent(out) :: ierr          ! status code on exit (0=OK)

    !-------------------------------------------------------------------
    logical :: init_flag
    integer :: iout
    !-------------------------------------------------------------------

    CALL ps_init_tag

    iout = Swim_err_out_file

    init_flag = state%itag .NE. ps_unInit

    if(init_flag) then
       write(iout,*) ' ?error in PS_INIT_USER_STATE: state "',trim(tagname), &
            '" already initialized.'
       ierr= 999
       return
    endif

    CALL ps_register_state(state,tagname,iout,ierr)
    if(ierr.ne.0) then
       write(iout,*) ' ?error in PS_INIT_USER_STATE'
       return
    endif

  END SUBROUTINE PS_INIT_USER_STATE

  SUBROUTINE PS_FREE_USER_STATE(state, ierr)

    !  Free memory; de-initialize a state object provided by the user.

    !  If the state was never initialized in the first place, an error 
    !  is reported.

    !  State objects contained in plasma_state_mod are not to be do-initialized
    !  with this subroutine.

    TYPE (plasma_state) :: state          ! state object to initialize...

    integer, intent(out) :: ierr          ! status code on exit (0=OK)

    !-------------------------------------------------------------------
    logical :: init_flag
    integer :: iout
    !-------------------------------------------------------------------

    CALL ps_init_tag

    iout = Swim_err_out_file

    init_flag = state%itag .NE. ps_unInit

    if(.NOT.init_flag) then
       write(iout,*) ' ?error in PS_FREE_USER_STATE: passed state object'// &
            ' was never initialized.'
       ierr= 999
       return
    endif

    CALL plasma_state_erase(state,ierr)
    if(ierr.ne.0) then
       write(iout,*) ' ?error calling plasma_state_erase in PS_FREE_USER_STATE'
       return
    endif

    CALL ps_deregister_state(state,iout,ierr)
    if(ierr.ne.0) then
       write(iout,*) ' ?error calling ps_deregister_state in PS_FREE_USER_STATE'
       return
    endif

  END SUBROUTINE PS_FREE_USER_STATE

  SUBROUTINE PS_MERGE_PLASMA_STATE(w1,state1,state2,ierr, cclist, &
       new_state,icheck,eqcheck,eqcheck_RZ, save_timestep)

    !  Form merger of two states.  Conceptually:
    !    if(present(new_state))   new_state = w1*state1 + (1-w1)*state2
    !    if(.not.present(new_state)) state2 = w1*state1 + (1-w1)*state2

    !  Array dimension data is not copied; the target state can receive
    !  partial data from the input states by having only a subset of 
    !  dimensions defined and arrays allocated.  The merging code
    !  checks the allocation status of source and target state arrays.

    !  Use PS_COPY_DIMS(...) to copy array dimension sets (only) between 
    !  state objects.

    !  the optional control cclist allows selection of components for
    !  which grids and profiles should NOT be merged:

    !     cclist(ps_cnum_<component>) = 1 to merge this component's data
    !     cclist(ps_cnum_<component>) = 0 not to merge

    !  the user can build a cclist, refering to components by name, using
    !  the ps_cclist_add and ps_cclist_remove subroutines...

    !  Weighted average applies to state_data section and state_profiles
    !  floating point quantities only.  Non-floating-point values and 
    !  initialization section element values (scalar or arrays allocated
    !  in both states) are taken from state1.

    !  All quantities which are allocated in both state1 and state2 must
    !  be conformal in shape; non-zero array dimensions MUST match.

    !  If (new_state) is present, dimensions of allocated arrays must 
    !  match dimensions of corresponding allocated arrays in the input
    !  states.  However, if (new_state) contains allocated arrays that
    !  are absent in the input states, this is no error: such arrays
    !  in (new_state) are left untouched.
    
    !  Allocated grids must match precisely.

    !  For non-floating point quantities there is a check option:
    !     icheck = ps_ignore -- just use the state2 values
    !     icheck = ps_warn   -- print message for each quantity where 
    !                           there is a value change btw state1 and state2
    !     icheck = ps_error  -- report error for each quantity where there
    !                           is a value change; set error code (ierr)

    REAL(KIND=rspec), INTENT(in) :: w1 ! weight for state1 (normally 0<w1<1)
    TYPE (plasma_state) :: state1  ! input state #1 (input weight w1)
    TYPE (plasma_state), target :: state2  ! input state #2 
    !                                       (input weight (1-w1));
    !  if(.not.present(new_state)) state2 = w1*state1 + (1-w1)*state2 on output

    INTEGER, INTENT(out) :: ierr   ! status code, 0=OK

    INTEGER, INTENT(IN), OPTIONAL :: cclist(ps_ccount)  ! component list

    TYPE (plasma_state), OPTIONAL, target :: new_state ! (separate) state out
    !  = w1*state1 + (1-w1)*state2

    INTEGER, INTENT(IN), OPTIONAL :: icheck    ! check code (default ps_error)
    !          for non-floating-point quantities
    !  ps_ignore (0) -- ignore
    !  ps_warn   (1) -- warn about value differences  **default**
    !  ps_error  (2) -- consider value differences to be errors

    LOGICAL, intent(in), optional :: eqcheck
    !  .TRUE. (default) to check/update equilibrium representation
    !  .FALSE. to skip this check

    LOGICAL, intent(IN), OPTIONAL :: eqcheck_RZ  ! .FALSE. to suppress [R,Z]
                                                 ! free bdy equilibrium check
                                                 ! (ignored if eqcheck=.FALSE.)
                                                 ! (if eqcheck=.TRUE., the
                                                 ! default is to do the check)

    LOGICAL, INTENT(IN), OPTIONAL :: save_timestep  ! .TRUE. to save time step
    !  (default: .FALSE.) -- if T, target state elements t0 & t1 are preserved.

    !----------------------------------
    !  even with icheck=ps_warn or ps_ignore, allocated arrays of state1 and
    !  state2 MUST be shape-conformal (i.e. all array dimensions must match).
    !
    !  if an array element is allocated in one of the two input states, this
    !  is not an error; the array in the output state is copied from the 
    !  allocated copy in the input
    !----------------------------------

    integer :: ii,ichk,iout,jcheck
    TYPE (plasma_state), pointer :: ss
    REAL(KIND=rspec) :: zt0,zt1
    integer :: loc_cclist(ps_ccount)

    !----------------------------------

    CALL ps_init_tag

    call chk_ini(state1)
    call chk_ini(state2)
    if(present(new_state)) call chk_ini(new_state)

    ierr = 0
    iout = SWIM_err_out_file

    if(present(cclist)) then
       loc_cclist = cclist
    else
       loc_cclist = ps_cc_all
    endif

    !  check partial-state-copy controls

    do ii=1,ps_ccount
       if((loc_cclist(ii).lt.0).or.(loc_cclist(ii).gt.1)) then
          if(ierr.eq.0) &
               CALL SWIM_error('plasma_state_merge','PS_MERGE_PLASMA_STATE')
          ierr=ierr + 1
          write(iout,*) ' cclist(',ii,')=',cclist(ii),'; should be 0 or 1.'
          write(iout,*) ' index refers to component: ',trim(ps_cnames(ii))
       endif
    enddo
    if(ierr.gt.0) then
       ierr=1
       return
    endif

    ichk = ps_warn
    if(present(icheck)) ichk = icheck

    jcheck=1
    if(present(eqcheck)) then
       if(.not.eqcheck) jcheck=0
    endif
    if(jcheck.eq.1) then
       if(present(eqcheck_RZ)) then
          if(.not.eqcheck_RZ) jcheck=2
       endif
    endif

    call plasma_state_empty_alloc(state1,ierr)
    if(ierr.ne.0) then
       write(iout,*) ' ?plasma_state_mod (ps_merge_plasma_state):'
       write(iout,*) '  (state1) incremental allocation failed.'
       ierr=1
       return
    endif

    call plasma_state_empty_alloc(state2,ierr)
    if(ierr.ne.0) then
       write(iout,*) ' ?plasma_state_mod (ps_merge_plasma_state):'
       write(iout,*) '  (state2) incremental allocation failed.'
       ierr=1
       return
    endif

    ss => state2
    if(present(new_state)) then
       ss => new_state
       call plasma_state_empty_alloc(ss,ierr)
       if(ierr.ne.0) then
          write(iout,*) ' ?plasma_state_mod (ps_merge_plasma_state):'
          write(iout,*) '  (new_state) incremental allocation failed.'
          ierr=1
          return
       endif
    endif

    zt0 = ss%t0
    zt1 = ss%t1

    call plasma_state_merge(loc_cclist,w1,state1,state2,ss,ichk,ierr)
    IF(ierr.NE.0) THEN
       CALL SWIM_error('plasma_state_merge','PS_MERGE_PLASMA_STATE')
       write(iout,*) 'plasma_state_merge failed!'
       return
    ENDIF

    CALL plasma_state_store(no_file,ss,jcheck,ierr)
    IF(ierr.NE.0) THEN
       CALL SWIM_error('plasma_state_store','PS_MERGE_PLASMA_STATE')
       write(iout,*) 'plasma_state_store failed!'
       return
    ENDIF

    if(present(save_timestep)) then
       if(present(save_timestep)) then
          ss%t0=zt0
          ss%t1=zt1
       endif
    endif

  END SUBROUTINE PS_MERGE_PLASMA_STATE

  !---------------------------------------------------------

  SUBROUTINE PS_COPY_DIMS(from_state, to_state, init, ierr, cclist, &
       icopy_all_lists, iwant_1deq)

    !  Copy array dimensions (only) from one state to another state.
    !  No allocation is done; the caller can zero out specific dimensions
    !  after this call, to restrict the data contents of a state object
    !  under construction.

    !  default is to copy dimensions from ALL components

    !  the optional control cclist allows selection of components for
    !  which profile and grid dimensions should NOT be copied:

    !     cclist(ps_cnum_<component>) = 1 to copy this component's dimensions
    !     cclist(ps_cnum_<component>) = 0 not to copy

    !  Normally, machine description list dimensions are copied, regardless
    !  of cclist(...) settings, but, use "icopy_all_lists" to change this
    !  behavior for list dimensions.  Note that species lists are shared
    !  accross components and their dimensions are ALWAYS copied regardless
    !  of "cclist" and "icopy_all_lists" option settings.

    !  set init=1 to cause contents of (to_state) to be erased prior to
    !  copy of dimensions; otherwise, the dimensions of (from_state) are
    !  added to (to_state) which may already have previously defined contents.

    !  if init is NOT 1:  dimensions with non-zero values in (from_state) and
    !  (to_state) on entry to this routine must have matching values.

    TYPE (plasma_state) :: from_state   ! copy FROM this state
    TYPE (plasma_state) :: to_state     ! copy TO this state

    INTEGER, INTENT(IN) :: init         ! =1 to erase (to_state) prior to copy

    INTEGER, INTENT(OUT) :: ierr        ! exit status code (0=OK)

    INTEGER, INTENT(IN), OPTIONAL :: cclist(ps_ccount)  ! component list

    INTEGER, INTENT(IN), OPTIONAL :: icopy_all_lists  ! list copy option
    !  =1 (default): copy all list dimensions, even when cclist(<component>)=0
    !  =0: copy species list dimensions only (this copy cannot be suppressed).
    !  The setting of this switch only affects treatment of data belonging to
    !  components for which cclist(<component>) is zero.

    INTEGER, INTENT(IN), OPTIONAL :: iwant_1deq  ! =1 to copy (nrho_eq)
    !  dimension, even if cclist(ps_cnum_EQ).eq.0
    !  (default: iwant_1deq = 0)

    !-----------------------------------------
    integer :: ii,itot,iout,idims_only,idata_only,iwant_1d,icopy_lists
    integer :: loc_cclist(ps_ccount)
    !-----------------------------------------

    CALL ps_init_tag

    call chk_ini(from_state)
    call chk_ini(to_state)

    ierr = 0
    iout = SWIM_err_out_file

    idims_only = 1
    idata_only = 0

    if(present(cclist)) then
       loc_cclist = cclist
    else
       loc_cclist = ps_cc_all
    endif

    icopy_lists=1
    if(present(icopy_all_lists)) then
       icopy_lists=max(0,min(1,icopy_all_lists))
    endif

    iwant_1d=0
    if(present(iwant_1deq)) then
       if(iwant_1deq.ne.0) iwant_1d=1
    endif

    CALL ps_init_tag

    !  erase any prior contents of target state

    if(init.eq.1) then
       call plasma_state_erase(to_state,ierr)
       if(ierr.ne.0) then
          CALL SWIM_error('plasma_state_erase','PS_COPY_DIMS')
          write(iout,*) '   preliminary erase of target state failed.'
          ierr=1
          return
       endif
    endif

    !  check partial-state-copy controls

    itot = 0
    do ii=1,ps_ccount
       if((loc_cclist(ii).lt.0).or.(loc_cclist(ii).gt.1)) then
          if(ierr.eq.0) &
               CALL SWIM_error('cclist check','PS_COPY_DIMS')
          ierr=ierr + 1
          write(iout,*) ' cclist(',ii,')=',cclist(ii),'; should be 0 or 1.'
          write(iout,*) ' index refers to component: ',trim(ps_cnames(ii))
       endif
       if(loc_cclist(ii).eq.1) itot=itot+1
    enddo
    if(ierr.gt.0) then
       ierr=1
       return
    endif

    ! *** full or partial copy of state dimensions ***

    CALL plasma_state_copy(loc_cclist, from_state, to_state, &
         idims_only, idata_only, icopy_lists, iwant_1d, ierr)
    IF(ierr.NE.0) THEN
       CALL SWIM_error('plasma_state_copy','PS_COPY_DIMS')
       write(iout,*) 'copy of state dimensions!'
       return
    ENDIF

  END SUBROUTINE PS_COPY_DIMS

   !---------------------------------------------------------

  SUBROUTINE PS_COPY_PLASMA_STATE(from_state, to_state, ierr, &
       cclist, inodims, icopy_all_lists, iwant_1deq)

    !  Copy a state, including underlying interpolation data
    !  There is no file I/O, this is a memory to memory copy

    !  All of the data is physically copied; memory is not shared between
    !  states; modification to values of state A never effects state B.

    !  default is to copy ALL components

    !  unless optional argument inodims=1 is set, the to_state is erased
    !  prior to copy; to_state ends up containing ONLY the copied data

    !  if inodims=1 is set, to_state must have its array dimensions already
    !  defined; its contents are NOT erased.  Its non-zero dimensions MUST
    !  match the non-zero dimensions of from_state, or an error will be
    !  reported when the copy is attempted.

    !  (inodims=1 & cclist settings allow a portion of a state to be copied
    !  into another, established state).

    !  the optional control cclist allows selection of components for
    !  which grids and profiles should NOT be copied:

    !     cclist(ps_cnum_<component>) = 1 to copy this component's profiles
    !     cclist(ps_cnum_<component>) = 0 not to copy

    !  Normally, machine description list dimensions are copied, regardless
    !  of cclist(...) settings, but, use "icopy_all_lists" to change this
    !  behavior for list dimensions.  Note that species lists are shared
    !  accross components and their dimensions are ALWAYS copied regardless
    !  of "cclist" and "icopy_all_lists" option settings.

    !  the user can build a cclist, refering to components by name, using
    !  the ps_cclist_add and ps_cclist_remove subroutines...

    TYPE (plasma_state) :: from_state   ! copy FROM this state
    TYPE (plasma_state) :: to_state     ! copy TO this state

    INTEGER, INTENT(OUT) :: ierr        ! exit status code (0=OK)

    INTEGER, INTENT(IN), OPTIONAL :: cclist(ps_ccount)  ! component list

    INTEGER, INTENT(IN), OPTIONAL :: inodims     ! =1 to copy data ONLY
    ! presume that to_state dimensions are already set; non-zero dimensions
    ! of from_state and to_state MUST match!  Default: inodims=0

    INTEGER, INTENT(IN), OPTIONAL :: icopy_all_lists  ! list copy option
    !  =1 (default): copy all lists, even when cclist(<component>)=0
    !  =0: copy species lists only (this copy cannot be suppressed).
    !  The setting of this switch only affects treatment of data belonging to
    !  components for which cclist(<component>) is zero.

    INTEGER, INTENT(IN), OPTIONAL :: iwant_1deq  ! =1 to copy (nrho_eq)
    !  dimension, even if cclist(ps_cnum_EQ).eq.0
    !  (default: iwant_1deq = 0)

    !-----------------------------------------
    integer :: ii,itot,iout,idims_only,idata_only,iwant_1d,icopy_lists
    integer :: loc_cclist(ps_ccount)
    !-----------------------------------------

    ierr = 0
    iout = SWIM_err_out_file

    idims_only = 0

    if(present(cclist)) then
       loc_cclist = cclist
    else
       loc_cclist = ps_cc_all
    endif

    idata_only = 0
    if(present(inodims)) then
       if(inodims.ne.0) idata_only=1
    endif

    icopy_lists=1
    if(present(icopy_all_lists)) then
       icopy_lists=max(0,min(1,icopy_all_lists))
    endif

    iwant_1d=0
    if(present(iwant_1deq)) then
       if(iwant_1deq.ne.0) iwant_1d=1
    endif

    CALL ps_init_tag

    !  erase any prior contents of target state -- if dimensions as well
    !  as data are being copied...

    call chk_ini(from_state)
    call chk_ini(to_state)

    if(idata_only.eq.0) then
       call plasma_state_erase(to_state,ierr)
       if(ierr.ne.0) then
          CALL SWIM_error('plasma_state_erase','PS_COPY_PLASMA_STATE')
          write(iout,*) '   preliminary erase of target state failed.'
          ierr=1
          return
       endif
    endif

    ! real data will be copied; check/store from_state interpolation data

    call plasma_state_store(no_file,from_state,1,ierr)
    if(ierr.ne.0) then
       CALL SWIM_error('plasma_state_store','PS_COPY_PLASMA_STATE')
       ierr=1
       return
    endif

    !  check partial-state-copy controls

    itot = 0
    do ii=1,ps_ccount
       if((loc_cclist(ii).lt.0).or.(loc_cclist(ii).gt.1)) then
          if(ierr.eq.0) &
               CALL SWIM_error('plasma_state_copy','PS_COPY_PLASMA_STATE')
          ierr=ierr + 1
          write(iout,*) ' cclist(',ii,')=',cclist(ii),'; should be 0 or 1.'
          write(iout,*) ' index refers to component: ',trim(ps_cnames(ii))
       endif
       if(loc_cclist(ii).eq.1) itot=itot+1
    enddo
    if(ierr.gt.0) then
       ierr=1
       return
    endif

    if((itot.eq.ps_ccount).and.(idata_only.eq.0)) then
       ! full copy
       ! copy xplasma too

       call ps_copy_xpl(from_state%iobj, to_state%iobj, iout, ierr)
    endif

    if(ierr.ne.0) then
       CALL SWIM_error('ps_copy_xpl','PS_COPY_PLASMA_STATE')
       ierr=1
       return
    endif

    ! *** full or partial copy of state object ***

    CALL plasma_state_copy(loc_cclist, from_state, to_state, &
         idims_only, idata_only, icopy_lists, iwant_1d, ierr)
    IF(ierr.NE.0) THEN
       CALL SWIM_error('plasma_state_copy','PS_COPY_PLASMA_STATE')
       write(iout,*) 'copy of state failed!'
       return
    ENDIF

    ! final post-processing of newly-copied-to state

    if((itot.eq.ps_ccount).and.(idata_only.eq.0)) then
       ! be sure zonwt IDs are set...
       CALL plasma_state_zonwt_update(to_state, ierr)
       IF(ierr.NE.0) THEN
          CALL SWIM_error('plasma_state_zonwt_update','PS_COPY_PLASMA_STATE')
          write(iout,*) 'plasma_state_zonwt_update failed!'
          return
       ENDIF

    else
       ! partial copy: fill in target xplasma
       CALL plasma_state_store(no_file,to_state,1,ierr)
       IF(ierr.NE.0) THEN
          CALL SWIM_error('plasma_state_store','PS_COPY_PLASMA_STATE')
          write(iout,*) 'plasma_state_store failed!'
          return
       ENDIF
    endif

  END SUBROUTINE PS_COPY_PLASMA_STATE

  !==========================================================================
  SUBROUTINE PS_UPDATE_HASHCODES(ierr, state, cclist, &
       mdescr_lock, sconfig_lock, siminit_lock)

    !-------------------------------------------------------------------
    !
    ! PS_UPDATE_HASHCODES
    !
    ! Update state hash codes.  Set locks on initialization
    ! sections of the state if optional arguments are set.
    !
    !-------------------------------------------------------------------
    
    !----------------------------------------------------------------------
    !
    !   Declare argument variables
    !
    !----------------------------------------------------------------------

    INTEGER, intent(out) :: ierr  ! exit status code (0=OK)

    TYPE (plasma_state), optional, target :: state
    !  update THIS state, if argument is present
    !  otherwise update "ps" state.

    INTEGER, INTENT(IN), OPTIONAL :: cclist(ps_ccount)  ! component list
    !  By default, all hash codes are updated.

    !  The optional control cclist allows selection of a subset of 
    !  components for which hash codes are to be updated

    !     cclist(ps_cnum_<component>) = 1 to update all component's hash codes

    !     cclist(ps_cnum_<component>) = 2 to update component's grid and
    !                                     profile hash codes only

    !     cclist(ps_cnum_<component>) = 0 not to update any hash codes for
    !                                     this component

    !  the user can build a cclist, refering to components by name, using
    !  the ps_cclist_add and ps_cclist_remove subroutines...

    !  NOTE: if any of the optional logicals are set, cclist is overridden
    !  and all hashcodes are updated...

    LOGICAL, intent(in), OPTIONAL :: mdescr_lock   ! TRUE to lock mDescr
    LOGICAL, intent(in), OPTIONAL :: sconfig_lock  ! TRUE to lock sConfig
    LOGICAL, intent(in), OPTIONAL :: siminit_lock  ! TRUE to lock simInit

    !----------------------------------------------------------------------
    ! Declare local variables

    INTEGER :: iout
    TYPE (plasma_state), pointer :: ss
    integer :: loc_cclist(ps_ccount)

    !----------------------------------------------------------------------

    ierr = 0
    iout = SWIM_err_out_file

    if(present(cclist)) then
       loc_cclist = cclist
       if(present(mdescr_lock)) then
          if(mdescr_lock) loc_cclist = ps_chkAll
       endif
       if(present(sconfig_lock)) then
          if(sconfig_lock) loc_cclist = ps_chkAll
       endif
       if(present(siminit_lock)) then
          if(siminit_lock) loc_cclist = ps_chkAll
       endif
    else
       loc_cclist = ps_chkAll
    endif

    CALL ps_init_tag

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    CALL plasma_state_hash_code(ss,loc_cclist,ierr)
    IF(ierr.NE.0) THEN
       WRITE(iout,*) 'plasma_state_mod: PS_UPDATE_HASHCODES: call failed.'
       RETURN
    ENDIF

    ss%hash_codes = ss%new_hash_codes

    IF(present(mdescr_lock)) THEN
       IF(mdescr_lock) ss%lock_machine_descr = ps_locked
    ENDIF

    IF(present(sconfig_lock)) THEN
       IF(sconfig_lock) ss%lock_shot_config = ps_locked
    ENDIF

    IF(present(siminit_lock)) THEN
       IF(siminit_lock) ss%lock_sim_init = ps_locked
    ENDIF

  END SUBROUTINE PS_UPDATE_HASHCODES

  SUBROUTINE PS_SAVE_HASH_CODES(iwarn, state)

    !-------------------------------------------------------------------
    !
    ! PS_SAVE_HASH_CODES
    !
    ! Save a copy of the state's current hash codes in the state object's
    ! saved_hash_codes(...) array element.
    !
    ! a warning status is set, if this overwrites a previously saved copy.
    ! SEE ALSO: PS_RESTORE_HASH_CODES
    !
    !-------------------------------------------------------------------
   
    !----------------------------------------------------------------------
    !
    !   Declare argument variables
    !
    !----------------------------------------------------------------------

    integer, intent(out) :: iwarn  ! warning status code (0=OK)

    TYPE (plasma_state), optional, target :: state

    !  use THIS state, if argument is present
    !  otherwise use the "ps" state.

    !----------------------------------------------------------------------
    ! Declare local variables

    INTEGER :: iout
    TYPE (plasma_state), pointer :: ss

    !----------------------------------------------------------------------

    iout = SWIM_err_out_file

    CALL ps_init_tag

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    iwarn = 0
    if(ss%saved_hash_codes(1).ne.ps_unInit) then
       iwarn=1
       write(iout,*) ' %PS_SAVE_HASH_CODES: prior saved_hashcodes array contents overwritten.'
    endif

    ss%saved_hash_codes = ss%hash_codes

  END SUBROUTINE PS_SAVE_HASH_CODES

  SUBROUTINE PS_RESTORE_HASH_CODES(ierr, state)

    !-------------------------------------------------------------------
    !
    ! PS_RESTORE_HASH_CODES
    !
    ! restore state's hash codes from a previously saved copy in the 
    ! state object's saved_hash_codes(...) array; reset saved_hash_codes(...)
    ! array to un-initialized state.
    !
    ! an error status is set, and the state's hash codes remain unchanged,
    ! if the code detects that the saved_hash_codes(...) array was never
    ! loaded by a prior PS_SAVE_HASH_CODES call.
    ! 
    ! SEE ALSO: PS_SAVE_HASH_CODES
    !
    !-------------------------------------------------------------------
   
    !----------------------------------------------------------------------
    !
    !   Declare argument variables
    !
    !----------------------------------------------------------------------

    integer, intent(out) :: ierr   ! error status code (0=OK)

    TYPE (plasma_state), optional, target :: state

    !  use THIS state, if argument is present
    !  otherwise use the "ps" state.

    !----------------------------------------------------------------------
    ! Declare local variables

    INTEGER :: iout
    TYPE (plasma_state), pointer :: ss

    !----------------------------------------------------------------------

    iout = SWIM_err_out_file

    CALL ps_init_tag

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    ierr = 0
    if(ss%saved_hash_codes(1).eq.ps_unInit) then
       ierr=1
       write(iout,*) ' ?PS_RESTORE_HASH_CODES: no prior saved_hashcodes found.'
       return
    endif

    ss%hash_codes = ss%saved_hash_codes
    ss%saved_hash_codes = ps_unInit

  END SUBROUTINE PS_RESTORE_HASH_CODES

  !==========================================================================
  SUBROUTINE PS_MDESCR_WRITE(filename, ierr, state)

    !-------------------------------------------------------------------
    !
    ! PS_MDESCR_WRITE
    !
    ! Write commented machine description namelist from contents of
    ! state file.  This may be useful for creating a template for
    ! hand prepared machine description namelists.
    !
    ! This routine should not be needed for normal operations.
    !-------------------------------------------------------------------
    
    !----------------------------------------------------------------------
    !
    !   Declare argument variables
    !
    !----------------------------------------------------------------------

    character*(*), intent(in) :: filename  ! name of file to write
    ! NOTE: file is opened with status "unknown"-- if a file by this
    ! name already exists, it is overwritten!

    integer, intent(out) :: ierr  ! exit status code (0=OK)

    TYPE (plasma_state), optional, target :: state

    !  write from THIS state, if argument is present
    !  otherwise write from "ps" state.

    !----------------------------------------------------------------------
    ! Declare local variables

    INTEGER :: iout
    TYPE (plasma_state), pointer :: ss

    !----------------------------------------------------------------------

    iout = SWIM_err_out_file

    CALL ps_init_tag

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    CALL ps_mdescr_namelist_write(filename,ss,ierr)

    IF(ierr.NE.0) THEN
       write(iout,*) 'plasma_state_mod: PS_MDESCR_WRITE: an error occurred.'
       return
    ENDIF

  END SUBROUTINE PS_MDESCR_WRITE

  !==========================================================================
  SUBROUTINE PS_MDESCR_READ(filename, ierr, state, action, g_filename)

    !-------------------------------------------------------------------
    !
    ! PS_MDESCR_READ
    !
    ! Read machine description namelist into (empty) state.
    ! Optionally specify a G-eqdsk file which can override the namelist
    !   Rmin/max, Zmin/max, and limiter contour.
    !
    ! Read this namelist BEFORE reading SCONFIG namelist!
    !-------------------------------------------------------------------
    
    !----------------------------------------------------------------------
    !
    !   Declare argument variables
    !
    !----------------------------------------------------------------------

    character*(*), intent(in) :: filename  ! name of file to read

    integer, intent(out) :: ierr  ! exit status code (0=OK)

    TYPE (plasma_state), optional, target :: state
    !  write from THIS state, if argument is present
    !  otherwise write from "ps" state.

    character*(*), intent(in), optional :: action

    character*(*), intent(in), optional :: g_filename  ! G-eqdsk file name

    !----------------------------------------------------------------------
    ! Declare local variables

    INTEGER :: iout
    TYPE (plasma_state), pointer :: ss
    character*256 :: gfile
    character*4 :: act   ! action, standardized
    logical :: iinit
    !----------------------------------------------------------------------

    iout = SWIM_err_out_file

    CALL ps_init_tag

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    act='ADD'
    if(present(action)) then
       act=action
       call uupper(act)  ! -> uppercase
       if(act.eq.'INIT') act='NEW'
    endif

    if(ss%lock_machine_descr.eq.ps_locked) then
       ierr=999
       write(iout,*) '?plasma_state_mod: PS_MDESCR_READ: the machine description is locked.'
       return
    endif

    if(present(g_filename)) then
       gfile = g_filename
    else
       gfile = ' '
    endif

    iinit = (act.eq.'NEW')
    CALL ps_mdescr_namelist_read(iinit,filename,' ',gfile,ss,ierr)

    IF(ierr.NE.0) THEN
       write(iout,*) '?plasma_state_mod: PS_MDESCR_READ: an error occurred.'
       return
    ENDIF

    CALL ps_mdescr_namelist_check(ss,iout,ierr)
    if(ierr.NE.0) return
    
    CALL ps_update_hashCodes(ierr, state=ss)

  END SUBROUTINE PS_MDESCR_READ

  !==========================================================================
  SUBROUTINE PS_SCONFIG_READ(filename, ierr, state)

    !-------------------------------------------------------------------
    !
    ! PS_SCONFIG_READ
    !
    ! Read shot configuration namelist into (nearly empty) state.
    ! Read this namelist only AFTER reading MDESCR namelist!
    !
    ! When done, complete species lists:  make calls to:
    !    ps_label_species
    !    ps_merge_species_lists
    !    ps_neutral_species
    !
    ! NOTE: state elements state%Z0max, state%Zimp1, and state%Zimp2
    ! affect these last 3 calls:
    !    Z0max: max atomic number of recycling neutral species
    !    Zimp1: lower limit on Z of low-Z impurity for reduced species list
    !    Zimp2: upper limit on Z of high-Z impurity for reduced species list
    ! Defaults are in the state definition: Z0max=2; no lower limit, no
    ! upper limit.  For other choices, caller must reset these values
    ! BEFORE calling PS_SCONFIG_READ.
    !-------------------------------------------------------------------
    
    !----------------------------------------------------------------------
    !
    !   Declare argument variables
    !
    !----------------------------------------------------------------------

    character*(*), intent(in) :: filename  ! name of file to read

    integer, intent(out) :: ierr  ! exit status code (0=OK)

    TYPE (plasma_state), optional, target :: state

    !  read data into THIS state, if argument is present
    !  otherwise read data into "ps" state.

    !----------------------------------------------------------------------
    ! Declare local variables

    INTEGER :: iout
    TYPE (plasma_state), pointer :: ss
    logical :: iinit

    !----------------------------------------------------------------------

    iout = SWIM_err_out_file

    CALL ps_init_tag

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    if(ss%lock_shot_config.eq.ps_locked) then
       ierr=999
       write(iout,*) '?plasma_state_mod: PS_SCONFIG_READ: the shot configuration is locked.'
       return
    endif

    iinit = .FALSE.
    CALL ps_sconfig_namelist_read(iinit,filename,' ',ss,ierr)

    IF(ierr.NE.0) THEN
       write(iout,*) '?plasma_state_mod: PS_SCONFIG_READ: an error occurred.'
       return
    ENDIF

    CALL ps_sconfig_namelist_check(ss,iout,ierr)
    if(ierr.NE.0) return

    ! OK: complete the species lists...

    CALL ps_label_species(ierr, state=ss)
    IF(ierr.NE.0) THEN
       WRITE(iout,*) ' ?ps_sconfig_read: ps_label_species error.'
       RETURN
    ENDIF
    
    CALL ps_merge_species_lists(ierr, state=ss)
    IF(ierr.NE.0) THEN
       WRITE(iout,*) ' ?ps_sconfig_read: ps_merge_species_lists error.'
       RETURN
    ENDIF
    
    CALL ps_neutral_species(ierr, state=ss)
    IF(ierr.NE.0) THEN
       WRITE(iout,*) ' ?ps_sconfig_read: ps_neutral_species error.'
       RETURN
    ENDIF
    
    CALL ps_update_hashCodes(ierr, state=ss)

  END SUBROUTINE PS_SCONFIG_READ

  !==========================================================================
  SUBROUTINE PS_SCONFIG_WRITE(filename, ierr, state)

    !-------------------------------------------------------------------
    !
    ! PS_SCONFIG_WRITE
    !
    ! Write commented shot configuration namelist from contents of
    ! state file.  This may be useful for creating a template for
    ! hand prepared shot configuration namelists.
    !
    ! This routine should not be needed for normal operations.
    !-------------------------------------------------------------------
    
    !----------------------------------------------------------------------
    !
    !   Declare argument variables
    !
    !----------------------------------------------------------------------

    character*(*), intent(in) :: filename  ! name of file to write
    ! NOTE: file is opened with status "unknown"-- if a file by this
    ! name already exists, it is overwritten!

    integer, intent(out) :: ierr  ! exit status code (0=OK)

    TYPE (plasma_state), optional, target :: state

    !  write from THIS state, if argument is present
    !  otherwise write from "ps" state.

    !----------------------------------------------------------------------
    ! Declare local variables

    INTEGER :: iout
    TYPE (plasma_state), pointer :: ss

    !----------------------------------------------------------------------

    iout = SWIM_err_out_file

    CALL ps_init_tag

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    CALL ps_sconfig_namelist_write(filename,ss,ierr)

    IF(ierr.NE.0) THEN
       write(iout,*) 'plasma_state_mod: PS_SCONFIG_WRITE: an error occurred.'
       return
    ENDIF

  END SUBROUTINE PS_SCONFIG_WRITE

  !==========================================================================
  SUBROUTINE PS_ALLOC_PLASMA_STATE(ierr, state)

    !-------------------------------------------------------------------
    !
    ! PS_ALLOC_PLASMA_STATE
    !
    ! incrementally allocate current state arrays
    !   allocate arrays for which non-zero dimensions are defined.
    !   Once allocated, the array size cannot be changed!
    !
    ! Generally used only during initialization of a simulation.
    !
    ! Only the "ps" state can be allocated directly; all other states
    ! must be created initially as copies of the "ps" state.
    !-------------------------------------------------------------------
    
    !----------------------------------------------------------------------
    !
    !   Declare local variables
    !
    !----------------------------------------------------------------------

    integer, intent(out) :: ierr  ! exit status code (0=OK)

    TYPE (plasma_state), optional, target :: state

    !  allocate arrays in THIS state, if argument is present
    !  otherwise allocate "ps" state.

    !----------------------------------------------------------------------

    TYPE (plasma_state), pointer :: ss

    !----------------------------------------------------------------------
    !
    !   Allocate plasma state
    !
    !----------------------------------------------------------------------

    CALL ps_init_tag

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    CALL plasma_state_empty_alloc(ss,ierr)
    IF(ierr.NE.0) THEN
       CALL SWIM_error('state_array_allocation','PS_ALLOC_PLASMA_STATE')
       write(SWIM_err_out_file,*) 'state array allocation failure!'
       return
    ENDIF

  END SUBROUTINE PS_ALLOC_PLASMA_STATE
      

  !
  ! -------------------- elindex -----------------
  ! private function returns the index into elex by atomic number or 0 if not found
  !
  function elindex(z) result(ii)
    integer, intent(in) :: z    ! atomic number
    integer             :: ii   ! index into elex

    integer :: j,kk  ! temp

    ii = 0
    if (z<1 .or. z>max_elements) return     ! z out of range

    kk = element_index(z)
    if (kk>=1 .and. kk<=max_elements) then
       if (elex(kk)%number==z) then         ! already found
          ii = kk
          return
       end if
    end if

    ! search -- should not be necessary
    do j=1, max_elements
       if (elex(j)%number==z) then
          ii = j
          element_index(z) = j
          return
       end if
    end do
  end function elindex

  !
  ! ---------------- standard_amu_rspec ---------------
  ! Return isotopic average atomic weight
  !
  subroutine standard_amu_rspec(z,a,ix) 
    integer,     intent(in)  :: z     ! atomic number (in)
    real(rspec), intent(out) :: a     ! standard atomic mass, amu (out)
    
    integer,optional,intent(out) :: ix   ! index into elex or 0 on error

    integer :: kk 
    
    kk = elindex(z)

    if (present(ix)) ix = kk

    if (kk<1 .or. kk>max_elements) then
       a = 0._rspec
    else
       a = elex(kk)%weight
    end if
  end subroutine standard_amu_rspec
 

  !
  ! -------------------- atom_amu_to_proton -------------------
  ! return the mass of an atom in protons -- account for electrons as if it makes a difference,
  ! the only thing missing is the binding energy which would need to be added to the result
  !
  function atom_amu_to_proton(z,ich,a) result(p)
    integer,    intent(in) :: z    ! atomic number
    integer,    intent(in) :: ich  ! charge state of atom
    real(rspec),intent(in) :: a    ! atomic weight in amu of neutral atom

    real(rspec) :: p               ! mass in protons
    
    if (ignore_electron_AMU) then
       p = max(0._rspec, a/proton_amu)
    else
       p = max(0._rspec, (a-max(0,min(ich,z))*electron_amu)/proton_amu)
    end if

    if (z==1 .and. ich==1) then
       if (abs(p-1._rspec)<2.e-3_rspec) then
          p=1.0_rspec   ! so hydrogen comes out exactly as 1
       end if
    end if
  end function atom_amu_to_proton


  !
  ! --------------------- use_amu -------------------
  ! convert the user requested amu to the one which should be used,
  ! set argument a=0. (or more generally a<z) to get the istopic average amu for the element,
  ! return 0. on an error
  !
  function use_amu(z,a,ix,is) result(u)
    integer,     intent(in) :: z    ! atomic number
    real(rspec), intent(in) :: a    ! atomic weight in amu as requested

    integer,optional,intent(out) :: ix   ! index into elex
    integer,optional,intent(out) :: is   ! index into isox or 0 if using isotopic average

    real(rspec) :: u    ! atomic weight to use
    
    integer :: kk   ! index into elex array
    integer :: ii   ! index into isox array of first isotope of element
    integer :: i    ! isox index
    
    real(rspec) :: adiff      ! difference between user amu and isotope amu
    real(rspec) :: adiff_min  ! minimum adiff encountered so far

    u = 0._rspec    ! default in case of error
    
    if (present(ix)) ix = 0
    if (present(is)) is = 0

    if (a<(z*proton_amu-.1_rspec)) then
       call standard_amu_rspec(z,u,ix)    ! user amu is too small, use isotopic average
    else
       kk = elindex(z)
       if (present(ix)) ix = kk
       if (kk>=1 .and. kk<=max_elements) then
          ii = elex(kk)%isoindex
          if (ii>0) then
             adiff_min = 1.e20_rspec
             do i = ii, max_isotopes
                if (isox(i)%number/=z) exit
                
                adiff = abs(a-isox(i)%weight)
                
                if (adiff<adiff_min) then
                   if (present(is)) is = i

                   u = isox(i)%weight         ! best match so far between a and known isotopes
                   adiff_min = adiff
                end if
             end do
          else
             if (ignore_user_AMU) then
                call standard_amu_rspec(z,u,ix)  ! ignore user AMU, use isotopic average
             else
                u = max(z*proton_amu,a)          ! element does not have known isotopes so use user supplied amu
             end if
          end if
       end if
    end if
  end function use_amu
     

  !==========================================================================
  SUBROUTINE ps_species_convert(iZatom, iZcharge, iAMU, qatom, qcharge, mass, &
       ierr)

    !--------------------------------
    ! D. McCune Dec. 2007
    !    Convert "integer" species specification
    !      {atomic number, ionic charge number, atomic weight in AMU}
    !    to "floating point" representation
    !      {atomic charge (C), ionic charge (C), atomic mass in kg}
    !
    !    If iZatom=-1 provide output quantites for electrons.
    !    If iZatom>3 and ignore_user_AMU=.true. then iAMU is ignored. (RGA Jun2013 change)
    !
    ! ...to be used during initialization of plasma state system at the
    ! start of a simulation...
    !
    !--------------------------------
    
    integer, intent(in) :: iZatom    ! atomic number: 1 for H, 2 for He, ...
    !  special: -1 for electrons

    integer, intent(in) :: iZcharge  ! ionic charge btw 1 and iZatom
    !  ignored for electrons

    integer, intent(in) :: iAMU      ! atomic weight in AMU, nearest integer

    REAL(KIND=rspec), intent(out) :: qatom    ! charge of fully stripped ion
    REAL(KIND=rspec), intent(out) :: qcharge  ! actual charge
    REAL(KIND=rspec), intent(out) :: mass     ! mass in kg

    integer, intent(out) :: ierr     ! status code, 0=OK

    !---------------------------
    integer :: iout,iq,iqc
    REAL(KIND=rspec) a
    !---------------------------

    ierr = 0
    iout = Swim_err_out_file

    qatom   = 0.0_rspec
    qcharge = 0.0_rspec
    mass    = 0.0_rspec

    if(iZatom.eq.-1) then
       qatom   = -ps_xe
       qcharge = -ps_xe
       mass    =  ps_me

    else if((iZatom.eq.0).or.(iZatom.lt.-1)) then
       ierr = 1
       write(iout,*) ' ?ps_species_convert: atomic number iZatom out of range low:', iZatom

    else
       a = use_amu(iZatom, real(iAmu,rspec))

       if (a<0.5_rspec) then 
          ierr = 2
          write(iout,*) ' ?ps_species_convert: atomic number iZatom out of range high:', iZatom

       else
          iq  = iZatom
          iqc = max(0,min(iq,iZcharge))

          qatom   = iq*ps_xe
          qcharge = iqc*ps_xe
          mass    = atom_amu_to_proton(iq,iqc,a)*ps_mp
       endif
    endif
  END SUBROUTINE ps_species_convert


  SUBROUTINE ps_set_beam_species(ierr, state)

    !  for the given state instance (or for instance   ps  if none is
    !  specified:

    !  construct neutral beam fast ion species list, from list of species
    !  injected by each neutral beam.

    !  ss%nbion(1:nbeam) gives the species injected by each neutral beam:
    !     "H" for Hydrogen, "D" for Deuterium, "T" for Tritium,
    !     "He3" for Helium-3, "He4" for Helium-4

    !  DMC Mod May 2010: allow impurity beams; allow trace elements in beams
    !     allowed impurity beams: noble gasses, Ne, Ar, Kr, Xe

    integer, intent(out) :: ierr ! status code: 0 = OK
    
    TYPE (plasma_state), optional, target :: state

    !--------------------------------------------
    TYPE (plasma_state), pointer :: ss
    integer :: ibeam,jbeam,inbs,iout,imatch,iertmp
    logical :: is_h,is_he,is_h2,is_he2
    character*32 :: cb1
    character*32, dimension(:), allocatable :: cblist

    integer, parameter :: nbok=9
    character*32 :: oklist(nbok)

    !--------------------------------------------
    !  currently known beam species-- uppercase labels:

    oklist(1)='H'
    oklist(2)='D'
    oklist(3)='T'
    oklist(4)='HE3'
    oklist(5)='HE4'

    oklist(6)='NE'
    oklist(7)='AR'
    oklist(8)='KR'
    oklist(9)='XE'

    !--------------------------------------------

    ierr = 0
    iout = Swim_err_out_file

    CALL ps_init_tag

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    if(allocated(ss%snbi_type)) then
       ierr=999
       write(iout,*) &
            ' ?ps_set_beam_species: SNBI species list already defined.'
       return
    endif

    ! just exit if there are no beams

    if(.not.allocated(ss%nbion)) return

    inbs=0
    allocate(cblist(ss%nbeam))
    do ibeam=1,ss%nbeam
       cb1=ss%nbion(ibeam)
       call uupper(cb1)

       is_h = (cb1.eq.'H').or.(cb1.eq.'D').or.(cb1.eq.'T')
       is_he = (cb1.eq.'HE3').or.(cb1.eq.'HE4')

       imatch=0
       do jbeam=1,inbs
          if(cb1.eq.cblist(jbeam)) then
             imatch=jbeam
             exit
          endif
       enddo

       if(imatch.eq.0) then
          ! add to list
          do jbeam=1,nbok
             if(cb1.eq.oklist(jbeam)) then
                imatch=jbeam
                exit
             endif
          enddo
          if(imatch.eq.0) then
             ierr=ierr + 1
             write(iout,*) ' ?ps_set_beam_species: beam isotope unknown: ',trim(cb1)
          else
             inbs = inbs + 1
             cblist(inbs)=cb1
          endif
       endif

       ! check for trace element

       ss%trace_flag(ibeam)=0

       cb1=ss%nbion_trace(ibeam)
       if(cb1.eq.' ') cycle

       call uupper(cb1)

       is_h2 = (cb1.eq.'H').or.(cb1.eq.'D').or.(cb1.eq.'T')
       is_he2 = (cb1.eq.'HE3').or.(cb1.eq.'HE4')

       ss%trace_flag(ibeam)=2

       if(is_h.AND.is_h2) ss%trace_flag(ibeam)=1  ! isotope match
       if(is_he.AND.is_he2) ss%trace_flag(ibeam)=1  ! isotope match

       imatch=0
       do jbeam=1,inbs
          if(cb1.eq.cblist(jbeam)) then
             imatch=jbeam
             exit
          endif
       enddo

       if(imatch.eq.0) then
          ! add to list
          do jbeam=1,nbok
             if(cb1.eq.oklist(jbeam)) then
                imatch=jbeam
                exit
             endif
          enddo
          if(imatch.eq.0) then
             ierr=ierr + 1
             write(iout,*) ' ?ps_set_beam_species: beam trace element unsupported: ',trim(cb1)
          else
             inbs = inbs + 1
             cblist(inbs)=cb1
          endif
       endif
       
    enddo

    if(ierr.ne.0) then
       ierr=999
       return
    endif

    !  OK beam species list now known in cblist(1:inbs) (character codes)
    
    ss%nspec_beam = inbs
    call plasma_state_empty_alloc(ss, ierr)

    do jbeam=1,inbs
       if(cblist(jbeam).eq.'H') then
          CALL ps_species_convert(1,1,1, &
               ss%qatom_snbi(jbeam),ss%q_snbi(jbeam),ss%m_snbi(jbeam),iertmp)

       else if(cblist(jbeam).eq.'D') then
          CALL ps_species_convert(1,1,2, &
               ss%qatom_snbi(jbeam),ss%q_snbi(jbeam),ss%m_snbi(jbeam),iertmp)

       else if(cblist(jbeam).eq.'T') then
          CALL ps_species_convert(1,1,3, &
               ss%qatom_snbi(jbeam),ss%q_snbi(jbeam),ss%m_snbi(jbeam),iertmp)

       else if(cblist(jbeam).eq.'HE3') then
          CALL ps_species_convert(2,2,3, &
               ss%qatom_snbi(jbeam),ss%q_snbi(jbeam),ss%m_snbi(jbeam),iertmp)

       else if(cblist(jbeam).eq.'HE4') then
          CALL ps_species_convert(2,2,4, &
               ss%qatom_snbi(jbeam),ss%q_snbi(jbeam),ss%m_snbi(jbeam),iertmp)

       else if(cblist(jbeam).eq.'NE') then
          CALL ps_species_convert(10,10,0, &
               ss%qatom_snbi(jbeam),ss%q_snbi(jbeam),ss%m_snbi(jbeam),iertmp)

       else if(cblist(jbeam).eq.'AR') then
          CALL ps_species_convert(18,18,0, &
               ss%qatom_snbi(jbeam),ss%q_snbi(jbeam),ss%m_snbi(jbeam),iertmp)

       else if(cblist(jbeam).eq.'KR') then
          CALL ps_species_convert(36,36,0, &
               ss%qatom_snbi(jbeam),ss%q_snbi(jbeam),ss%m_snbi(jbeam),iertmp)

       else if(cblist(jbeam).eq.'XE') then
          CALL ps_species_convert(54,54,0, &
               ss%qatom_snbi(jbeam),ss%q_snbi(jbeam),ss%m_snbi(jbeam),iertmp)

       endif
    enddo

  END SUBROUTINE ps_set_beam_species

  SUBROUTINE ps_check_beam_species(ierr, state)

    !  for the given state instance (or for instance   ps  if none is
    !  specified:

    !  check neutral beam fast ion species list, that all beam species
    !  are supported:  H, D, T, He3, He4, certain noble gas impurities

    !  DMC Mod May 2010: allow impurity beams; allow trace elements in beams
    !     allowed impurity beams: noble gasses, Ne, Ar, Kr, Xe

    integer, intent(out) :: ierr ! status code: 0 = OK
    
    TYPE (plasma_state), optional, target :: state

    !--------------------------------------------
    TYPE (plasma_state), pointer :: ss
    integer :: ibeam,jbeam,inbs,iout,imatch,iertmp
    logical :: is_h,is_he,is_h2,is_he2
    real*8 :: zq,zqa,zm
    character*32 :: cb1
    character*32, dimension(:), allocatable :: cblist

    integer, parameter :: nbok=9
    character*32 :: oklist(nbok)

    !--------------------------------------------
    !  currently known beam species-- uppercase labels:

    oklist(1)='H'
    oklist(2)='D'
    oklist(3)='T'
    oklist(4)='HE3'
    oklist(5)='HE4'

    oklist(6)='NE'
    oklist(7)='AR'
    oklist(8)='KR'
    oklist(9)='XE'

    !--------------------------------------------

    ierr = 0
    iout = Swim_err_out_file

    CALL ps_init_tag

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    if(.NOT.allocated(ss%snbi_type)) then
       write(iout,*) &
            ' %ps_check_beam_species: no SNBI species list.'
       if(allocated(ss%nbion)) then
          write(iout,*) &
               ' ?ps_check_beam_species: this is an error, beams are present.'
          ierr=1
       endif
       return
    endif

    inbs=0
    allocate(cblist(ss%nbeam))
    do ibeam=1,ss%nbeam
       cb1=ss%nbion(ibeam)
       call uupper(cb1)

       is_h = (cb1.eq.'H').or.(cb1.eq.'D').or.(cb1.eq.'T')
       is_he = (cb1.eq.'HE3').or.(cb1.eq.'HE4')

       imatch=0
       do jbeam=1,inbs
          if(cb1.eq.cblist(jbeam)) then
             imatch=jbeam
             exit
          endif
       enddo

       if(imatch.eq.0) then
          ! add to list
          do jbeam=1,nbok
             if(cb1.eq.oklist(jbeam)) then
                imatch=jbeam
                exit
             endif
          enddo
          if(imatch.eq.0) then
             ierr=ierr + 1
             write(iout,*) ' ?ps_check_beam_species: beam isotope unknown: ',trim(cb1)
          else
             inbs = inbs + 1
             cblist(inbs)=cb1
          endif
       endif

       ! check for trace element

       ss%trace_flag(ibeam)=0

       cb1=ss%nbion_trace(ibeam)
       if(cb1.eq.' ') cycle

       call uupper(cb1)

       is_h2 = (cb1.eq.'H').or.(cb1.eq.'D').or.(cb1.eq.'T')
       is_he2 = (cb1.eq.'HE3').or.(cb1.eq.'HE4')

       ss%trace_flag(ibeam)=2

       if(is_h.AND.is_h2) ss%trace_flag(ibeam)=1  ! isotope match
       if(is_he.AND.is_he2) ss%trace_flag(ibeam)=1  ! isotope match

       imatch=0
       do jbeam=1,inbs
          if(cb1.eq.cblist(jbeam)) then
             imatch=jbeam
             exit
          endif
       enddo

       if(imatch.eq.0) then
          ! add to list
          do jbeam=1,nbok
             if(cb1.eq.oklist(jbeam)) then
                imatch=jbeam
                exit
             endif
          enddo
          if(imatch.eq.0) then
             ierr=ierr + 1
             write(iout,*) ' ?ps_check_beam_species: beam trace element unsupported: ',trim(cb1)
          else
             inbs = inbs + 1
             cblist(inbs)=cb1
          endif
       endif
       
    enddo

    if(ierr.ne.0) then
       ierr=999
       return
    endif

    !  OK beam species list now known in cblist(1:inbs) (character codes)
    
    if(inbs.ne.ss%nspec_beam) then
       write(iout,*) ' ?ps_check_beam_species: error in no. of beam species:'
       write(iout,*) '  expected: ',inbs,' found in state: ',ss%nspec_beam
       ierr=1
       return
    endif

    do jbeam=1,inbs
       if(cblist(jbeam).eq.'H') then
          CALL ps_species_convert(1,1,1, &
               zqa,zq,zm,iertmp)
          CALL chk1b(1,'H')

       else if(cblist(jbeam).eq.'D') then
          CALL ps_species_convert(1,1,2, &
               zqa,zq,zm,iertmp)
          CALL chk1b(1,'D')

       else if(cblist(jbeam).eq.'T') then
          CALL ps_species_convert(1,1,3, &
               zqa,zq,zm,iertmp)
          CALL chk1b(1,'T')

       else if(cblist(jbeam).eq.'HE3') then
          CALL ps_species_convert(2,2,3, &
               zqa,zq,zm,iertmp)
          CALL chk1b(1,'HE3')

       else if(cblist(jbeam).eq.'HE4') then
          CALL ps_species_convert(2,2,4, &
               zqa,zq,zm,iertmp)
          CALL chk1b(1,'HE4')

       else if(cblist(jbeam).eq.'NE') then
          CALL ps_species_convert(10,10,0, &
               zqa,zq,zm,iertmp)
          CALL chk1b(0,'NE')

       else if(cblist(jbeam).eq.'AR') then
          CALL ps_species_convert(18,18,0, &
               zqa,zq,zm,iertmp)
          CALL chk1b(0,'AR')

       else if(cblist(jbeam).eq.'KR') then
          CALL ps_species_convert(36,36,0, &
               zqa,zq,zm,iertmp)
          CALL chk1b(0,'KR')

       else if(cblist(jbeam).eq.'XE') then
          CALL ps_species_convert(54,54,0, &
               zqa,zq,zm,iertmp)
          CALL chk1b(0,'XE')

       endif
    enddo

    CONTAINS
      subroutine chk1b(ickq,sname)
        !  check for species match: charge, mass
        !  if ickq=1, check ionization state & atomic charge
        !  if ickq=0, check atomic charge only
        !  always check the mass

        integer, intent(in) :: ickq
        character*(*), intent(in) :: sname

        real*8, parameter :: ztol=1.0d-12
        real*8, parameter :: ZERO=0.0d0

        real*8 :: zmaxa,zdiff

        !------------------------
        
        zmaxa=max(abs(zqa),abs(ss%qatom_snbi(jbeam)))
        if(zmaxa.eq.ZERO) then
           ierr=1
           write(iout,*) ' ?ps_check_beam_species(chk1b): '//trim(sname)// &
                ' atomic charges are zero!'
        else
           zdiff=abs(zqa-ss%qatom_snbi(jbeam))
           if(zdiff.gt.ztol) then
              write(iout,*) ' ?ps_check_beam_species(chk1b): '//trim(sname)// &
                   ' atomic charge deviation exceeds tolerance:'
              write(iout,*) '  ',zqa,ss%qatom_snbi(jbeam)
              write(iout,*) '  species name in state: '//trim(ss%snbi_name(jbeam))
              write(iout,*) '  (name mismatch indicates initialization error).'
              ierr=1
           endif
        endif

        zmaxa=max(abs(zm),abs(ss%m_snbi(jbeam)))
        if(zmaxa.eq.ZERO) then
           ierr=1
           write(iout,*) ' ?ps_check_beam_species(chk1b): '//trim(sname)// &
                ' atomic masses are zero!'
        else
           zdiff=abs(zm-ss%m_snbi(jbeam))
           if(zdiff.gt.ztol) then
              write(iout,*) ' ?ps_check_beam_species(chk1b): '//trim(sname)// &
                   ' atomic mass deviation exceeds tolerance:'
              write(iout,*) '  ',zm,ss%m_snbi(jbeam)
              write(iout,*) '  species name in state: '//trim(ss%snbi_name(jbeam))
              write(iout,*) '  (name mismatch indicates initialization error).'
              ierr=1
           endif
        endif
        
        if(ickq.eq.0) return
        
        zmaxa=max(abs(zq),abs(ss%q_snbi(jbeam)))
        if(zmaxa.eq.ZERO) then
           ierr=1
           write(iout,*) ' ?ps_check_beam_species(chk1b): '//trim(sname)// &
                ' ion charges are zero!'
        else
           zdiff=abs(zq-ss%q_snbi(jbeam))
           if(zdiff.gt.ztol) then
              write(iout,*) ' ?ps_check_beam_species(chk1b): '//trim(sname)// &
                   ' ionic charge deviation exceeds tolerance:'
              write(iout,*) '  ',zq,ss%q_snbi(jbeam)
              write(iout,*) '  species name in state: '//trim(ss%snbi_name(jbeam))
              write(iout,*) '  (name mismatch indicates initialization error).'
              ierr=1
           endif
        endif
        
      end subroutine chk1b

  END SUBROUTINE ps_check_beam_species

  SUBROUTINE ps_label_species(ierr, state)

    !  for the given state instance (or for instance   ps  if none is
    !  specified:

    !  for each of these species lists that is allocated:
    !  generate labels for:  thermal species (s)
    !                        beam species (snbi)
    !                        RF minority species (rfmin)
    !                        fusion product species (sfus)
    !  also set species type codes.
    !
    !  perform error checks:  index (0) species of (s) must be electrons;
    !                         all species must be unique within each list
    !                         atomic number Z must map to a recognized element
    !

    integer, intent(out) :: ierr ! status code: 0 = OK
    
    TYPE (plasma_state), optional, target :: state

    !--------------------------------------------
    TYPE (plasma_state), pointer :: ss
    integer :: itype, iout, iertmp
    integer :: iq,iqc,iamu,ii,izi
    !--------------------------------------------

    ierr = 0
    iout = Swim_err_out_file

    CALL ps_init_tag

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    IF(allocated(ss%nbion)) then
       IF(.not.allocated(ss%snbi_type)) then
          write(iout,*) ' %ps_label_species: set up beam ion species list.'
          call ps_set_beam_species(ierr, state=ss)
       ELSE
          call ps_check_beam_species(ierr, state=ss)
       ENDIF
       if(ierr.ne.0) return
    ENDIF

    izi = ss%Z0max + 1  ! 1st impurity Z
    if(izi.lt.2) then
       izi=2
       ss%Z0max = 1
       write(iout,*) ' %plasma_state_label:  ss%Z0max reset to minimum: 1.'
    else if(izi.gt.4) then
       izi=4
       ss%Z0max = 3
       write(iout,*) ' %plasma_state_label:  ss%Z0max reset to maximum: 3.'
    endif

    if(allocated(ss%s_type)) then
       iq = ss%qatom_s(0)/ps_xe - 0.1_rspec
       if(iq.ne.-1) then
          ierr = ierr + 1
          write(iout,*) ' ?ps_label_species: element (0) of species list "s"'
          write(iout,*) '  is reserved for electrons, but found ss%qatom_s(0)=', &
               ss%qatom_s(0)
       endif

       do ii=0,ss%nspec_th
          if(ii.eq.0) then
             iq = ss%qatom_s(ii)/ps_xe - 0.1_rspec
             iqc = ss%q_s(ii)/ps_xe - 0.1_rspec
          else
             iq = ss%qatom_s(ii)/ps_xe + 0.1_rspec
             iqc = ss%q_s(ii)/ps_xe + 0.1_rspec
          endif
          if((ii.gt.0).and.(iqc.le.0)) then
             ierr = ierr + 1
             write(iout,*) ' ?ps_label_species: ion elements of species list "s"'
             write(iout,*) '  must have positive charge, but found ss%q_s(',ii,')=',ss%q_s(ii)
          endif

          if (ignore_electron_AMU) then
             iamu = proton_amu*ss%m_s(ii)/ps_mp + 0.1_rspec  ! amu of neutral atom
          else
             iamu = proton_amu*ss%m_s(ii)/ps_mp + iqc*electron_amu + 0.1_rspec 
          end if

          !  assign type

          if(iq.lt.0) then
             itype = ps_electron
          else if(iq.lt.izi) then
             itype = ps_therm_ion
          else
             itype = ps_impurity
          endif

          ss%s_type(ii)=itype
          call ps_label_spectype(iq, iqc, iamu, itype, &
               ss%s_name(ii), ss%qatom_s(ii), ss%q_s(ii), ss%m_s(ii), iertmp)

          if(iertmp.gt.0) ierr = ierr + 1
       enddo
    endif

    ! do non-thermal species lists

    if(allocated(ss%qatom_snbi)) then
       call proc("snbi",ss%nspec_beam,ps_beam_ion, &
            ss%qatom_snbi,ss%q_snbi,ss%m_snbi,ss%snbi_name,ss%snbi_type)
    endif

    if(allocated(ss%qatom_rfmin)) then
       call proc("rfmin",ss%nspec_rfmin,ps_rf_minority, &
            ss%qatom_rfmin,ss%q_rfmin,ss%m_rfmin,ss%rfmin_name,ss%rfmin_type)
    endif

    if(allocated(ss%qatom_sfus)) then
       call proc("sfus",ss%nspec_fusion,ps_fusion_ion, &
            ss%qatom_sfus,ss%q_sfus,ss%m_sfus,ss%sfus_name,ss%sfus_type)
    endif

    if(ierr.gt.0) then
       write(iout,*) ' ?ps_label_species: errors were detected.'
       ierr = 999
    endif

    CONTAINS
      subroutine proc(lbl,n,itype, &
        qatom,q,m,name,type)

        ! proc non-thermal species list
        ! all elements must be fully stripped with Z=1 or 2

        character*(*), intent(in) :: lbl
        integer, intent(in) :: n
        integer, intent(in) :: itype

        real(kind=rspec) :: qatom(n)   ! atomic charge
        real(kind=rspec) :: q(n)       ! ionic charge
        real(kind=rspec) :: m(n)       ! atomic mass in kg
        character*(*) :: name(n)       ! labels for individual species
        integer :: type(n)             ! saved type code for each species

        !--------------------------

        do ii=1,n
           iq = qatom(ii)/ps_xe + 0.1_rspec
           iqc = q(ii)/ps_xe + 0.1_rspec
           if((iq.le.0).or.(iq.gt.2)) then
              if(lbl.ne.'snbi') then
                 ierr = ierr + 1
                 write(iout,*) ' ?ps_label_species: "'//trim(lbl)//'" species must be an isotope'
                 write(iout,*) '  of H or He, but found Z=',iq
              endif
           endif

           if (ignore_electron_AMU) then
              iamu = proton_amu*m(ii)/ps_mp + 0.1_rspec
           else
              iamu = proton_amu*m(ii)/ps_mp + iqc*electron_amu + 0.1_rspec  ! amu of neutral atom
           end if

           type(ii)=itype
           call ps_label_spectype(iq, iqc, iamu, itype, &
                name(ii), qatom(ii), q(ii), m(ii), iertmp)

           if(iertmp.gt.0) ierr = ierr + 1
        enddo
      end subroutine proc

  end SUBROUTINE ps_label_species

  !  **PRIVATE**
  SUBROUTINE ps_label_spectype(iZatom,iZcharge,iAMU,itype, &
       label, qatom, qcharge, mass, ierr)

    !--------------------------------
    ! D. McCune Oct 10 2006
    ! module subroutine for plasma species labeling
    !
    ! ...to be used during initialization of plasma state system at the
    ! start of a simulation...
    !
    !--------------------------------
    
    integer, intent(in) :: iZatom    ! atomic number: 1 for H, 2 for He, ...
    !  special: -1 for electrons

    integer, intent(in) :: iZcharge  ! ionic charge btw 1 and iZatom
    !  ignored for electrons

    integer, intent(in) :: iAMU      ! atomic weight in AMU, nearest integer
    !  ignored unless 1 <= iZatom <= 2

    integer, intent(in) :: iType     ! species "type", defined as follows:
    !          -1 for electrons (iZatom=-1)
    !           1 for thermal ions (1 <= iZatom <= 2)
    !           2 for "impurity" thermal ions (iZatom > 2)
    !           4 for beam specie (suffix="beam")
    !           5 for RF Minority specie (suffix="rfmi")
    !           6 for fusion product species (suffix="fusn")
    !-------------

    character*(*), intent(out) :: label   ! generated species label

    REAL(KIND=rspec), intent(out) :: qatom    ! charge of fully stripped ion
    REAL(KIND=rspec), intent(out) :: qcharge  ! actual charge
    REAL(KIND=rspec), intent(out) :: mass     ! mass in kg

    integer, intent(out) :: ierr

    !---------------------------
    integer :: iout
    character*4 suffix
    !---------------------------

    ierr = 0
    iout = Swim_err_out_file

    if(itype.eq.ps_electron) then
       suffix=' '
    else if((itype.ge.ps_therm_ion).and.(itype.le.ps_impurity)) then
       suffix=' '
    else if(itype.eq.ps_beam_ion) then
       suffix='beam'
    else if(itype.eq.ps_rf_minority) then
       suffix='rfmi'
    else if(itype.eq.ps_fusion_ion) then
       suffix='fusn'
    else
       ierr = 1
       write(iout,*) ' ?? plasma_state_mod internal error: unrecognized itype: ',itype
       return
    endif

    call ps_label_species_1(iZatom,iZcharge,iAMU,suffix, &
       label, qatom, qcharge, mass, ierr)

  end SUBROUTINE ps_label_spectype

  !==========================================================================
  ! ** PRIVATE **

  SUBROUTINE ps_label_species_1(iZatom,iZcharge,iAMU,suffix, &
       label, qatom, qcharge, mass, ierr)

    !--------------------------------
    ! D. McCune Oct 10 2006
    ! module subroutine for plasma species labeling
    !
    ! ...to be used during initialization of plasma state system at the
    ! start of a simulation...
    !--------------------------------
    
    integer, intent(in) :: iZatom    ! atomic number: 1 for H, 2 for He, ...
    !  special: -1 for electrons

    integer, intent(in) :: iZcharge  ! ionic charge btw 1 and iZatom
    !  ignored for electrons

    integer, intent(in) :: iAMU      ! atomic weight in AMU, nearest integer
    !  ignored unless 1 <= iZatom <= 2

    character*(*), intent(in) :: suffix   ! suffix for generated label (short!)

    !-------------

    character*(*), intent(out) :: label   ! generated species label

    REAL(KIND=rspec), intent(out) :: qatom    ! charge of fully stripped ion
    REAL(KIND=rspec), intent(out) :: qcharge  ! actual charge
    REAL(KIND=rspec), intent(out) :: mass     ! mass in kg

    integer, intent(out) :: ierr   ! status code (0=OK)

    !  if iZatom does not match any available data, label='?' is returned.

    !  examples of results: (input) -> (output)
    !  (1,1,2," ") -> ("D", ps_xe, ps_xe, 2.014102)
    !  (1,1,1,"mino") -> ("H_mino", ps_xe, ps_xe, 1.0000)
    !  (2,2,3," ") -> ("He3", 2*ps_xe, 2*ps_xe, 3.016029)
    !  (6,6,0," ") -> ("C", 6*ps_xe, 6*ps_xe, 12.011)
    !  (6,6,0,"imp") -> ("C_imp", 6*ps_xe, 6*ps_xe, 12.011)
    !  (6,4,0," ") -> ("C_4", 6*ps_xe, 4*ps_xe, 12.011)
    !  (-1,0,0,"??") -> ("e", -ps_xe, -ps_xe, ps_me/ps_mp) (suffix ignored)

    !------------------------------
    !  local:
    integer :: i,ii,iq,iqc, iout
    REAL(KIND=rspec) :: a
    character*3 tmp_label
    character*3 isymbol
    !------------------------------

    ierr = 0
    iout = Swim_err_out_file

    if(iZatom.eq.-1) then
       !  Electron...
       if(suffix.eq.' ') then
          label = "e"
       else
          label = "e_"//trim(suffix)
       endif
       qatom = -ps_xe
       qcharge = -ps_xe
       mass = ps_me

    else if((iZatom.eq.0).or.(iZatom.lt.-1)) then
       write(iout,*) ' ?? ps_label_species_1: unexpected atomic number: ',iZatom
       write(iout,*) '    probably qatom_xx element was not set in state object.'
       label = "???"
       qatom = 0
       qcharge = 0
       mass = 0

       ierr = 1

    else
       label   = "????"
       qatom   = 0
       qcharge = 0
       mass    = 0

       a = use_amu(iZatom, real(iAmu,rspec), ix=i, is=ii)  ! i->element index, ii->isotope index or 0

       if (a<0.5_rspec) then 
          write(iout,*) ' ?? ps_label_species_1: unexpected atomic number: ',iZatom
          write(iout,*) '    probably qatom_xx element improperly set in state object.'
          ierr = 1

       else
          iq  = iZatom
          iqc = max(0,min(iq,iZcharge))

          if (ii>0 .and. ii<=max_isotopes) then
             isymbol = isox(ii)%symbol                ! element restricted to known isotopes
          else if (i>0 .and. i<=max_elements) then
             isymbol = elex(i)%symbol                 ! element only has isotopic average
          else
             isymbol = '??'
          end if

          if(iq.eq.iqc) then
             if(suffix.eq.' ') then
                label = isymbol
             else
                label = trim(isymbol)//'_'//trim(suffix)
             endif
          else
             tmp_label='   '
             if(iqc.lt.10) then
                write(tmp_label,'(I1)') iqc
             else if(iqc.lt.100) then
                write(tmp_label,'(I2.2)') iqc
             else
                write(tmp_label,'(I3.3)') iqc
             endif
             if(suffix.eq.' ') then
                label = trim(isymbol)//'_'//trim(tmp_label)
             else
                label = trim(isymbol)//'_'//trim(tmp_label)// &
                     '_'//trim(suffix)
             endif
          endif

          qatom   = iq*ps_xe
          qcharge = iqc*ps_xe
          mass    = atom_amu_to_proton(iq,iqc,a)*ps_mp
       endif  ! a
    endif     ! iZatom
  END SUBROUTINE ps_label_species_1

  !==========================================================================
  SUBROUTINE ps_merge_species_lists(ierr, state, indices)

    !-------------------------------------------------------------------
    !
    ! PS_MERGE_SPECIES_LISTS
    !
    ! From species lists s, snbi, rfmin, sfus create:
    !
    !      sa = reduced "s" list:  non-impurity ions + 2 impurities at most
    !           both s and sa include electrons; if s has 2 or fewer impurities
    !           then s and sa end up being identical
    ! 
    !     all = union of s, snbi, rfmin, sfus
    !    alla = union of sa, snbi, rfmin, sfus
    !
    ! Usage:  complete initialization of s, snbi, rfmin, and sfus lists
    !    before calling this routine!  If any of the fast ion species
    !    lists are unallocated, it is assumed that there are no such 
    !    fast ions in the simulation.
    !
    ! Error checks:  this routine checks that the 
    !    necessary state element arrays are allocated, and that the
    !    species names are all defined and unique (call PS_LABEL_SPECIES
    !    before calling this routine).
    !
    ! More error checks: sa, all, and alla should be UNallocated on entry
    ! to this routine!
    !
    ! Optional arguments: Z of the 2 impurities for sa; if defaulted,
    !    the lowest Z and highest Z impurity atomic species, maximal 
    !    available ionization state for each species, are chosen from
    !    the "s" list.
    !
    ! If s has 2 or fewer impurity ion species, s = sa will result, unless
    !    there are less-than-fully-stripped non-impurity ion species present.
    !-------------------------------------------------------------------
    !
    !   Arguments:

    integer, intent(out) :: ierr  ! exit status code (0=OK)
     integer, intent(in), optional :: indices(2)
   
    TYPE (plasma_state), optional, target :: state
    
    !----------------------------------------------------------------------
    !
    !   Declare local variables
    !
    !----------------------------------------------------------------------

    TYPE (plasma_state), pointer :: ss
    integer :: iout
    integer :: iZmin,iZmax,iZ1,iZ2,iZsmin,iZsmax,indx1,indx2
    integer :: iZsminZ,iZsmaxZ,iZ,iZc
    integer :: iZ2min,iZ2minZ,iZ2max,iZ2maxZ,in2x1,in2x2
    integer :: ii,jj,insa,indx

    !  count number of not-fully-stripped non-impurity ions
    integer :: ic_thpartial

    !  count number of impurity ions (each charge state counted separately)
    integer :: ic_impions

    !  flags if there are only zero or one impurity atomic species
    !                        (may have multiple charge states)
    logical :: iimp_single,iimp_none

    integer :: icskip  ! sum of ic_thpartial and ic_impions

    logical :: ssa_match

    !----------------------------------------------------------------------
    !
    !   Merge species lists
    !
    !----------------------------------------------------------------------

    CALL ps_init_tag

    ierr = 0
    iout = Swim_err_out_file

    !  handle optional arguments

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    iZ1=ss%Zimp1 + 0.01_rspec
    iZ2=ss%Zimp2 + 0.01_rspec

    iZmin=min(iZ1,iZ2)
    iZmax=max(iZ1,iZ2)

    ! these may be adjusted further depending on the Z range of impurities
    ! available in s.

    !--------------------------
    !  error checks:

    if(allocated(ss%sa_type)) ierr = ierr+1
    if(allocated(ss%all_type)) ierr = ierr+1
    if(allocated(ss%alla_type)) ierr = ierr+1

    if(ierr.gt.0) then
       write(iout,*) ' ?ps_merge_species_lists: one or more of the species lists "sa", "all", "alla"'
       write(iout,*) '  already exist.  These are meant to be created by this call.'
       ierr=999
       return
    endif

    if(.not.allocated(ss%s_type)) then
       write(iout,*) ' ?ps_merge_species_lists: call prior to definition of "s" thermal species list.'
       write(iout,*) '  Define "s", "snbi", "rfmin, and "sfus" lists before calling this routine.'
       ierr=999
       return
    endif

    if(ss%s_name(0).eq.' ') then
       write(iout,*) ' ?ps_merge_species_lists: s(0) unlabeled; call ps_label_species first!'
       ierr=999
       return
    endif

    call dupchk("s",ss%s_name,ss%nspec_th)
    if(allocated(ss%snbi_name)) call dupchk("snbi",ss%snbi_name,ss%nspec_beam)
    if(allocated(ss%rfmin_name)) call dupchk("rfmin",ss%rfmin_name,ss%nspec_rfmin)
    if(allocated(ss%sfus_name)) call dupchk("sfus",ss%sfus_name,ss%nspec_fusion)

    if(ierr.gt.0) then
       ierr=999
       return
    endif

    !-------------------------------------
    !  OK -- create "sa" list

    ! find min and max impurity atomic number in s...
    ! find max charge state of these impurities

    izSmin=10000
    izSmax=0
    izSminZ=0
    izSmaxZ=0
    indx1=0
    indx2=0

    iz2min=10000
    iz2max=0
    iz2minZ=0
    iz2maxZ=0
    in2x1=0
    in2x2=0

    insa=0

    ic_thpartial = 0
    ic_impions = 0

    do ii=1,ss%nspec_th    ! start at 1, not 0 (skip electrons)
       iZ=ss%qatom_s(ii)/ps_xe + 0.1_rspec
       iZc=ss%q_s(ii)/ps_xe + 0.1_rspec
       if(ss%s_type(ii).ne.ps_impurity) then
          if(iZ.eq.iZc) then
             insa=insa+1      ! non-impurity ion, fully stripped
          else
             ic_thpartial = ic_thpartial + 1   ! He+1 for example
             cycle  ! skip: not fully stripped
          endif

       else
          ic_impions = ic_impions + 1
          if(present(indices)) then
             if(ii.eq.indices(1)) then
                ss%zimp1=iz
                indx1=ii
                in2x1=ii
                iZsmin=iz
                iz2min=iz
             endif
             if(ii.eq.indices(2)) then
                ss%zimp2=iz
                indx2=ii
                in2x2=ii
                iZsmax=iz
                iz2max=iz
             endif

          else

          ! unconstrained by iZmin,iZmax:

             if(iZ.lt.izSmin) then
                indx1=ii
                izSmin=iZ
                izSminZ=iZc
             else if(iZ.eq.izSmin) then
                if(iZc.gt.izSminZ) then
                   indx1=ii
                   izSminZ=iZc
                endif
             endif
             
             if(iZ.gt.izSmax) then
                indx2=ii
                izSmax=iZ
                izSmaxZ=iZc
             else if(iZ.eq.izSmax) then
                if(iZc.gt.izSmaxZ) then
                   indx2=ii
                   izSmaxZ=iZc
                endif
             endif

             if((iZ.ge.iZmin).and.(iZ.le.iZmax)) then

             ! constrained
                
                if(iZ.lt.iz2min) then
                   in2x1=ii
                   iz2min=iZ
                   iz2minZ=iZc
                else if(iZ.eq.iz2min) then
                   if(iZc.gt.iz2minZ) then
                      in2x1=ii
                      iz2minZ=iZc
                   endif
                endif
                
                if(iZ.gt.iz2max) then
                   in2x2=ii
                   iz2max=iZ
                   iz2maxZ=iZc
                else if(iZ.eq.iz2max) then
                   if(iZc.gt.iz2maxZ) then
                      in2x2=ii
                      iz2maxZ=iZc
                   endif
                endif
             endif
          endif
       endif
    enddo

    iimp_none=.FALSE.
    iimp_single=.FALSE.

    if(iZsmin.eq.1000) then
       write(iout,*) ' %ps_merge_species_lists: no impurities found.'
       iimp_none=.TRUE.

    else if(iZsmin.eq.iZsmax) then
       ! there is only a single impurity atomic species
       write(iout,*) ' %ps_merge_species_lists: only one impurity species: ', &
            'Z=',iZsmin
       iimp_single = .TRUE.

    else
       if((iz2min.eq.1000).or.(iz2min.eq.iz2max)) then
          write(iout,*) ' %ps_merge_species_lists: impurity constraints waived:'
          write(iout,*) '  not enough species in range: ',iZmin,iZmax
          iZ2min=iZsmin
          iZ2max=iZsmax
          in2x1=indx1
          in2x2=indx2
       endif

       ! reset Zimp constraints to match available impurities, if constraints
       ! were not binding

       if(indx1.eq.in2x1) ss%zimp1=iz2min
       if(indx2.eq.in2x2) ss%zimp2=iz2max
    endif

    icskip = ic_thpartial + ic_impions

    if((icskip.le.2).or.(ic_impions.lt.2).or.(iimp_none).or.(iimp_single)) then
       ! s = sa if #of skipped species <=2 or #impurity ions <2
       !   or #impurity species < 2.

       !   (usually there are no skipped species except impurities)
       !   (if simulation contains partially-ionized He then non-impurity
       !   skipped species for sa are possible).

       ss%nspec_tha = ss%nspec_th
       ssa_match = .TRUE.

    else
       ssa_match = .FALSE.
       ss%nspec_tha = insa + 2 ! two impurities in sa; 2 or more in s.

       if(in2x1.eq.in2x2) then
          write(iout,*) &
               ' %ps_merge_species_lists: <2 impurity species in Z range: ', &
               ss%Zimp1,ss%Zimp2
          write(iout,*) '  reverting to use of impuries of min & max Z: ', &
               iZsmin,iZsmax
       else
          indx1 = in2x1
          indx2 = in2x2
       endif
    endif

    ! combined lists:

    ss%nspec_all = ss%nspec_th + ss%nspec_beam + ss%nspec_fusion + ss%nspec_rfmin
    ss%nspec_alla = ss%nspec_tha + ss%nspec_beam + ss%nspec_fusion + ss%nspec_rfmin

    !-------------------------------------
    ! ALLOCATE new species arrays

    CALL plasma_state_empty_alloc(ss,ierr)
    if(ierr.ne.0) then
       write(iout,*) ' ?ps_merge_species lists: ALLOC error.'
       return
    endif

    !-------------------------------------
    !  fill in the "sa" list

    indx=-1  ! index 0 for electrons in sa as in s...

    do ii=0,ss%nspec_th
       if(ssa_match.or.(ss%s_type(ii).ne.ps_impurity)) then
          if(ss%qatom_s(ii).lt.0.0_rspec) then
             iZ=ss%qatom_s(ii)/ps_xe - 0.1_rspec
             iZc=ss%q_s(ii)/ps_xe - 0.1_rspec
          else
             iZ=ss%qatom_s(ii)/ps_xe + 0.1_rspec
             iZc=ss%q_s(ii)/ps_xe + 0.1_rspec
          endif
          if(ssa_match.or.(ii.eq.0).or.(iZ.eq.iZc)) then
             indx = indx+1
             ! electrons and fully stripped light ions ONLY
             ! unless ssa_match=T in which case all ions pass
             ss%sa_name(indx) = ss%s_name(ii)
             ss%sa_type(indx) = ss%s_type(ii)
             ss%qatom_sa(indx) = ss%qatom_s(ii)
             ss%q_sa(indx) = ss%q_s(ii)
             ss%m_sa(indx) = ss%m_s(ii)
             ss%sa_index(indx) = ii
             ss%s_to_sa(ii) = indx
          else
             ss%s_to_sa(ii) = -1
          endif
       else
          ss%s_to_sa(ii) = -1
       endif
    enddo

    if(.not.ssa_match) then
       indx=indx+1
       ss%sa_name(indx) = ss%s_name(indx1)
       ss%sa_type(indx) = ss%s_type(indx1)
       ss%qatom_sa(indx) = ss%qatom_s(indx1)
       ss%q_sa(indx) = ss%q_s(indx1)
       ss%m_sa(indx) = ss%m_s(indx1)
       ss%sa_index(indx) = -1

       indx=indx+1
       ss%sa_name(indx) = ss%s_name(indx2)
       ss%sa_type(indx) = ss%s_type(indx2)
       ss%qatom_sa(indx) = ss%qatom_s(indx2)
       ss%q_sa(indx) = ss%q_s(indx2)
       ss%m_sa(indx) = ss%m_s(indx2)
       ss%sa_index(indx) = -1

       write(iout,*) ' %ps_merge_species_lists: impurities in "sa" abridged thermal species list:'
       write(iout,*) '    '//trim(ss%sa_name(indx-1))//' '//trim(ss%sa_name(indx))
    endif

    !-------------------------------------
    !  fill in the "all" list

    indx=-1  ! index 0 for electrons in all as in s...

    do ii=0,ss%nspec_th
       indx = indx+1
       ss%all_name(indx) = ss%s_name(ii)
       ss%all_type(indx) = ss%s_type(ii)
       ss%qatom_all(indx) = ss%qatom_s(ii)
       ss%q_all(indx) = ss%q_s(ii)
       ss%m_all(indx) = ss%m_s(ii)
       ss%all_index(indx) = ii
       ! for (0:ss%nspec_th), s and all lists match.
    enddo

    do ii=1,ss%nspec_beam
       indx = indx + 1
       ss%all_name(indx) = ss%snbi_name(ii)
       ss%all_type(indx) = ss%snbi_type(ii)
       ss%qatom_all(indx) = ss%qatom_snbi(ii)
       ss%q_all(indx) = ss%q_snbi(ii)
       ss%m_all(indx) = ss%m_snbi(ii)
       ss%all_index(indx) = ii
       ss%snbi_to_all(ii) = indx
    enddo

    do ii=1,ss%nspec_rfmin
       indx = indx + 1
       ss%all_name(indx) = ss%rfmin_name(ii)
       ss%all_type(indx) = ss%rfmin_type(ii)
       ss%qatom_all(indx) = ss%qatom_rfmin(ii)
       ss%q_all(indx) = ss%q_rfmin(ii)
       ss%m_all(indx) = ss%m_rfmin(ii)
       ss%all_index(indx) = ii
       ss%rfmin_to_all(ii) = indx
    enddo

    do ii=1,ss%nspec_fusion
       indx = indx + 1
       ss%all_name(indx) = ss%sfus_name(ii)
       ss%all_type(indx) = ss%sfus_type(ii)
       ss%qatom_all(indx) = ss%qatom_sfus(ii)
       ss%q_all(indx) = ss%q_sfus(ii)
       ss%m_all(indx) = ss%m_sfus(ii)
       ss%all_index(indx) = ii
       ss%sfus_to_all(ii) = indx
    enddo

    !-------------------------------------
    !  fill in the "alla" list

    indx=-1  ! index 0 for electrons in all as in s...

    do ii=0,ss%nspec_tha
       indx = indx+1
       ss%alla_name(indx) = ss%sa_name(ii)
       ss%alla_type(indx) = ss%sa_type(ii)
       ss%qatom_alla(indx) = ss%qatom_sa(ii)
       ss%q_alla(indx) = ss%q_sa(ii)
       ss%m_alla(indx) = ss%m_sa(ii)
       ss%alla_index(indx) = ss%sa_index(ii)
       ! for (0:ss%nspec_tha), sa and alla lists match.
    enddo

    do ii=1,ss%nspec_beam
       indx = indx + 1
       ss%alla_name(indx) = ss%snbi_name(ii)
       ss%alla_type(indx) = ss%snbi_type(ii)
       ss%qatom_alla(indx) = ss%qatom_snbi(ii)
       ss%q_alla(indx) = ss%q_snbi(ii)
       ss%m_alla(indx) = ss%m_snbi(ii)
       ss%alla_index(indx) = ii
       ss%snbi_to_alla(ii) = indx
    enddo

    do ii=1,ss%nspec_rfmin
       indx = indx + 1
       ss%alla_name(indx) = ss%rfmin_name(ii)
       ss%alla_type(indx) = ss%rfmin_type(ii)
       ss%qatom_alla(indx) = ss%qatom_rfmin(ii)
       ss%q_alla(indx) = ss%q_rfmin(ii)
       ss%m_alla(indx) = ss%m_rfmin(ii)
       ss%alla_index(indx) = ii
       ss%rfmin_to_alla(ii) = indx
    enddo

    do ii=1,ss%nspec_fusion
       indx = indx + 1
       ss%alla_name(indx) = ss%sfus_name(ii)
       ss%alla_type(indx) = ss%sfus_type(ii)
       ss%qatom_alla(indx) = ss%qatom_sfus(ii)
       ss%q_alla(indx) = ss%q_sfus(ii)
       ss%m_alla(indx) = ss%m_sfus(ii)
       ss%alla_index(indx) = ii
       ss%sfus_to_alla(ii) = indx
    enddo

    !  All done.

    CONTAINS
      subroutine dupchk(lbl,names,n)
        character*(*), intent(in) :: lbl
        integer, intent(in) :: n
        character*(*), intent(inout) :: names(n)

        integer :: idup

        idup=0
        do jj=1,n-1
           do ii=jj+1,n
              if(names(ii).eq.names(jj)) then
                 if(names(ii).eq."H_fusn") then
                    names(ii)="H14_fusn"
                 else
                    idup=idup+1
                 endif
              endif
           enddo
        enddo

        if(idup.gt.0) then
           ierr = ierr + 1
           write(iout,*) &
                ' ?? ps_merge_species_lists: duplicate species in "'// &
                trim(lbl)//'" list detected:'
           do ii = 1,n
              write(iout,*) ii,' "'//trim(names(ii))//'"'
           enddo
        endif
        
      end subroutine dupchk

    end SUBROUTINE ps_merge_species_lists
   !==========================================================================
    SUBROUTINE ps_merge_species_lists_match_abridge(ierr, indx_not_abridge, state)
    !-------------------------------------------------------------------
    !
    ! PS_MERGE_SPECIES_LISTS
    !
    ! From species lists s, snbi, rfmin, sfus create:
    !
    !      sa = reduced "s" list:  non-impurity ions + 2 impurities at most and these impurity will match user's namelist request
    !           both s and sa include electrons; 
    !      
    ! 
    !     all = union of s, snbi, rfmin, sfus
    !    alla = union of sa, snbi, rfmin, sfus
    !
    ! Usage:  complete initialization of s, snbi, rfmin, and sfus lists
    !    before calling this routine!  If any of the fast ion species
    !    lists are unallocated, it is assumed that there are no such 
    !    fast ions in the simulation.
    !
    ! Error checks:  this routine checks that the 
    !    necessary state element arrays are allocated, and that the
    !    species names are all defined and unique (call PS_LABEL_SPECIES
    !    before calling this routine).
    !
    ! More error checks: sa, all, and alla should be UNallocated on entry
    ! to this routine!
    !
    ! Optional arguments: Z of the 2 impurities for sa; if defaulted,
    !    the lowest Z and highest Z impurity atomic species, maximal 
    !    available ionization state for each species, are chosen from
    !    the "s" list.
    !
    ! If s has 2 or fewer impurity ion species, s = sa will result, unless
    !    there are less-than-fully-stripped non-impurity ion species present.
    !-------------------------------------------------------------------
    !
    !   Arguments:

    integer, intent(out) :: ierr  ! exit status code (0=OK)
    integer, intent(in) :: indx_not_abridge(2)  ! indcies for 2 impurities species to be added  in "sa" list
    
    TYPE (plasma_state), optional, target :: state
    
    !----------------------------------------------------------------------
    !
    !   Declare local variables
    !
    !----------------------------------------------------------------------

    TYPE (plasma_state), pointer :: ss
    integer :: iout
    integer :: indx1,indx2
    integer :: iZ,iZc
    integer :: ii,jj,insa,indx

    !  count number of not-fully-stripped non-impurity ions
    integer :: ic_thpartial

    !  count number of impurity ions (each charge state counted separately)
    integer :: ic_impions

    !  flags if there are only zero or one impurity atomic species
    !                        (may have multiple charge states)
    logical :: iimp_single,iimp_none

    integer :: icskip  ! sum of ic_thpartial and ic_impions

    logical :: ssa_match

    !----------------------------------------------------------------------
    !
    !   Merge species lists
    !
    !----------------------------------------------------------------------

    CALL ps_init_tag

    ierr = 0
    iout = Swim_err_out_file

    !  handle optional arguments

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif


    ! these may be adjusted further depending on the Z range of impurities
    ! available in s.

    !--------------------------
    !  error checks:

    if(allocated(ss%sa_type)) ierr = ierr+1
    if(allocated(ss%all_type)) ierr = ierr+1
    if(allocated(ss%alla_type)) ierr = ierr+1

    if(ierr.gt.0) then
       write(iout,*) ' ?ps_merge_species_lists: one or more of the species lists "sa", "all", "alla"'
       write(iout,*) '  already exist.  These are meant to be created by this call.'
       ierr=999
       return
    endif

    if(.not.allocated(ss%s_type)) then
       write(iout,*) ' ?ps_merge_species_lists: call prior to definition of "s" thermal species list.'
       write(iout,*) '  Define "s", "snbi", "rfmin, and "sfus" lists before calling this routine.'
       ierr=999
       return
    endif

    if(ss%s_name(0).eq.' ') then
       write(iout,*) ' ?ps_merge_species_lists: s(0) unlabeled; call ps_label_species first!'
       ierr=999
       return
    endif

    call dup_chk("s",ss%s_name,ss%nspec_th)
    if(allocated(ss%snbi_name)) call dup_chk("snbi",ss%snbi_name,ss%nspec_beam)
    if(allocated(ss%rfmin_name)) call dup_chk("rfmin",ss%rfmin_name,ss%nspec_rfmin)
    if(allocated(ss%sfus_name)) call dup_chk("sfus",ss%sfus_name,ss%nspec_fusion)

    if(ierr.gt.0) then
       ierr=999
       return
    endif

    !-------------------------------------
    !  OK -- create "sa" list

    ! find min and max impurity atomic number in s...
    ! find max charge state of these impurities

    indx1=0
    indx2=0

    insa=0

    ic_thpartial = 0
    ic_impions = 0

    do ii=1,ss%nspec_th    ! start at 1, not 0 (skip electrons)
       iZ=ss%qatom_s(ii)/ps_xe + 0.1_rspec
       iZc=ss%q_s(ii)/ps_xe + 0.1_rspec
       if(ss%s_type(ii).ne.ps_impurity) then
          if(iZ.eq.iZc) then
             insa=insa+1      ! non-impurity ion, fully stripped
          else
             ic_thpartial = ic_thpartial + 1   ! He+1 for example
             cycle  ! skip: not fully stripped
          endif

       else
          ic_impions = ic_impions + 1
          if(ii.eq.indx_not_abridge(1))then
             ss%zimp1=iz
             indx1=ii
          endif
          if(ii.eq.indx_not_abridge(2))then
             ss%zimp2=iz
             indx2=ii
          endif
       endif
    enddo

    iimp_none=.FALSE.
    iimp_single=.FALSE.


    icskip = ic_thpartial + ic_impions

    if((icskip.le.2).or.(ic_impions.lt.2).or.(iimp_none).or.(iimp_single)) then
       ! s = sa if #of skipped species <=2 or #impurity ions <2
       !   or #impurity species < 2.

       !   (usually there are no skipped species except impurities)
       !   (if simulation contains partially-ionized He then non-impurity
       !   skipped species for sa are possible).

       ss%nspec_tha = ss%nspec_th
       ssa_match = .TRUE.

    else
       ssa_match = .FALSE.
       ss%nspec_tha = insa + 2 ! two impurities in sa; 2 or more in s.
    endif

    ! combined lists:

    ss%nspec_all = ss%nspec_th + ss%nspec_beam + ss%nspec_fusion + ss%nspec_rfmin
    ss%nspec_alla = ss%nspec_tha + ss%nspec_beam + ss%nspec_fusion + ss%nspec_rfmin

    !-------------------------------------
    ! ALLOCATE new species arrays

    CALL plasma_state_empty_alloc(ss,ierr)
    if(ierr.ne.0) then
       write(iout,*) ' ?ps_merge_species lists: ALLOC error.'
       return
    endif

    !-------------------------------------
    !  fill in the "sa" list

    indx=-1  ! index 0 for electrons in sa as in s...

    do ii=0,ss%nspec_th
       if(ssa_match.or.(ss%s_type(ii).ne.ps_impurity)) then
          if(ss%qatom_s(ii).lt.0.0_rspec) then
             iZ=ss%qatom_s(ii)/ps_xe - 0.1_rspec
             iZc=ss%q_s(ii)/ps_xe - 0.1_rspec
          else
             iZ=ss%qatom_s(ii)/ps_xe + 0.1_rspec
             iZc=ss%q_s(ii)/ps_xe + 0.1_rspec
          endif
          if(ssa_match.or.(ii.eq.0).or.(iZ.eq.iZc)) then
             indx = indx+1
             ! electrons and fully stripped light ions ONLY
             ! unless ssa_match=T in which case all ions pass
             ss%sa_name(indx) = ss%s_name(ii)
             ss%sa_type(indx) = ss%s_type(ii)
             ss%qatom_sa(indx) = ss%qatom_s(ii)
             ss%q_sa(indx) = ss%q_s(ii)
             ss%m_sa(indx) = ss%m_s(ii)
             ss%sa_index(indx) = ii
             ss%s_to_sa(ii) = indx
          else
             ss%s_to_sa(ii) = -1
          endif
       else
          ss%s_to_sa(ii) = -1
       endif
    enddo

    if(.not.ssa_match) then
       indx=indx+1
       ss%sa_name(indx) = ss%s_name(indx1)
       ss%sa_type(indx) = ss%s_type(indx1)
       ss%qatom_sa(indx) = ss%qatom_s(indx1)
       ss%q_sa(indx) = ss%q_s(indx1)
       ss%m_sa(indx) = ss%m_s(indx1)
       ss%sa_index(indx) = -1

       indx=indx+1
       ss%sa_name(indx) = ss%s_name(indx2)
       ss%sa_type(indx) = ss%s_type(indx2)
       ss%qatom_sa(indx) = ss%qatom_s(indx2)
       ss%q_sa(indx) = ss%q_s(indx2)
       ss%m_sa(indx) = ss%m_s(indx2)
       ss%sa_index(indx) = -1

       write(iout,*) ' %ps_merge_species_lists: impurities in "sa" abridged thermal species list:'
       write(iout,*) '    '//trim(ss%sa_name(indx-1))//' '//trim(ss%sa_name(indx))
    endif

    !-------------------------------------
    !  fill in the "all" list

    indx=-1  ! index 0 for electrons in all as in s...

    do ii=0,ss%nspec_th
       indx = indx+1
       ss%all_name(indx) = ss%s_name(ii)
       ss%all_type(indx) = ss%s_type(ii)
       ss%qatom_all(indx) = ss%qatom_s(ii)
       ss%q_all(indx) = ss%q_s(ii)
       ss%m_all(indx) = ss%m_s(ii)
       ss%all_index(indx) = ii
       ! for (0:ss%nspec_th), s and all lists match.
    enddo

    do ii=1,ss%nspec_beam
       indx = indx + 1
       ss%all_name(indx) = ss%snbi_name(ii)
       ss%all_type(indx) = ss%snbi_type(ii)
       ss%qatom_all(indx) = ss%qatom_snbi(ii)
       ss%q_all(indx) = ss%q_snbi(ii)
       ss%m_all(indx) = ss%m_snbi(ii)
       ss%all_index(indx) = ii
       ss%snbi_to_all(ii) = indx
    enddo

    do ii=1,ss%nspec_rfmin
       indx = indx + 1
       ss%all_name(indx) = ss%rfmin_name(ii)
       ss%all_type(indx) = ss%rfmin_type(ii)
       ss%qatom_all(indx) = ss%qatom_rfmin(ii)
       ss%q_all(indx) = ss%q_rfmin(ii)
       ss%m_all(indx) = ss%m_rfmin(ii)
       ss%all_index(indx) = ii
       ss%rfmin_to_all(ii) = indx
    enddo

    do ii=1,ss%nspec_fusion
       indx = indx + 1
       ss%all_name(indx) = ss%sfus_name(ii)
       ss%all_type(indx) = ss%sfus_type(ii)
       ss%qatom_all(indx) = ss%qatom_sfus(ii)
       ss%q_all(indx) = ss%q_sfus(ii)
       ss%m_all(indx) = ss%m_sfus(ii)
       ss%all_index(indx) = ii
       ss%sfus_to_all(ii) = indx
    enddo

    !-------------------------------------
    !  fill in the "alla" list

    indx=-1  ! index 0 for electrons in all as in s...

    do ii=0,ss%nspec_tha
       indx = indx+1
       ss%alla_name(indx) = ss%sa_name(ii)
       ss%alla_type(indx) = ss%sa_type(ii)
       ss%qatom_alla(indx) = ss%qatom_sa(ii)
       ss%q_alla(indx) = ss%q_sa(ii)
       ss%m_alla(indx) = ss%m_sa(ii)
       ss%alla_index(indx) = ss%sa_index(ii)
       ! for (0:ss%nspec_tha), sa and alla lists match.
    enddo

    do ii=1,ss%nspec_beam
       indx = indx + 1
       ss%alla_name(indx) = ss%snbi_name(ii)
       ss%alla_type(indx) = ss%snbi_type(ii)
       ss%qatom_alla(indx) = ss%qatom_snbi(ii)
       ss%q_alla(indx) = ss%q_snbi(ii)
       ss%m_alla(indx) = ss%m_snbi(ii)
       ss%alla_index(indx) = ii
       ss%snbi_to_alla(ii) = indx
    enddo

    do ii=1,ss%nspec_rfmin
       indx = indx + 1
       ss%alla_name(indx) = ss%rfmin_name(ii)
       ss%alla_type(indx) = ss%rfmin_type(ii)
       ss%qatom_alla(indx) = ss%qatom_rfmin(ii)
       ss%q_alla(indx) = ss%q_rfmin(ii)
       ss%m_alla(indx) = ss%m_rfmin(ii)
       ss%alla_index(indx) = ii
       ss%rfmin_to_alla(ii) = indx
    enddo

    do ii=1,ss%nspec_fusion
       indx = indx + 1
       ss%alla_name(indx) = ss%sfus_name(ii)
       ss%alla_type(indx) = ss%sfus_type(ii)
       ss%qatom_alla(indx) = ss%qatom_sfus(ii)
       ss%q_alla(indx) = ss%q_sfus(ii)
       ss%m_alla(indx) = ss%m_sfus(ii)
       ss%alla_index(indx) = ii
       ss%sfus_to_alla(ii) = indx
    enddo

    !  All done.

    CONTAINS
      subroutine dup_chk(lbl,names,n)
        character*(*), intent(in) :: lbl
        integer, intent(in) :: n
        character*(*), intent(in) :: names(n)

        integer :: idup

        idup=0
        do jj=1,n-1
           do ii=jj+1,n
              if(names(ii).eq.names(jj)) idup=idup+1
           enddo
        enddo

        if(idup.gt.0) then
           ierr = ierr + 1
           write(iout,*) &
                ' ?? ps_merge_species_lists: duplicate species in "'// &
                trim(lbl)//'" list detected:'
           do ii = 1,n
              write(iout,*) ii,' "'//trim(names(ii))//'"'
           enddo
        endif
        
      end subroutine dup_chk
      
    end SUBROUTINE ps_merge_species_lists_match_abridge

   !==========================================================================
    SUBROUTINE ps_neutral_species(ierr, state)

    !-------------------------------------------------------------------
    !
    ! PS_NEUTRAL_SPECIES
    !
    ! From the species list "s", for each atomic species with Z <= ss%Z0max
    ! construct a corresponding neutral atom species in "sgas";
    ! for each atomic species with Z > ss%Z0max construct a neutral atom
    ! species in "simp0".
    !
    ! Mod DMC Oct 2010: also call ps_imp_species at end-- construct
    ! impurity species list
    !
    ! Usage:  complete initialization of s species list
    !    before calling this routine!
    !
    ! Error checks:  this routine checks that the 
    !    necessary species list s element arrays are allocated, and that the
    !    ion species have non-zero charges.
    !
    ! More error checks: sgas should be UNallocated on entry to this routine!
    !
    !-------------------------------------------------------------------
    !
    !   Arguments:

    integer, intent(out) :: ierr  ! exit status code (0=OK)
    
    TYPE (plasma_state), optional, target :: state

    !  Care should be taken that the neutral gas model used in the
    !  simulation can handle the value of Z0max chosen.

    !----------------------------------------------------------------------
    !
    !   Declare local variables
    !
    !----------------------------------------------------------------------

    integer :: ii,jj,imatch,ingas,inimp0,iA,iZ,iZc,iZ0max,iout,iertmp
    integer, dimension(:), allocatable :: izimp0s

    TYPE (plasma_state), pointer :: ss

    !----------------------------------------------------------------------
    !
    !   Merge species lists
    !
    !----------------------------------------------------------------------

    CALL ps_init_tag

    ierr = 0
    iout = Swim_err_out_file

    !  handle optional arguments

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    iZ0max = ss%Z0max

    !  error checks

    if(.not.allocated(ss%s_type)) then
       write(iout,*) '? ps_neutral_species: "s" species list not allocated.'
       write(iout,*) '  thermal plasma species must be defined before calling this routine.'
       ierr=999
       return
    endif

    if(allocated(ss%sgas_type)) then
       write(iout,*) '? ps_neutral_species: "sgas" species list already exists.'
       write(iout,*) '  this routine can only be called once in the history of any given state.'
       ierr=999
       return
    endif

    if(allocated(ss%simp0_type)) then
       write(iout,*) '? ps_neutral_species: "simp0" species list already exists.'
       write(iout,*) '  this routine can only be called once in the history of any given state.'
       ierr=999
       return
    endif

    !-------------------------
    !  count the number of neutral species

    inimp0 = size(ss%s_type)  ! this is an upper bound
    allocate(iZimp0s(inimp0))

    ingas = 0
    inimp0 = 0
    iZimp0s = 0

    do ii=1,ss%nspec_th
       iZ=ss%qatom_s(ii)/ps_xe + 0.1_rspec
       if(iZ.eq.0) then
          write(iout,*) '? ps_neutral_species: Z=0 detected in "s" list.'
          write(iout,*) '  thermal plasma species list initialization must be completed before calling this routine.'
          ierr=999
          exit
       endif

       if(iZ.le.iZ0max) then
          iZc=ss%q_s(ii)/ps_xe + 0.1_rspec
          if(iZ.eq.iZc) then
             ! count fully stripped ions only
             ingas = ingas + 1
          endif
       else
          imatch=0
          do jj=1,inimp0
             if(iZ.eq.iZimp0s(jj)) then
                imatch=jj
                exit
             endif
          enddo
          if(imatch.eq.0) then
             ! new species
             inimp0 = inimp0 + 1
             iZimp0s(inimp0) = iZ
          endif
       endif
    enddo

    if(ierr.ne.0) return

    !-------------------------
    !  allocate the neutral gas arrays

    ss%nspec_gas = ingas
    ss%nspec_imp0 = inimp0

    CALL plasma_state_empty_alloc(ss,ierr)
    if(ierr.ne.0) then
       write(iout,*) ' ?ps_merge_species lists: ALLOC error.'
       return
    endif

    !-------------------------
    !  fill the array elements

    ingas = 0
    inimp0 = 0
    iZimp0s = 0

    do ii=1,ss%nspec_th
       iZ=ss%qatom_s(ii)/ps_xe + 0.1_rspec

       if(iZ.le.iZ0max) then
          iZc=ss%q_s(ii)/ps_xe + 0.1_rspec
          if(iZ.eq.iZc) then
             ! count fully stripped ions only
             ingas = ingas + 1

             ss%sgas_type(ingas) = ss%s_type(ii)
             if(len(trim(ss%s_name(ii))).eq.1) then
                ss%sgas_name(ingas) = trim(ss%s_name(ii))//'0' ! e.g. "D0"
             else
                ss%sgas_name(ingas) = trim(ss%s_name(ii))//'_0' ! e.g. "HE4_0"
             endif
             ss%qatom_sgas(ingas) = ss%qatom_s(ii)
             ss%q_sgas(ingas) = 0   ! (neutral gas species)
             ss%m_sgas(ingas) = ss%m_s(ii)

             ss%sgas_to_s(ingas) = ii
          endif
       else
          imatch=0
          do jj=1,inimp0
             if(iZ.eq.iZimp0s(jj)) then
                imatch=jj
                exit
             endif
          enddo
          if(imatch.eq.0) then
             ! new species
             inimp0 = inimp0 + 1
             iZimp0s(inimp0) = iZ
 
             iA=proton_amu*ss%m_s(ii)/ps_mp + 0.1_rspec

             ss%simp0_type(inimp0) = ss%s_type(ii)

             call ps_label_species_1(iZ,0,iA,' ', &
                  ss%simp0_name(inimp0),ss%qatom_simp0(inimp0), &
                  ss%q_simp0(inimp0),ss%m_simp0(inimp0),iertmp)

             ss%simp0_to_s(inimp0) = ii
          endif
       endif
    enddo

    call ps_imp_species(ss,ierr)

  END SUBROUTINE ps_neutral_species

  ! (private...)

  SUBROUTINE ps_imp_species(ss,ierr)

    type (plasma_state) :: ss
    integer, intent(out) :: ierr

    ! private routine, create impurity ion species lists-- all charged
    ! particle species with atomic number > ss%Z0max.

    !------------------------------------
    integer :: ii,inum,iout
    real*8 :: Zi
    !------------------------------------

    ierr = 0
    iout = Swim_err_out_file

    inum = 0
    do ii=1,ss%nspec_th
       Zi = ss%qatom_s(ii)/ps_xe

       if(Zi*0.99 .le. ss%Z0max) cycle

       inum = inum + 1
    enddo

    if(ss%nspec_impi.gt.0) then
       if(inum.ne.ss%nspec_impi) then
          write(iout,*) ' ?ps_imp_species: impurity species count error.'
          write(iout,*) '  ss%nspec_impi = ',ss%nspec_impi
          write(iout,*) '  count from full thermal list: ',inum
          ierr = 1
          return
       endif
    else if(inum.gt.0) then
       ss%nspec_impi = inum
       call plasma_state_empty_alloc(ss,ierr)
       if(ierr.ne.0) then
          write(iout,*) ' ?ps_imp_species: alloc error.'
          return
       endif
    else
       return
    endif

    inum = 0
    do ii=1,ss%nspec_th
       Zi = ss%qatom_s(ii)/ps_xe

       if(Zi*0.99 .le. ss%Z0max) cycle

       inum = inum + 1
       ss%simpi_to_s(inum) = ii

       ss%simpi_type(inum) = ss%s_type(ii)
       ss%simpi_name(inum) = ss%s_name(ii)
       ss%qatom_simpi(inum) = ss%qatom_s(ii)
       ss%q_simpi(inum) = ss%q_s(ii)
       ss%m_simpi(inum) = ss%m_s(ii)
    enddo

  END SUBROUTINE ps_imp_species

 !==========================================================================
  subroutine ps_init_neutral(ss,ngmax,ierr)

  ! initialize up normalized source and neutral density profiles (per unit influx)
  ! in the provided plasma state object

  ! this is data related to edge sources and e-i recombination sources
  ! typically computed from heatlib/frantic_sorce1.for ...


    implicit NONE

  !---------------------------
  !  arguments:
    type (plasma_state) :: ss
    integer, intent(out) :: ierr
    integer, intent(in) :: ngmax ! MAX ANTICIPATED NUMBER OF SPECIES THIS RUN

  !---------------------------
  !  local:
    INTEGER, PARAMETER :: R8=SELECTED_REAL_KIND(12,100)
    REAL*8, PARAMETER :: ZERO = 0.0_R8

    integer :: in0,ig,ilen,ilen0
    integer :: iout
    character*20 znam

  !---------------------------
    iout=SWIM_err_out_file

    ierr=0

  !  check neutrals... for this application, there are to be 
  !  two neutral sources per neutral gas species, one for recycling
  !  and one for gas flow

    if(ss%ngsc0.eq.0) then
       if(ss%nspec_gas.gt.0) then

          in0 = ss%nspec_gas
          if(in0.ne.ngmax) then
             write(iout,*) ' ?ps_init_neutral: in0=',in0
             write(iout,*) '  does not match ngmax=',ngmax,' as expected.'
             ierr=1
             return
          endif

          ss%ngsc0 = 2 * in0    ! 1 recyc & 1 gas flow per neutral gas
          call ps_alloc_plasma_state(ierr, state=ss)
          if(ierr.ne.0) then
             write(iout,*) &
                  ' ?ps_init_neutral: ps_alloc_plasma_state error (ngsc0).'
             ierr=1
             return
          endif

          do ig=1,in0
             znam = ss%sgas_name(ig)
             ilen0=len(trim(znam))
             if(znam(ilen0-1:ilen0).eq.'_0') then
                ilen=ilen0-2
             else if(znam(ilen0:ilen0).eq.'0') then
                ilen=ilen0-1
             else
                ilen=ilen0
             endif
             
             ss%gs_name(ig) = znam(1:ilen0)//'rcy'       ! "H0rcy", etc.
             ss%gas_atom(ig) = znam(1:ilen)              ! "H", "D", "T" ...
             ss%is_recycling(ig) = 1
             
             ss%gs_name(in0+ig) = znam(1:ilen0)//'gf'    ! "H0gf", etc.
             ss%gas_atom(in0+ig) = znam(1:ilen)          ! "H", "D", "T" ...
             ss%is_recycling(in0+ig) = 0
          enddo
          
          call ps_gsc0_species_map(ierr, state=ss)
          if(ierr.ne.0) then
             write(iout,*) ' ?ps_init_neutral: ps_gsc0_species_map error!'
             return
          endif
       endif
    else if((2*ss%nspec_gas).ne.ss%ngsc0) then

       write(iout,*) ' ?ps_init_neutral: 2*ss%nspec_gas .ne. ss%ngsc0'
       write(iout,*) '  this code assumes exactly 2 sources per gas specie.'
       ierr=1

    endif

    if(ss%nspec_gas.ne.ngmax) then
       write(iout,*) ' %ps_init_neutral: ss%nspec_gas = ',ss%nspec_gas
       write(iout,*) '  trcom ngmax = ',ngmax,'; match was expected.'
       ierr = 1
    endif

    if(ierr.ne.0) return

  !---------------------------
  !  EDGE SOURCE
  !  neutral gas data
  !  ----------------
  !  ionization power per unit influx, for each recycling ion species
  !  charge exchange power per unit influx per keV difference (Ti-T0)
  !  effective neutral temperature T0 for charge exchange
  !  specie sources (incl. electrons) per unit influx of each species
  !    (CX -> influx of D leads to source/sink of H,D,T)

    ss%sc0 = ZERO  ! (#/s): recyc(1:ngmax)//gas_flow(1:ngmax)

    ss%e0_av = ZERO
    ss%vphi0_av = ZERO
    
    ss%sprof0 = ZERO
    ss%sprof0e = ZERO
    ss%qioniz = ZERO
    ss%tqioniz = ZERO
    ss%qqcx = ZERO
    ss%tqqcx = ZERO
    ss%t0cx = ZERO
    ss%omeg0cx = ZERO
    
    ss%n0norm = ZERO
    ss%T0sc0 = ZERO
    ss%omeg0sc0 = ZERO


  !---------------------------
  !  RECOMBINATION SOURCE neutral gas data

    ss%s0reco = ZERO
    ss%s0reco_recap = ZERO
    ss%s0reco_e = ZERO
    
    ss%p0_reco = ZERO
    ss%psc_reco = ZERO
    ss%psc_reco = ZERO
    
    ss%tq0_reco = ZERO
    ss%tqsc_reco = ZERO
    ss%tqcx_reco = ZERO
    
    ss%n0_reco = ZERO
    ss%T0_reco = ZERO
    ss%omeg0_reco = ZERO
    

  end subroutine ps_init_neutral
  !==========================================================================
  SUBROUTINE ps_gsc0_species_map(ierr, state)

    !-------------------------------------------------------------------
    !
    ! PS_GSC0_SPECIES_MAP
    !
    ! From the character* array state%gas_atom(1:state%ngsc0) compute the 
    ! index map sc0_to_sgas(ngsc0)
    !
    !   expected gas_atom values are "H", "D", "T", "HE3" or "HE4"
    !
    ! Error checks:  this routine checks that the each species in gas_atom
    !    corresponds to a species in the sgas list.  Species lists should
    !    be set up (and ps_neutral_species called) before calling this 
    !    routine.
    !
    !-------------------------------------------------------------------
    !
    !   Arguments:

    integer, intent(out) :: ierr  ! exit status code (0=OK)
    
    TYPE (plasma_state), optional, target :: state

    !  Care should be taken that the neutral gas model used in the
    !  simulation can handle the value of Z0max chosen.

    !----------------------------------------------------------------------
    !
    !   Declare local variables
    !
    !----------------------------------------------------------------------

    integer :: ii,jj,imatch,ingas,insc0,iout,ilen,iertot

    character*10 ztest1,ztest2

    TYPE (plasma_state), pointer :: ss

    !----------------------------------------------------------------------

    ierr = 0
    iout = Swim_err_out_file

    !  handle optional arguments

    if(present(state)) then
       ss => state
    else
       ss => ps
    endif

    ingas = ss%nspec_gas
    insc0 = ss%ngsc0

    if(ingas.eq.0) then
       write(iout,*) ' ?ps_gsc0_species_map: state%nspec_gas = 0'
       write(iout,*) '  neutral gas species have not been defined.'
       ierr=999
       return
    endif

    if(.not.allocated(ss%sgas_name)) then
       write(iout,*) ' ?ps_gsc0_species_map: state SGAS species list not allocated.'
       ierr=999
       return
    endif

    if(insc0.eq.0) then
       write(iout,*) ' ?ps_gsc0_species_map: state%ngsc0 = 0'
       write(iout,*) '  neutral gas sources have not been defined.'
       ierr=999
       return
    endif

    if(.not.allocated(ss%gas_atom)) then
       write(iout,*) ' ?ps_gsc0_species_map: state GAS_ATOM array not allocated.'
       ierr=999
       return
    endif

    iertot = 0

    do ii=1,insc0
       ztest1=ss%gas_atom(ii)
       call uupper(ztest1)
       ilen=len(trim(ztest1))

       !  naming scheme to match that of ps_neutral_species routine

       if(ilen.eq.1) then 
          ztest1 = ztest1(1:ilen)//'0'
       else
          ztest1 = ztest1(1:ilen)//'_0'
       endif

       imatch = 0
       do jj=1,ingas
          ztest2 = ss%sgas_name(jj)
          call uupper(ztest2)
          if(ztest1.eq.ztest2) then
             imatch=jj
             exit
          endif
       enddo

       ss%sc0_to_sgas(ii) = imatch

       if(imatch.eq.0) then
          write(iout,*) ' ?ps_gsc0_species_map: no match found for neutral species "'//trim(ss%gas_atom(ii))//'".'
          iertot=iertot + 1
       endif
    enddo

    if(iertot.gt.0) ierr=99

  END SUBROUTINE ps_gsc0_species_map

  SUBROUTINE ps_chk_nbfrac(ichk, ierr, state)

    !-------------------------------------------------------------------
    !
    ! PS_CHK_NBFRAC -- utility routine for beam energy fractions
    !
    ! For He or negative_ion beams (ib): set ss%frac_full(ib)=1 and
    !     ss%frac_half(ib)=0:  these beams are at 100% full energy
    !
    ! For H/D/T beams with ss%beam_type(ib).eq."standard":
    !   if ss%nbion(ib).eq."H" check H energy fraction tables
    !   if ss%nbion(ib).eq."D" check H energy fraction tables
    !   if ss%nbion(ib).eq."T" check H energy fraction tables
    !   ...and interpolate to get full, 1/2, and 1/3 energy fractions of
    !   beam current for each beam
    !
    !   if ichk=1 set the error code if a needed energy table has not been
    !   initialized; if ichk=0, do nothing in this case.
    
    !----------------------------------------------------------------------
    !
    !   Declare local variables
    !
    !----------------------------------------------------------------------

    integer, intent(in) :: ichk              ! =1 to set error code if
    !  energy fraction table(s) are not ready.

    integer, intent(out) :: ierr             ! exit status code (0=OK)

    !-------------------
    !  OPTIONAL variables...

    TYPE (plasma_state), target, OPTIONAL :: state   ! update *this* state
                                  ! the default is to update the "ps" state.

    !----------------------------------------------------------------------

    INTEGER :: ib,inb,iout
    TYPE (plasma_state), pointer :: ss
    character*32 ztype,zbtype

    !----------------------------------------------------------------------

    ierr = 0
    iout = SWIM_err_out_file

    if(present(state)) then
       ss => state
    else
       ss => ps
    endif

    inb = ss%nbeam
    if(inb.eq.0) return

    do ib=1,inb
       ztype = ss%beam_type(ib)
       call uupper(ztype)

       if(ztype.eq.'NEGATIVE_ION') then
          ss%frac_full(ib)=1.0_rspec
          ss%frac_half(ib)=0.0_rspec

       else if(ztype.eq.'IMPURITY') then
          ss%frac_full(ib)=1.0_rspec
          ss%frac_half(ib)=0.0_rspec

       else if(ztype.eq.'HELIUM') then
          ss%frac_full(ib)=1.0_rspec
          ss%frac_half(ib)=0.0_rspec

       else if(ztype.eq.'STANDARD') then

          zbtype = ss%nbion(ib)  ! beam species
          call uupper(zbtype)

          if(zbtype.eq.'H') then
             if(allocated(ss%H_ffull_standard)) then
                call fset(ss%kvolt_nbi(ib),ss%n_H_einj_standard, &
                     ss%H_einj_standard, &
                     ss%H_ffull_standard, ss%H_fhalf_standard, &
                     ss%frac_full(ib), ss%frac_half(ib))
             else
                if(ichk.eq.1) then
                   ierr = ierr + 1
                   write(iout,*) ' ?ps_chk_nbfrac: "H" beam detected but energy fraction tables not set up.'
                endif
             endif

          else if(zbtype.eq.'D') then
             if(allocated(ss%D_ffull_standard)) then
                call fset(ss%kvolt_nbi(ib),ss%n_D_einj_standard, &
                     ss%D_einj_standard, &
                     ss%D_ffull_standard, ss%D_fhalf_standard, &
                     ss%frac_full(ib), ss%frac_half(ib))
             else
                if(ichk.eq.1) then
                   ierr = ierr + 1
                   write(iout,*) ' ?ps_chk_nbfrac: "D" beam detected but energy fraction tables not set up.'
                endif
             endif

          else if(zbtype.eq.'T') then
             if(allocated(ss%T_ffull_standard)) then
                call fset(ss%kvolt_nbi(ib),ss%n_T_einj_standard, &
                     ss%T_einj_standard, &
                     ss%T_ffull_standard, ss%T_fhalf_standard, &
                     ss%frac_full(ib), ss%frac_half(ib))
             else
                if(ichk.eq.1) then
                   ierr = ierr + 1
                   write(iout,*) ' ?ps_chk_nbfrac: "T" beam detected but energy fraction tables not set up.'
                endif
             endif

          else if(zbtype.eq.'HE3') then
             ss%frac_full(ib)=1.0_rspec
             ss%frac_half(ib)=0.0_rspec
             
          else if(zbtype.eq.'HE4') then
             ss%frac_full(ib)=1.0_rspec
             ss%frac_half(ib)=0.0_rspec
             
          else
             ierr = ierr + 1
             write(iout,*) ' ?ps_chk_nbfrac:  beam #',ib,' species (NBION value) invalid: "'//trim(zbtype)//'".'
          endif

       else
          ierr = ierr + 1
          if(ztype.eq.' ') then
             write(iout,*) ' ?ps_chk_nbfrac:  beam #',ib,' beam_type not set.'
          else
             write(iout,*) ' ?ps_chk_nbfrac:  beam #',ib,' beam_type unknown: ',trim(ztype)
          endif
       endif

    enddo

    CONTAINS
      subroutine fset(ekev,ntabl,etabl,ffull_tabl,fhalf_tabl,ffull,fhalf)

        REAL(KIND=rspec), intent(in) :: ekev
        INTEGER, intent(in) :: ntabl
        REAL(KIND=rspec), intent(in) :: etabl(ntabl)
        REAL(KIND=rspec), intent(in) :: ffull_tabl(ntabl)
        REAL(KIND=rspec), intent(in) :: fhalf_tabl(ntabl)

        REAL(KIND=rspec), intent(out) :: ffull
        REAL(KIND=rspec), intent(out) :: fhalf

        !------------------------
        integer :: it
        REAL(KIND=rspec) :: zf
        !------------------------

        do it=1,ntabl-1
           if(etabl(it+1).le.etabl(it)) then
              write(iout,*) ' ?ps_chk_nbfrac: error in beam fraction table'
              write(iout,*) '  Energies not strict ascending: ',etabl
              ffull = 0.0_rspec
              fhalf = 0.0_rspec
              ierr = ierr + 1
              exit
           endif
        enddo
        if(ierr.ne.0) return

        if(ekev.le.etabl(1)) then
           ffull = ffull_tabl(1)
           fhalf = fhalf_tabl(1)

        else if(ekev.gt.etabl(ntabl)) then
           ffull = ffull_tabl(ntabl)
           fhalf = fhalf_tabl(ntabl)

        else
           do it=1,ntabl-1
              if((etabl(it).le.ekev).and.(ekev.le.etabl(it+1))) then
                 zf = (ekev-etabl(it))/(etabl(it+1)-etabl(it))

                 ffull = ffull_tabl(it) + &
                      zf*(ffull_tabl(it+1)-ffull_tabl(it))

                 fhalf = fhalf_tabl(it) + &
                      zf*(fhalf_tabl(it+1)-fhalf_tabl(it))
                 exit
              endif
           enddo
        endif

      end subroutine fset

  END SUBROUTINE ps_chk_nbfrac
  !==========================================================================
  SUBROUTINE ps_update_equilibrium(ierr, g_filepath, bdy_crat, &
       kcur_option, rho_curbrk, pprime_option, rho_psmoo, delrho_psmoo, &
       state)

    !-------------------------------------------------------------------
    !
    ! PS_UPDATE_EQUILIBRIUM
    !
    ! update MHD equilibrium from a G-EQdsk file; update associated
    ! state profile quantities (e.g. metrics and flux surface averages).
    !
    ! if g_filepath is omitted, use trim(state_path)//state%eqdsk_file
    !-------------------------------------------------------------------
    
    !----------------------------------------------------------------------
    !
    !   Declare local variables
    !
    !----------------------------------------------------------------------

    integer, intent(out) :: ierr             ! exit status code (0=OK)

    !-------------------
    !  OPTIONAL variables...

    character*(*), intent(in), OPTIONAL :: g_filepath
                                  ! G-eqdsk filename or MDS+ path
                                  ! (override eqdsk path in state itself)

    real(kind=rspec), intent(in), OPTIONAL :: bdy_crat   ! bdy curvature ratio
                                  ! (default 0.080) minimum allowed ratio
                                  ! of the smallest local radius of 
                                  ! curvature to the midplane half width
                                  ! of the boundary surface (see more
                                  ! comments below).

    integer, intent(in), OPTIONAL :: kcur_option  ! control to select method
                                  ! for obtaining q(rho) and I(rho) enclosed
                                  ! toroidal current function (details below).

                                  ! Default = -1: choose based on file label;

                                  !   =0: use EFIT file data for total current
                                  !       and q(psi) profile

                                  !   =1: compute total current and q(psi)
                                  !       profile from Psi(R,Z); supersede
                                  !       EFIT file data for these quantities.

    real(kind=rspec), intent(in), OPTIONAL :: rho_curbrk ! break location for
                                  ! I(rho) -- point at which to transition
                                  ! from Psi(R,Z) Ampere's Law integration 
                                  ! to a parabolic interpolation to reach
                                  ! the total enclosed current (boundary 
                                  ! value). Default = 0.9_rspec.

    logical, intent(in), OPTIONAL :: pprime_option  ! .TRUE. to request 
                                  ! recomputation of pressure profile using
                                  ! PPRIME data from G-eqdsk input file;
                                  ! default is .FALSE.

    real(kind=rspec), dimension(:), intent(in), OPTIONAL :: rho_psmoo
                                  ! rho location(s) for pressure profile
                                  ! smoothing array delrho_psmoo; if this
                                  ! is present, delrho_psmoo(...) must
                                  ! also be present, and, sizes must match.

    real(kind=rspec), dimension(:), intent(in), OPTIONAL :: delrho_psmoo
                                  ! array of smoothing parameters for pressure
                                  ! profile; together with rho_psmoo this
                                  ! defines a profile to be piecewise linearly
                                  ! interpolated to produce local delta_rho
                                  ! values for smoothing.  At each rho location
                                  ! the pressure profile is smoothed with a 
                                  ! triangular weighted average of neighboring
                                  ! data, from rho-delta_rho to rho+delta_rho.
                                  ! if delrho_psmoo(...) is present, the array
                                  ! rho_psmoo(...) must also be present, and, 
                                  ! sizes must match.

    TYPE (plasma_state), target, OPTIONAL :: state   ! update *this* state
                                  ! the default is to update the "ps" state.

    !----------
    !  more on bdy_crat:  this controls the boundary selection, by providing
    !  an adaptive means of selecting a boundary that is a safe step in from
    !  the last closed flux surface in case of a separatrix.  The boundary
    !  surface shape kink at a separatrix has local radius of curvature of
    !  zero; surfaces just in from this have increasing minimum local radii
    !  of curvature, the further the surface is from the separatrix.  The
    !  underlying software has been used for years for selection of boundaries
    !  for TRANSP use, starting from EFIT data; the default value of 0.080 is
    !  safe in this application, and yet yields a surface that is usually 
    !  between 0.98 and 0.999 (in sqrt(tor.flux)) of the LCFS.
    !
    !  Some applications may need a somewhat larger value of bdy_crat.  A 
    !  smaller value should only be used with extreme caution.
    !  The range of accepted values of bdy_crat are:  0.01d0 to 0.15d0;
    !  the silently applies a min & max to enforce this range.  A zero or
    !  negative value is reported as an error.

    !----------
    !  more on kcur_option:
    !
    !  We have found that not all EFIT datasets are created equal.  Some 
    !  contain high quality free boundary GS solutions; others (e.g. from
    !  TRXPL/TRANSP time slices) are interpolations of prescribed boundary
    !  GS solutions with smoothed extrapolation to fill the EFIT [R,Z]
    !  rectangular domain.  In the latter case, grad(Psi) near the plasma
    !  boundary is subject to errors.  So...
    !
    !  set kcur_option = -1 (the default) to read the g_filename file label
    !  to determine processing:  
    !     If label says "TRXPL" use icur_option = 0; take total current and
    !     q(psi) from the EFIT G-eqdsk data;
    !
    !     Otherwise use icur_option = 1, compute total enclosed current
    !     and q(psi) from contour integrals over Psi(R,Z)
    !
    !  set kcur_option = 0 => icur_option = 0, to use the EFIT data
    !     for q(psi) and total current, regardless of file labeling.
    !     An Ampere's law expression is still used for I(rho) toroidal
    !     enclosed by interior surfaces; at rho_curbrk transition to a
    !     parabolic interpolation to reach the EFIT specified boundary
    !     value.
    !
    !  set kcur_option = 1 => icur_option = 1, to recompute q(psi) and the
    !     boundary total enclosed current, as well as the I(rho) within
    !     interior surfaces, using contour integrals (Ampere's Law) on
    !     the EFIT Psi(R,Z) data.
    !     From rho=rho_curbrk to rho=1 a parabolic interpolation to the
    !     computed boundary value is used, to obtain a smooth dI/drho
    !     and <J.B> profile in the edge region.
    
    !----------

    TYPE (plasma_state), pointer :: ss
    character*256 :: gfile
    character*80 :: hline
    integer :: ilen

    integer :: icur_option, io_unit, istat, iout, isize
    real(kind=rspec) :: rho_curbreak

    real(kind=rspec) :: crat

    integer :: ipprime,ismoo
    real(kind=rspec), dimension(:), allocatable :: rho_smoo,delrho_smoo

    character*4 :: zmds

    !----------------------------------------------------------------------
    !
    !   Update MHD equilibrium in current state
    !
    !----------------------------------------------------------------------

    CALL ps_init_tag

    iout = SWIM_err_out_file

    !-------------------------------------
    !   bdy_crat option

    if(present(bdy_crat)) then
       crat = bdy_crat
       if(crat.le.0.0d0) then
          write(iout,*) &
               'PS_UPDATE_EQUILIBRIUM: bdy_crat argument not > 0.0: ',crat
          ierr=999
          return
       endif
    else
       crat = 0.080d0
    endif

    !-------------------------------------
    !   state optional argument

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    !-------------------------------------
    !   g file path optional argument

    if(present(g_filepath)) then
       gfile = g_filepath
    else
       if(state_path.eq.' ') then
          gfile = ss%eqdsk_file
       else
          ilen=len(trim(state_path))
          if(state_path(ilen:ilen).eq.'/') then
             gfile = trim(state_path)//trim(ss%eqdsk_file)
          else
             gfile = trim(state_path)//'/'//trim(ss%eqdsk_file)
          endif
       endif
    endif

    !-------------------------------------
    !   optional arguments related to current profile and q(rho)

    icur_option = -1
    if(present(kcur_option)) then
       icur_option = max(-1, min(1, kcur_option))
    endif

    rho_curbreak = 0.9_rspec
    if(present(rho_curbrk)) then
       rho_curbreak = max(0.75_rspec, min(0.95_rspec, rho_curbrk))
    endif

    !-------------------------------------
    !   optional arguments related to pressure profile

    ipprime=0
    if(present(pprime_option)) then
       if(pprime_option) ipprime=1
    endif

    if(present(delrho_psmoo).or.present(rho_psmoo)) then
       if(.not.(present(delrho_psmoo).AND.present(rho_psmoo))) then
          write(iout,*) ' ?ps_update_equilibrium:  if ONE of these optional arrays is present, BOTH'
          write(iout,*) '  must be present:'
          if(present(delrho_psmoo)) then
             write(iout,*) '     DELRHO_PSMOO: present.'
          else
             write(iout,*) '     DELRHO_PSMOO: absent.'
          endif
          if(present(rho_psmoo)) then
             write(iout,*) '     RHO_PSMOO: present.'
          else
             write(iout,*) '     RHO_PSMOO: absent.'
          endif
          ierr=9999
          return
       endif

       !  both present; check sizes
       if(size(rho_psmoo).ne.size(delrho_psmoo)) then
          write(iout,*) ' ?ps_update_equilibrium: size mismatch in optional arrays:'
          write(iout,*) '  size(RHO_PSMOO)    = ',size(rho_psmoo)
          write(iout,*) '  size(DELRHO_PSMOO) = ',size(delrho_psmoo)
          write(iout,*) '  ...these sizes must be identical.'
          ierr=9999
          return
       endif

       isize=size(rho_psmoo)
       allocate(rho_smoo(isize),delrho_smoo(isize))
       rho_smoo = rho_psmoo
       delrho_smoo = delrho_psmoo

    else
       allocate(rho_smoo(1),delrho_smoo(1))
       isize=1
       rho_smoo=0.0_rspec
       delrho_smoo=0.0_rspec
    endif

    !-------------------------------------
    !  open gfile, read 1st line as char string

    zmds=gfile(1:4)
    call uupper(zmds)

    if(zmds.ne.'MDS+') then
       !  assume a file; read header line to determine processing option...
       call find_io_unit(io_unit)
       open(unit=io_unit,file=trim(gfile),status='old',action='read', &
            iostat=istat)
       if(istat.ne.0) then
          write(iout,*) &
               'PS_UPDATE_EQUILIBRIUM: gfile open failure!'
          write(iout,*) '  name of gfile: '//trim(gfile)
          ierr=1
          return
       endif

       read(io_unit,'(A)') hline
       close(unit=io_unit)

       if(icur_option.eq.-1) then
          if((hline(1:5).eq.'TRXPL').or.(hline(1:5).eq.'TRXfb')) then
             icur_option = 0  ! use q(psi) and I @ bdy
          endif
       endif
    endif

    !  mod DMC: following test modified to support MDS+ EFIT records...
    !  RGA, moved here so TRXPL will use TRANSP data

    if (icur_option.eq.-1) icur_option = 1   ! use Psi(R,Z) to recompute q(psi) & I @ bdy (default)

    !-------------------------------------
    !  read in the gfile equilibrium

    CALL ps_update_mhdeq(gfile,crat,icur_option,rho_curbreak,ss, &
         iout,ierr)
    IF(ierr.NE.0) THEN
       write(iout,*) &
            'PS_UPDATE_EQUILIBRIUM: error in ps_update_mhdeq call!'
       return
    ENDIF

    !-------------------------------------
    !  apply optional pressure profile treatments

    call ps_mhdeq_pchk(ss,ipprime,isize,rho_smoo,delrho_smoo,iout,ierr)
    IF(ierr.NE.0) THEN
       write(iout,*) &
            'PS_UPDATE_EQUILIBRIUM: error in ps_mhdeq_pchk call!'
       return
    ENDIF

    !-------------------------------------
    !  finish up

    CALL ps_state_memory_update(ierr, state=ss)
    IF(ierr.NE.0) THEN
       write(iout,*) &
            'PS_UPDATE_EQUILIBRIUM: error in ps_state_memory_update call!'
       return
    ENDIF

  END SUBROUTINE ps_update_equilibrium

  !==========================================================================
  SUBROUTINE ps_wr_geqdsk(ierr, g_filepath, state)

    ! write geqdsk file based on contents of plasma state
    ! the state must contain:  Psi(R,Z), [R,Z] vs. (rho,theta),
    ! q profile, g profile, P profile.

    ! The grids ps%nR, ps%nZ, ps%rho_eq, ps%th_eq must be defined.

    integer, intent(out) :: ierr   ! status code, 0=OK

    character*(*), intent(in), OPTIONAL :: g_filepath
                                  ! G-eqdsk filename or MDS+ path
                                  ! (override eqdsk_file in state itself)
                                  ! default is trim(state_path)//
                                  !   trim(state%eqdsk_file)

    TYPE (plasma_state), target, OPTIONAL :: state   ! update *this* state
                                  ! the default is to update the "ps" state.

    !--------------------
    character*240 :: gfile
    integer :: ilen,iout

    TYPE (plasma_state), pointer :: ss
    !--------------------

    CALL ps_init_tag

    ierr = 0
    iout = Swim_err_out_file

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    if(present(g_filepath)) then
       gfile = g_filepath
    else
       if(state_path.eq.' ') then
          gfile = ss%eqdsk_file
       else
          gfile = trim(state_path)//'/'//trim(ss%eqdsk_file)
       endif
    endif

    call ps_state_memory_update(ierr, state=ss)
    if(ierr.ne.0) then
       write(iout,*) ' ?ps_wr_geqdsk: ps_state_memory_update checks failed.'
       return
    endif

    call ps_wr_geqdsk_file(ss,gfile,iout,ierr)

  end SUBROUTINE ps_wr_geqdsk

  !==========================================================================
  SUBROUTINE ps_update_rzsurf(Rarr2d,Zarr2d,idim_rho, ierr, &
       state, ibcR0,zbcR0, ibcR1,zbcR1, ibcZ0,zbcZ0, ibcZ1,zbcZ1)

    !-------------------------------------------------------------------
    !
    ! PS_UPDATE_RZSURF
    !
    ! update {R(rho,theta),Z(rho,theta)} flux surface geometry from the
    ! data provided, optionally with boundary conditions.
    !
    !   idim_rho=1 means: arrays are dimensioned in order (rho,theta)
    !                     i.e. rho is the first dimension.
    !
    !   idim_rho=2 means: arrays are dimensioned in order (theta,rho)
    !                     i.e. theta is the first dimension.
    !-------------------------------------------------------------------
    
    !----------------------------------------------------------------------
    !
    !   Declare passed variables
    !
    !----------------------------------------------------------------------

    REAL(kind=rspec), dimension(:,:), intent(in) :: Rarr2d,Zarr2d
                                             ! R and Z surface data

    integer, intent(in) :: idim_rho          ! specify dimension order
                                             ! see description above

    integer, intent(out) :: ierr             ! exit status code (0=OK)

    !-------------------
    !  OPTIONAL variables...

    TYPE (plasma_state), target, OPTIONAL :: state   ! update *this* state
                                  ! the default is to update the "ps" state.

    INTEGER, intent(in), optional :: ibcR0   ! R bdy condition at rho=0
    !  default: 0 for "not a knot"; 1 for d/drho BC; 2 for d2/drho2 BC
    REAL(kind=rspec), dimension(:), intent(in), optional :: zbcR0
    !  rho=0 R bdy condition data, default 0.0

    INTEGER, intent(in), optional :: ibcR1   ! R bdy condition at rho=1
    !  default: 0 for "not a knot"; 1 for d/drho BC; 2 for d2/drho2 BC
    REAL(kind=rspec), dimension(:), intent(in), optional :: zbcR1
    !  rho=1 R bdy condition data, default 0.0

    INTEGER, intent(in), optional :: ibcZ0   ! Z bdy condition at rho=0
    !  default: 0 for "not a knot"; 1 for d/drho BC; 2 for d2/drho2 BC
    REAL(kind=rspec), dimension(:), intent(in), optional :: zbcZ0
    !  rho=0 Z bdy condition data, default 0.0

    INTEGER, intent(in), optional :: ibcZ1   ! Z bdy condition at rho=1
    !  default: 0 for "not a knot"; 1 for d/drho BC; 2 for d2/drho2 BC
    REAL(kind=rspec), dimension(:), intent(in), optional :: zbcZ1
    !  rho=1 Z bdy condition data, default 0.0

    !-------------------
    !  local variables
    !-------------------

    real(kind=rspec), dimension(:), allocatable :: bcR0,bcR1,bcZ0,bcZ1
    integer :: jbcR0,jbcR1,jbcZ0,jbcZ1

    TYPE (plasma_state), pointer :: ss

    integer :: idim1,idim2,iertot,iout,inrho,inth

    real(kind=rspec), parameter :: zsmall = 1.0e-12_rspec
    real(kind=rspec), parameter :: z2pi   = 6.2831853071795862D+00

    logical :: iexist
    !----------------------------------------------------------------------

    ierr = 0
    iout = Swim_err_out_file

    CALL ps_init_tag

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    ! check that equilibrium (rho_eq) and (th_eq) grids are defined.
    ! these must be defined BEFORE calling this routine.

    iertot = 0

    inrho=ss%nrho_eq
    inth =ss%nth_eq

    if(.not.allocated(ss%rho_eq)) then
       iertot=iertot+1
       write(iout,*) ' ?PS_UPDATE_RZSURF: ss%rho_eq unallocated.'
    else if(abs(ss%rho_eq(inrho)-1.0_rspec).gt.zsmall) then
       iertot=iertot+1
       write(iout,*) ' ?PS_UPDATE_RZSURF: ss%rho_eq values not set.'
    endif

    if(.not.allocated(ss%th_eq)) then
       iertot=iertot+1
       write(iout,*) ' ?PS_UPDATE_RZSURF: ss%rho_eq unallocated.'
    else if(abs(ss%th_eq(inth)-ss%th_eq(1)-z2pi).gt.zsmall) then
       iertot=iertot+1
       write(iout,*) ' ?PS_UPDATE_RZSURF: ss%th_eq values not set.'
    endif

    if(iertot.ne.0) then
       ierr=1
       return
    endif

    ! check array dimensions; these must be consistent with the sizes
    ! of the (rho_eq) and (th_eq) grids.

    if(idim_rho.ge.2) then
       idim1=inth
       idim2=inrho
    else
       idim1=inrho
       idim2=inth
    endif

    if((size(Rarr2d,1).ne.idim1).or.(size(Rarr2d,2).ne.idim2)) then
       iertot=iertot+1
       write(iout,*) ' ?PS_UPDATE_RZSURF: Rarr2d argument dimensioning error.'
       write(iout,*) '     size(Rarr2d,1) = ',size(Rarr2d,1), &
            '; expected: ',idim1
       write(iout,*) '     size(Rarr2d,2) = ',size(Rarr2d,2), &
            '; expected: ',idim2
    endif

    if((size(Zarr2d,1).ne.idim1).or.(size(Zarr2d,2).ne.idim2)) then
       iertot=iertot+1
       write(iout,*) ' ?PS_UPDATE_RZSURF: Zarr2d argument dimensioning error.'
       write(iout,*) '     size(Zarr2d,1) = ',size(Zarr2d,1), &
            '; expected: ',idim1
       write(iout,*) '     size(Zarr2d,2) = ',size(Zarr2d,2), &
            '; expected: ',idim2
    endif

    if(iertot.gt.0) then
       if(idim_rho.ge.2) then
          write(iout,*) '  Dimensioning (ss%nrho_eq,ss%nth_eq) was expected.'
       else
          write(iout,*) '  Dimensioning (ss%nth_eq,ss%nrho_eq) was expected.'
       endif
       ierr=1
       return
    endif

    ! required arguments seem OK.  Check optional arguments.
    ! any supplied boundary condition data dimensioning must match (th_eq)

    jbcR0=0
    jbcR1=0

    jbcZ0=0
    jbcZ1=0

    if(present(ibcR0)) jbcR0=ibcR0
    if(present(ibcR1)) jbcR1=ibcR1

    if(present(ibcZ0)) jbcZ0=ibcZ0
    if(present(ibcZ1)) jbcZ1=ibcZ1

    if(present(zbcR0)) then
       if(size(zbcR0).ne.inth) then
          write(iout,*) ' ?PS_UPDATE_RZSURF: zbcR0 boundary condition array', &
               ' size=',size(zbcR0)
          write(iout,*) '     does not match required value ss%nth_eq: ',inth
          iertot = iertot + 1
       endif
    endif

    if(present(zbcR1)) then
       if(size(zbcR1).ne.inth) then
          write(iout,*) ' ?PS_UPDATE_RZSURF: zbcR1 boundary condition array', &
               ' size=',size(zbcR1)
          write(iout,*) '     does not match required value ss%nth_eq: ',inth
          iertot = iertot + 1
       endif
    endif

    if(present(zbcZ0)) then
       if(size(zbcZ0).ne.inth) then
          write(iout,*) ' ?PS_UPDATE_RZSURF: zbcZ0 boundary condition array', &
               ' size=',size(zbcZ0)
          write(iout,*) '     does not match required value ss%nth_eq: ',inth
          iertot = iertot + 1
       endif
    endif

    if(present(zbcZ1)) then
       if(size(zbcZ1).ne.inth) then
          write(iout,*) ' ?PS_UPDATE_RZSURF: zbcZ1 boundary condition array', &
               ' size=',size(zbcZ1)
          write(iout,*) '     does not match required value ss%nth_eq: ',inth
          iertot = iertot + 1
       endif
    endif

    !  OK -- all optional arguments OK -- create local copies of BC data

    if(iertot.ne.0) then
       ierr=1
       return
    endif

    allocate(bcR0(inth))
    allocate(bcR1(inth))

    allocate(bcZ0(inth))
    allocate(bcZ1(inth))

    if(present(zbcR0)) then
       bcR0=zbcR0
    else
       bcR0=0.0_rspec
    endif

    if(present(zbcR1)) then
       bcR1=zbcR1
    else
       bcR1=0.0_rspec
    endif

    if(present(zbcZ0)) then
       bcZ0=zbcZ0
    else
       bcZ0=0.0_rspec
    endif

    if(present(zbcZ1)) then
       bcZ1=zbcZ1
    else
       bcZ1=0.0_rspec
    endif

    !---------------------------------
    !  verify that underyling xplasma representation setup is complete;
    !  if not try to complete it.

    call ps_grid_exist(ss%iobj,'RHO_EQ',iout,iexist)

    if(.not.iexist) then
       ! this is somewhat unusual -- update all hash codes; call ps_state_xload
       call ps_update_hashcodes(ierr, state=ss)
       if(ierr.eq.0) then
          call plasma_state_xload(ss,ierr)
          if(ierr.eq.0) then
             call ps_grid_exist(ss%iobj,'RHO_EQ',iout,iexist)  ! try again
          endif
       endif
    endif

    if(.not.iexist) then
       write(iout,*) ' ?PS_UPDATE_RZSURF: interpolation object setup failure.'
       write(iout,*) '  Grid (Rho_eq) could not be verified.'
       ierr=1
    endif

    !---------------------------------
    !  now transfer the surface data
    !  the internal representation is converted to bicubic hermite with
    !  explicit derivatives; therefore the entire state object is passed.
    !---------------------------------

    if(ierr.eq.0) then

       call ps_rzsurf_sdata(ss, idim1,idim2, Rarr2d,Zarr2d, idim_rho, inth, &
            jbcR0,bcR0, jbcR1,bcR1, jbcZ0,bcZ0, jbcZ1,bcZ1, iout,ierr)

    endif

    !  cleanup...

    deallocate(bcR0,bcR1,bcZ0,bcZ1)
          

  END SUBROUTINE ps_update_rzsurf

  !==========================================================================
  SUBROUTINE ps_mhdeq_verify(ierr, state, reltol, rhowk, &
       update_phit, update_psi, update_q, check_phit, check_q, rho_check, &
       pcur, eqcheck_RZ, relerr_max)

    !-------------------------------------------------------------------
    !
    ! PS_MHDEQ_VERIFY
    !
    ! Verify and/or update profiles related to magnetic flux.
    !
    ! Verify e.g. that profile of enclosed toroidal flux state%phit(:) is 
    ! consistent with definition of rho coordinate, i.e. that the relation
    !
    !     [Tor.flux] = rho**2*[Tor.flux.at.bdy]
    !
    ! is satisfied to reasonable accuracy (2% or value of optional argument
    ! reltol).
    !
    ! If flags are set, actually update the profiles state%phit(:),
    ! state%psipol(:), and/or state%q_eq(:).
    !    Note#1: all update controls if defaulted are taken as .FALSE.
    !    Note#2: update of Psipol depends on q_eq; update of q_eq depends
    !            on Psipol.  Therefore update_psi and update_q cannot BOTH
    !            be .TRUE. (an error would be reported).
    !
    ! if (check_q) is set, check consistency of q_eq and Psipol profiles.
    !              this is the default.
    !
    ! Note: consistency of MHDEQ pressure profile NOT checked.  This routine
    !       does not check the self-consistency of the Grad-Shafranov MHD
    !       equilibrium in the state.  Only the fluxes are checked.
    !
    ! If the check reveals no error, after any requested profiles are 
    ! updated, internal splines representing equilibrium field quantities
    ! are updated; this allows all PS_MHDEQ_DERIVE actions to be carried
    ! out.
    !
    ! Profiles of current (state%curt, state%jdotb) are updated IF the
    ! optional argument PCUR is present, specifying the total toroidal
    ! current in amps.
    !-------------------------------------------------------------------
    
    !----------------------------------------------------------------------
    !
    !   Declare local variables
    !
    !----------------------------------------------------------------------

    integer, intent(out) :: ierr             ! exit status code (0=OK)

    !-------------------
    !  OPTIONAL variables...

    TYPE (plasma_state), target, OPTIONAL :: state   ! update *this* state
                                  ! the default is to update the "ps" state.

    REAL(kind=rspec), intent(IN), OPTIONAL :: reltol ! error tolerance for
                                  ! toroidal flux check (default: 2%).

    REAL(kind=rspec), dimension(:), intent(IN), OPTIONAL :: rhowk
                                  ! working grid for <1/R^2> integration
                                  ! if defaulted use state%rho_eq_geo.
                                  ! some codes may want a coarser grid for
                                  ! performance reasons.

    ! all LOGICAL optional controls default to .FALSE.:

    LOGICAL, intent(IN), OPTIONAL :: update_phit ! update toroidal flux profile
                                                 ! state%phit(:)

    LOGICAL, intent(IN), OPTIONAL :: update_psi  ! update poloidal flux profile
                                                 ! state%psipol(:)

    LOGICAL, intent(IN), OPTIONAL :: update_q    ! update q profile
                                                 ! state%q_eq(:)

    LOGICAL, intent(IN), OPTIONAL :: check_phit  ! check consistency of
                                                 ! toroidal flux profile
                                                 ! default: .TRUE.

    LOGICAL, intent(IN), OPTIONAL :: check_q     ! check consistency of q
                                                 ! and psipol profiles
                                                 ! default: .TRUE.

    REAL(kind=rspec), intent(in), OPTIONAL :: rho_check  ! suppress checks
                                                 ! for rho < rho_check
                                                 ! (default rho_check=0.1)

    REAL(kind=rspec), intent(in), OPTIONAL :: pcur  ! total plasma current
                                  ! if PCUR is provided, the profiles of
                                  ! enclosed current (state%curt) and of
                                  ! parallel current * B (state%jdotb) are 
                                  ! updated.

    LOGICAL, intent(IN), OPTIONAL :: eqcheck_RZ  ! .FALSE. to suppress [R,Z]
                                                 ! free bdy equilibrium check
                                                 ! (default is to do the check)

    REAL(kind=rspec), intent(out), OPTIONAL :: relerr_max  ! maximum rel. error
                                                 ! in phit or q tests
    ! (relerr_max evaluation bounded away from magnetic axis)

    !-------------------
    !  local variables
    !-------------------

    TYPE (plasma_state), pointer :: ss

    real(kind=rspec), parameter :: zsmall = 1.0e-12_rspec
    real(kind=rspec), parameter :: z2pi   = 6.2831853071795862D+00

    integer :: iout,inrho,inth,iertot
    real(kind=rspec) :: ztol,zdrho,zsum,zsump,zerr,zerrmax,zdpsimin,zrho_brk
    real(kind=rspec) :: dphit,dpsi,zdenom,zrho_check

    logical :: iupdate_phit,iupdate_psi,iupdate_q,icheck_q,icheck_phit
    logical :: iupdate_curs,icheck_RZ

    integer, parameter :: imul = 10
    integer :: nrho_test,inzc,ict,icti,ii

    real(kind=rspec), dimension(:), allocatable :: zrho, zrhozc
    real(kind=rspec), dimension(:), allocatable :: zg,zr2i,zdvdrho
    real(kind=rspec), dimension(:), allocatable :: zphit,zqnew,zpsi_new
    real(kind=rspec), dimension(:), allocatable :: zq,zpsi_test
    !----------------------------------------------------------------------

    ierr = 0
    if(present(relerr_max)) relerr_max=0.0_rspec

    iout = Swim_err_out_file

    CALL ps_init_tag

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    ztol = 0.02_rspec   ! 2% default
    if(present(reltol)) ztol=abs(reltol)

    ztol = max(1.0e-4_rspec,ztol)  ! 0.01% minimum
    ztol = min(0.1_rspec,ztol)     ! 10.00% maximum

    zrho_check = 0.1_rspec
    if(present(rho_check)) zrho_check = rho_check

    iupdate_phit=.FALSE.; iupdate_psi=.FALSE.; iupdate_q=.FALSE.
    iupdate_curs=.FALSE.

    if(present(update_phit)) iupdate_phit = update_phit

    if(present(update_psi)) iupdate_psi  = update_psi

    if(present(update_q)) iupdate_q    = update_q

    icheck_q = .TRUE.
    if(present(check_q)) icheck_q = check_q

    icheck_phit = .TRUE.
    if(present(check_phit)) icheck_phit = check_phit

    if(present(pcur)) iupdate_curs = .TRUE.

    ! check that equilibrium (rho_eq) and (th_eq) grids are defined.
    ! these must be defined BEFORE calling this routine.

    iertot = 0

    inrho=ss%nrho_eq
    inth =ss%nth_eq

    if(.not.allocated(ss%rho_eq)) then
       iertot=iertot+1
       write(iout,*) ' ?PS_UPDATE_VERIFY: ss%rho_eq unallocated.'
    else if(abs(ss%rho_eq(inrho)-1.0_rspec).gt.zsmall) then
       iertot=iertot+1
       write(iout,*) ' ?PS_UPDATE_VERIFY: ss%rho_eq values not set.'
    endif

    if(.not.allocated(ss%rho_eq_geo)) then
       iertot=iertot+1
       write(iout,*) ' ?PS_UPDATE_VERIFY: ss%rho_eq_geo unallocated.'
    else if(abs(ss%rho_eq_geo(ss%nrho_eq_geo)-1.0_rspec).gt.zsmall) then
       iertot=iertot+1
       write(iout,*) ' ?PS_UPDATE_VERIFY: ss%rho_eq_geo values not set.'
    endif

    if(.not.allocated(ss%th_eq)) then
       iertot=iertot+1
       write(iout,*) ' ?PS_UPDATE_VERIFY: ss%rho_eq unallocated.'
    else if(abs(ss%th_eq(inth)-ss%th_eq(1)-z2pi).gt.zsmall) then
       iertot=iertot+1
       write(iout,*) ' ?PS_UPDATE_VERIFY: ss%th_eq values not set.'
    endif

    !  check against simultaneous requests to update q and psi

    if(iupdate_psi.AND.iupdate_q) then
       iertot=iertot+1
       write(iout,*) ' ?PS_UPDATE_VERIFY: incompatible requests to update BOTH "q" and "psi".'
    endif

    if(iertot.ne.0) then
       ierr=1
       return
    endif

    ! OK.  Need to make sure that profiles of volume and <1/R**2> are
    ! up-to-date.

    if(present(rhowk)) then
       call ps_mhdeq_derive('grho2r2i', ierr, state=ss, rhowk=rhowk)
    else
       call ps_mhdeq_derive('grho2r2i', ierr, state=ss)
    endif
    if(ierr.ne.0) then
       write(iout,*) ' ?PS_MHDEQ_VERIFY: call to PS_MHDEQ_DERIVE("grho2r2i") failed.'
       return
    endif

    ! set up test grid

    nrho_test = 1 + imul*(inrho - 1)
    inzc = nrho_test - 1

    do
       allocate(zrho(nrho_test),zrhozc(inzc))

       zrho(1)=0.0_rspec
       do ii=2,nrho_test
          zrho(ii)=(ii-1)*1.0_rspec/(nrho_test-1)
          zrhozc(ii-1)=0.5_rspec*(zrho(ii-1)+zrho(ii))
       enddo

       zdrho = 1.0_rspec/(nrho_test-1)

       allocate(zg(inzc),zdvdrho(inzc),zr2i(inzc))
       allocate(zq(inzc),zpsi_test(nrho_test))

       !  get g=R*|B_phi|

       call ps_intrp_1d_vec(zrhozc,ss%id_g_eq,zg,ierr,state=ss)
       if(ierr.ne.0) iertot=iertot + 1

       !  get <1/R^2>

       call ps_intrp_1d_vec(zrhozc,ss%id_gr2i,zr2i,ierr,state=ss)
       if(ierr.ne.0) iertot=iertot + 1

       !  get dV/drho

       call ps_intrp_1d_vec(zrhozc,ss%id_vol,zdvdrho,ierr,state=ss,ideriv=1)
       if(ierr.ne.0) iertot=iertot + 1

       if(iupdate_q) then
          ! get Psipol: q profile to be replaced.
          call ps_intrp_1d_vec(zrho,ss%id_Psipol,zpsi_test,ierr,state=ss)
          call ringchk(ss%rho_eq,ss%Psipol,zrho,zpsi_test)
          if(ierr.ne.0) iertot=iertot + 1
          zdpsimin = 1.0e-2_rspec*(zpsi_test(nrho_test)-zpsi_test(1))/ &
               (nrho_test*nrho_test)
       else
          ! get q profile -- will be used to evaluate Psi(rho)
          call ps_intrp_1d_vec(zrhozc,ss%id_q_eq,zq,ierr,state=ss)
          call ringchk(ss%rho_eq,ss%q_eq,zrhozc,zq)
          if(ierr.ne.0) iertot=iertot + 1
       endif

       if(iertot.gt.0) then
          write(iout,*) ' ?PS_MHDEQ_VERIFY: profile interpolation error.'
          ierr=999
          exit
       endif

       allocate(zphit(inrho),zqnew(inrho),zpsi_new(inrho))

       zsum=0.0_rspec
       zphit(1)=zsum

       zpsi_new(1)=0.0_rspec
       if(.not.iupdate_q) zpsi_test(1)=0.0_rspec

       ict=1
       icti=0
       do ii=1,inzc
          zsump = zsum
          icti=icti + 1
          !  add up toroidal flux
          zsum = zsum + zdrho*zg(ii)*zdvdrho(ii)*zr2i(ii)/z2pi
          dphit=zsum-zsump

          if(iupdate_q) then
             dpsi=max(zdpsimin,(zpsi_test(ii+1)-zpsi_test(ii)))
             zq(ii)=dphit/(z2pi*dpsi)
          else
             dpsi=dphit/(z2pi*zq(ii))
             zpsi_test(ii+1)=zpsi_test(ii) + dpsi
          endif

          if(icti.eq.imul) then
             icti=0
             ict=ict+1
             zphit(ict)=zsum
             zpsi_new(ict)=zpsi_test(ii+1)  ! if(iupdate_q): original Psi
          endif
       enddo

       if(zphit(ict).le.0.0_rspec) then
          ierr=9999
          write(iout,*) ' ?PS_MHDEQ_VERIFY: integrated flux not positive: ', &
               zphit(ict)
          exit
       endif

       if(iupdate_phit) ss%phit = zphit
       if(iupdate_psi)  ss%Psipol = zpsi_new

       if(iupdate_q) then
          ss%q_eq(1)=zq(1) + 0.125_rspec*(zq(2)-zq(1))  ! parab. extrapolation.
          ss%q_eq(inrho)=zq(inzc)+0.5_rspec*(zq(inzc)-zq(inzc-1)) ! lin. extrap
          ict=1
          icti=0
          do ii=1,inzc-1
             icti=icti+1
             if(icti.eq.imul) then
                icti=0
                ict=ict+1
                ss%q_eq(ict)=0.5_rspec*(zq(ii)+zq(ii+1))
             endif
          enddo
       endif

       ! toroidal flux self-consistency check: Phit(rho) ~ rho**2*Phit(rho=1)
       ! error criterion relaxed near mag. axis

       zerrmax=0.0_rspec

       if(icheck_phit) then
          do ii=2,inrho-1
             if(ss%rho_eq(ii).lt.zrho_check) cycle
             zerr=abs(1.0_rspec - &
                  zphit(ii)/(ss%rho_eq(ii)*ss%rho_eq(ii)*zphit(inrho)))
             zerrmax=max(zerr,zerrmax)
             if(zerr.gt.ztol) then
                iertot=iertot+1
                if(iertot.eq.1) then
                   write(iout,*) &
                        ' ?PS_MHDEQ_VERIFY: Phit(rho) - rho**2*Phit(bdy) error limit exceeded:'
                   write(iout,*) '  found: ',zerr,' tolerance: ',ztol
                endif
             endif
          enddo
          if(iertot.ne.0) then
             write(iout,*) ' ?PS_MHDEQ_VERIFY: toroidal flux rho^2 consistency test failed at ',iertot,' grid points.'
             ierr=ierr+1
             iertot=0
          endif
       endif

       if(icheck_q.and.(.not.(iupdate_psi.or.iupdate_q))) then
          zdpsimin=0.01_rspec*(ss%psipol(inrho)-ss%psipol(1))
          do ii=2,inrho
             if(ss%rho_eq(ii).lt.zrho_check) cycle
             zdenom=max(abs(ss%psipol(ii)),abs(zpsi_new(ii)))
             zdenom=max(zdpsimin,zdenom)
             zerr=abs(ss%psipol(ii)-zpsi_new(ii))/zdenom
             zerrmax=max(zerr,zerrmax)
             if(zerr.gt.ztol) then
                iertot=iertot+1
                if(iertot.eq.1) then
                   write(iout,*) &
                        ' ?PS_MHDEQ_VERIFY: computed Psipol(rho) - state value: error limit exceeded:'
                   write(iout,*) '  found: ',zerr,' tolerance: ',ztol
                endif
             endif
          enddo
          if(iertot.ne.0) then
             write(iout,*) ' ?PS_MHDEQ_VERIFY: poloidal flux & q comparison test failed at ',iertot,' grid points.'
             ierr=ierr+1
             iertot=0
          endif
       endif

       if(icheck_phit.and.(.not.iupdate_phit)) then
          ! zphit-phit consistency check

          do ii=2,inrho
             if(ss%rho_eq(ii).lt.zrho_check) cycle
             zerr=abs(ss%phit(ii)-zphit(ii))/zphit(ii)
             zerrmax=max(zerr,zerrmax)
             if(zerr.gt.ztol) then
                iertot=iertot+1
                if(iertot.eq.1) then
                   write(iout,*) &
                        ' ?PS_MHDEQ_VERIFY: computed Phit(rho) - state%Phit(rho) error limit exceeded:'
                   write(iout,*) '  found: ',zerr,' tolerance: ',ztol
                endif
             endif
          enddo
          if(iertot.ne.0) then
             write(iout,*) ' ?PS_MHDEQ_VERIFY: toroidal flux integration test failed at ',iertot,' grid points.'
             ierr=ierr+1
             iertot=0
          endif
       endif

       if(ierr.gt.0) ierr=999
       exit
    enddo

    if(allocated(zrho)) deallocate(zrho,zrhozc)
    if(allocated(zg)) deallocate(zg,zdvdrho,zr2i)
    if(allocated(zphit)) deallocate(zphit)
    if(allocated(zq)) deallocate(zq,zpsi_test)

    if(present(relerr_max)) relerr_max=zerrmax

    ! if the equilibrium is verified, be sure that all internal splines
    ! are updated as well...

    icheck_RZ=.TRUE.
    if(present(eqcheck_RZ)) icheck_RZ=eqcheck_RZ

    if(ierr.eq.0) then
       call ps_state_memory_update(ierr,state=ss, eqcheck_RZ=icheck_RZ)
    else
       return
    endif

    !---------------------------------------
    ! update the current profiles if requested and if total current is provided.

    if(iupdate_curs) then
       zrho_brk=0.9d0
       call ps_update_cur(ss,pcur,zrho_brk,iout,ierr)
    endif

  end SUBROUTINE ps_mhdeq_verify

  !---------------------------
  ! private -- prevent spline overshoot

  subroutine ringchk(rho_orig,f_orig,rho_targ,f_targ)

    ! usage: *after* Spline or Hermite interpolation, call this routine
    ! with the original data on the original grid, and with the 
    ! interpolation result.  Then:
    !   for rho_orig(k) <= rho_targ(j) <= rho_orig(k+1) enforce:
    !   min(f_orig(k),f_orig(k+1)) <= f_targ(j) <= min(f_orig(k),f_orig(k+1))

    REAL(kind=rspec), dimension(:), intent(in) :: rho_orig,f_orig
    REAL(kind=rspec), dimension(:), intent(in) :: rho_targ
    REAL(kind=rspec), dimension(:), intent(inout) :: f_targ

    ! the rho grids are assumed strict ascending, without checking!

    ! the sizes of rho_orig & f_orig must match

    ! the sizes of rho_targ & f_targ must match

    !----------------------
    integer :: ilun_dbg=0
    integer :: j,k
    integer :: isize_orig,isize_targ
    REAL(kind=rspec) :: fmin,fmax
    !----------------------

    isize_orig = size(rho_orig)
    if(size(f_orig).ne.isize_orig) then
       write(ilun_dbg,*) ' ?internal error in plasma_state_mod (ringchk):'
       write(ilun_dbg,*) '  rho_orig & f_orig vector sizes do not match.'
       return
    endif

    isize_targ = size(rho_targ)
    if(size(f_targ).ne.isize_targ) then
       write(ilun_dbg,*) ' ?internal error in plasma_state_mod (ringchk):'
       write(ilun_dbg,*) '  rho_targ & f_targ vector sizes do not match.'
       return
    endif

    k=1
    fmin=min(f_orig(k),f_orig(k+1))
    fmax=max(f_orig(k),f_orig(k+1))
    do j=1,isize_targ
       do while (rho_targ(j) > rho_orig(k+1))
          k = k + 1
          if(k.ge.isize_orig) exit
          fmin=min(f_orig(k),f_orig(k+1))
          fmax=max(f_orig(k),f_orig(k+1))
       enddo
       if(k.ge.isize_orig) exit

       f_targ(j)=max(fmin,min(fmax,f_targ(j)))

    enddo

  end subroutine ringchk
  !==========================================================================
  SUBROUTINE ps_mhdeq_derive(action, ierr, state, rhowk)

    !-------------------------------------------------------------------
    !
    ! PS_MHDEQ_DERIVE
    !
    ! compute flux surface averages and other profiles derived from
    ! the MHD equilibrium geometry and fields
    !-------------------------------------------------------------------
    
    !----------------------------------------------------------------------
    !
    !   Declare local variables
    !
    !----------------------------------------------------------------------

    character*(*), intent(in) :: action      ! specify action(s) to be taken

    ! computations are performed according to keyword substrings found in 
    ! "action" (converted to UPPERCASE in actu).

    integer, intent(out) :: ierr             ! exit status code (0=OK)

    !-------------------
    !  OPTIONAL variables...

    TYPE (plasma_state), target, OPTIONAL :: state   ! update *this* state
                                  ! the default is to update the "ps" state.

    REAL(kind=rspec), dimension(:), intent(IN), OPTIONAL :: rhowk
                                  ! working grid for flux surface averages
                                  ! if defaulted use state%rho_eq_geo.
                                  ! some codes may want a coarser grid for
                                  ! performance reasons.

    !-------------------
    !  local variables
    !-------------------

    TYPE (plasma_state), pointer :: ss
    integer :: iout,inrho,inth,inrho_geo,ierloc,iertot,iact,ilena
    character*128 :: actu

    real(kind=rspec), parameter :: zsmall = 1.0e-12_rspec
    real(kind=rspec), parameter :: z2pi   = 6.2831853071795862D+00

    integer :: loc_cclist(ps_ccount)

    logical :: iall

    !----------------------------------------------------------------------

    ierr = 0

    iout = Swim_err_out_file
    ierloc = 0

    CALL ps_init_tag

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    ilena=len(trim(action))
    actu(1:ilena) = action
    call uupper(actu(1:ilena))   ! uppercase conversion

    !---------------
    ! rudimentary error check: grids

    iertot = 0

    inrho=ss%nrho_eq
    inrho_geo=ss%nrho_eq_geo
    inth =ss%nth_eq

    if(.not.allocated(ss%rho_eq)) then
       iertot=iertot+1
       write(iout,*) ' ?PS_UPDATE_DERIVE: ss%rho_eq unallocated.'
    else if(abs(ss%rho_eq(inrho)-1.0_rspec).gt.zsmall) then
       iertot=iertot+1
       write(iout,*) ' ?PS_UPDATE_DERIVE: ss%rho_eq values not set.'
    endif

    if(.not.allocated(ss%rho_eq_geo)) then
       iertot=iertot+1
       write(iout,*) ' ?PS_UPDATE_DERIVE: ss%rho_eq_geo unallocated.'
    else if(abs(ss%rho_eq_geo(inrho_geo)-1.0_rspec).gt.zsmall) then
       iertot=iertot+1
       write(iout,*) ' ?PS_UPDATE_DERIVE: ss%rho_eq_geo values not set.'
    endif

    if(.not.allocated(ss%th_eq)) then
       iertot=iertot+1
       write(iout,*) ' ?PS_UPDATE_DERIVE: ss%rho_eq unallocated.'
    else if(abs(ss%th_eq(inth)-ss%th_eq(1)-z2pi).gt.zsmall) then
       iertot=iertot+1
       write(iout,*) ' ?PS_UPDATE_DERIVE: ss%th_eq values not set.'
    endif

    if(iertot.ne.0) then
       ierr=999
       return
    endif

    !-------------------------------
    iact = 0

    if((index(actu(1:ilena),'ALL').gt.0).or. &
         (index(actu(1:ilena),'EVERYTHING').gt.0)) then
       iall = .TRUE.
    else
       iall = .FALSE.
    endif

    if(iall.or.index(actu(1:ilena),'GRHO2R2I').gt.0) then
       iact = iact + 1
       if(ps_debug.gt.0) then
          write(iout,*) ' %ps_mhdeq_derive: compute <1/R^2> and <|grad(rho)|^2/R^2>.'
       endif
       !
       !  compute <1/R^2> and <|grad(rho)|^2/R^2>
       !  only RZSURF data needed; equilibrium need not be complete.
       !
       if(present(rhowk)) then
          call ps_mhdeq_grho2r2i(ss,size(rhowk),rhowk,iout,ierr)
       else
          call ps_mhdeq_grho2r2i(ss,inrho_geo,ss%rho_eq_geo,iout,ierr)
       endif

    endif
    if(iall.or.index(actu(1:ilena),'METRIC_AVG').gt.0) then
       iact = iact + 1
       if(ps_debug.gt.0) then
          write(iout,*) ' %ps_mhdeq_derive: compute metric averages <1/R>, <|grad(rho)|>, etc.'
       endif
       !
       !  compute additional flux surface averages of powers and combinations
       !  of R, 1/R, and grad(rho).
       !  only RZSURF data needed; equilibrium need not be complete.
       !
       if(present(rhowk)) then
          call ps_mhdeq_geoprof(ss,size(rhowk),rhowk,iout,ierr)
       else
          call ps_mhdeq_geoprof(ss,inrho_geo,ss%rho_eq_geo,iout,ierr)
       endif

    endif
    if(iall.or.index(actu(1:ilena),'SURF_PARAM').gt.0) then
       iact = iact + 1
       if(ps_debug.gt.0) then
          write(iout,*) ' %ps_mhdeq_derive: compute R & Z minmax profiles; triangularity, etc.'
       endif
       !
       !  compute non-flux-surface-averaged geometical parameters 
       !  that characterize the surfaces:  elongation, upper/lower 
       !  triangularity, Rmin, Rmax, Zmin, Zmax, midplane location 
       !  for each flux surface.  Only RZSURF data is needed.
       !
       if(present(rhowk)) then
          call ps_mhdeq_surfparam(ss,size(rhowk),rhowk,iout,ierr)
       else
          call ps_mhdeq_surfparam(ss,inrho_geo,ss%rho_eq_geo,iout,ierr)
       endif

    endif
    if((iall.or.index(actu(1:ilena),'FIELD_AVG').gt.0).or. &
         (index(actu(1:ilena),'NCLASS').gt.0)) then
       iact = iact + 1
       if(ps_debug.gt.0) then
          write(iout,*) ' %ps_mhdeq_derive: compute <1/B^2>, NCLASS moments, etc.'
       endif
       !
       !  compute <B>, <B**2>, <B**-2>, <grad(xi)**2/B**2>
       !   and neoclassical Pfirsch Schlutter moments and related profiles
       !
       !   also compute <B*R**2>
       !
       if(present(rhowk)) then
          call ps_mhdeq_nclass(ss,size(rhowk),rhowk,iout,ierr)
       else
          call ps_mhdeq_nclass(ss,inrho_geo,ss%rho_eq_geo,iout,ierr)
       endif

    endif
    if(iall.or.index(actu(1:ilena),'NC_AUX').gt.0) then
       iact = iact + 1
       if(ps_debug.gt.0) then
          write(iout,*) ' %ps_mhdeq_derive: compute NC sqrt-singular flux surface averages.'
       endif
       !
       !  NCF(H)=H**-2*(1-SQRT(1-H)*(1+H/2)); H=B/Bmax
       !        special integrator (sqrt(0) singularity)
       !
       !  NCF(H)=[(B**-2)*((1-H)**0.5-(1/3)*(1-H)**1.5)]; h=B/Bmax
       !        special integrator (sqrt(0) singularity)
       !
       if(present(rhowk)) then
          call ps_mhdeq_geosing(ss,size(rhowk),rhowk,iout,ierr)
       else
          call ps_mhdeq_geosing(ss,inrho_geo,ss%rho_eq_geo,iout,ierr)
       endif

    endif
    if(iall.or.index(actu(1:ilena),'BMINMAX').gt.0) then
       iact = iact + 1
       if(ps_debug.gt.0) then
          write(iout,*) ' %ps_mhdeq_derive: compute profiles of Bmin, Bmax.'
       endif
       !
       !  mod(B) min & max values on each flux surface
       !
       if(present(rhowk)) then
          call ps_mhdeq_bminmax(ss,size(rhowk),rhowk,iout,ierr)
       else
          call ps_mhdeq_bminmax(ss,inrho_geo,ss%rho_eq_geo,iout,ierr)
       endif

    endif

    if(iact.eq.0) then
       write(iout,*) ' ?ps_mhdeq_derive: unrecognized ACTION code: "', &
            trim(action),'".'
       ierr=999
       ierloc = 1
    else

       ! make sure output can be interpolated.

       loc_cclist = ps_cczero
       loc_cclist(ps_cnum_EQ)=ps_chkProfs ! update EQ profile hashcodes only...
       call plasma_state_hash_code(ss,loc_cclist,ierr)
       if(ierr.eq.0) then
          call plasma_state_xload(ss,ierr)
       endif
       if(ierr.ne.0) then
          write(iout,*) ' ?PS_MHDEQ_DERIVE: profile interpolation load failed.'
          return
       endif

    endif

    if(ierloc.eq.0) then
       if(ierr.ne.0) then
          write(iout,*) ' ?ps_mhdeq_derive: ACTION "'//trim(action)// &
               '" failed!'
          ierr=9999
       endif
    endif

  end SUBROUTINE ps_mhdeq_derive

  !==========================================================================
  SUBROUTINE ps_clear_profs(ierr, state, cclist, save_hash)

    !-------------------------------------------------------------------
    !
    ! PS_CLEAR_PROFS
    !
    ! set all allocated profile arrays to 0.0_rspec
    ! grids and scalars and species lists, etc., are untouched.
    !-------------------------------------------------------------------
    
    !----------------------------------------------------------------------
    !
    !   Declare local variables
    !
    !----------------------------------------------------------------------

    integer, intent(out) :: ierr  ! exit status code (0=OK)

    TYPE (plasma_state), target, OPTIONAL :: state

    ! the default is to clear the "ps" state.

    TYPE (plasma_state), pointer :: ss

    INTEGER, INTENT(IN), OPTIONAL :: cclist(ps_ccount)  ! component list
    !  default: clear profiles of ALL components; presence of this list
    !  allows selection of subset.

    LOGICAL, INTENT(IN), OPTIONAL :: save_hash  ! .TRUE. to save hash codes
    !  set in the state prior to this call; by default they are not saved.

    !----------------------------------------------------------------------
    !
    !   Clear profiles
    !
    !----------------------------------------------------------------------

    !----------------------------------------------------------------------
    ! local:
    integer :: loc_cclist(ps_ccount)
    integer :: ii,iout
    integer :: ihash_save(ps_hash_total)
    logical :: isave_hash
    !----------------------------------------------------------------------

    iout = Swim_err_out_file

    CALL ps_init_tag

    if(present(state)) then
       ss => state
    else
       ss => ps
    endif

    isave_hash=.FALSE.
    if(present(save_hash)) isave_hash=save_hash

    if(isave_hash) then
       ihash_save = ss%hash_codes
    endif

    if(present(cclist)) then
       loc_cclist = cclist
    else
       loc_cclist = ps_cc_all
    endif

    ierr = 0
    do ii=1,ps_ccount
       if((loc_cclist(ii).lt.0).or.(loc_cclist(ii).gt.1)) then
          if(ierr.eq.0) &
               CALL SWIM_error('cclist check','PS_CLEAR_PROFS')
          ierr=ierr + 1
          write(iout,*) ' cclist(',ii,')=',cclist(ii),'; should be 0 or 1.'
          write(iout,*) ' index refers to component: ',trim(ps_cnames(ii))
       endif
    enddo
    if(ierr.gt.0) then
       ierr=1
       return
    endif

    CALL plasma_state_clear_profs(loc_cclist,ss,ierr)
    IF(ierr.NE.0) THEN
       write(SWIM_err_out_file,*) &
            'PS_CLEAR_PROFS: error in plasma_state_clear_profs call!'
       return
    ENDIF

    CALL ps_state_memory_update(ierr, state=ss)  ! also zero the interpolating 
    ! functions 

    IF(ierr.NE.0) THEN
       write(SWIM_err_out_file,*) &
            'PS_CLEAR_PROFS: error in ps_state_memory_update call!'
       return
    ENDIF

    if(isave_hash) then
       ss%hash_codes = ihash_save
    endif

  END SUBROUTINE ps_clear_profs

  SUBROUTINE ps_psimap(psi,rhopsi,ierr, tol,state,iwarn)

    !-------------------------------------------------
    ! D. McCune 14 May 2007
    !    From Psi(...) find rho(Psi(...))
    !    I.e. find rho values corresponding to a poloidal flux grid
    !-------------------------------------------------

    !  the following two arrays must be of the same size:

    REAL(KIND=rspec), DIMENSION(:), INTENT(IN) :: Psi      ! Psi array
    REAL(KIND=rspec), DIMENSION(:), INTENT(OUT) :: rhoPsi  ! rho(Psi)

    INTEGER, INTENT(OUT) :: ierr  ! status code, 0=OK

    REAL(KIND=rspec), INTENT(IN), OPTIONAL :: tol  ! accuracy tolerance on Psi
    !  for bounds checking (relative).  Default: 1.0e-10_rspec;
    !  On output, abs(Psi(rhopsi(i))-Psi(i)) <= tol*Psimax
    !    where Psi(rhopsi(i)) refers to spline interpolation of Psi profile

    TYPE (plasma_state), TARGET, OPTIONAL :: state ! map from this state
    !  default: mapping is for "ps" state.

    INTEGER, INTENT(OUT), OPTIONAL :: iwarn        ! #of out-of-bounds Psi pts.
    !  this means elements Psi(j) that were not in the range of Psi values
    !  spanned by the state equilibrium.
    !     For psi(j) <= Psimin, rhopsi(j)=0.0 is returned;
    !     For psi(j) >= Psimax, rhopsi(j)=1.0 is returned.
    !  The error flag is set if psi(j) is out of bounds beyond tolerance.

    !----------------------------------
    !  local:

    TYPE (plasma_state), pointer :: ss

    REAL(KIND=rspec) :: ztol,zpsi_min,zpsi_max
    INTEGER :: ii,jwarn,iout
    !----------------------------------

    call ps_init_tag

    iout = SWIM_ERR_out_file

    ierr = 0
    if(present(iwarn)) iwarn = 0

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    ztol = 1.0e-10_rspec
    if(present(tol)) ztol = max(ztol*1.0e-5_rspec,min(1.0e-3_rspec, tol))

    if(.not.allocated(ss%psipol)) then
       ierr=1
       write(iout,*) ' %ps_psimap: state equilibrium not ready (psipol alloc).'
       return
    endif

    if(ss%psipol(ss%nrho_eq).eq.0.0_rspec) then
       ierr=1
       write(iout,*) ' %ps_psimap: state equilibrium not ready (psipol=0).'
       return
    endif

    if(size(psi).ne.size(rhopsi)) then
       ierr=2
       write(iout,*) ' %ps_psimap: "psi" and "rhopsi" array sizes differ:'
       write(iout,*) '  size(psi) = ',size(psi),'; size(rhopsi) = ',size(rhopsi)
       return
    endif

    call ps_psimap_sub(size(psi),psi,rhopsi,tol,jwarn,ss%iobj,iout,ierr)

    if(present(iwarn)) iwarn = jwarn

    if(ierr.ne.0) then
       write(iout,*) ' %ps_psimap: psimap subroutine error, ierr=',ierr
       return
    endif

    if(jwarn.gt.0) then
       ztol = ztol*max(abs(ss%psipol(1)),abs(ss%psipol(ss%nrho_eq)))
       zpsi_min = psi(1)
       zpsi_max = zpsi_min
       do ii=1,size(psi)
          if((ss%psipol(1)-psi(ii)).gt.ztol) ierr = ierr + 1
          if((psi(ii)-ss%psipol(ss%nrho_eq)).gt.ztol) ierr = ierr + 1
          zpsi_min=min(zpsi_min,psi(ii))
          zpsi_max=max(zpsi_max,psi(ii))
       enddo

       if(ierr.gt.0) then
          write(iout,*) ' ?ps_psimap: ',ierr, &
               ' Psi points outside equilibrium Psi range:'
          write(iout,*) '  Psi at axis: ',ss%psipol(1)
          write(iout,*) '  Psi at edge: ',ss%psipol(ss%nrho_eq)
          write(iout,*) '  input psi(...) range of values: ',zpsi_min,zpsi_max
          ierr = 3
       endif
    endif
       
  END SUBROUTINE ps_psimap

  !==========================================================================
  !  Routines to fetch "nspec_tha" species parameters -- abridged thermal 
  !  ion species list:
  !
  !  Species list dimensioning is (1:ss%nspec_tha) -- 0 index for
  !  electrons omitted!
  !
  !  ps_tha_fetch(...) -- profiles on original grid
  !  ps_tha_rezone(...) -- profiles rezoned to target grid; smoothing option
  !

  SUBROUTINE ps_tha_fetch(ierr,state,tol_zero,ns,Ts,v_pars,iwarn)

    !-------------------------------------------------
    ! D. McCune 24 Feb 2008:  for the "abridged" thermal species list:
    !   fetch densities, Temperatures, and pll flow velocities on orig. grid
    !
    !   NOTE: only the ION species parameters are returned:
    !      species dimensions 1:ss%nspec_tha, NOT 0:ps_nspec_tha; electrons
    !      NOT included.  Output arrays must be dimensioned accordingly.
    !-------------------------------------------------

    INTEGER, intent(out) :: ierr      ! completion code (0=OK)

    !-------------------------
    !  OPTIONAL arguments...

    TYPE (plasma_state), OPTIONAL, target :: state
    !  operate on THIS state, if argument is present;
    !  otherwise use "ps" state.

    REAL(KIND=rspec), intent(in), OPTIONAL :: tol_zero  ! consider as zero
    !                             ! densities < tol_zero*ne;
    !                             !   default tolerance 1.0e-12_rspec

    !-------------------------
    !  UNITS:  ns, m^-3; Ts, keV; v_pars, m/sec

    REAL(KIND=rspec), dimension(:,:), OPTIONAL, intent(out) :: ns ! density
    REAL(KIND=rspec), dimension(:,:), OPTIONAL, intent(out) :: Ts ! temperature
    REAL(KIND=rspec), dimension(:,:), OPTIONAL, intent(out) :: v_pars ! <vpll>

    integer, OPTIONAL, intent(out) :: iwarn   ! optional warning flag
    !  if present: iwarn=1 is set if zero or near zero imp. density occurs; 
    !              iwarn=2 is set if simultaneous Zeff and quasineutrality
    !                      solution wield a negative density & so Zeff match
    !                      is abandoned; only quasineutrality is satisfied.
    !                      (if total impurity density < 0 occurs: ierr is set).
    !-------------------------
    ! required dimensioning of output arrays: (nrho-1,nspec_tha)
    ! it is an error if the needed species list or grid is undefined.
    !-------------------------
    
    ! Local:

    TYPE (plasma_state), pointer :: ss

    REAL(KIND=rspec), dimension(:), allocatable :: zav,z2av,nimp
    REAL(KIND=rspec), dimension(:), allocatable :: zTav,zvav

    integer :: iout,ispec,inzon,iZ1,iZ2,indx,icount,irho,ix1,ix2,iertot,iertmp
    REAL(KIND=rspec) :: Zion,zf1,zf2,ztol,ztoln
    !-------------------------

    ierr=0
    if(present(ns)) ns=0.0_rspec
    if(present(Ts)) Ts=0.0_rspec
    if(present(v_pars)) v_pars=0.0_rspec
    if(present(iwarn)) iwarn=0

    iout=SWIM_err_out_file

    CALL ps_init_tag

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    ztol=1.0e-12_rspec
    if(present(tol_zero)) then
       ztol = max(1.0e-14_rspec,min(1.0e-4_rspec,tol_zero))
    endif

    call tha_gridchk('ps_tha_fetch',ss,iout,ierr)
    if(ierr.ne.0) return

    if(present(ns)) then
       call tha_dimchk('ps_tha_fetch','ns',ss%nrho-1,ss%nspec_tha,ns, &
            iout,ierr)
       if(ierr.ne.0) return
    endif

    if(present(Ts)) then
       call tha_dimchk('ps_tha_fetch','Ts',ss%nrho-1,ss%nspec_tha,Ts, &
            iout,ierr)
       if(ierr.ne.0) return
    endif

    if(present(v_pars)) then
       call tha_dimchk('ps_tha_fetch','v_pars',ss%nrho-1,ss%nspec_tha,v_pars, &
            iout,ierr)
       if(ierr.ne.0) return
    endif

    inzon=ss%nrho-1

    allocate(zav(inzon),z2av(inzon),nimp(inzon))
    zav = 0.0_rspec; z2av = 0.0_rspec; nimp = 0.0_rspec

    if(present(TS)) then
       allocate(ztav(inzon)); ztav=0.0_rspec
    endif

    if(present(v_pars)) then
       allocate(zvav(inzon)); zvav=0.0_rspec
    endif

    !  map s -> sa  (full list to abridged; add up impurities & Z moments)

    icount = 0
    do ispec=1,ss%nspec_th
       indx=ss%s_to_sa(ispec)
       if(indx.gt.0) then
          if(present(ns)) ns(:,indx) = ss%ns(:,ispec)
          if(present(Ts)) Ts(:,indx) = ss%Ts(:,ispec)
          if(present(v_pars)) v_pars(:,indx) = ss%v_pars(:,ispec)
       else
          icount = icount + 1
          Zion = ss%q_s(ispec)/ps_xe
          nimp(:) = nimp(:) + ss%ns(:,ispec)
          if(present(Ts)) then
             zTav(:) = zTav(:) + ss%Ts(:,ispec)*ss%ns(:,ispec)
          endif
          if(present(v_pars)) then
             zvav(:) = zvav(:) + ss%v_pars(:,ispec)*ss%ns(:,ispec)
          endif
          zav(:) = zav(:) + Zion*ss%ns(:,ispec)
          z2av(:) = z2av(:) + Zion*Zion*ss%ns(:,ispec)
       endif
    enddo

    iertot = 0

    if(icount.gt.0) then
       ! last two indices of abridged list are amalgamated impurities
       
       ix1=ss%nspec_tha-1
       ix2=ss%nspec_tha

       iZ1=ss%q_sa(ix1)/ps_xe + 0.1_rspec
       iZ2=ss%q_sa(ix2)/ps_xe + 0.1_rspec

       do irho=1,inzon
          ztoln = ztol*ss%ns(irho,0)  ! small fraction of electron density

          if(nimp(irho).lt.ztoln) then
             if(nimp(irho).lt.-ztoln) then
                iertot = iertot + 1
             endif
             if(present(iwarn)) iwarn=max(1,iwarn)
             !  (output array elements were already cleared to zero)
          else
             zav(irho)=zav(irho)/nimp(irho)
             z2av(irho)=z2av(irho)/nimp(irho)
             if(present(Ts)) zTav(irho)=zTav(irho)/nimp(irho)
             if(present(v_pars)) zvav(irho)=zvav(irho)/nimp(irho)
             
             !  Zeff/quasi-neutrality match solution
             call split_imp(zav(irho),z2av(irho),iZ1,iZ2,zf1,zf2,iertmp)

             if(min(zf1,zf2).lt.-ztol) then
                if(present(iwarn)) iwarn=max(2,iwarn)
             endif

             if(min(zf1,zf2).lt.ztol) then
                if(zf2.lt.ztol) then
                   zf1=1.0_rspec; zf2=0.0_rspec
                else
                   zf1=0.0_rspec; zf2=1.0_rspec
                endif
             endif

             if(present(ns)) then
                ns(irho,ix1)=zf1*nimp(irho)
                ns(irho,ix2)=zf2*nimp(irho)
             endif

             if(present(Ts)) then
                Ts(irho,ix1)=ztav(irho)
                Ts(irho,ix2)=ztav(irho)
             endif

             if(present(v_pars)) then
                v_pars(irho,ix1)=zvav(irho)
                v_pars(irho,ix2)=zvav(irho)
             endif
          endif
       enddo
    endif

    if(iertot.gt.0) then
       ierr=900
       write(iout,*) ' ?ps_tha_fetch: negative impurity density detected.'
    endif

    deallocate(zav,z2av,nimp)
    if(allocated(ztav)) deallocate(ztav)
    if(allocated(zvav)) deallocate(zvav)
  end SUBROUTINE ps_tha_fetch

  SUBROUTINE ps_tha_rezone(rho,ierr,state,tol_zero,zonesmoo, &
       ns,Ts,v_pars,zonevol,iwarn)

    !-------------------------------------------------
    ! D. McCune 24 Feb 2008:  for the "abridged" thermal species list:
    !   rezone densities, Temperatures, and pll flow velocities on target
    !   grid.  Standard rezoning method is used, so that volume integrals
    !   of (ns), (ns*Ts), (ns*v_pars) are conserved.
    !
    !   NOTE: only the ION species parameters are returned:
    !      species dimensions 1:ss%nspec_tha, NOT 0:ps_nspec_tha; electrons
    !      NOT included.  Output arrays must be dimensioned accordingly.
    !-------------------------------------------------

    REAL(KIND=rspec), dimension(:) :: rho  ! target rho grid
    !  This must be strict ascending, covering the range [0,1] exactly,
    !  delineating zone BOUNDARIES.
    !  The 1st dimension of output arrays must be SIZE(rho)-1 (output by zone).

    INTEGER, intent(out) :: ierr      ! completion code (0=OK)

    !-------------------------
    !  OPTIONAL arguments...

    TYPE (plasma_state), OPTIONAL, target :: state
    !  operate on THIS state, if argument is present;
    !  otherwise use "ps" state.

    REAL(KIND=rspec), intent(in), OPTIONAL :: tol_zero  ! consider as zero
    !                             ! densities < tol_zero*ne;
    !                             !   default tolerance 1.0e-12_rspec

    !  (general control common to all rezoning routines):
    logical, intent(in), optional :: zonesmoo ! T to apply 1/2 zone width
    ! integral smoothing operator.  Default is F.  This option is not 
    ! recommended unless mapping coarse step function data to a fine grid
    ! with requirement to remove step structure from smoothed interpolant.

    !----------------
    !  UNITS:  ns, m^-3; Ts, keV; v_pars, m/sec

    REAL(KIND=rspec), dimension(:,:), OPTIONAL, intent(out) :: ns ! density
    REAL(KIND=rspec), dimension(:,:), OPTIONAL, intent(out) :: Ts ! temperature
    REAL(KIND=rspec), dimension(:,:), OPTIONAL, intent(out) :: v_pars ! <vpll>
    REAL(KIND=rspec), dimension(:), OPTIONAL, intent(out) :: zoneVol
                                              ! volume of zones, m^3.

    integer, OPTIONAL, intent(out) :: iwarn   ! optional warning flag
    !  if present: iwarn=1 is set if zero or near zero imp. density occurs; 
    !              iwarn=2 is set if simultaneous Zeff and quasineutrality
    !                      solution wield a negative density & so Zeff match
    !                      is abandoned; only quasineutrality is satisfied.
    !                      (if total impurity density < 0 occurs: ierr is set).
    !-------------------------
    ! required dimensioning of output arrays: (nrho-1,nspec_tha)
    ! it is an error if the needed species list or grid is undefined.
    !-------------------------
    
    ! Local:

    TYPE (plasma_state), pointer :: ss

    REAL(KIND=rspec), dimension(:), allocatable :: zav,z2av,nimp
    REAL(KIND=rspec), dimension(:), allocatable :: zTav,zvav,zvol

    REAL(kind=rspec), dimension(:,:), allocatable :: zns,zTs,zvs

    logical :: ismoo

    integer :: iout,ispec,inzon,iZ1,iZ2,indx,icount,irho,ix1,ix2,iertot,iertmp
    integer :: inspec
    REAL(KIND=rspec) :: Zion,zf1,zf2,ztol,ztoln
    !-------------------------

    ierr=0
    iout=SWIM_err_out_file

    CALL ps_init_tag

    if(present(ns)) ns=0.0_rspec
    if(present(Ts)) Ts=0.0_rspec
    if(present(v_pars)) v_pars=0.0_rspec
    if(present(iwarn)) iwarn=0

    ismoo=.FALSE.
    if(present(zonesmoo)) ismoo=zonesmoo

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    ztol=1.0e-12_rspec
    if(present(tol_zero)) then
       ztol = max(1.0e-14_rspec,min(1.0e-4_rspec,tol_zero))
    endif

    call tha_gridchk('ps_tha_rezone',ss,iout,ierr)
    if(ierr.ne.0) return

    inzon=size(rho)-1

    if(present(ns)) then
       call tha_dimchk('ps_tha_rezone','ns',inzon,ss%nspec_tha,ns, &
            iout,ierr)
       if(ierr.ne.0) return
    endif

    if(present(Ts)) then
       call tha_dimchk('ps_tha_rezone','Ts',inzon,ss%nspec_tha,Ts, &
            iout,ierr)
       if(ierr.ne.0) return
    endif

    if(present(v_pars)) then
       call tha_dimchk('ps_tha_rezone','v_pars',inzon,ss%nspec_tha,v_pars, &
            iout,ierr)
       if(ierr.ne.0) return
    endif
    
    inspec=ss%nspec_th  ! unabridged thermal ion species list size

    !  density needed, even if only Ts and/or v_pars are requested.
    !   (because it weights sums and averages).

    allocate(zns(inzon,0:inspec),zvol(inzon))  ! include electrons here
    call ps_rho_rezone_arr1(rho, ss%id_ns(0:inspec), zns, ierr, &
         state=ss, zonesmoo=ismoo, zonediff=zvol)
    if(present(zonevol)) zonevol=zvol
    if(ierr.ne.0) then
       write(iout,*) ' ?ps_tha_rezone: thermal species density rezone failure.'
       deallocate(zns,zvol)
       return
    endif

    if(present(Ts)) then
       allocate(zts(inzon,inspec))
       call ps_rho_rezone_arr1(rho, ss%id_ts(1:inspec), zts, ierr, &
            state=ss, zonesmoo=ismoo)
       if(ierr.ne.0) then
          write(iout,*) ' ?ps_tha_rezone: thermal species temperature rezone failure.'
          deallocate(zns,zvol,zts)
          return
       endif
    endif

    if(present(v_pars)) then
       allocate(zvs(inzon,inspec))
       call ps_rho_rezone_arr1(rho, ss%id_v_pars(1:inspec), zvs, ierr, &
            state=ss, zonesmoo=ismoo)
       if(ierr.ne.0) then
          write(iout,*) ' ?ps_tha_rezone: thermal species <vpll> rezone failure.'
          deallocate(zns,zvol,zvs)
          if(allocated(zts)) deallocate(zts)
          return
       endif
    endif

    allocate(zav(inzon),z2av(inzon),nimp(inzon))
    zav = 0.0_rspec; z2av = 0.0_rspec; nimp = 0.0_rspec

    if(present(Ts)) then
       allocate(ztav(inzon)); ztav=0.0_rspec
    endif

    if(present(v_pars)) then
       allocate(zvav(inzon)); zvav=0.0_rspec
    endif

    !  map s -> sa  (full list to abridged; add up impurities & Z moments)

    icount = 0
    do ispec=1,ss%nspec_th
       indx=ss%s_to_sa(ispec)
       if(indx.gt.0) then
          if(present(ns)) ns(:,indx) = zns(:,ispec)
          if(present(Ts)) Ts(:,indx) = zTs(:,ispec)
          if(present(v_pars)) v_pars(:,indx) = zvs(:,ispec)
       else
          icount = icount + 1
          Zion = ss%q_s(ispec)/ps_xe
          nimp(:) = nimp(:) + zns(:,ispec)
          if(present(Ts)) then
             zTav(:) = zTav(:) + zTs(:,ispec)*zns(:,ispec)
          endif
          if(present(v_pars)) then
             zvav(:) = zvav(:) + zvs(:,ispec)*zns(:,ispec)
          endif
          zav(:) = zav(:) + Zion*zns(:,ispec)
          z2av(:) = z2av(:) + Zion*Zion*zns(:,ispec)
       endif
    enddo

    iertot = 0

    if(icount.gt.0) then
       ! last two indices of abridged list are amalgamated impurities
       
       ix1=ss%nspec_tha-1
       ix2=ss%nspec_tha

       iZ1=ss%q_sa(ix1)/ps_xe + 0.1_rspec
       iZ2=ss%q_sa(ix2)/ps_xe + 0.1_rspec

       do irho=1,inzon
          ztoln = ztol*zns(irho,0)  ! small fraction of electron density

          if(nimp(irho).lt.ztoln) then
             if(nimp(irho).lt.-ztoln) then
                iertot = iertot + 1
             endif
             if(present(iwarn)) iwarn=max(1,iwarn)
             !  (output array elements were already cleared to zero)
          else
             zav(irho)=zav(irho)/nimp(irho)
             z2av(irho)=z2av(irho)/nimp(irho)
             if(present(Ts)) zTav(irho)=zTav(irho)/nimp(irho)
             if(present(v_pars)) zvav(irho)=zvav(irho)/nimp(irho)
             
             !  Zeff/quasi-neutrality match solution
             call split_imp(zav(irho),z2av(irho),iZ1,iZ2,zf1,zf2,iertmp)

             if(min(zf1,zf2).lt.-ztol) then
                if(present(iwarn)) iwarn=max(2,iwarn)
             endif

             if(min(zf1,zf2).lt.ztol) then
                if(zf2.lt.ztol) then
                   zf1=1.0_rspec; zf2=0.0_rspec
                else
                   zf1=0.0_rspec; zf2=1.0_rspec
                endif
             endif

             if(present(ns)) then
                ns(irho,ix1)=zf1*nimp(irho)
                ns(irho,ix2)=zf2*nimp(irho)
             endif

             if(present(Ts)) then
                Ts(irho,ix1)=ztav(irho)
                Ts(irho,ix2)=ztav(irho)
             endif

             if(present(v_pars)) then
                v_pars(irho,ix1)=zvav(irho)
                v_pars(irho,ix2)=zvav(irho)
             endif
          endif
       enddo
    endif

    if(iertot.gt.0) then
       ierr=900
       write(iout,*) ' ?ps_tha_rezone: negative impurity density detected.'
    endif

    deallocate(zav,z2av,nimp)
    if(allocated(ztav)) deallocate(ztav)
    if(allocated(zvav)) deallocate(zvav)
    if(allocated(zns)) deallocate(zns,zvol)
    if(allocated(zts)) deallocate(zts)
    if(allocated(zvs)) deallocate(zvs)

  end SUBROUTINE ps_tha_rezone

  !==========================================================================
  !  Routines to fetch parameters summed over all thermal ion species:
  !  density, average temperature, <vpar>, rotation velocity.
  !
  !  ps_ti_fetch(...) -- profiles on original grid (ps%nrho)
  !  ps_ti_rezone(...) -- profiles rezoned to target grid; smoothing option
  !

  SUBROUTINE ps_ti_fetch(ierr,state,ni,Ti,omegat)

    !-------------------------------------------------
    ! D. McCune 1 Apr 2008:  fetch total density and density weighted
    !   average temperature and toroidal angular velocity, as requested
    !   by presence of optional outputs.
    !-------------------------------------------------

    INTEGER, intent(out) :: ierr      ! completion code (0=OK)

    !-------------------------
    !  OPTIONAL arguments...

    TYPE (plasma_state), OPTIONAL, target :: state
    !  operate on THIS state, if argument is present;
    !  otherwise use "ps" state.

    !-------------------------
    !  UNITS:  ni, m^-3; Ti, keV; omegat, rad/sec

    REAL(KIND=rspec), dimension(:), OPTIONAL, intent(out) :: ni ! density
    REAL(KIND=rspec), dimension(:), OPTIONAL, intent(out) :: Ti ! temperature
    REAL(KIND=rspec), dimension(:), OPTIONAL, intent(out) :: omegat
                                              ! toroidal angular velocity

    !-------------------------
    
    ! Local:

    TYPE (plasma_state), pointer :: ss
    integer :: iertot,iout,inzon,izon,ispec

    REAL(KIND=rspec), dimension(:), allocatable :: zni
    REAL(KIND=rspec), dimension(:), allocatable :: zg,zbr2,zx
    !-------------------------

    ierr=0

    iout=SWIM_err_out_file

    if(present(state)) then
       ss => state
    else
       ss => ps
    endif

    iertot = 0

    inzon=ss%nrho-1

    if(present(ni)) then
       ni=0.0_rspec
       call chksize_ti('ni',ni,inzon,iout,iertot)
    endif
    if(present(Ti)) then
       Ti=0.0_rspec
       call chksize_ti('Ti',Ti,inzon,iout,iertot)
    endif
    if(present(omegat)) then
       omegat=0.0_rspec
       call chksize_ti('omegat',omegat,inzon,iout,iertot)
       if(.not.allocated(ss%g_eq)) then
          iertot=iertot+1
          write(iout,*) ' ?ps_ti_fetch: equilibrium G_EQ profile needed'
          write(iout,*) '  for conversion of <v_par> to omegat.'
       endif
       if(.not.allocated(ss%gbr2)) then
          iertot=iertot+1
          write(iout,*) ' ?ps_ti_fetch: equilibrium <|B|*R^2> profile needed'
          write(iout,*) '  for conversion of <v_par> to omegat.'
       endif
       if(iertot.eq.0) then
          allocate(zx(inzon),zg(inzon),zbr2(inzon))
          zx = (ss%rho(1:inzon) + ss%rho(2:inzon+1))*0.5_rspec
          call ps_intrp_1d(zx,ss%id_g_eq,zg,ierr,state=ss)
          if(ierr.ne.0) then
             iertot = iertot + 1
             write(iout,*) ' ?ps_ti_fetch: equilibrium G_EQ profile interpolation error.'
          endif
          call ps_intrp_1d(zx,ss%id_gbr2,zbr2,ierr,state=ss)
          if(ierr.ne.0) then
             iertot = iertot + 1
             write(iout,*) ' ?ps_ti_fetch: equilibrium <|B|*R^2> profile interpolation error.'
          endif
          if(iertot.eq.0) then
             if(minval(zg).le.0.0_rspec) then
                iertot = iertot + 1
                write(iout,*) ' ?ps_ti_fetch: zero or negative value detected in G_EQ profile.'
             endif
             if(minval(zbr2).le.0.0_rspec) then
                iertot = iertot + 1
                write(iout,*) ' ?ps_ti_fetch: zero or negative value detected in <|B|*R^2> profile.'
             endif
          endif
       endif
    endif

    if(iertot.gt.0) then
       ierr=999
       if(allocated(zg)) deallocate(zx,zg,zbr2)
       return
    endif

    allocate(zni(inzon)); zni = 0.0_rspec

    do ispec=1,ss%nspec_th
       zni = zni + ss%ns(:,ispec)
       if(present(Ti)) ti = ti + ss%ns(:,ispec)*ss%Ts(:,ispec)
       if(present(omegat)) omegat = omegat + ss%ns(:,ispec)*ss%v_pars(:,ispec)
    enddo
    do izon=1,inzon
       if(present(ni)) ni(izon)=max(0.0_rspec,zni(izon))
       if(zni(izon).gt.0.0_rspec) then
          if(present(Ti)) Ti(izon)=Ti(izon)/zni(izon)
          if(present(omegat)) omegat(izon) = &
               (zg(izon)*omegat(izon))/(zni(izon)*zbr2(izon))
       else
          if(present(Ti)) Ti(izon)=0.0_rspec
          if(present(omegat)) omegat(izon)=0.0_rspec
       endif
    enddo

    deallocate(zni)
    if(allocated(zg)) deallocate(zx,zg,zbr2)

  end SUBROUTINE ps_ti_fetch

  SUBROUTINE ps_ti_rezone(rho,ierr,state,zonesmoo,ni,Ti,omegat,zonevol)

    !-------------------------------------------------
    ! D. McCune 1 Apr 2008:  fetch total density and density weighted
    !   average temperature and mass density weighted average toroidal 
    !   angular velocity, as requested by presence of optional outputs.
    !
    ! Rezone to target grid with conservation of volume integral of
    ! density or density weighted products.
    !-------------------------------------------------

    REAL(KIND=rspec), dimension(:) :: rho  ! target rho grid
    !  This must be strict ascending, covering the range [0,1] exactly,
    !  delineating zone BOUNDARIES.
    !  The 1st dimension of output arrays must be SIZE(rho)-1 (output by zone).

    INTEGER, intent(out) :: ierr      ! completion code (0=OK)

    !-------------------------
    !  OPTIONAL arguments...

    TYPE (plasma_state), OPTIONAL, target :: state
    !  operate on THIS state, if argument is present;
    !  otherwise use "ps" state.

    logical, intent(in), optional :: zonesmoo ! T to apply 1/2 zone width
    ! integral smoothing operator.  Default is F.  This option is not 
    ! recommended unless mapping coarse step function data to a fine grid
    ! with requirement to remove step structure from smoothed interpolant.

    !----------------
    !  UNITS:  ni, m^-3; Ti, keV; omegat, rad/sec

    REAL(KIND=rspec), dimension(:), OPTIONAL, intent(out) :: ni ! density
    REAL(KIND=rspec), dimension(:), OPTIONAL, intent(out) :: Ti ! temperature
    REAL(KIND=rspec), dimension(:), OPTIONAL, intent(out) :: omegat
                                              ! toroidal angular velocity
    REAL(KIND=rspec), dimension(:), OPTIONAL, intent(out) :: zoneVol
                                              ! volume of zones, m^3.

    !-------------------------
    
    ! Local:

    TYPE (plasma_state), pointer :: ss
    integer :: iertot,iout,inzon,izon,ispec
    logical :: ismoo

    REAL(KIND=rspec), dimension(:), allocatable :: zni,zmi,zfni,zfi,zvol
    REAL(KIND=rspec), dimension(:), allocatable :: zg,zbr2,zx
    !-------------------------

    ierr=0

    iout=SWIM_err_out_file

    if(present(state)) then
       ss => state
    else
       ss => ps
    endif

    iertot = 0

    inzon = size(rho)-1

    if(present(ni)) then
       ni=0.0_rspec
       call chksize_ti('ni',ni,inzon,iout,iertot)
    endif
    if(present(Ti)) then
       Ti=0.0_rspec
       call chksize_ti('Ti',Ti,inzon,iout,iertot)
    endif
    if(present(omegat)) then
       omegat=0.0_rspec
       call chksize_ti('omegat',omegat,inzon,iout,iertot)
       if(.not.allocated(ss%g_eq)) then
          iertot=iertot+1
          write(iout,*) ' ?ps_ti_fetch: equilibrium G_EQ profile needed'
          write(iout,*) '  for conversion of <v_par> to omegat.'
       endif
       if(.not.allocated(ss%gbr2)) then
          iertot=iertot+1
          write(iout,*) ' ?ps_ti_fetch: equilibrium <|B|*R^2> profile needed'
          write(iout,*) '  for conversion of <v_par> to omegat.'
       endif
       if(iertot.eq.0) then
          allocate(zx(inzon),zg(inzon),zbr2(inzon))
          zx = (rho(1:inzon) + rho(2:inzon+1))*0.5_rspec
          call ps_intrp_1d(zx,ss%id_g_eq,zg,ierr,state=ss)
          if(ierr.ne.0) then
             iertot = iertot + 1
             write(iout,*) ' ?ps_ti_fetch: equilibrium G_EQ profile interpolation error.'
          endif
          call ps_intrp_1d(zx,ss%id_gbr2,zbr2,ierr,state=ss)
          if(ierr.ne.0) then
             iertot = iertot + 1
             write(iout,*) ' ?ps_ti_fetch: equilibrium <|B|*R^2> profile interpolation error.'
          endif
          if(iertot.eq.0) then
             if(minval(zg).le.0.0_rspec) then
                iertot = iertot + 1
                write(iout,*) ' ?ps_ti_fetch: zero or negative value detected in G_EQ profile.'
             endif
             if(minval(zbr2).le.0.0_rspec) then
                iertot = iertot + 1
                write(iout,*) ' ?ps_ti_fetch: zero or negative value detected in <|B|*R^2> profile.'
             endif
          endif
       endif
    endif
    if(present(zoneVol)) then
       zoneVol = 0.0_rspec
       call chksize_ti('zoneVol',zoneVol,inzon,iout,iertot)
    endif

    if(iertot.gt.0) then
       ierr=999
       if(allocated(zg)) deallocate(zx,zg,zbr2)
       return
    endif

    allocate(zni(inzon),zfni(inzon),zfi(inzon),zvol(inzon)); zni = 0.0_rspec
    allocate(zmi(inzon)); zmi = 0.0_rspec  ! mass density

    ismoo=.FALSE.
    if(present(zonesmoo)) ismoo=zonesmoo

    !  as a test, get the volumes & electron density;
    !  if no errors here, assume that subsequent ps_rho_rezone1 calls work...

    call ps_rho_rezone1(rho,ss%id_ns(0),zfni,ierr, &
         state=ss, zonesmoo=ismoo, zonediff=zvol)

    if(ierr.ne.0) then
       deallocate(zni,zmi,zfni,zfi,zvol)
       if(allocated(zg)) deallocate(zx,zg,zbr2)
       write(iout,*) ' ?ps_ti_rezone: ps_rho_rezone1 error.'
       return
    endif

    !  OK...

    if(present(zoneVol)) zoneVol = zvol

    do ispec=1,ss%nspec_th
       call ps_rho_rezone1(rho,ss%id_ns(ispec),zfni,ierr, &
            state=ss, zonesmoo=ismoo)
       zni = zni + zfni
       zmi = zmi + zfni*ss%m_s(ispec)
       if(present(Ti)) then
          call ps_rho_rezone1(rho,ss%id_Ts(ispec),zfi,ierr, &
               state=ss, zonesmoo=ismoo)
          ti = ti + zfi*zfni
       endif
       if(present(omegat)) then
          call ps_rho_rezone1(rho,ss%id_v_pars(ispec),zfi,ierr, &
               state=ss, zonesmoo=ismoo)
          omegat = omegat + zfi*zfni*ss%m_s(ispec)
       endif
    enddo

    do izon=1,inzon
       if(present(ni)) ni(izon)=max(0.0_rspec,zni(izon))
       if(zni(izon).gt.0.0_rspec) then
          if(present(Ti)) Ti(izon)=Ti(izon)/zni(izon)
          if(present(omegat)) omegat(izon) = &
               (zg(izon)*omegat(izon))/(zmi(izon)*zbr2(izon))
       else
          if(present(Ti)) Ti(izon)=0.0_rspec
          if(present(omegat)) omegat(izon)=0.0_rspec
       endif
    enddo

    deallocate(zni,zmi,zfni,zfi,zvol)

  end SUBROUTINE ps_ti_rezone

  !-------------------------------------
  ! (private) error checking routines

  subroutine chksize_ti(znam,zarr,isiz,iout,iertot)

    character*(*), intent(in) :: znam
    REAL(KIND=rspec), dimension(:), intent(in) :: zarr
    integer, intent(in) :: isiz
    integer, intent(in) :: iout
    integer, intent(inout) :: iertot

    if(size(zarr).ne.isiz) then
       iertot = iertot + 1
       write(iout,*) ' ?ps_ti_fetch or ps_ti_rezone: array '//trim(znam)//' has incorrect size:'
       write(iout,*) '  expected size: ',isiz,'  actual size: ',size(zarr)
    endif

  end subroutine chksize_ti

  subroutine tha_dimchk(srnam,anam,idim1,idim2,array,iout,ierr)
    ! check that two dimensional array sizes are correct

    character*(*), intent(in) :: srnam  ! subroutine name
    character*(*), intent(in) :: anam   ! array name
    integer, intent(in) :: idim1,idim2  ! expected dimension sizes
    REAL(KIND=rspec), dimension(:,:) :: array   ! array to check
    integer, intent(in) :: iout
    integer, intent(out) :: ierr

    !----------------------
    integer :: isiz1,isiz2
    !----------------------
    
    isiz1=size(array,1)
    isiz2=size(array,2)

    if((idim1.ne.isiz1).or.(idim2.ne.isiz2)) then
       write(iout,*) ' ?error in plasma_state_mod subroutine: ',trim(srnam)
       write(iout,*) '  '//trim(anam)//' array dimension sizes do not match expected sizes:'
       write(iout,*) '                      expected   received'
       write(iout,*) '     grid dimension:   ',idim1,'   ',isiz1
       write(iout,*) '  species dimension:   ',idim2,'   ',isiz2
       ierr=9999
    else
       ierr=0
    endif

  end subroutine tha_dimchk
    
  subroutine tha_gridchk(srnam,ss,iout,ierr)
    ! check: (a) species list dimension defined & list is allocated
    !        (b) grid dimension defined & associated profiles allocated

    character*(*), intent(in) :: srnam
    TYPE (plasma_state) :: ss
    integer, intent(in) :: iout
    integer, intent(out) :: ierr

    !---------------
    integer :: iersum
    !---------------

    iersum = 0
    if(ss%nspec_th.le.0) then
       write(iout,*) ' ?main thermal species list undefined: ss%nspec_th = 0'
       iersum = iersum + 1
    else if(ss%nspec_tha.le.0) then
       write(iout,*) ' ?abridged thermal species list undefined: ss%nspec_tha = 0'
       iersum = iersum + 1
    else
       if(.not.allocated(ss%s_to_sa)) then
          write(iout,*) ' ?abridged species mapping ss%s_to_sa not allocated.'
          iersum = iersum + 1
       else if(ss%s_to_sa(1).eq.0) then
          write(iout,*) ' ?abridged species mapping ss%s_to_sa not initialized (0 element detected).'
          iersum = iersum + 1
       endif
    endif

    if(.not.allocated(ss%ns)) then
       write(iout,*) ' ?main thermal species density array "ns" not allocated.'
       iersum = iersum + 1
    endif

    if(iersum.gt.0) then
       write(iout,*) ' ? error(s) detected in plasma_state_mod subroutine: ', &
            trim(srnam)
       ierr=999
    else
       ierr=0
    endif

  end subroutine tha_gridchk
  !-------------------------------------
  !==========================================================================
  !  Ps_intrp_1d  --  scalar or vector target interpolation
  !    on one profile or arrays of profiles over a single coordinate
  !==========================================================================

  SUBROUTINE ps_intrp_1d_sc(x,id,ans,ierr,state,ideriv,iccw_th)

    !-------------------------------------------------
    ! D. McCune 12 Oct 2006
    ! interpolation routine for a single 1d profile
    ! scalar argument & answer
    !-------------------------------------------------

    REAL(KIND=rspec), intent(in) :: x ! interpolation target point
    INTEGER, intent(in) :: id         ! ID of profile to interpolate
    REAL(KIND=rspec), intent(out) :: ans  ! result of interpolation
    INTEGER, intent(out) :: ierr      ! completion code (0=OK)

    TYPE (plasma_state), optional, target :: state
    !  operate on THIS state, if argument is present;
    !  otherwise use "ps" state.

    integer, intent(in), optional :: ideriv   ! derivative control
    ! 0 (default) for none, 1 for d/dx, 2 for d2/dx2
    ! error will be reported if interpolation method does not support 

    integer, intent(in), optional :: iccw_th
    ! flag for reversal of "theta" coordinate (default: ps_ccw_select).
    ! this is included with all interpolation routines, but is only needed
    ! when one of the grids interpolated on corresponds to the poloidal
    ! angle flux coordinate-- and clockwise interpretation is desired.

    !--------------------
    !  local:
    REAL(KIND=rspec) :: xv(1),ansv(1)
    !--------------------

    xv=x
    CALL ps_intrp_1d_vec(xv,id,ansv,ierr,state,ideriv,iccw_th)
    if(ierr.eq.0) then
       ans=ansv(1)
    else
       ans=0.0_rspec
    endif

  END SUBROUTINE ps_intrp_1d_sc
    
  SUBROUTINE ps_intrp_1d_vec(xv,id,ansv,ierr,state,ideriv,iccw_th)

    !-------------------------------------------------
    ! D. McCune 12 Oct 2006
    ! interpolation routine for a single 1d profile
    ! vector argument & answer
    !-------------------------------------------------

    REAL(KIND=rspec), dimension(:), intent(in) :: xv ! interpolation target
    INTEGER, intent(in) :: id                        ! ID of profile 
    REAL(KIND=rspec), dimension(:), intent(out) :: ansv  ! results
    INTEGER, intent(out) :: ierr          ! completion code (0=OK)

    TYPE (plasma_state), optional, target :: state
    !  operate on THIS state, if argument is present;
    !  otherwise use "ps" state.

    integer, intent(in), optional :: ideriv   ! derivative control
    ! 0 (default) for none, 1 for d/dx, 2 for d2/dx2
    ! error will be reported if interpolation method does not support 

    integer, intent(in), optional :: iccw_th
    ! flag for reversal of "theta" coordinate (default: ps_ccw_select).
    ! this is included with all interpolation routines, but is only needed
    ! when one of the grids interpolated on corresponds to the poloidal
    ! angle flux coordinate-- and clockwise interpretation is desired.

    !--------------------
    !  local:

    integer :: iout,jderiv,jccw_th

    TYPE (plasma_state), pointer :: ss
    !--------------------

    iout=SWIM_err_out_file

    call schk('ps_intrp_1d','size(xv)',size(xv),'size(ansv)',size(ansv),ierr)
    if(ierr.ne.0) return

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    jderiv=0
    if(present(ideriv)) jderiv=ideriv

    jccw_th = ps_ccw_select
    if(present(iccw_th)) jccw_th = iccw_th

    CALL ps_intrp1(size(xv),xv,id,ss%iobj,jderiv,jccw_th,ansv,iout,ierr)
    if(ierr.ne.0) then
       write(iout,*) &
            ' ?ps_intrp_1d: ps_intrp1 returned non-zero error code: ',ierr
       ansv = 0.0_rspec
    endif

  END SUBROUTINE ps_intrp_1d_vec

  SUBROUTINE ps_intrp_1d_sc_arr1(x,idv,ansv,ierr,state,ideriv,iderivs,iccw_th)

    !-------------------------------------------------
    ! D. McCune 12 Oct 2006
    ! interpolation routine for a list of 1d profiles
    ! scalar argument; 1 answer for each 1d profile
    !-------------------------------------------------

    REAL(KIND=rspec), intent(in) :: x  ! interpolation target point
    INTEGER, intent(in), dimension(:) :: idv  ! vector of IDs of profiles
    REAL(KIND=rspec), dimension(:), intent(out) :: ansv  ! results
    INTEGER, intent(out) :: ierr      ! completion code (0=OK)

    TYPE (plasma_state), optional, target :: state
    !  operate on THIS state, if argument is present;
    !  otherwise use "ps" state.

    integer, intent(in), optional :: ideriv   ! derivative control
    ! 0 (default) for none, 1 for d/dx, 2 for d2/dx2
    ! error will be reported if interpolation method does not support 
    ! this control applies to all profiles

    integer, intent(in), dimension(:), optional :: iderivs ! derivative control
    ! same as ideriv, but separate control for each profile (overrides ideriv).

    integer, intent(in), optional :: iccw_th
    ! flag for reversal of "theta" coordinate (default: ps_ccw_select).
    ! this is included with all interpolation routines, but is only needed
    ! when one of the grids interpolated on corresponds to the poloidal
    ! angle flux coordinate-- and clockwise interpretation is desired.

    !--------------------
    !  local:
    integer :: iout
    REAL(KIND=rspec) :: xv(1),ansa(1,size(idv))
    !--------------------

    call schk('ps_intrp_1d','size(idv)',size(idv),'size(ansv)',size(ansv),ierr)
    if(ierr.ne.0) return

    xv = x
    CALL ps_intrp_1d_vec_arr1(xv,idv,ansa,ierr,state,ideriv,iderivs,iccw_th)
    if(ierr.eq.0) then
       ansv = ansa(1,1:size(idv))
    else
       ansv = 0.0_rspec
    endif

  END SUBROUTINE ps_intrp_1d_sc_arr1
    
  SUBROUTINE ps_intrp_1d_vec_arr1(xv,idv,ansa,ierr,state,ideriv,iderivs,iccw_th)

    !-------------------------------------------------
    ! D. McCune 12 Oct 2006
    ! interpolation routine for a list of 1d profile
    ! vector argument & list of vector answer
    !-------------------------------------------------

    REAL(KIND=rspec), dimension(:), intent(in) :: xv ! interpolation target
    INTEGER, intent(in), dimension(:) :: idv         ! ID of profiles
    REAL(KIND=rspec), dimension(:,:), intent(out) :: ansa  ! results
    INTEGER, intent(out) :: ierr      ! completion code (0=OK)

    TYPE (plasma_state), optional, target :: state
    !  operate on THIS state, if argument is present;
    !  otherwise use "ps" state.

    integer, intent(in), optional :: ideriv   ! derivative control
    ! 0 (default) for none, 1 for d/dx, 2 for d2/dx2
    ! error will be reported if interpolation method does not support 
    ! this control applies to all profiles

    integer, intent(in), dimension(:), optional :: iderivs ! derivative control
    ! same as ideriv, but separate control for each profile (overrides ideriv).

    integer, intent(in), optional :: iccw_th
    ! flag for reversal of "theta" coordinate (default: ps_ccw_select).
    ! this is included with all interpolation routines, but is only needed
    ! when one of the grids interpolated on corresponds to the poloidal
    ! angle flux coordinate-- and clockwise interpretation is desired.

    !--------------------
    !  local:
    integer :: iout,jderivs(size(idv))
    integer :: jccw_th

    TYPE (plasma_state), pointer :: ss
    !--------------------

    iout=SWIM_err_out_file

    call schk('ps_intrp_1d','size(xv)',size(xv),'size(ansa,1)',size(ansa,1), &
         ierr)
    if(ierr.ne.0) return

    call schk('ps_intrp_1d','size(idv)',size(idv),'size(ansa,2)',size(ansa,2),&
         ierr)
    if(ierr.ne.0) return

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    if(present(iderivs)) then
       call schk('ps_intrp_1d','size(idv)',size(idv), &
            'size(iderivs)',size(iderivs), ierr)
       if(ierr.ne.0) return
    endif

    if(present(iderivs)) then
       jderivs = iderivs
    else if(present(ideriv)) then
       jderivs = ideriv
    else
       jderivs = 0
    endif

    jccw_th = ps_ccw_select
    if(present(iccw_th)) jccw_th = iccw_th

    CALL ps_intrp1s(size(xv),xv,size(idv),idv,ss%iobj,jderivs,jccw_th,ansa, &
         iout,ierr)
    if(ierr.ne.0) then
       write(iout,*) &
            ' ?ps_intrp_1d: ps_intrp1s returned non-zero error code: ',ierr
       ansa = 0.0_rspec
    endif

  END SUBROUTINE ps_intrp_1d_vec_arr1

  SUBROUTINE ps_intrp_1d_sc_arr2(x,ida,ansa,ierr,state,ideriv,iderivs,iccw_th)

    !-------------------------------------------------
    ! D. McCune 12 Oct 2006
    ! interpolation routine for a 2d list of 1d profiles
    ! scalar argument; 1 answer for each 1d profile
    !-------------------------------------------------

    REAL(KIND=rspec), intent(in) :: x  ! interpolation target point
    INTEGER, intent(in), dimension(:,:) :: ida  ! vector of IDs of profiles
    REAL(KIND=rspec), dimension(:,:), intent(out) :: ansa  ! results
    INTEGER, intent(out) :: ierr      ! completion code (0=OK)

    TYPE (plasma_state), optional, target :: state
    !  operate on THIS state, if argument is present;
    !  otherwise use "ps" state.

    integer, intent(in), optional :: ideriv   ! derivative control
    ! 0 (default) for none, 1 for d/dx, 2 for d2/dx2
    ! error will be reported if interpolation method does not support 
    ! this control applies to all profiles

    integer, intent(in), dimension(:,:), optional :: iderivs ! deriv. control
    ! same as ideriv, but separate control for each profile (overrides ideriv).

    integer, intent(in), optional :: iccw_th
    ! flag for reversal of "theta" coordinate (default: ps_ccw_select).
    ! this is included with all interpolation routines, but is only needed
    ! when one of the grids interpolated on corresponds to the poloidal
    ! angle flux coordinate-- and clockwise interpretation is desired.

    !--------------------
    !  local:
    integer :: iout
    REAL(KIND=rspec) :: xv(1)
    REAL(KIND=rspec), dimension(:,:,:), allocatable :: ansaa
    !--------------------

    call schk('ps_intrp_1d','size(ida,1)',size(ida,1), &
         'size(ansa,1)',size(ansa,1), ierr)
    if(ierr.ne.0) return

    call schk('ps_intrp_1d','size(ida,2)',size(ida,2), &
         'size(ansa,2)',size(ansa,2), ierr)
    if(ierr.ne.0) return

    allocate(ansaa(1,size(ansa,1),size(ansa,2)))

    xv = x
    CALL ps_intrp_1d_vec_arr2(xv,ida,ansaa,ierr,state,ideriv,iderivs,iccw_th)
    if(ierr.eq.0) then
       ansa = ansaa(1,1:size(ansa,1),1:size(ansa,2))
    else
       ansa = 0.0_rspec
    endif

    deallocate(ansaa)

  END SUBROUTINE ps_intrp_1d_sc_arr2
    
  SUBROUTINE ps_intrp_1d_vec_arr2(xv,ida,ansa,ierr,state,ideriv,iderivs,iccw_th)

    !-------------------------------------------------
    ! D. McCune 12 Oct 2006
    ! interpolation routine for a list of 1d profile
    ! vector argument & list of vector answer
    !-------------------------------------------------

    REAL(KIND=rspec), dimension(:), intent(in) :: xv ! interpolation target
    INTEGER, intent(in), dimension(:,:) :: ida       ! ID of profiles
    REAL(KIND=rspec), dimension(:,:,:), intent(out) :: ansa  ! results
    INTEGER, intent(out) :: ierr      ! completion code (0=OK)

    TYPE (plasma_state), optional, target :: state
    !  operate on THIS state, if argument is present;
    !  otherwise use "ps" state.

    integer, intent(in), optional :: ideriv   ! derivative control
    ! 0 (default) for none, 1 for d/dx, 2 for d2/dx2
    ! error will be reported if interpolation method does not support 
    ! this control applies to all profiles

    integer, intent(in), dimension(:,:), optional :: iderivs ! deriv. control
    ! same as ideriv, but separate control for each profile (overrides ideriv).

    integer, intent(in), optional :: iccw_th
    ! flag for reversal of "theta" coordinate (default: ps_ccw_select).
    ! this is included with all interpolation routines, but is only needed
    ! when one of the grids interpolated on corresponds to the poloidal
    ! angle flux coordinate-- and clockwise interpretation is desired.

    !--------------------
    !  local:
    integer :: iout,ii,i1,i2,ivec,isiz1,isiz2,isiza
    integer, dimension(:), allocatable :: idv,jderivs
    REAL(KIND=rspec), dimension(:,:), allocatable :: ansaa
    integer :: jccw_th

    TYPE (plasma_state), pointer :: ss
    !--------------------

    iout=SWIM_err_out_file

    ivec = size(xv)
    isiz1 = size(ida,1)
    isiz2 = size(ida,2)
    isiza = isiz1*isiz2

    call schk('ps_intrp_1d','size(xv)',ivec, &
         'size(ansa,1)',size(ansa,1), ierr)

    call schk('ps_intrp_1d','size(ida,1)',isiz1, &
         'size(ansa,2)',size(ansa,2), ierr)
    if(ierr.ne.0) return

    call schk('ps_intrp_1d','size(ida,2)',isiz2, &
         'size(ansa,3)',size(ansa,3), ierr)
    if(ierr.ne.0) return

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    if(present(iderivs)) then
       call schk('ps_intrp_1d','size(ida,1)',isiz1, &
            'size(iderivs,1)',size(iderivs,1), ierr)
       if(ierr.ne.0) return

       call schk('ps_intrp_1d','size(ida,2)',isiz2, &
            'size(iderivs,2)',size(iderivs,2), ierr)
       if(ierr.ne.0) return
    endif

    allocate(idv(isiza),jderivs(isiza))
    allocate(ansaa(ivec,isiza))
    
    ii = 0
    do i2=1,isiz2
       do i1=1,isiz1
          ii = ii + 1
          idv(ii) = ida(i1,i2)
          if(present(iderivs)) then
             jderivs(ii) = iderivs(i1,i2)
          else if(present(ideriv)) then
             jderivs(ii) = ideriv
          else
             jderivs(ii) = 0
          endif
       enddo
    enddo

    jccw_th = ps_ccw_select
    if(present(iccw_th)) jccw_th = iccw_th

    CALL ps_intrp1s(ivec,xv,isiza,idv,ss%iobj,jderivs,jccw_th,ansaa, &
         iout,ierr)

    if(ierr.ne.0) then
       write(iout,*) &
            ' ?ps_intrp_1d: ps_intrp1s returned non-zero error code: ',ierr
       ansa = 0.0_rspec

    else
       ii = 0
       do i2=1,isiz2
          do i1=1,isiz1
             ii = ii + 1
             ansa(1:ivec,i1,i2) = ansaa(1:ivec,ii)
          enddo
       enddo
    endif

    deallocate(idv,jderivs,ansaa)

  END SUBROUTINE ps_intrp_1d_vec_arr2

  !==========================================================================
  !  ps_intrp_2d  --  scalar or vector target interpolation
  !    on one profile or arrays of profiles over a single coordinate
  !==========================================================================

  SUBROUTINE ps_intrp_2d_sc(x1,x2,id,ans,ierr,state,ideriv1,ideriv2,iccw_th)

    !-------------------------------------------------
    ! D. McCune 12 Oct 2006
    ! interpolation routine for a single 2d profile
    ! scalar argument & answer
    !-------------------------------------------------

    REAL(KIND=rspec), intent(in) :: x1,x2 ! interpolation target point
    INTEGER, intent(in) :: id         ! ID of profile to interpolate
    REAL(KIND=rspec), intent(out) :: ans  ! result of interpolation
    INTEGER, intent(out) :: ierr      ! completion code (0=OK)

    TYPE (plasma_state), optional, target :: state
    !  operate on THIS state, if argument is present;
    !  otherwise use "ps" state.

    integer, intent(in), optional :: ideriv1,ideriv2   ! derivative control
    ! 0 (default) for none, 1 for d/dx, 2 for d2/dx2
    ! error will be reported if interpolation method does not support 
    ! one control for each coordinate.

    integer, intent(in), optional :: iccw_th
    ! flag for reversal of "theta" coordinate (default: ps_ccw_select).
    ! this is included with all interpolation routines, but is only needed
    ! when one of the grids interpolated on corresponds to the poloidal
    ! angle flux coordinate-- and clockwise interpretation is desired.

    !--------------------
    !  local:
    REAL(KIND=rspec) :: x1v(1),x2v(1),ansv(1)
    !--------------------

    x1v=x1
    x2v=x2

    CALL ps_intrp_2d_vec(x1v,x2v,id,ansv,ierr,state,ideriv1,ideriv2,iccw_th)
    if(ierr.eq.0) then
       ans=ansv(1)
    else
       ans=0.0_rspec
    endif

  END SUBROUTINE ps_intrp_2d_sc
    
  SUBROUTINE ps_intrp_2d_vec(x1v,x2v,id,ansv,ierr,state,ideriv1,ideriv2,iccw_th)

    !-------------------------------------------------
    ! D. McCune 12 Oct 2006
    ! interpolation routine for a single 2d profile
    ! vector argument & answer
    !-------------------------------------------------

    REAL(KIND=rspec), dimension(:), intent(in) :: x1v,x2v ! target vectors
    INTEGER, intent(in) :: id                        ! ID of profile 
    REAL(KIND=rspec), dimension(:), intent(out) :: ansv  ! results
    INTEGER, intent(out) :: ierr          ! completion code (0=OK)

    TYPE (plasma_state), optional, target :: state
    !  operate on THIS state, if argument is present;
    !  otherwise use "ps" state.

    integer, intent(in), optional :: ideriv1,ideriv2   ! derivative control
    ! 0 (default) for none, 1 for d/dx, 2 for d2/dx2
    ! error will be reported if interpolation method does not support 
    ! one control for each coordinate.

    integer, intent(in), optional :: iccw_th
    ! flag for reversal of "theta" coordinate (default: ps_ccw_select).
    ! this is included with all interpolation routines, but is only needed
    ! when one of the grids interpolated on corresponds to the poloidal
    ! angle flux coordinate-- and clockwise interpretation is desired.

    !--------------------
    !  local:

    integer :: iout,jderiv1,jderiv2,jccw_th

    TYPE (plasma_state), pointer :: ss
    !--------------------

    iout=SWIM_err_out_file

    call schk('ps_intrp_2d','size(x1v)',size(x1v),'size(ansv)',size(ansv),ierr)
    if(ierr.ne.0) return

    call schk('ps_intrp_2d','size(x2v)',size(x2v),'size(ansv)',size(ansv),ierr)
    if(ierr.ne.0) return

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    jderiv1=0
    if(present(ideriv1)) jderiv1=ideriv1

    jderiv2=0
    if(present(ideriv2)) jderiv2=ideriv2

    jccw_th = ps_ccw_select
    if(present(iccw_th)) jccw_th = iccw_th

    CALL ps_intrp2(size(x1v),x1v,x2v,id,ss%iobj,jderiv1,jderiv2,jccw_th, &
         ansv,iout,ierr)
    if(ierr.ne.0) then
       write(iout,*) &
            ' ?ps_intrp_2d: ps_intrp2 returned non-zero error code: ',ierr
       ansv = 0.0_rspec
    endif

  END SUBROUTINE ps_intrp_2d_vec

  SUBROUTINE ps_intrp_2d_sc_arr1(x1,x2,idv,ansv,ierr, &
       state,ideriv1,ideriv2,ideriv1s,ideriv2s,iccw_th)

    !-------------------------------------------------
    ! D. McCune 12 Oct 2006
    ! interpolation routine for a list of 2d profiles
    ! scalar argument; 1 answer for each 2d profile
    !-------------------------------------------------

    REAL(KIND=rspec), intent(in) :: x1,x2  ! interpolation target point
    INTEGER, intent(in), dimension(:) :: idv  ! vector of IDs of profiles
    REAL(KIND=rspec), dimension(:), intent(out) :: ansv  ! results
    INTEGER, intent(out) :: ierr      ! completion code (0=OK)

    TYPE (plasma_state), optional, target :: state
    !  operate on THIS state, if argument is present;
    !  otherwise use "ps" state.

    integer, intent(in), optional :: ideriv1,ideriv2   ! derivative control
    ! 0 (default) for none, 1 for d/dx, 2 for d2/dx2
    ! error will be reported if interpolation method does not support 
    ! this control applies to all profiles

    integer, intent(in), dimension(:), optional :: ideriv1s,ideriv2s
    ! derivative control
    ! same as {ideriv1,ideriv2}, but, separate control for each profile
    ! (overrides {ideriv1,ideriv2}).

    integer, intent(in), optional :: iccw_th
    ! flag for reversal of "theta" coordinate (default: ps_ccw_select).
    ! this is included with all interpolation routines, but is only needed
    ! when one of the grids interpolated on corresponds to the poloidal
    ! angle flux coordinate-- and clockwise interpretation is desired.

    !--------------------
    !  local:
    integer :: iout
    REAL(KIND=rspec) :: x1v(1),x2v(1),ansa(1,size(idv))
    !--------------------

    call schk('ps_intrp_2d','size(idv)',size(idv),'size(ansv)',size(ansv),ierr)
    if(ierr.ne.0) return

    x1v = x1
    x2v = x2
    CALL ps_intrp_2d_vec_arr1(x1v,x2v,idv,ansa,ierr,state, &
         ideriv1,ideriv2,ideriv1s,ideriv2s,iccw_th)
    if(ierr.eq.0) then
       ansv = ansa(1,1:size(idv))
    else
       ansv = 0.0_rspec
    endif

  END SUBROUTINE ps_intrp_2d_sc_arr1
    
  SUBROUTINE ps_intrp_2d_vec_arr1(x1v,x2v,idv,ansa,ierr, &
       state,ideriv1,ideriv2,ideriv1s,ideriv2s,iccw_th)

    !-------------------------------------------------
    ! D. McCune 12 Oct 2006
    ! interpolation routine for a list of 2d profile
    ! vector argument & list of vector answer
    !-------------------------------------------------

    REAL(KIND=rspec), dimension(:), intent(in) :: x1v,x2v ! interp. target
    INTEGER, intent(in), dimension(:) :: idv         ! ID of profiles
    REAL(KIND=rspec), dimension(:,:), intent(out) :: ansa  ! results
    INTEGER, intent(out) :: ierr      ! completion code (0=OK)

    TYPE (plasma_state), optional, target :: state
    !  operate on THIS state, if argument is present;
    !  otherwise use "ps" state.

    integer, intent(in), optional :: ideriv1,ideriv2   ! derivative control
    ! 0 (default) for none, 1 for d/dx, 2 for d2/dx2
    ! error will be reported if interpolation method does not support 
    ! this control applies to all profiles

    integer, intent(in), dimension(:), optional :: ideriv1s,ideriv2s
    ! derivative control
    ! same as {ideriv1,ideriv2}, but, separate control for each profile
    ! (overrides {ideriv1,ideriv2}).

    integer, intent(in), optional :: iccw_th
    ! flag for reversal of "theta" coordinate (default: ps_ccw_select).
    ! this is included with all interpolation routines, but is only needed
    ! when one of the grids interpolated on corresponds to the poloidal
    ! angle flux coordinate-- and clockwise interpretation is desired.

    !--------------------
    !  local:
    integer :: iout,jderiv1s(size(idv)),jderiv2s(size(idv))
    integer :: jccw_th

    TYPE (plasma_state), pointer :: ss
    !--------------------

    iout=SWIM_err_out_file

    call schk('ps_intrp_2d','size(x1v)',size(x1v),'size(ansa,1)',size(ansa,1),&
         ierr)
    if(ierr.ne.0) return

    call schk('ps_intrp_2d','size(x2v)',size(x2v),'size(ansa,1)',size(ansa,1),&
         ierr)
    if(ierr.ne.0) return

    call schk('ps_intrp_2d','size(idv)',size(idv),'size(ansa,2)',size(ansa,2),&
         ierr)
    if(ierr.ne.0) return

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    if(present(ideriv1s)) then
       call schk('ps_intrp_2d','size(idv)',size(idv), &
            'size(ideriv1s)',size(ideriv1s), ierr)
       if(ierr.ne.0) return
    endif

    if(present(ideriv1s)) then
       jderiv1s = ideriv1s
    else if(present(ideriv1)) then
       jderiv1s = ideriv1
    else
       jderiv1s = 0
    endif

    if(present(ideriv2s)) then
       call schk('ps_intrp_2d','size(idv)',size(idv), &
            'size(ideriv2s)',size(ideriv2s), ierr)
       if(ierr.ne.0) return
    endif

    if(present(ideriv2s)) then
       jderiv2s = ideriv2s
    else if(present(ideriv2)) then
       jderiv2s = ideriv2
    else
       jderiv2s = 0
    endif

    jccw_th = ps_ccw_select
    if(present(iccw_th)) jccw_th = iccw_th

    CALL ps_intrp2s(size(x1v),x1v,x2v,size(idv),idv, &
         ss%iobj,jderiv1s,jderiv2s,jccw_th,ansa, &
         iout,ierr)
    if(ierr.ne.0) then
       write(iout,*) &
            ' ?ps_intrp_2d: ps_intrp2s returned non-zero error code: ',ierr
       ansa = 0.0_rspec
    endif

  END SUBROUTINE ps_intrp_2d_vec_arr1

  SUBROUTINE ps_intrp_2d_sc_arr2(x1,x2,ida,ansa,ierr, &
       state,ideriv1,ideriv2,ideriv1s,ideriv2s,iccw_th)

    !-------------------------------------------------
    ! D. McCune 12 Oct 2006
    ! interpolation routine for a 2d list of 2d profiles
    ! scalar argument; 1 answer for each 2d profile
    !-------------------------------------------------

    REAL(KIND=rspec), intent(in) :: x1,x2  ! interpolation target point
    INTEGER, intent(in), dimension(:,:) :: ida  ! vector of IDs of profiles
    REAL(KIND=rspec), dimension(:,:), intent(out) :: ansa  ! results
    INTEGER, intent(out) :: ierr      ! completion code (0=OK)

    TYPE (plasma_state), optional, target :: state
    !  operate on THIS state, if argument is present;
    !  otherwise use "ps" state.

    integer, intent(in), optional :: ideriv1,ideriv2   ! derivative control
    ! 0 (default) for none, 1 for d/dx, 2 for d2/dx2
    ! error will be reported if interpolation method does not support 
    ! this control applies to all profiles

    integer, intent(in), dimension(:,:), optional :: ideriv1s,ideriv2s
    ! derivative control
    ! same as {ideriv1,ideriv2}, but, separate control for each profile
    ! (overrides {ideriv1,ideriv2}).

    integer, intent(in), optional :: iccw_th
    ! flag for reversal of "theta" coordinate (default: ps_ccw_select).
    ! this is included with all interpolation routines, but is only needed
    ! when one of the grids interpolated on corresponds to the poloidal
    ! angle flux coordinate-- and clockwise interpretation is desired.

    !--------------------
    !  local:
    integer :: iout
    REAL(KIND=rspec) :: x1v(1),x2v(1)
    REAL(KIND=rspec), dimension(:,:,:), allocatable :: ansaa
    !--------------------

    call schk('ps_intrp_2d','size(ida,1)',size(ida,1), &
         'size(ansa,1)',size(ansa,1), ierr)
    if(ierr.ne.0) return

    call schk('ps_intrp_2d','size(ida,2)',size(ida,2), &
         'size(ansa,2)',size(ansa,2), ierr)
    if(ierr.ne.0) return

    allocate(ansaa(1,size(ansa,1),size(ansa,2)))

    x1v = x1
    x2v = x2
    CALL ps_intrp_2d_vec_arr2(x1v,x2v,ida,ansaa,ierr,state, &
         ideriv1,ideriv2,ideriv1s,ideriv2s,iccw_th)
    if(ierr.eq.0) then
       ansa = ansaa(1,1:size(ansa,1),1:size(ansa,2))
    else
       ansa = 0.0_rspec
    endif

    deallocate(ansaa)

  END SUBROUTINE ps_intrp_2d_sc_arr2
    
  SUBROUTINE ps_intrp_2d_vec_arr2(x1v,x2v,ida,ansa,ierr, &
       state,ideriv1,ideriv2,ideriv1s,ideriv2s,iccw_th)

    !-------------------------------------------------
    ! D. McCune 12 Oct 2006
    ! interpolation routine for a list of 2d profile
    ! vector argument & list of vector answer
    !-------------------------------------------------

    REAL(KIND=rspec), dimension(:), intent(in) :: x1v,x2v ! interp. target
    INTEGER, intent(in), dimension(:,:) :: ida       ! ID of profiles
    REAL(KIND=rspec), dimension(:,:,:), intent(out) :: ansa  ! results
    INTEGER, intent(out) :: ierr      ! completion code (0=OK)

    TYPE (plasma_state), optional, target :: state
    !  operate on THIS state, if argument is present;
    !  otherwise use "ps" state.

    integer, intent(in), optional :: ideriv1,ideriv2   ! derivative control
    ! 0 (default) for none, 1 for d/dx, 2 for d2/dx2
    ! error will be reported if interpolation method does not support 
    ! this control applies to all profiles

    integer, intent(in), dimension(:,:), optional :: ideriv1s,ideriv2s
    ! derivative control
    ! same as {ideriv1,ideriv2}, but, separate control for each profile
    ! (overrides {ideriv1,ideriv2}).

    integer, intent(in), optional :: iccw_th
    ! flag for reversal of "theta" coordinate (default: ps_ccw_select).
    ! this is included with all interpolation routines, but is only needed
    ! when one of the grids interpolated on corresponds to the poloidal
    ! angle flux coordinate-- and clockwise interpretation is desired.

    !--------------------
    !  local:
    integer :: iout,ii,i1,i2,ivec,isiz1,isiz2,isiza
    integer, dimension(:), allocatable :: idv,jderiv1s,jderiv2s
    REAL(KIND=rspec), dimension(:,:), allocatable :: ansaa
    integer :: jccw_th

    TYPE (plasma_state), pointer :: ss
    !--------------------

    iout=SWIM_err_out_file

    ivec = size(x1v)
    isiz1 = size(ida,1)
    isiz2 = size(ida,2)
    isiza = isiz1*isiz2

    call schk('ps_intrp_2d','size(x1v)',size(x1v), &
         'size(ansa,1)',size(ansa,1), ierr)

    call schk('ps_intrp_2d','size(x2v)',size(x2v), &
         'size(ansa,1)',size(ansa,1), ierr)

    call schk('ps_intrp_2d','size(ida,1)',isiz1, &
         'size(ansa,2)',size(ansa,2), ierr)
    if(ierr.ne.0) return

    call schk('ps_intrp_2d','size(ida,2)',isiz2, &
         'size(ansa,3)',size(ansa,3), ierr)
    if(ierr.ne.0) return

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    if(present(ideriv1s)) then
       call schk('ps_intrp_2d','size(ida,1)',isiz1, &
            'size(ideriv1s,1)',size(ideriv1s,1), ierr)
       if(ierr.ne.0) return

       call schk('ps_intrp_2d','size(ida,2)',isiz2, &
            'size(ideriv1s,2)',size(ideriv1s,2), ierr)
       if(ierr.ne.0) return
    endif

    if(present(ideriv2s)) then
       call schk('ps_intrp_2d','size(ida,1)',isiz1, &
            'size(ideriv2s,1)',size(ideriv2s,1), ierr)
       if(ierr.ne.0) return

       call schk('ps_intrp_2d','size(ida,2)',isiz2, &
            'size(ideriv2s,2)',size(ideriv2s,2), ierr)
       if(ierr.ne.0) return
    endif

    allocate(idv(isiza),jderiv1s(isiza),jderiv2s(isiza))
    allocate(ansaa(ivec,isiza))
    
    ii = 0
    do i2=1,isiz2
       do i1=1,isiz1
          ii = ii + 1
          idv(ii) = ida(i1,i2)
          if(present(ideriv1s)) then
             jderiv1s(ii) = ideriv1s(i1,i2)
          else if(present(ideriv1)) then
             jderiv1s(ii) = ideriv1
          else
             jderiv1s(ii) = 0
          endif
          if(present(ideriv2s)) then
             jderiv2s(ii) = ideriv2s(i1,i2)
          else if(present(ideriv2)) then
             jderiv2s(ii) = ideriv2
          else
             jderiv2s(ii) = 0
          endif
       enddo
    enddo

    jccw_th = ps_ccw_select
    if(present(iccw_th)) jccw_th = iccw_th

    CALL ps_intrp2s(ivec,x1v,x2v,isiza,idv,ss%iobj, &
         jderiv1s,jderiv2s,jccw_th,ansaa,iout,ierr)

    if(ierr.ne.0) then
       write(iout,*) &
            ' ?ps_intrp_2d: ps_intrp2s returned non-zero error code: ',ierr
       ansa = 0.0_rspec

    else
       ii = 0
       do i2=1,isiz2
          do i1=1,isiz1
             ii = ii + 1
             ansa(1:ivec,i1,i2) = ansaa(1:ivec,ii)
          enddo
       enddo
    endif

    deallocate(idv,jderiv1s,jderiv2s,ansaa)

  END SUBROUTINE ps_intrp_2d_vec_arr2

  !=========================================================================
  SUBROUTINE ps_rz_eval_sc(rho,th,ierr,state,fs_kmax, &
       R,Z,dRdrho,dZdrho,dRdth,dZdth)

    !  scalar routine: (rho,th) -> (R,Z)
    !    FS or direct bicubic spline evaluation
    !    derivatives available

    REAL(KIND=rspec), intent(in) :: rho,th   ! flux coordinates (rho,th) input
    INTEGER, intent(out) :: ierr   ! status code 0=OK

    TYPE (plasma_state), optional, target :: state
    !  operate on THIS state, if argument is present;
    !  otherwise use "ps" state.

    INTEGER, intent(in), optional :: fs_kmax ! If .gt. 0, the number of 
    !  Fourier Spline moments to use; actual number used is
    !  min(<# available moments>,max(3,fs_kmax)); if fs_kmax=0, the default,
    !  direct bicubic spline evaluation is used.

    REAL(KIND=rspec), intent(out), optional :: R,Z ! (R,Z) plane position

    REAL(KIND=rspec), intent(out), optional :: dRdrho  ! derivative
    REAL(KIND=rspec), intent(out), optional :: dZdrho  ! derivative
    REAL(KIND=rspec), intent(out), optional :: dRdth   ! derivative
    REAL(KIND=rspec), intent(out), optional :: dZdth   ! derivative

    !--------------------
    REAL(KIND=rspec) :: rhovec(1),thvec(1),Rvec(1),Zvec(1)
    REAL(KIND=rspec) :: dRdrho_vec(1),dZdrho_vec(1)
    REAL(KIND=rspec) :: dRdth_vec(1), dZdth_vec(1)
    logical :: val_only, deriv_only
    integer :: ifs_kmax
    TYPE (plasma_state), pointer :: ss
    !--------------------
    ierr = 0

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    ifs_kmax=0
    if(present(fs_kmax)) ifs_kmax=fs_kmax

    val_only=.TRUE.
    deriv_only=.TRUE.

    if(present(R)) deriv_only=.FALSE.
    if(present(Z)) deriv_only=.FALSE.

    if(present(dRdrho)) val_only=.FALSE.
    if(present(dZdrho)) val_only=.FALSE.
    if(present(dRdth)) val_only=.FALSE.
    if(present(dZdth)) val_only=.FALSE.

    if(val_only.AND.deriv_only) return  ! this would mean, no output

    rhovec(1)=rho
    thvec(1)=th

    if(val_only) then
       call ps_rz_eval_vec(rhovec,thvec,ierr, state=ss, fs_kmax=ifs_kmax, &
            R=Rvec, Z=Zvec)

    else if(deriv_only) then
       call ps_rz_eval_vec(rhovec,thvec,ierr, state=ss, fs_kmax=ifs_kmax, &
            dRdrho=dRdrho_vec, dZdrho=dZdrho_vec, &
            dRdth=dRdth_vec, dZdth=dZdth_vec)


    else
       call ps_rz_eval_vec(rhovec,thvec,ierr, state=ss, fs_kmax=ifs_kmax, &
            R=Rvec, Z=Zvec, &
            dRdrho=dRdrho_vec, dZdrho=dZdrho_vec, &
            dRdth=dRdth_vec, dZdth=dZdth_vec)
    endif

    if(present(R)) R = Rvec(1)
    if(present(Z)) Z = Zvec(1)
    if(present(dRdrho)) dRdrho = dRdrho_vec(1)
    if(present(dZdrho)) dZdrho = dZdrho_vec(1)
    if(present(dRdth)) dRdth = dRdth_vec(1)
    if(present(dZdth)) dZdth = dZdth_vec(1)

  END SUBROUTINE ps_rz_eval_sc

  SUBROUTINE ps_rz_eval_vec(rho,th,ierr,state,fs_kmax, &
       R,Z,dRdrho,dZdrho,dRdth,dZdth)

    !  vector routine: (rho,th) -> (R,Z)
    !    FS or direct bicubic spline evaluation
    !    derivatives available

    REAL(KIND=rspec), dimension(:), intent(in) :: rho,th   ! flux coordinates
                                                           ! (rho,th) input

    INTEGER, intent(out) :: ierr   ! status code 0=OK

    TYPE (plasma_state), optional, target :: state
    !  operate on THIS state, if argument is present;
    !  otherwise use "ps" state.

    INTEGER, intent(in), optional :: fs_kmax ! If .gt. 0, the number of 
    !  Fourier Spline moments to use; actual number used is
    !  min(<# available moments>,max(3,fs_kmax)); if fs_kmax=0, the default,
    !  direct bicubic spline evaluation is used.

    REAL(KIND=rspec), dimension(:), intent(out),optional :: R,Z ! (R,Z) 
                                                 ! plane position

    REAL(KIND=rspec), dimension(:), intent(out),optional :: dRdrho ! derivative
    REAL(KIND=rspec), dimension(:), intent(out),optional :: dZdrho ! derivative
    REAL(KIND=rspec), dimension(:), intent(out),optional :: dRdth  ! derivative
    REAL(KIND=rspec), dimension(:), intent(out),optional :: dZdth  ! derivative

    !--------------------
    integer :: isizmin,isizmax,isize
    integer :: ifs_kmax

    integer :: idvec,ids(6),iderivs1(6),iderivs2(6),ik,im,ii,ifac,iout
    integer, dimension(:), allocatable :: ids_fs,iderivs_fs
    logical :: rho_deriv

    real*8, dimension(:,:), allocatable :: zbuf2,zsinn,zcosn

    TYPE (plasma_state), pointer :: ss
    !--------------------

    ierr = 0

    iout=SWIM_err_out_file

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    if(ss%id_R_geo.eq.0) then
       write(iout,*) ' ?plasma_state_mod(ps_RZ_eval): flux surface geometry (R_geo,Z_geo) not found.'
       ierr=1
       return
    endif

    ifs_kmax=0
    if(present(fs_kmax)) ifs_kmax=max(0,fs_kmax)
    if(ifs_kmax.gt.0) then
       ifs_kmax = min(ss%nmom, max(3, ifs_kmax))
       if(.not.allocated(ss%id_xRjcos_momeq)) then
          ierr=1
       else if(ss%id_xRjcos_momeq(1).eq.0) then
          ierr=1
       endif
       if(ierr.gt.0) then
          write(iout,*) ' ?plasma_state_mod(ps_RZ_eval): flux surface Fourier spline not found.'
          return
       endif
    endif

    isizmin = size(rho)
    isizmax = isizmin

    isize=size(th)
    isizmin=min(isizmin,isize); isizmax=max(isizmax,isize)

    idvec = 0

    rho_deriv=.FALSE.

    if(present(R)) then
       isize=size(R)
       isizmin=min(isizmin,isize); isizmax=max(isizmax,isize)

       idvec=idvec+1
       if(ifs_kmax.eq.0) then
          ids(idvec) = ss%id_R_geo
          iderivs1(idvec) = 0
          iderivs2(idvec) = 0
       endif
    endif

    if(present(Z)) then
       isize=size(Z)
       isizmin=min(isizmin,isize); isizmax=max(isizmax,isize)

       idvec=idvec+1
       if(ifs_kmax.eq.0) then
          ids(idvec) = ss%id_Z_geo
          iderivs1(idvec) = 0
          iderivs2(idvec) = 0
       endif
    endif

    if(present(dRdrho)) then
       isize=size(dRdrho)
       isizmin=min(isizmin,isize); isizmax=max(isizmax,isize)
       rho_deriv = .TRUE.

       idvec=idvec+1
       if(ifs_kmax.eq.0) then
          ids(idvec) = ss%id_R_geo
          iderivs1(idvec) = 1
          iderivs2(idvec) = 0
       endif
    endif

    if(present(dZdrho)) then
       isize=size(dZdrho)
       isizmin=min(isizmin,isize); isizmax=max(isizmax,isize)
       rho_deriv = .TRUE.

       idvec=idvec+1
       if(ifs_kmax.eq.0) then
          ids(idvec) = ss%id_Z_geo
          iderivs1(idvec) = 1
          iderivs2(idvec) = 0
       endif
    endif

    if(present(dRdth)) then
       isize=size(dRdth)
       isizmin=min(isizmin,isize); isizmax=max(isizmax,isize)

       idvec=idvec+1
       if(ifs_kmax.eq.0) then
          ids(idvec) = ss%id_R_geo
          iderivs1(idvec) = 0
          iderivs2(idvec) = 1
       endif
    endif

    if(present(dZdth)) then
       isize=size(dZdth)
       isizmin=min(isizmin,isize); isizmax=max(isizmax,isize)

       idvec=idvec+1
       if(ifs_kmax.eq.0) then
          ids(idvec) = ss%id_Z_geo
          iderivs1(idvec) = 0
          iderivs2(idvec) = 1
       endif
    endif

    if(isizmin.ne.isizmax) then
       write(iout,*) ' ?plasma_state_mod(ps_RZ_eval): vector sizes not consistent.'
       write(iout,*) '  min & max vector sizes: ',isizmin,isizmax
       ierr=1
       return
    endif

    if(idvec.eq.0) return   ! no output...

    isize = isizmin ! = isizmax
    if(ifs_kmax.gt.0) then
       if(rho_deriv) then
          idvec = 4 + 8*ifs_kmax
       else
          idvec = 2 + 4*ifs_kmax
       endif
       allocate(ids_fs(idvec),iderivs_fs(idvec))
    endif

    allocate(zbuf2(isize,idvec))

    if(ifs_kmax.gt.0) then
       if(rho_deriv) then
          ifac = 2
          ids_fs(1:2)=ss%id_R0_momeq
          ids_fs(3:4)=ss%id_Z0_momeq
          iderivs_fs(1:4) = (/ 0, 1, 0, 1 /)
          ik=-4
          do im=1,ifs_kmax
             ik=ik+8
             ids_fs(ik+1:ik+2)=ss%id_xRjcos_momeq(im)
             ids_fs(ik+3:ik+4)=ss%id_xRjsin_momeq(im)
             ids_fs(ik+5:ik+6)=ss%id_xZjcos_momeq(im)
             ids_fs(ik+7:ik+8)=ss%id_xZjsin_momeq(im)
             iderivs_fs(ik+1:ik+8) = (/ 0, 1, 0, 1, 0, 1, 0, 1 /)
          enddo
       else
          ifac = 1
          iderivs_fs(1:idvec)=0
          ids_fs(1)=ss%id_R0_momeq
          ids_fs(2)=ss%id_Z0_momeq
          ik=-2
          do im=1,ifs_kmax
             ik=ik+4
             ids_fs(ik+1)=ss%id_xRjcos_momeq(im)
             ids_fs(ik+2)=ss%id_xRjsin_momeq(im)
             ids_fs(ik+3)=ss%id_xZjcos_momeq(im)
             ids_fs(ik+4)=ss%id_xZjsin_momeq(im)
          enddo
       endif

       !  fill tables: cos(th),cos(2*th),cos(3*th),...
       !  fill tables: sin(th),sin(2*th),sin(3*th),...

       allocate(zsinn(ifs_kmax,isize),zcosn(ifs_kmax,isize))
       do ii=1,isize
          call r8sincos(th(ii),ifs_kmax,zsinn(:,ii),zcosn(:,ii))
       enddo

       call ps_intrp1s(isize,rho,idvec,ids_fs,ss%iobj,iderivs_fs,0, &
            zbuf2,iout,ierr)

    else

       call ps_intrp2s(isize,rho,th,idvec,ids,ss%iobj, &
            iderivs1,iderivs2,0,zbuf2,iout,ierr)

    endif

    if(ierr.ne.0) then
       write(iout,*) ' ?ps_rz_Eval: data interpolation error.'
       return
    endif

    if(ifs_kmax.eq.0) then
       idvec = 0

       if(present(R)) then
          idvec=idvec + 1
          R = zbuf2(:,idvec)
       endif

       if(present(Z)) then
          idvec=idvec + 1
          Z = zbuf2(:,idvec)
       endif

       if(present(dRdrho)) then
          idvec=idvec + 1
          dRdrho = zbuf2(:,idvec)
       endif

       if(present(dZdrho)) then
          idvec=idvec + 1
          dZdrho = zbuf2(:,idvec)
       endif

       if(present(dRdth)) then
          idvec=idvec + 1
          dRdth = zbuf2(:,idvec)
       endif

       if(present(dZdth)) then
          idvec=idvec + 1
          dZdth = zbuf2(:,idvec)
       endif

    else

       ! initialize the FS sums

       if(present(R)) then
          R = zbuf2(:,1)
       endif

       if(present(Z)) then
          Z = zbuf2(:,1+ifac)
       endif

       if(present(dRdrho)) then
          dRdrho = zbuf2(:,2)
       endif

       if(present(dZdrho)) then
          dZdrho = zbuf2(:,4)
       endif

       if(present(dRdth)) then
          dRdth = 0.0d0
       endif

       if(present(dZdth)) then
          dZdth = 0.0d0
       endif

       ! add in all moments contributions

       do im=1,ifs_kmax

          ik = ifac*(2+4*(im-1))  ! pt before first R coeff (im) data
          ii = ik+2*ifac          ! pt before first Z coeff (im) data

          if(present(R)) then
             R = R + rho(:)* &
                  (zbuf2(:,ik+1)*zcosn(im,:) + zbuf2(:,ik+1+ifac)*zsinn(im,:))
          endif

          if(present(Z)) then
             Z = Z + rho(:)* &
                  (zbuf2(:,ii+1)*zcosn(im,:) + zbuf2(:,ii+1+ifac)*zsinn(im,:))
          endif

          if(present(dRdrho)) then
             dRdrho = dRdrho + &
                  zbuf2(:,ik+1)*zcosn(im,:) + zbuf2(:,ik+3)*zsinn(im,:) +&
                  rho(:)* &
                    (zbuf2(:,ik+2)*zcosn(im,:) + zbuf2(:,ik+4)*zsinn(im,:))
          endif

          if(present(dZdrho)) then
             dZdrho = dZdrho + &
                  zbuf2(:,ii+1)*zcosn(im,:) + zbuf2(:,ii+3)*zsinn(im,:) +&
                  rho(:)* &
                    (zbuf2(:,ii+2)*zcosn(im,:) + zbuf2(:,ii+4)*zsinn(im,:))
          endif

          if(present(dRdth)) then
             dRdth = dRdth + im*rho(:)* &
                  (zbuf2(:,ik+1+ifac)*zcosn(im,:) - zbuf2(:,ik+1)*zsinn(im,:))
          endif

          if(present(dZdth)) then
             dZdth = dZdth + im*rho(:)* &
                  (zbuf2(:,ii+1+ifac)*zcosn(im,:) - zbuf2(:,ii+1)*zsinn(im,:))
          endif

       enddo

    endif

  end SUBROUTINE ps_rz_eval_vec

  !=========================================================================
  SUBROUTINE ps_rhoth2rz_sc(rho,th,R,Z,ierr,state)

    !  scalar routine: (rho,th) -> (R,Z)

    REAL(KIND=rspec), intent(in) :: rho,th   ! flux coordinates (rho,th) input
    REAL(KIND=rspec), intent(out) :: R,Z     ! (R,Z) plane position output
    INTEGER, intent(out) :: ierr   ! status code 0=OK

    TYPE (plasma_state), optional, target :: state
    !  operate on THIS state, if argument is present;
    !  otherwise use "ps" state.

    !--------------------
    REAL(KIND=rspec) :: rhovec(1),thvec(1),Rvec(1),Zvec(1)
    !--------------------

    rhovec(1)=rho
    thvec(1)=th

    call ps_rhoth2rz_vec(rhovec,thvec,Rvec,Zvec,ierr,state)

    R=Rvec(1)
    Z=Zvec(1)

  END SUBROUTINE ps_rhoth2rz_sc

  SUBROUTINE ps_rhoth2rz_vec(rho,th,R,Z,ierr,state)

    !  vector routine: (rho,th) -> (R,Z)
    !  *** ALL VECTORS must be of EQUAL length ***

    REAL(KIND=rspec), dimension(:), intent(in) :: rho,th   ! flux coordinates
    REAL(KIND=rspec), dimension(:), intent(out) :: R,Z     ! (R,Z) positions
    INTEGER, intent(out) :: ierr   ! status code 0=OK

    TYPE (plasma_state), optional, target :: state
    !  operate on THIS state, if argument is present;
    !  otherwise use "ps" state.

    !--------------------
    INTEGER :: isize,iout
    TYPE (plasma_state), pointer :: ss
    !--------------------

    ierr=0

    iout=SWIM_err_out_file

    isize=size(rho)
    if((size(th).ne.isize).or.(size(R).ne.isize).or.(size(Z).ne.isize)) then
       write(iout,*) ' ?ps_rhoth2rz_vec: vector size inconsistency detected:'
       write(iout,*) '    size(rho) = ',size(rho)
       write(iout,*) '    size(th)  = ',size(th)
       write(iout,*) '    size(R)   = ',size(R)
       write(iout,*) '    size(Z)   = ',size(Z)
       R=0.0_rspec
       Z=0.0_rspec
       return
    endif

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    call psxpl_rhoth2rz(ss,isize,rho,th,R,Z,iout,ierr)

  END SUBROUTINE ps_rhoth2rz_vec

  !=========================================================================
  SUBROUTINE ps_rz2rhoth_sc(R,Z,rho,th,ierr,state,maptype,tol)

    !  scalar routine: (R,Z) -> (rho,th)

    REAL(KIND=rspec), intent(in) :: R,Z      ! (R,Z) plane position input
    REAL(KIND=rspec), intent(out) :: rho,th  ! flux coordinates (rho,th) output
    INTEGER, intent(out) :: ierr   ! status code 0=OK

    TYPE (plasma_state), optional, target :: state
    !  operate on THIS state, if argument is present;
    !  otherwise use "ps" state.

    INTEGER, intent(in), optional :: maptype ! (default: 1) select 
    !                       (R,Z) -> (rho,th) inverse map type:
    !                                  =1: slow, exact: Newton map
    !                                      |R-Rspline(rho,th)| < tol*R
    !                                      |Z-Zspline(rho,th)| < tol*R
    !                                  =2: polar map -- much faster, 
    !                                      equivalent equivalent tol ~ 1.0e-4
    !                                      THIS IS THE DEFAULT
    !                                  =3: bilinear map -- fastest after
    !                                      map setup; equivalent tol ~ 2.0e-3

    REAL(KIND=rspec), intent(in), optional :: tol  ! inverse map accuracy
    !                                  relative tolerance; default=1.0e-10

    !--------------------
    REAL(KIND=rspec) :: rhovec(1),thvec(1),Rvec(1),Zvec(1)
    !--------------------

    Rvec(1)=R
    Zvec(1)=Z

    call ps_rz2rhoth_vec(rhovec,thvec,Rvec,Zvec,ierr,state,maptype,tol)

    rho=rhovec(1)
    th=thvec(1)

  END SUBROUTINE ps_rz2rhoth_sc

  SUBROUTINE ps_rz2rhoth_vec(R,Z,rho,th,ierr,state,maptype,tol)

    !  vector routine: (R,Z) -> (rho,th)
    !  *** ALL VECTORS must be of EQUAL length ***

    REAL(KIND=rspec), dimension(:), intent(in) :: R,Z      ! (R,Z) positions
    REAL(KIND=rspec), dimension(:), intent(out) :: rho,th  ! flux coordinates
    INTEGER, intent(out) :: ierr   ! status code 0=OK

    TYPE (plasma_state), optional, target :: state
    !  operate on THIS state, if argument is present;
    !  otherwise use "ps" state.

    INTEGER, intent(in), optional :: maptype ! (default: 1) select 
    !                       (R,Z) -> (rho,th) inverse map type:
    !                                  =1: slow, exact: Newton map
    !                                      |R-Rspline(rho,th)| < tol*R
    !                                      |Z-Zspline(rho,th)| < tol*R
    !                                  =2: polar map -- much faster, 
    !                                      equivalent equivalent tol ~ 1.0e-4
    !                                      THIS IS THE DEFAULT
    !                                  =3: bilinear map -- fastest after
    !                                      map setup; equivalent tol ~ 2.0e-3

    REAL(KIND=rspec), intent(in), optional :: tol  ! inverse map accuracy
    !                                  relative tolerance; default=1.0e-10

    !--------------------
    INTEGER :: isize
    TYPE (plasma_state), pointer :: ss
    integer :: imap,iout
    REAL(KIND=rspec) :: rtol
    !--------------------

    ierr=0

    iout=SWIM_err_out_file

    isize=size(rho)
    if((size(th).ne.isize).or.(size(R).ne.isize).or.(size(Z).ne.isize)) then
       write(iout,*) ' ?ps_rz2rhoth_vec: vector size inconsistency detected:'
       write(iout,*) '    size(rho) = ',size(rho)
       write(iout,*) '    size(th)  = ',size(th)
       write(iout,*) '    size(R)   = ',size(R)
       write(iout,*) '    size(Z)   = ',size(Z)
       rho=0.0_rspec
       th=0.0_rspec
       return
    endif

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    rtol=1.0e-10_rspec
    imap=2

    if(present(tol)) rtol=min(1.0e-2_rspec,max(1.0e-15_rspec,tol))
    if(present(maptype)) imap= max(1,min(3,maptype))

    call psxpl_rz2rhoth(ss,isize,R,Z,rho,th,iout,imap,rtol,ierr)

  END SUBROUTINE ps_rz2rhoth_vec

  SUBROUTINE ps_zone2bdy_rezone(id,ansv,ierr, state)

    ! map zone oriented f(rho) quantity to boundary values, by rezoning.
    ! The id of the 1d f(rho) quantity is used to recover its grid; the
    ! output array size must exactly match the size of the grid (rho)
    ! over which f is defined -- i.e. 1 larger than the size of the f array
    ! if f is a step function profile.

    ! this is intended primarily for densities, temperatures, and rotation
    ! velocities that are input from the state to a physics component such
    ! as an RF code.  It is not intended for use with "output" source profiles
    ! with interpolation methods "step*dV" or "step*dA"!

    INTEGER, intent(in) :: id       ! ID of profile item (f)
    !   for example, for ps%ns(:,0) this is ps%id_ns(0) (electron density).

    REAL(KIND=rspec), dimension(:), intent(out) :: ansv   ! result
    INTEGER, intent(out) :: ierr    ! completion status (0=OK)

    TYPE (plasma_state), OPTIONAL, target :: state
    !  operate on THIS state, if argument is present;
    !  otherwise use "ps" state.

    !------------------------------------------------
    integer :: isize_ansv,isize_rho,iout,ii,idi
    character*32 :: gridName,profName
    REAL(KIND=rspec), dimension(:), allocatable :: rho_orig,rho_zcx

    TYPE (plasma_state), pointer :: ss
    !------------------------------------------------
    
    ierr=0
    ansv=0.0_rspec

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    iout=SWIM_err_out_file

    !-----------
    ! check that output size matches grid size of profile specified by ID

    isize_ansv=size(ansv)

    idi=id
    call ps_1dprof_grid(ss%iobj,idi,profName,gridName,iout,ierr)
    if(ierr.ne.0) then
       write(iout,*) ' ?ps_zone2bdy_rezone: grid lookup failure.'
       return
    endif

    ! we know we have a grid, so success is guaranteed:
    call ps_grid_size(ss%iobj,gridName,isize_rho,iout,ierr)

    if(isize_rho.ne.isize_ansv) then
       write(iout,*) &
            ' ?ps_zone2bdy_rezone: output array size does not match grid size:'
       write(iout,*) '  size=',isize_rho,' gridName = '//trim(gridName)// &
            ' profName = '//trim(profName)
       ierr=2
       return
    endif

    !-----------
    ! get the grid & construct rezone grid

    allocate(rho_orig(isize_rho),rho_zcx(isize_rho + 1))

    ! success guaranteed here: grid was verified already...
    call ps_grid(ss%iobj,gridName,isize_rho,rho_orig,iout,ierr)

    rho_zcx(1) = rho_orig(1)
    rho_zcx(isize_rho + 1) = rho_orig(isize_rho)

    do ii = 2, isize_rho
       rho_zcx(ii) = 0.5_rspec*(rho_orig(ii-1)+rho_orig(ii))
    enddo

    !-----------
    ! now execute the rezone.  This will pick up density weighting if needed.

    call ps_rho_rezone(rho_zcx, id, ansv, ierr)

  END SUBROUTINE ps_zone2bdy_rezone

  !==========================================================================
  !  ps_rho_rezone -- rezone profile or group of profiles with conservation
  !    of volume (or area) integral.
  !==========================================================================

  SUBROUTINE ps_rho_rezone1(rho, id, ansv, ierr, &
       state, curdens, nonorm, zonesmoo, zonediff)

    !-------------------------------------------------
    ! D. McCune 16 Oct 2006
    !  "rezone" a quantity such that an appropriately weighted sum of
    !  the zone values matches the equivalently weighted integral of the
    !  profile interpolating function.
    !
    !  The zone boundaries are input as rho(...); the output is zone
    !  oriented, i.e. size(ansv) = size(rho) - 1; this is checked.
    !
    !  Example of use:  if "id" refers to an interpolating profile
    !  nD(rho) containing "density of Deuterons", the output will
    !  satisfy
    !
    !     ansv(j)*vol(rho(j):rho(j+1))
    !        = integral(rho(j):rho(j+1)){nd(rho)*dVol(rho)}
    !
    !  where vol(rho(j):rho(j+1)) denotes the volume enclosed between
    !  the boundaries rho(j) and rho(j+1).  These volume elements can
    !  optionally be returned in the array zonediff(...).  These could
    !  also be cross sectional area elements in the case of an area 
    !  integral applied e.g. to a toroidal current density.
    !     if it is present, size(zonediff) = size(rho) - 1 is expected.
    !
    !  This routine usually normally returns rezoned density profile data
    !  normalized the same units of density (e.g. ptcls/m^3 or Amps/m^2).
    !  To suppress the density normalization (returning profile data e.g.
    !  in units of #ptcls or Amps instead), set optional argument 
    !  nonorm = .TRUE.; this switch has no effect if the profile being
    !  rezoned is not a density (e.g. no effect on "average temperature"
    !  rezone operation).
    !-------------------------------------------------

    REAL(KIND=rspec), dimension(:), intent(in) :: rho ! rezone target
    !  rho MUST span range [0:1]

    INTEGER, intent(in) :: id                         ! ID of profile to rezone
    REAL(KIND=rspec), dimension(:), intent(out) :: ansv  ! results
    INTEGER, intent(out) :: ierr          ! completion code (0=OK)

    TYPE (plasma_state), optional, target :: state
    !  operate on THIS state, if argument is present;
    !  otherwise use "ps" state.

    logical, intent(in), optional :: curdens  ! T if quantity is a current
    ! density (area normalization); default is F (volume normalization).

    logical, intent(in), optional :: nonorm   ! T to suppress density 
    ! normalization of output; default is F.

    logical, intent(in), optional :: zonesmoo ! T to apply 1/2 zone width
    ! integral smoothing operator.  Default is F.  This option is not 
    ! recommended unless mapping coarse step function data to a fine grid
    ! with requirement to remove step structure from smoothed interpolant.
    
    REAL(KIND=rspec), dimension(:), intent(out), optional :: zonediff 
    ! the array of
    ! finite difference volumes (or AREAs if curdens=.TRUE.) corresponding 
    ! to the rho(...) grid provided.
    
    !------------------------------------------------
    integer :: nrho,iout
    logical :: jcurdens,jnnorm,jsmoo
    REAL(KIND=rspec), dimension(:), allocatable :: zdvol
    integer :: idvec(1),idwvec(1)
    REAL(KIND=rspec), dimension(:,:), allocatable :: ansa

    TYPE (plasma_state), pointer :: ss
    !------------------------------------------------

    ierr=0
    ansv = 0.0_rspec
    if(present(zonediff)) zonediff = 0.0_rspec

    iout=SWIM_err_out_file

    nrho = size(rho)
    if((abs(rho(1)).gt.1.0e-10_rspec).or. &
         (abs(rho(nrho)-1.0_rspec).gt.1.0e-10_rspec)) then
       write(iout,*) ' ?ps_rho_rezone: target rho grid does not span [0:1]:'
       write(iout,*) '  rho(1) = ',rho(1)
       write(iout,*) '  rho(',nrho,') = ',rho(nrho)
       ierr = 999
       return
    endif

    call schk('ps_rho_rezone','size(rho)',nrho, &
         'size(ansv)+1',size(ansv)+1,ierr)
    if(ierr.ne.0) return

    if(present(zonediff)) then
       call schk('ps_rho_rezone','size(rho)',nrho, &
            'size(zonediff)+1',size(zonediff)+1,ierr)
       if(ierr.ne.0) return
    endif

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    !-----------------------------
    ! OK, proceed...

    jcurdens = .FALSE.
    if(present(curdens)) jcurdens = curdens

    jnnorm = .FALSE.
    if(present(nonorm)) jnnorm = nonorm

    jsmoo = .FALSE.
    if(present(zonesmoo)) jsmoo = zonesmoo

    allocate(zdvol(nrho-1))

    allocate(ansa(nrho-1,1))
    idvec = id
    idwvec = ss%id_zonwt(id)

    call ps_rezone_rho(ss,nrho,rho,1,idvec,idwvec,jcurdens,jnnorm,jsmoo, &
         ansa,zdvol,iout,ierr)

    if(ierr.ne.0) then
       write(iout,*) ' ?ps_rho_rezone: ps_rezone_rho subroutine call failed.'
    else
       ansv = ansa(:,1)
    endif
    
    if(present(zonediff)) then
       zonediff = zdvol
    endif
    deallocate(zdvol,ansa)

  END SUBROUTINE ps_rho_rezone1

  SUBROUTINE ps_rho_rezone_arr1(rho, idv, ansa, ierr, &
       state, curdens, nonorm, zonesmoo, zonediff)

    !-------------------------------------------------
    ! D. McCune 16 Oct 2006
    !  "rezone" quantities-- same as ps_rho_rezone1, except apply to a 1d
    !  list of profile quantities, instead of just a single quantity.
    !-------------------------------------------------

    REAL(KIND=rspec), dimension(:), intent(in) :: rho ! rezone target
    !  rho MUST span range [0:1]

    INTEGER, intent(in), dimension(:) :: idv          ! list of profile IDs

    REAL(KIND=rspec), dimension(:,:), intent(out) :: ansa  ! results
    INTEGER, intent(out) :: ierr          ! completion code (0=OK)

    TYPE (plasma_state), optional, target :: state
    !  operate on THIS state, if argument is present;
    !  otherwise use "ps" state.

    logical, intent(in), optional :: curdens  ! T if quantity is a current
    ! density (area normalization); default is F (volume normalization).

    logical, intent(in), optional :: nonorm   ! T to suppress density 
    ! normalization of output; default is F.

    logical, intent(in), optional :: zonesmoo ! T to apply 1/2 zone width
    ! integral smoothing operator.  Default is F.  This option is not 
    ! recommended unless mapping coarse step function data to a fine grid
    ! with requirement to remove step structure from smoothed interpolant.

    REAL(KIND=rspec), dimension(:), intent(out), optional :: zonediff
    ! the array of
    ! finite difference volumes (or AREAs if curdens=.TRUE.) corresponding 
    ! to the rho(...) grid provided.
    
    !------------------------------------------------
    integer :: idwv(size(idv))

    integer :: nrho,iout,i
    logical :: jcurdens,jnnorm,jsmoo
    REAL(KIND=rspec), dimension(:), allocatable :: zdvol

    TYPE (plasma_state), pointer :: ss
    !------------------------------------------------

    ierr=0
    ansa = 0.0_rspec
    if(present(zonediff)) zonediff = 0.0_rspec

    iout=SWIM_err_out_file

    nrho = size(rho)
    if((abs(rho(1)).gt.1.0e-10_rspec).or. &
         (abs(rho(nrho)-1.0_rspec).gt.1.0e-10_rspec)) then
       write(iout,*) ' ?ps_rho_rezone: target rho grid does not span [0:1]:'
       write(iout,*) '  rho(1) = ',rho(1)
       write(iout,*) '  rho(',nrho,') = ',rho(nrho)
       ierr = 999
       return
    endif

    call schk('ps_rho_rezone','size(rho)',nrho, &
         'size(ansa,1)+1',size(ansa,1)+1,ierr)
    if(ierr.ne.0) return

    call schk('ps_rho_rezone','size(idv)',size(idv), &
         'size(ansa,2)',size(ansa,2),ierr)
    if(ierr.ne.0) then
       write(iout,*) ' number of profiles does not match output array size.'
       return
    endif

    if(present(zonediff)) then
       call schk('ps_rho_rezone','size(rho)',nrho, &
            'size(zonediff)+1',size(zonediff)+1,ierr)
       if(ierr.ne.0) return
    endif

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    !-----------------------------
    ! OK, proceed...

    jcurdens = .FALSE.
    if(present(curdens)) jcurdens = curdens

    jnnorm = .FALSE.
    if(present(nonorm)) jnnorm = nonorm

    jsmoo = .FALSE.
    if(present(zonesmoo)) jsmoo = zonesmoo

    allocate(zdvol(nrho-1))

    do i=1,size(idv)
       idwv(i)=ss%id_zonwt(idv(i))
    enddo

    call ps_rezone_rho(ss,nrho,rho,size(idv),idv,idwv, &
         jcurdens,jnnorm,jsmoo, &
         ansa,zdvol,iout,ierr)
    if(ierr.ne.0) then
       write(iout,*) ' ?ps_rho_rezone: ps_rezone_rho subroutine call failed.'
       ansa = 0.0_rspec
    endif

    if(present(zonediff)) then
       zonediff = zdvol
    endif
    deallocate(zdvol)

  END SUBROUTINE ps_rho_rezone_arr1

  SUBROUTINE ps_rho_rezone_arr2(rho, ida, ansa, ierr, &
       state, curdens, nonorm, zonesmoo, zonediff)

    !-------------------------------------------------
    ! D. McCune 16 Oct 2006
    !  "rezone" quantities-- same as ps_rho_rezone1, except apply to a 2d
    !  list of profile quantities, instead of just a single quantity.
    !-------------------------------------------------

    REAL(KIND=rspec), dimension(:), intent(in) :: rho ! rezone target
    !  rho MUST span range [0:1]

    INTEGER, intent(in), dimension(:,:) :: ida        ! 2d list of profile IDs

    REAL(KIND=rspec), dimension(:,:,:), intent(out) :: ansa  ! results
    INTEGER, intent(out) :: ierr          ! completion code (0=OK)

    TYPE (plasma_state), optional, target :: state
    !  operate on THIS state, if argument is present;
    !  otherwise use "ps" state.

    logical, intent(in), optional :: curdens  ! T if quantity is a current
    ! density (area normalization); default is F (volume normalization).

    logical, intent(in), optional :: nonorm   ! T to suppress density 
    ! normalization of output; default is F.

    logical, intent(in), optional :: zonesmoo ! T to apply 1/2 zone width
    ! integral smoothing operator.  Default is F.  This option is not 
    ! recommended unless mapping coarse step function data to a fine grid
    ! with requirement to remove step structure from smoothed interpolant.

    REAL(KIND=rspec), dimension(:), intent(out), optional :: zonediff 
    ! the array of
    ! finite difference volumes (or AREAs if curdens=.TRUE.) corresponding 
    ! to the rho(...) grid provided.
    
    !------------------------------------------------
    integer :: nrho,iout,isizid,i1,i2,ii
    logical :: jcurdens,jnnorm,jsmoo
    REAL(KIND=rspec), dimension(:), allocatable :: zdvol
    REAL(KIND=rspec), dimension(:,:), allocatable :: ansaa
    INTEGER, dimension(:), allocatable :: idvec,idwvec

    TYPE (plasma_state), pointer :: ss
    !------------------------------------------------

    ierr=0
    ansa = 0.0_rspec
    if(present(zonediff)) zonediff = 0.0_rspec

    iout=SWIM_err_out_file

    nrho = size(rho)
    if((abs(rho(1)).gt.1.0e-10_rspec).or. &
         (abs(rho(nrho)-1.0_rspec).gt.1.0e-10_rspec)) then
       write(iout,*) ' ?ps_rho_rezone: target rho grid does not span [0:1]:'
       write(iout,*) '  rho(1) = ',rho(1)
       write(iout,*) '  rho(',nrho,') = ',rho(nrho)
       ierr = 999
       return
    endif

    call schk('ps_rho_rezone','size(rho)',nrho, &
         'size(ansa,1)+1',size(ansa,1)+1,ierr)
    if(ierr.ne.0) return

    call schk('ps_rho_rezone','size(ida,1)',size(ida,1), &
         'size(ansa,2)',size(ansa,2),ierr)
    if(ierr.ne.0) then
       write(iout,*) ' number of profiles does not match output array size.'
       return
    endif

    call schk('ps_rho_rezone','size(ida,2)',size(ida,2), &
         'size(ansa,3)',size(ansa,3),ierr)
    if(ierr.ne.0) then
       write(iout,*) ' number of profiles does not match output array size.'
       return
    endif

    if(present(zonediff)) then
       call schk('ps_rho_rezone','size(rho)',nrho, &
            'size(zonediff)+1',size(zonediff)+1,ierr)
       if(ierr.ne.0) return
    endif

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    !-----------------------------
    ! OK, proceed...

    isizid = size(ida)

    jcurdens = .FALSE.
    if(present(curdens)) jcurdens = curdens

    jnnorm = .FALSE.
    if(present(nonorm)) jnnorm = nonorm

    jsmoo = .FALSE.
    if(present(zonesmoo)) jsmoo = zonesmoo

    allocate(zdvol(nrho-1),idvec(isizid),idwvec(isizid),ansaa(nrho-1,isizid))

    ii = 0
    do i2=1,size(ida,2)
       do i1=1,size(ida,1)
          ii = ii + 1
          idvec(ii) = ida(i1,i2)
          idwvec(ii) = ss%id_zonwt(idvec(ii))
       enddo
    enddo

    call ps_rezone_rho(ss,nrho,rho,isizid,idvec,idwvec, &
         jcurdens,jnnorm,jsmoo, &
         ansaa,zdvol,iout,ierr)
    if(ierr.ne.0) then
       write(iout,*) ' ?ps_rho_rezone: ps_rezone_rho subroutine call failed.'
    else
       ii = 0
       do i2=1,size(ida,2)
          do i1=1,size(ida,1)
             ii = ii + 1
             ansa(:,i1,i2) = ansaa(:,ii)
          enddo
       enddo
    endif

    if(present(zonediff)) then
       zonediff = zdvol
    endif
    deallocate(zdvol,idvec,idwvec,ansaa)

  END SUBROUTINE ps_rho_rezone_arr2

  !==========================================================================
  !  ps_user_rezone -- rezone profile or group of profiles with conservation
  !    of volume (or area) integral.
  !==========================================================================

  SUBROUTINE ps_user_rezone1(rho_orig, rho_target, f_orig, f_target, ierr, &
       w_orig, interp, &
       state, curdens, nonorm, zonesmoo, zonediff)

    !-------------------------------------------------
    ! D. McCune 6 Apr 2007
    !  "rezone" a quantity such that an appropriately weighted sum of
    !  the zone values matches the equivalently weighted integral of the
    !  profile interpolating function.
    !
    !  The quantity to be rezoned is explicitly input: f_orig.  It is defined
    !  over the input grid (rho_orig(:)).  Optionally, a weighting profile
    !  can be provided: w_orig.
    !
    !  The zone boundaries for the output rezoned profile are given as 
    !  rho_target(...); the output is zone oriented, 
    !  i.e. size(f_target) = size(rho_target) - 1; this is checked.
    !
    !  This routine is very similar to ps_rho_rezone1.  The difference is that
    !  the profile data is passed rather than referenced in the state object.
    !
    !  This routine usually normally returns rezoned density profile data
    !  normalized the same units of density (e.g. ptcls/m^3 or Amps/m^2).
    !  To suppress the density normalization (returning profile data e.g.
    !  in units of #ptcls or Amps instead), set optional argument 
    !  nonorm = .TRUE.; this switch has no effect if the profile being
    !  rezoned is not a density (i.e. no effect if a non-zero weighting 
    !  profile is provided).
    !-------------------------------------------------

    REAL(KIND=rspec), dimension(:), intent(in) :: rho_orig ! source grid
    !  rho_orig MUST span range [0:1]

    REAL(KIND=rspec), dimension(:), intent(in) :: rho_target ! rezone target
    !  rho_target MUST span range [0:1]

    REAL(KIND=rspec), dimension(:), intent(in) :: f_orig ! source profile
    !  NOTE: nonorm=T *AND* size(f_orig) = size(rho_orig)-1 (i.e. f_orig is
    !  a step function) *AND* w_orig is absent, then f_orig is assumed to be
    !  in un-normalized units (as f_target will be): e.g. ptcls/zone rather
    !  than ptcls/m^3, or, Amps/zone rather than Amps/m^2.  The necessary
    !  zonal dVol or dArea will be divided out before computing the volume
    !  or area integral for rezoning.

    REAL(KIND=rspec), dimension(:), intent(out) :: f_target  ! rezoning result

    INTEGER, intent(out) :: ierr          ! completion code (0=OK)

    REAL(KIND=rspec), dimension(:), intent(in), optional :: w_orig
    !  weighting profile.  Ignored if =0.0 everywhere.  Default: no weighting
    !  and f_orig*dV or f_orig*dA is conserved.  If present then
    !  f_orig*w_orig*dV or f_orig*w_orig*dA is conserved.
    !  NOTE: if nonorm=T *AND* size(w_orig)=size(rho_orig-1) (i.e. w_orig is
    !  a step function), it is assumed that w_orig is in un-normalized units,
    !  e.g. particles per zone, not particles / m^3.  The zonal dVol will be
    !  divided out to form the weighting function used in the integral.

    integer, intent(in), optional :: interp   ! interpolation selection control
    !  default: step function (-1) if size(f_orig) = size(rho_orig) - 1
    !  default: piecewise lin (0)  if size(f_orig) = size(rho_orig)
    !    (1) for C1 cubic Hermite interpolation
    !    (2) for C2 cubic Spline interpolation

    TYPE (plasma_state), optional, target :: state
    !  operate on THIS state, if argument is present;
    !  otherwise use "ps" state.

    logical, intent(in), optional :: curdens  ! T if quantity is a current
    ! density (area normalization); default is F (volume normalization).

    logical, intent(in), optional :: nonorm   ! T to suppress density 
    ! normalization; default is F.
    !  *CAUTION* this can affect interpretation of input as well as output
    !  profiles.

    logical, intent(in), optional :: zonesmoo ! T to apply 1/2 zone width
    ! integral smoothing operator.  Default is F.  This option is not 
    ! recommended unless mapping coarse step function data to a fine grid
    ! with requirement to remove step structure from smoothed interpolant.
    
    REAL(KIND=rspec), dimension(:), intent(out), optional :: zonediff 
    ! the array of
    ! finite difference volumes (or AREAs if curdens=.TRUE.) corresponding 
    ! to the rho_target(...) grid provided.
    
    !------------------------------------------------
    integer :: nrho_target,nrho_orig,iout,inpro
    logical :: jcurdens,jnnorm,jsmoo,inorm
    REAL(KIND=rspec), dimension(:), allocatable :: zdvol,zwk,zdva_orig
    integer :: idvec(1),idwvec(1),idrho,jntrp
    REAL(KIND=rspec), dimension(:,:), allocatable :: ansa

    TYPE (plasma_state), pointer :: ss
    !------------------------------------------------

    ierr=0
    f_target = 0.0_rspec
    if(present(zonediff)) zonediff = 0.0_rspec

    iout=SWIM_err_out_file

    nrho_target = size(rho_target)
    if((abs(rho_target(1)).gt.1.0e-10_rspec).or. &
         (abs(rho_target(nrho_target)-1.0_rspec).gt.1.0e-10_rspec)) then
       write(iout,*) ' ?ps_user_rezone: target rho grid does not span [0:1]:'
       write(iout,*) '  rho_target(1) = ',rho_target(1)
       write(iout,*) '  rho_target(',nrho_target,') = ',rho_target(nrho_target)
       ierr = 999
       return
    endif

    call schk('ps_user_rezone','size(rho_target)',nrho_target, &
         'size(f_target)+1',size(f_target)+1,ierr)
    if(ierr.ne.0) return

    if(present(zonediff)) then
       call schk('ps_user_rezone','size(rho_target)',nrho_target, &
            'size(zonediff)+1',size(zonediff)+1,ierr)
       if(ierr.ne.0) return
    endif

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    inpro = size(f_orig)
    nrho_orig = size(rho_orig)

    if(inpro.eq.nrho_orig) then
       jntrp=0
    else
       jntrp=-1
    endif

    if(present(interp)) jntrp=interp

    if((jntrp.lt.-1).or.(jntrp.gt.2)) then
       write(iout,*) ' ?ps_user_rezone: illegal argument value, interp=',interp
       ierr = 999
       return
    endif

    if(jntrp.eq.-1) then
       call schk('ps_user_rezone','size(rho_orig)',nrho_orig, &
            'size(f_orig)+1',inpro+1,ierr)
       if(ierr.ne.0) return
       if(present(w_orig)) then
          call schk('ps_user_rezone','size(rho_orig)',nrho_orig, &
               'size(w_orig)+1',size(w_orig)+1,ierr)
          if(ierr.ne.0) return
       endif
    else
       call schk('ps_user_rezone','size(rho_orig)',nrho_orig, &
            'size(f_orig)',inpro,ierr)
       if(ierr.ne.0) return
       if(present(w_orig)) then
          call schk('ps_user_rezone','size(rho_orig)',nrho_orig, &
               'size(w_orig)',size(w_orig),ierr)
          if(ierr.ne.0) return
       endif
    endif

    !-----------------------------
    ! OK, proceed...

    jcurdens = .FALSE.
    if(present(curdens)) jcurdens = curdens

    jnnorm = .FALSE.
    if(present(nonorm)) jnnorm = nonorm
    
    inorm = .FALSE.
    if((jntrp.eq.-1).and.jnnorm) then
       inorm = .TRUE.  ! need to divide output dVol or dArea from input
       allocate(zdva_orig(nrho_orig-1),zwk(nrho_orig))
       if(jcurdens) then
          call ps_eq_area(ss%iobj,nrho_orig,rho_orig,zwk,iout,ierr)
       else
          call ps_eq_volume(ss%iobj,nrho_orig,rho_orig,zwk,iout,ierr)
       endif
       if(ierr.ne.0) return
       zdva_orig = zwk(2:nrho_orig)-zwk(1:nrho_orig-1)
    endif

    !  create temporary grid 
    call ps_rhotmp(ss%iobj,nrho_orig,rho_orig,1,idrho,iout,ierr)
    if(ierr.ne.0) then
       write(iout,*) ' error in ps_user_rezone: rho_orig grid.'
       return
    endif

    if(inorm) then
       call ps_protmp(ss%iobj,idrho,inpro,f_orig/zdva_orig,1,jntrp,idvec(1), &
            iout,ierr)
    else
       call ps_protmp(ss%iobj,idrho,inpro,f_orig,1,jntrp,idvec(1),iout,ierr)
    endif

    if(ierr.ne.0) then
       write(iout,*) ' error in ps_user_rezone: f_orig profile.'
       call ps_freetmp(ss%iobj,idrho,iout)
       return
    endif

    idwvec(1)=0
    if(present(w_orig)) then
       if(inorm) then
          call ps_wprotmp(ss%iobj,idrho,inpro,w_orig/zdva_orig, &
               2,jntrp,idwvec(1), &
               iout,ierr)
       else
          call ps_wprotmp(ss%iobj,idrho,inpro,w_orig,2,jntrp,idwvec(1), &
               iout,ierr)
       endif
       if(ierr.ne.0) then
          write(iout,*) ' error in ps_user_rezone: w_orig weight profile.'
          call ps_freetmp(ss%iobj,idvec(1),iout)
          call ps_freetmp(ss%iobj,idrho,iout)  ! must do this one last
          return
       endif
    endif

    jsmoo = .FALSE.
    if(present(zonesmoo)) jsmoo = zonesmoo

    allocate(zdvol(nrho_target-1))

    allocate(ansa(nrho_target-1,1))

    call ps_rezone_rho(ss,nrho_target,rho_target,1,idvec,idwvec, &
         jcurdens,jnnorm,jsmoo, &
         ansa,zdvol,iout,ierr)

    if(ierr.ne.0) then
       write(iout,*) ' ?ps_user_rezone: ps_rezone_rho subroutine call failed.'
    else
       f_target = ansa(:,1)
    endif
    
    if(present(zonediff)) then
       zonediff = zdvol
    endif
    deallocate(zdvol,ansa)

    call ps_freetmp(ss%iobj,idvec(1),iout)
    call ps_freetmp(ss%iobj,idwvec(1),iout)
    call ps_freetmp(ss%iobj,idrho,iout)  ! must do this one last

  END SUBROUTINE ps_user_rezone1

  SUBROUTINE ps_user_rezone_arr1(rho_orig, rho_target, f_orig, f_target, ierr,&
       w_orig, interp, &
       state, curdens, nonorm, zonesmoo, zonediff)

    !-------------------------------------------------
    ! D. McCune 6 Apr 2007
    !  "rezone" quantities-- same as ps_user_rezone1, except apply to a 1d
    !  list of profile quantities, instead of just a single quantity.
    !-------------------------------------------------

    REAL(KIND=rspec), dimension(:), intent(in) :: rho_orig ! source grid
    !  rho_orig MUST span range [0:1]

    REAL(KIND=rspec), dimension(:), intent(in) :: rho_target ! rezone target
    !  rho_target MUST span range [0:1]

    REAL(KIND=rspec), dimension(:,:), intent(in) :: f_orig  ! vector of
    !  size(f_orig,2) input profiles; jth profile is f_orig(:,j).
    !  NOTE: nonorm=T *AND* size(f_orig) = size(rho_orig)-1 (i.e. f_orig is
    !  a step function) *AND* w_orig is absent, then f_orig is assumed to be
    !  in un-normalized units (as f_target will be): e.g. ptcls/zone rather
    !  than ptcls/m^3, or, Amps/zone rather than Amps/m^2.  The necessary
    !  zonal dVol or dArea will be divided out before computing the volume
    !  or area integral for rezoning.

    REAL(KIND=rspec), dimension(:,:), intent(out) :: f_target  ! results

    INTEGER, intent(out) :: ierr          ! completion code (0=OK)

    REAL(KIND=rspec), dimension(:,:), intent(in), optional :: w_orig
    !  weighting profiles.  Ignored where =0.0 everywhere.  Default: no 
    !  weighting and f_orig*dV or f_orig*dA is conserved.  If present then
    !  f_orig*w_orig*dV or f_orig*w_orig*dA is conserved.
    !     indexed the same as f_orig.
    !  NOTE: if nonorm=T *AND* size(w_orig)=size(rho_orig-1) (i.e. w_orig is
    !  a step function), it is assumed that w_orig is in un-normalized units,
    !  e.g. particles per zone, not particles / m^3.  The zonal dVol will be
    !  divided out to form the weighting function used in the integral.

    integer, intent(in), optional :: interp   ! interpolation selection control
    !  default: step function (-1) if size(f_orig) = size(rho_orig) - 1
    !  default: piecewise lin (0)  if size(f_orig) = size(rho_orig)
    !    (1) for C1 cubic Hermite interpolation
    !    (2) for C2 cubic Spline interpolation

    TYPE (plasma_state), optional, target :: state
    !  operate on THIS state, if argument is present;
    !  otherwise use "ps" state.

    logical, intent(in), optional :: curdens  ! T if quantity is a current
    ! density (area normalization); default is F (volume normalization).

    logical, intent(in), optional :: nonorm   ! T to suppress density 
    ! normalization; default is F.
    !  *CAUTION* this can affect interpretation of input as well as output
    !  profiles.

    logical, intent(in), optional :: zonesmoo ! T to apply 1/2 zone width
    ! integral smoothing operator.  Default is F.  This option is not 
    ! recommended unless mapping coarse step function data to a fine grid
    ! with requirement to remove step structure from smoothed interpolant.
    
    REAL(KIND=rspec), dimension(:), intent(out), optional :: zonediff
    ! the array of
    ! finite difference volumes (or AREAs if curdens=.TRUE.) corresponding 
    ! to the rho_target(...) grid provided.
    
    !------------------------------------------------
    integer, dimension(:), allocatable :: idv, idwv

    integer :: nrho_target,nrho_orig,iout,i,ivec,inpro,iertmp
    logical :: jcurdens,jnnorm,jsmoo,inorm
    REAL(KIND=rspec), dimension(:), allocatable :: zdvol,zwk,zdva_orig
    integer :: idrho,jntrp

    TYPE (plasma_state), pointer :: ss
    !------------------------------------------------

    ierr=0
    f_target = 0.0_rspec
    if(present(zonediff)) zonediff = 0.0_rspec

    iout=SWIM_err_out_file

    nrho_target = size(rho_target)
    if((abs(rho_target(1)).gt.1.0e-10_rspec).or. &
         (abs(rho_target(nrho_target)-1.0_rspec).gt.1.0e-10_rspec)) then
       write(iout,*) ' ?ps_user_rezone: target rho_target grid does not span [0:1]:'
       write(iout,*) '  rho_target(1) = ',rho_target(1)
       write(iout,*) '  rho_target(',nrho_target,') = ',rho_target(nrho_target)
       ierr = 999
       return
    endif

    call schk('ps_user_rezone','size(rho_target)',nrho_target, &
         'size(f_target,1)+1',size(f_target,1)+1,ierr)
    if(ierr.ne.0) return

    ivec=size(f_target,2)
    inpro=size(f_orig,1)

    call schk('ps_user_rezone','size(f_target,2)',size(f_target,2), &
         'size(f_orig,2)',size(f_orig,2),ierr)
    if(ierr.ne.0) then
       write(iout,*) ' number of profiles does not match output array size.'
       return
    endif

    if(present(zonediff)) then
       call schk('ps_user_rezone','size(rho_target)',nrho_target, &
            'size(zonediff)+1',size(zonediff)+1,ierr)
       if(ierr.ne.0) return
    endif

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    nrho_orig = size(rho_orig)

    if(inpro.eq.nrho_orig) then
       jntrp=0
    else
       jntrp=-1
    endif

    if(present(interp)) jntrp=interp

    if((jntrp.lt.-1).or.(jntrp.gt.2)) then
       write(iout,*) ' ?ps_user_rezone: illegal argument value, interp=',interp
       ierr = 999
       return
    endif

    if(jntrp.eq.-1) then
       call schk('ps_user_rezone','size(rho_orig)',nrho_orig, &
            'size(f_orig,1)+1',inpro+1,ierr)
       if(ierr.ne.0) return
       if(present(w_orig)) then
          call schk('ps_user_rezone','size(rho_orig)',nrho_orig, &
               'size(w_orig,1)+1',size(w_orig,1)+1,ierr)
          if(ierr.ne.0) return
       endif
    else
       call schk('ps_user_rezone','size(rho_orig)',nrho_orig, &
            'size(f_orig,1)',inpro,ierr)
       if(ierr.ne.0) return
       if(present(w_orig)) then
          call schk('ps_user_rezone','size(rho_orig)',nrho_orig, &
               'size(w_orig,1)',size(w_orig,1),ierr)
          if(ierr.ne.0) return
       endif
    endif

    !-----------------------------
    ! OK, proceed...

    jcurdens = .FALSE.
    if(present(curdens)) jcurdens = curdens

    jnnorm = .FALSE.
    if(present(nonorm)) jnnorm = nonorm
    
    inorm = .FALSE.
    if((jntrp.eq.-1).and.jnnorm) then
       inorm = .TRUE.  ! need to divide output dVol or dArea from input
       allocate(zdva_orig(nrho_orig-1),zwk(nrho_orig))
       if(jcurdens) then
          call ps_eq_area(ss%iobj,nrho_orig,rho_orig,zwk,iout,ierr)
       else
          call ps_eq_volume(ss%iobj,nrho_orig,rho_orig,zwk,iout,ierr)
       endif
       if(ierr.ne.0) return
       zdva_orig = zwk(2:nrho_orig)-zwk(1:nrho_orig-1)
    endif

    !  create temporary grid 
    call ps_rhotmp(ss%iobj,nrho_orig,rho_orig,1,idrho,iout,ierr)
    if(ierr.ne.0) then
       write(iout,*) ' error in ps_user_rezone: rho_orig grid.'
       return
    endif

    allocate(idv(ivec),idwv(ivec))

    do i=1,ivec
       if(inorm) then
          call ps_protmp(ss%iobj,idrho,inpro,f_orig(:,i)/zdva_orig, &
               i,jntrp,idv(i), &
               iout,iertmp)
       else
          call ps_protmp(ss%iobj,idrho,inpro,f_orig(:,i),i,jntrp,idv(i), &
               iout,iertmp)
       endif
       ierr=max(ierr,iertmp)
    enddo

    if(ierr.ne.0) then
       write(iout,*) ' error in ps_user_rezone: f_orig profiles.'
       do i=1,ivec
          call ps_freetmp(ss%iobj,idv(i),iout)
       enddo
       call ps_freetmp(ss%iobj,idrho,iout)  ! do this last
       deallocate(idv,idwv)
       return
    endif

    idwv = 0
    if(present(w_orig)) then
       do i=1,ivec
          if(inorm) then
             call ps_wprotmp(ss%iobj,idrho,inpro,w_orig(:,i)/zdva_orig, &
                  ivec+i,jntrp,idwv(i),&
                  iout,iertmp)
          else
             call ps_wprotmp(ss%iobj,idrho,inpro,w_orig(:,i), &
                  ivec+i,jntrp,idwv(i),&
                  iout,iertmp)
          endif
          ierr=max(ierr,iertmp)
       enddo

       if(ierr.ne.0) then
          write(iout,*) ' error in ps_user_rezone: w_orig profiles.'
          do i=1,ivec
             call ps_freetmp(ss%iobj,idv(i),iout)
             call ps_freetmp(ss%iobj,idwv(i),iout)
          enddo
          call ps_freetmp(ss%iobj,idrho,iout)  ! do this last
          deallocate(idv,idwv)
          return
       endif
    endif

    jsmoo = .FALSE.
    if(present(zonesmoo)) jsmoo = zonesmoo

    allocate(zdvol(nrho_target-1))

    call ps_rezone_rho(ss,nrho_target,rho_target,size(idv),idv,idwv, &
         jcurdens,jnnorm,jsmoo, &
         f_target,zdvol,iout,ierr)
    if(ierr.ne.0) then
       write(iout,*) ' ?ps_user_rezone: ps_rezone_rho subroutine call failed.'
       f_target = 0.0_rspec
    endif

    if(present(zonediff)) then
       zonediff = zdvol
    endif
    deallocate(zdvol)

    do i=1,ivec
       call ps_freetmp(ss%iobj,idv(i),iout)
       call ps_freetmp(ss%iobj,idwv(i),iout)
    enddo
    call ps_freetmp(ss%iobj,idrho,iout)  ! do this last

  END SUBROUTINE ps_user_rezone_arr1

  SUBROUTINE ps_user_rezone_arr2(rho_orig, rho_target, f_orig, f_target, ierr,&
       w_orig, interp, &
       state, curdens, nonorm, zonesmoo, zonediff)

    !-------------------------------------------------
    ! D. McCune 6 Apr 2007
    !  "rezone" quantities-- same as ps_user_rezone1, except apply to a 2d
    !  list of profile quantities, instead of just a single quantity.
    !-------------------------------------------------

    REAL(KIND=rspec), dimension(:), intent(in) :: rho_orig ! source grid
    !  rho_orig MUST span range [0:1]

    REAL(KIND=rspec), dimension(:), intent(in) :: rho_target ! rezone target
    !  rho_target MUST span range [0:1]

    REAL(KIND=rspec), dimension(:,:,:), intent(in) :: f_orig  ! 2d array of
    !  input profiles; (j,k)th profile is f_orig(:,j,k).
    !  NOTE: nonorm=T *AND* size(f_orig) = size(rho_orig)-1 (i.e. f_orig is
    !  a step function) *AND* w_orig is absent, then f_orig is assumed to be
    !  in un-normalized units (as f_target will be): e.g. ptcls/zone rather
    !  than ptcls/m^3, or, Amps/zone rather than Amps/m^2.  The necessary
    !  zonal dVol or dArea will be divided out before computing the volume
    !  or area integral for rezoning.

    REAL(KIND=rspec), dimension(:,:,:), intent(out) :: f_target  ! results

    INTEGER, intent(out) :: ierr          ! completion code (0=OK)

    REAL(KIND=rspec), dimension(:,:,:), intent(in), optional :: w_orig
    !  weighting profiles.  Ignored where =0.0 everywhere.  Default: no 
    !  weighting and f_orig*dV or f_orig*dA is conserved.  If present then
    !  f_orig*w_orig*dV or f_orig*w_orig*dA is conserved.
    !     indexed the same as f_orig.
    !  NOTE: if nonorm=T *AND* size(w_orig)=size(rho_orig-1) (i.e. w_orig is
    !  a step function), it is assumed that w_orig is in un-normalized units,
    !  e.g. particles per zone, not particles / m^3.  The zonal dVol will be
    !  divided out to form the weighting function used in the integral.

    integer, intent(in), optional :: interp   ! interpolation selection control
    !  default: step function (-1) if size(f_orig) = size(rho_orig) - 1
    !  default: piecewise lin (0)  if size(f_orig) = size(rho_orig)
    !    (1) for C1 cubic Hermite interpolation
    !    (2) for C2 cubic Spline interpolation

    TYPE (plasma_state), optional, target :: state
    !  operate on THIS state, if argument is present;
    !  otherwise use "ps" state.

    logical, intent(in), optional :: curdens  ! T if quantity is a current
    ! density (area normalization); default is F (volume normalization).

    logical, intent(in), optional :: nonorm   ! T to suppress density 
    ! normalization; default is F.
    !  *CAUTION* this can affect interpretation of input as well as output
    !  profiles.

    logical, intent(in), optional :: zonesmoo ! T to apply 1/2 zone width
    ! integral smoothing operator.  Default is F.  This option is not 
    ! recommended unless mapping coarse step function data to a fine grid
    ! with requirement to remove step structure from smoothed interpolant.
    
    REAL(KIND=rspec), dimension(:), intent(out), optional :: zonediff 
    ! the array of
    ! finite difference volumes (or AREAs if curdens=.TRUE.) corresponding 
    ! to the rho(...) grid provided.
    
    !------------------------------------------------
    integer :: nrho_target,nrho_orig,iout,isizid,i1,i2,ii
    integer :: inpro,jntrp,idrho,iertmp
    logical :: jcurdens,jnnorm,jsmoo,inorm
    REAL(KIND=rspec), dimension(:), allocatable :: zdvol,zwk,zdva_orig
    REAL(KIND=rspec), dimension(:,:), allocatable :: ansaa
    INTEGER, dimension(:), allocatable :: idvec,idwvec

    TYPE (plasma_state), pointer :: ss
    !------------------------------------------------

    ierr=0
    f_target = 0.0_rspec
    if(present(zonediff)) zonediff = 0.0_rspec

    iout=SWIM_err_out_file

    nrho_target = size(rho_target)
    if((abs(rho_target(1)).gt.1.0e-10_rspec).or. &
         (abs(rho_target(nrho_target)-1.0_rspec).gt.1.0e-10_rspec)) then
       write(iout,*) ' ?ps_user_rezone: target rho_target grid does not span [0:1]:'
       write(iout,*) '  rho_target(1) = ',rho_target(1)
       write(iout,*) '  rho_target(',nrho_target,') = ',rho_target(nrho_target)
       ierr = 999
       return
    endif

    call schk('ps_user_rezone','size(rho_target)',nrho_target, &
         'size(f_target,1)+1',size(f_target,1)+1,ierr)
    if(ierr.ne.0) return

    call schk('ps_user_rezone','size(f_orig,2)',size(f_orig,2), &
         'size(f_target,2)',size(f_target,2),ierr)
    if(ierr.ne.0) then
       write(iout,*) ' number of profiles does not match output array size.'
       return
    endif

    call schk('ps_user_rezone','size(f_orig,3)',size(f_orig,3), &
         'size(f_target,3)',size(f_target,3),ierr)
    if(ierr.ne.0) then
       write(iout,*) ' number of profiles does not match output array size.'
       return
    endif

    if(present(zonediff)) then
       call schk('ps_user_rezone','size(rho_target)',nrho_target, &
            'size(zonediff)+1',size(zonediff)+1,ierr)
       if(ierr.ne.0) return
    endif

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    inpro = size(f_orig,1)

    nrho_orig = size(rho_orig)

    if(inpro.eq.nrho_orig) then
       jntrp=0
    else
       jntrp=-1
    endif

    if(present(interp)) jntrp=interp

    if((jntrp.lt.-1).or.(jntrp.gt.2)) then
       write(iout,*) ' ?ps_user_rezone: illegal argument value, interp=',interp
       ierr = 999
       return
    endif

    if(jntrp.eq.-1) then
       call schk('ps_user_rezone','size(rho_orig)',nrho_orig, &
            'size(f_orig,1)+1',inpro+1,ierr)
       if(ierr.ne.0) return
       if(present(w_orig)) then
          call schk('ps_user_rezone','size(rho_orig)',nrho_orig, &
               'size(w_orig,1)+1',size(w_orig,1)+1,ierr)
          if(ierr.ne.0) return
       endif
    else
       call schk('ps_user_rezone','size(rho_orig)',nrho_orig, &
            'size(f_orig,1)',inpro,ierr)
       if(ierr.ne.0) return
       if(present(w_orig)) then
          call schk('ps_user_rezone','size(rho_orig)',nrho_orig, &
               'size(w_orig,1)',size(w_orig,1),ierr)
          if(ierr.ne.0) return
       endif
    endif

    !-----------------------------
    ! OK, proceed...

    jcurdens = .FALSE.
    if(present(curdens)) jcurdens = curdens

    jnnorm = .FALSE.
    if(present(nonorm)) jnnorm = nonorm
    
    inorm = .FALSE.
    if((jntrp.eq.-1).and.jnnorm) then
       inorm = .TRUE.  ! need to divide output dVol or dArea from input
       allocate(zdva_orig(nrho_orig-1),zwk(nrho_orig))
       if(jcurdens) then
          call ps_eq_area(ss%iobj,nrho_orig,rho_orig,zwk,iout,ierr)
       else
          call ps_eq_volume(ss%iobj,nrho_orig,rho_orig,zwk,iout,ierr)
       endif
       if(ierr.ne.0) return
       zdva_orig = zwk(2:nrho_orig)-zwk(1:nrho_orig-1)
    endif

    !  create temporary grid 
    call ps_rhotmp(ss%iobj,nrho_orig,rho_orig,1,idrho,iout,ierr)
    if(ierr.ne.0) then
       write(iout,*) ' error in ps_user_rezone: rho_orig grid.'
       return
    endif

    isizid = size(f_orig,2)*size(f_orig,3)
    allocate(idvec(isizid),idwvec(isizid))

    idvec = 0
    idwvec = 0

    ii = 0
    do i2=1,size(f_orig,3)
       do i1=1,size(f_orig,2)
          ii = ii + 1
          if(inorm) then
             call ps_protmp(ss%iobj,idrho,inpro,f_orig(:,i1,i2)/zdva_orig,ii, &
                  jntrp,idvec(ii),iout,iertmp)
          else
             call ps_protmp(ss%iobj,idrho,inpro,f_orig(:,i1,i2),ii, &
                  jntrp,idvec(ii),iout,iertmp)
          endif
          ierr=max(ierr,iertmp)
          if(present(w_orig)) then
             if(inorm) then
                call ps_wprotmp(ss%iobj,idrho,inpro,w_orig(:,i1,i2)/zdva_orig, &
                     ii+isizid, &
                     jntrp,idwvec(ii),iout,iertmp)
             else
                call ps_wprotmp(ss%iobj,idrho,inpro,w_orig(:,i1,i2),ii+isizid, &
                     jntrp,idwvec(ii),iout,iertmp)
             endif
             ierr=max(ierr,iertmp)
          endif
       enddo
    enddo

    if(ierr.ne.0) then
       write(iout,*) ' error in ps_user_rezone: f_orig or w_orig profiles.'
       do ii=1,isizid
          call ps_freetmp(ss%iobj,idvec(ii),iout)
          call ps_freetmp(ss%iobj,idwvec(ii),iout)
       enddo
       call ps_freetmp(ss%iobj,idrho,iout)  ! do this last
       deallocate(idvec,idwvec)
       return
    endif

    jcurdens = .FALSE.
    if(present(curdens)) jcurdens = curdens

    jnnorm = .FALSE.
    if(present(nonorm)) jnnorm = nonorm

    jsmoo = .FALSE.
    if(present(zonesmoo)) jsmoo = zonesmoo

    allocate(zdvol(nrho_target-1),ansaa(nrho_target-1,isizid))

    call ps_rezone_rho(ss,nrho_target,rho_target,isizid,idvec,idwvec, &
         jcurdens,jnnorm,jsmoo, &
         ansaa,zdvol,iout,ierr)
    if(ierr.ne.0) then
       write(iout,*) ' ?ps_user_rezone: ps_rezone_rho subroutine call failed.'
    else
       ii = 0
       do i2=1,size(f_orig,3)
          do i1=1,size(f_orig,2)
             ii = ii + 1
             f_target(:,i1,i2) = ansaa(:,ii)
          enddo
       enddo
    endif

    if(present(zonediff)) then
       zonediff = zdvol
    endif

    do ii=1,isizid
       call ps_freetmp(ss%iobj,idvec(ii),iout)
       call ps_freetmp(ss%iobj,idwvec(ii),iout)
    enddo
    call ps_freetmp(ss%iobj,idrho,iout)  ! do this last

    deallocate(zdvol,idvec,idwvec,ansaa)

  END SUBROUTINE ps_user_rezone_arr2

  !-------------------------------------------
  !  ps_user_1dintrp -- interpolation of user provided data
  !  (note: these routines do not depend on a Plasma State object; they
  !  are just a convenience for interpolation)
  !    NOTE: no boundary conditions options yet...
  !-------------------------------------------

  SUBROUTINE ps_user_1dintrp_sc(x,x_orig,f_orig,ans,ierr, &
       interp,ideriv)

    !-------------------------------------------
    ! D. McCune 9 Feb 2010
    !  on-the-fly interpolation of user provided profile
    !    [f_orig(:) vs. x_orig(:)]
    !  single target x value; spline setup done if required; not saved.
    !-------------------------------------------

    REAL(KIND=rspec), intent(in) :: x   ! target value for interpolation

    REAL(KIND=rspec), DIMENSION(:), intent(in) :: x_orig ! input profile x
    ! values (must be in strict ascending order)

    REAL(KIND=rspec), DIMENSION(:), intent(in) :: f_orig  ! input profile

    REAL(KIND=rspec), intent(out) :: ans ! interpolation results

    INTEGER, intent(out) :: ierr   ! exit status code; 0=normal

    !---optional---

    integer, intent(in), optional :: interp   ! interpolation selection control
    !  required default: step function (-1) if size(f_orig) = size(x_orig) - 1
    !  default: piecewise lin (0)  if size(f_orig) = size(x_orig)
    !    (1) for C1 cubic Hermite interpolation
    !    (2) for C2 cubic Spline interpolation

    integer, intent(in), optional :: ideriv   ! derivative control
    ! 0 (default) for none, 1 for d/dx, 2 for d2/dx2
    ! error will be reported if interpolation method does not support 
    
    !------------------------------------------
    ! local:

    REAL(KIND=rspec), dimension(1,1) :: ansloc
    REAL(KIND=rspec), dimension(1) :: xtarg
    REAL(KIND=rspec), dimension(:,:), allocatable :: floc
    REAL(KIND=rspec), dimension(:,:), allocatable :: xpkg
    INTEGER, dimension(1) :: idxloc

    INTEGER :: idimx,idimf,intrp,iout
    !==========================================
    ans  = 0.0_rspec
    ierr = 0

    iout = SWIM_err_out_file
    !----------------------
    idimx=size(x_orig)
    idimf=size(f_orig)

    allocate(floc(idimf,1))
    floc(1:idimf,1) = f_orig

    if(idimf.ne.idimx) then
       intrp=-1
    else
       intrp=0
    endif
    if(present(interp)) intrp=interp

    idxloc=0
    if(present(ideriv)) idxloc=ideriv

    allocate(xpkg(idimx,4))
    xtarg = x
    call chk_xorder('ps_user_1dintrp_sc',x_orig,idimf,intrp,idxloc, &
         xpkg,xtarg,ierr)
    if(ierr.ne.0) return

    call interp_1d(xtarg,xpkg,floc,ansloc,intrp,idxloc,ierr)

    if(ierr.eq.0) then
       ans=ansloc(1,1)
    endif

  END SUBROUTINE ps_user_1dintrp_sc

  SUBROUTINE ps_user_1dintrp_vec(xvec,x_orig,f_orig,ansv,ierr, &
       interp,ideriv)

    !-------------------------------------------
    ! D. McCune 9 Feb 2010
    !  on-the-fly interpolation of user provided profile
    !    [f_orig(:) vs. x_orig(:)]
    !  vector of target x values; spline setup done if required; not saved.
    !-------------------------------------------

    REAL(KIND=rspec), DIMENSION(:), intent(in) :: xvec   ! target x vector
    ! for interpolation

    REAL(KIND=rspec), DIMENSION(:), intent(in) :: x_orig  ! input profile x 
    ! values (must be in strict ascending order)

    REAL(KIND=rspec), DIMENSION(:), intent(in) :: f_orig  ! input profile

    REAL(KIND=rspec), DIMENSION(:), intent(out) :: ansv ! interp. results

    INTEGER, intent(out) :: ierr   ! exit status code; 0=normal

    !---optional---

    integer, intent(in), optional :: interp   ! interpolation selection control
    !  required default: step function (-1) if size(f_orig) = size(x_orig) - 1
    !  default: piecewise lin (0)  if size(f_orig) = size(x_orig)
    !    (1) for C1 cubic Hermite interpolation
    !    (2) for C2 cubic Spline interpolation

    integer, intent(in), optional :: ideriv   ! derivative control
    ! 0 (default) for none, 1 for d/dx, 2 for d2/dx2
    ! error will be reported if interpolation method does not support 
    
    !------------------------------------------
    ! local:

    REAL(KIND=rspec), dimension(:,:), allocatable :: ansloc
    REAL(KIND=rspec), dimension(:,:), allocatable :: floc
    REAL(KIND=rspec), dimension(:,:), allocatable :: xpkg
    REAL(KIND=rspec), dimension(:), allocatable :: xtarg
    INTEGER, dimension(1) :: idxloc

    INTEGER :: idimx,idimf,idimv,intrp,iout,ii
    !==========================================
    ansv = 0.0_rspec
    ierr = 0

    iout = SWIM_err_out_file
    !----------------------
    idimx=size(x_orig)
    idimf=size(f_orig)
    idimv=size(ansv)

    allocate(floc(idimf,1))
    floc(1:idimf,1) = f_orig

    if(idimf.ne.idimx) then
       intrp=-1
    else
       intrp=0
    endif
    if(present(interp)) intrp=interp

    idxloc=0
    if(present(ideriv)) idxloc=ideriv

    allocate(xpkg(idimx,4),xtarg(idimv))
    xtarg = xvec
    call chk_xorder('ps_user_1dintrp_vec',x_orig,idimf,intrp,idxloc, &
         xpkg,xtarg,ierr)
    if(ierr.ne.0) return

    allocate(ansloc(idimv,1))

    call interp_1d(xtarg,xpkg,floc,ansloc,intrp,idxloc,ierr)

    if(ierr.eq.0) then
       do ii=1,idimv
          ansv(ii)=ansloc(ii,1)
       enddo
    endif

  END SUBROUTINE ps_user_1dintrp_vec

  SUBROUTINE ps_user_1dintrp_sc_arr1(x,x_orig,f_orig,ansv,ierr, &
       interp,iswap,ideriv,iderivs)

    !-------------------------------------------
    ! D. McCune 9 Feb 2010
    !  on-the-fly interpolation of array of user provided 1d profiles
    !    [jth profile f_orig(:,j) vs. x_orig(:)]
    !    if (iswap) [jth profile f_orig(j,:) vs. x_orig(:)]
    !  single target x value; spline setup done if required; not saved.
    !-------------------------------------------

    REAL(KIND=rspec), intent(in) :: x   ! target value for interpolation

    REAL(KIND=rspec), DIMENSION(:), intent(in) :: x_orig  ! input profile x
    ! values (must be in strict ascending order)

    REAL(KIND=rspec), DIMENSION(:,:), intent(in) :: f_orig !input profiles

    REAL(KIND=rspec), DIMENSION(:), intent(out) :: ansv ! interp. results

    INTEGER, intent(out) :: ierr   ! exit status code; 0=normal

    !---optional---

    integer, intent(in), optional :: interp   ! interpolation selection control
    !  required default: step function (-1) if size(f_orig) = size(x_orig) - 1
    !  default: piecewise lin (0)  if size(f_orig) = size(x_orig)
    !    (1) for C1 cubic Hermite interpolation
    !    (2) for C2 cubic Spline interpolation

    logical, intent(in), optional :: iswap    ! set TRUE to indicate profile
    !  variation over 2nd dimension of f_orig instead of 1st dimension.
    !    iswap=.FALSE. (default) => jth profile is f_orig(:,j)
    !    iswap=.TRUE.            => jth profile is f_orig(j,:)

    integer, intent(in), optional :: ideriv   ! derivative control
    ! 0 (default) for none, 1 for d/dx, 2 for d2/dx2
    ! error will be reported if interpolation method does not support 
    ! this control applies to all profiles

    integer, intent(in), dimension(:), optional :: iderivs ! deriv. control
    ! same as ideriv, but separate control for each profile (overrides ideriv).
    
    !------------------------------------------
    ! local:

    REAL(KIND=rspec), dimension(:,:), allocatable :: ansloc
    REAL(KIND=rspec), dimension(:,:), allocatable :: floc
    REAL(KIND=rspec), dimension(:,:), allocatable :: xpkg
    REAL(KIND=rspec), dimension(1) :: xtarg
    INTEGER, dimension(:), allocatable :: idxloc

    INTEGER :: idimx,idimf,inumf,intrp,ii,iout
    LOGICAL :: swapf

    !==========================================
    ansv = 0.0_rspec
    ierr = 0

    iout = SWIM_err_out_file
    !----------------------
    swapf=.FALSE.
    if(present(iswap)) swapf=iswap

    idimx=size(x_orig)
    if(swapf) then
       inumf=size(f_orig,1)
       idimf=size(f_orig,2)
    else
       inumf=size(f_orig,2)
       idimf=size(f_orig,1)
    endif

    allocate(floc(idimf,inumf))
    do ii=1,inumf
       if(swapf) then
          floc(:,ii) = f_orig(ii,:)
       else
          floc(:,ii) = f_orig(:,ii)
       endif
    enddo

    if(idimf.ne.idimx) then
       intrp=-1
    else
       intrp=0
    endif
    if(present(interp)) intrp=interp

    allocate(idxloc(inumf)); idxloc=0
    if(present(ideriv)) idxloc=ideriv

    if(present(iderivs)) then
       if(size(iderivs).ne.inumf) then
          write(iout,*) ' ?ps_user_1dintrp_arr1: error in iderivs optional argument:'
          write(iout,*) '  array size (',size(iderivs),' does not match number of profiles: ',inumf
          ierr=99
          return
       endif

       idxloc = iderivs  ! OK
    endif

    allocate(xpkg(idimx,4))
    xtarg = x
    call chk_xorder('ps_user_1dintrp_sc_arr1',x_orig,idimf,intrp,idxloc, &
         xpkg,xtarg,ierr)
    if(ierr.ne.0) return

    allocate(ansloc(1,inumf))

    call interp_1d(xtarg,xpkg,floc,ansloc,intrp,idxloc,ierr)

    if(ierr.eq.0) then
       do ii=1,inumf
          ansv(ii)=ansloc(1,ii)
       enddo
    endif

  END SUBROUTINE ps_user_1dintrp_sc_arr1

  SUBROUTINE ps_user_1dintrp_vec_arr1(xvec,x_orig,f_orig,ansa,ierr, &
       interp,iswap,iswapa,ideriv,iderivs)

    !-------------------------------------------
    ! D. McCune 9 Feb 2010
    !  on-the-fly interpolation of array of user provided 1d profiles
    !    [jth profile f_orig(:,j) vs. x_orig(:)]
    !    if (iswap) [jth profile f_orig(j,:) vs. x_orig(:)]
    !  vector of target x values; spline setup done if required; not saved.
    !-------------------------------------------

    REAL(KIND=rspec), DIMENSION(:), intent(in) :: xvec   ! target x vector 
    ! for interpolation

    REAL(KIND=rspec), DIMENSION(:), intent(in) :: x_orig  ! input profile x
    ! values (must be in strict ascending order)

    REAL(KIND=rspec), DIMENSION(:,:), intent(in) :: f_orig !input profiles

    REAL(KIND=rspec), DIMENSION(:,:), intent(out) :: ansa ! interp. results

    INTEGER, intent(out) :: ierr   ! exit status code; 0=normal

    !---optional---

    integer, intent(in), optional :: interp   ! interpolation selection control
    !  required default: step function (-1) if size(f_orig) = size(x_orig) - 1
    !  default: piecewise lin (0)  if size(f_orig) = size(x_orig)
    !    (1) for C1 cubic Hermite interpolation
    !    (2) for C2 cubic Spline interpolation

    logical, intent(in), optional :: iswap    ! set TRUE to indicate profile
    !  variation over 2nd dimension of f_orig instead of 1st dimension.
    !    iswap=.FALSE. (default) => jth profile is f_orig(:,j)
    !    iswap=.TRUE.            => jth profile is f_orig(j,:)

    logical, intent(in), optional :: iswapa   ! set TRUE to indicate swap
    !  of output array dimensions:
    !    iswapa=.FALSE. (default) => jth profile interpolated to ansa(:,j)
    !    iswapa=.TRUE.            => jth profile interpolated to ansa(j,:)

    integer, intent(in), optional :: ideriv   ! derivative control
    ! 0 (default) for none, 1 for d/dx, 2 for d2/dx2
    ! error will be reported if interpolation method does not support 
    ! this control applies to all profiles

    integer, intent(in), dimension(:), optional :: iderivs ! deriv. control
    ! same as ideriv, but separate control for each profile (overrides ideriv).
    
    !------------------------------------------
    ! local:

    REAL(KIND=rspec), dimension(:,:), allocatable :: ansloc
    REAL(KIND=rspec), dimension(:,:), allocatable :: floc
    REAL(KIND=rspec), dimension(:,:), allocatable :: xpkg
    REAL(KIND=rspec), dimension(:), allocatable :: xtarg
    INTEGER, dimension(:), allocatable :: idxloc

    INTEGER :: idimx,idimf,idimv,inumf,intrp,ii,jj,iout
    LOGICAL :: swapf,swapa
    
    !==========================================
    ansa=0.0_rspec
    ierr = 0

    iout = SWIM_err_out_file
    !----------------------

    idimx=size(x_orig)

    swapf=.FALSE.
    if(present(iswap)) swapf=iswap

    swapa=.FALSE.
    if(present(iswapa)) swapa=iswapa

    if(swapf) then
       inumf=size(f_orig,1)
       idimf=size(f_orig,2)
    else
       inumf=size(f_orig,2)
       idimf=size(f_orig,1)
    endif

    if(swapa) then
       idimv=size(ansa,2)
    else
       idimv=size(ansa,1)
    endif

    allocate(floc(idimf,inumf))
    do ii=1,inumf
       if(swapf) then
          floc(:,ii) = f_orig(ii,:)
       else
          floc(:,ii) = f_orig(:,ii)
       endif
    enddo

    if(idimf.ne.idimx) then
       intrp=-1
    else
       intrp=0
    endif
    if(present(interp)) intrp=interp

    allocate(idxloc(inumf)); idxloc=0
    if(present(ideriv)) idxloc=ideriv

    if(present(iderivs)) then
       if(size(iderivs).ne.inumf) then
          write(iout,*) ' ?ps_user_1dintrp_arr1: error in iderivs optional argument:'
          write(iout,*) '  array size (',size(iderivs),' does not match number of profiles: ',inumf
          ierr=99
          return
       endif

       idxloc = iderivs  ! OK
    endif

    allocate(xpkg(idimx,4),xtarg(idimv))
    xtarg = xvec
    call chk_xorder('ps_user_1dintrp_vec_arr1',x_orig,idimf,intrp,idxloc, &
         xpkg,xtarg,ierr)
    if(ierr.ne.0) return

    allocate(ansloc(idimv,inumf))

    call interp_1d(xtarg,xpkg,floc,ansloc,intrp,idxloc,ierr)

    if(ierr.eq.0) then
       do jj=1,inumf
          do ii=1,idimv
             if(swapa) then
                ansa(jj,ii)=ansloc(ii,jj)
             else
                ansa(ii,jj)=ansloc(ii,jj)
             endif
          enddo
       enddo
    endif

  END SUBROUTINE ps_user_1dintrp_vec_arr1

  !-------------------------------------------
  !  shared private 1d profile set interpolation
  !  consistency of dimensioning of arrays assumed checked by callers

  subroutine interp_1d(xtarg,xpkg,fa,ansa,intrp,idxa,ierr)

    REAL(KIND=rspec), dimension(:), intent(in) :: xtarg ! interpolation targets
    REAL(KIND=rspec), dimension(:,:), intent(in) :: xpkg  ! profile x data
    REAL(KIND=rspec), dimension(:,:), intent(in) :: fa  ! profiles

    REAL(KIND=rspec), dimension(:,:), intent(inout) :: ansa  ! results
    ! was initialized to zero; left there if error occurs

    INTEGER, intent(in) :: intrp
    INTEGER, intent(in), dimension(:) :: idxa

    INTEGER, intent(out) :: ierr

    !---------------------------------
    integer :: idimx,inumf,idimt
    integer :: ix,if,it,iwarn,ilinx,ict(3)

    integer, dimension(:), allocatable :: iv
    REAL(KIND=rspec), dimension(:), allocatable :: dxn,hv,hiv
    REAL(KIND=rspec) :: hv1,hiv1,hv2,hiv2
    REAL(KIND=rspec), dimension(:,:), allocatable :: coef
    !---------------------------------

    ierr = 0

    ! array dimension lookups:

    idimt=size(xtarg)
    idimx=size(xpkg,1)
    inumf=size(fa,2)

    !  xtarg(idimt) -- idimt target points
    !  xpkg(idimx,4) -- x data array (pspline xpkg)
    !  fa(idimx,inumf) -- profile data
    !  idxa(inumf) -- derivative request

    !  ansa(idimt,inumf) -- interpolation results

    !  lookup...
    allocate(iv(idimt),dxn(idimt),hv(idimt),hiv(idimt))
    if(idimx.eq.2) then
       iwarn=0
       iv(1:idimt)=1
       hv(1:idimt)=(xpkg(2,1)-xpkg(1,1))
       hiv(1:idimt)=1.0_rspec/hv(1)
       do it=1,idimt
          dxn(it)=(xtarg(it)-xpkg(1,1))*hiv(1)
       enddo
    else if(idimx.eq.3) then
       iwarn=0
       hv1=(xpkg(2,1)-xpkg(1,1))
       hiv1=1.0_rspec/hv1
       hv2=(xpkg(3,1)-xpkg(2,1))
       hiv2=1.0_rspec/hv2
       do it=1,idimt
          if(xtarg(it).ge.xpkg(2,1)) then
             iv(it)=2
             hv(it)=hv2
             hiv(it)=hiv2
             dxn(it)=(xtarg(it)-xpkg(2,1))*hiv2
          else
             iv(it)=1
             hv(it)=hv1
             hiv(it)=hiv1
             dxn(it)=(xtarg(it)-xpkg(1,1))*hiv1
          endif
       enddo
    else
       call xlookup(idimt,xtarg,idimx,xpkg,2,iv,dxn,hv,hiv,iwarn)
    endif

    ilinx=0
    allocate(coef(2,idimx))

    do if=1,inumf

       ! derivative selection
       ict=0
       if(idxa(if).eq.0) then
          ict(1)=1
       else if(idxa(if).eq.1) then
          ict(2)=1
       else if(idxa(if).eq.2) then
          ict(3)=1
       endif

       ! interpolation coeff. setup (if needed)
       if(intrp.eq.1) then
          coef(1,:)=fa(:,if)
          call akherm1(xpkg,idimx,coef,ilinx,ierr)
          if(ierr.ne.0) exit
       else if(intrp.eq.2) then
          coef(1,:)=fa(:,if)
          call mkspline(xpkg,idimx,coef,0,0.0_rspec,0,0.0_rspec,ilinx,ierr)
          if(ierr.ne.0) exit
       endif

       ! interpolation evaluation
       if(intrp.lt.0) then
          ! step fcn
          do it=1,idimt
             ansa(it,if) = fa(iv(it),if)
          enddo
       else if(intrp.eq.0) then
          ! piecewise linear
          do it=1,idimt
             ansa(it,if) = (1.0_rspec-dxn(it))*fa(iv(it),if) + &
                  dxn(it)*fa(iv(it)+1,if)
          enddo
       else if(intrp.eq.1) then
          ! hermite eval
          call herm1fcn(ict,idimt,idimt,ansa(:,if), &
               iv,dxn,hv,hiv,coef,idimx)
       else if(intrp.eq.2) then
          ! spline eval
          call fvspline(ict,idimt,idimt,ansa(:,if), &
               iv,dxn,hv,hiv,coef,idimx)
       endif
    enddo

  end subroutine interp_1d

  !-------------------------------------------
  ! private error checking routine

  subroutine chk_xorder(slbl,xarray,idimf,intrp,iderivs,xpkg,xtarg,ierr)

    character*(*), intent(in) :: slbl   ! name of calling routine
    REAL(KIND=rspec), dimension(:), intent(in) :: xarray  ! x grid array
    integer, intent(in) :: idimf   ! x array dimension size of profile data
    integer, intent(in) :: intrp   ! interpolation method code
    integer, dimension(:), intent(in) :: iderivs   ! derivatives

    REAL(KIND=rspec), dimension(:,:), intent(out) :: xpkg  ! x lookup pkg
    REAL(KIND=rspec), dimension(:), intent(inout) :: xtarg   ! bounded target x
    integer, intent(out) :: ierr   ! status code returned, 0=OK

    ! check consistency of:
    !   x grid strict ascending
    !   x grid dimension (idimx) & f array x grid dimension (idimf)
    !     for interpolation methods:
    !       intrp=-1 (step) idimf = idimx - 1 is expected
    !       intrp=0,1,2,... idimf = idimx is expected
    !         0: piecewise linear; 1: C1-Hermite; 2: C2-Spline
    !   also consistency of derivatives requests w/ interpolation method
    
    !---------------------------
    integer :: idimx,ideriv_max,ideriv_min,ideriv_max_ok,iout,ixord
    integer :: idimt,ix
    !---------------------------

    idimx=size(xarray)
    idimt=size(xtarg)
    ideriv_max = maxval(iderivs)
    ideriv_min = minval(iderivs)

    iout = SWIM_err_out_file

    ierr = 0

    if(intrp.eq.-1) then
       if(idimf.ne.(idimx-1)) then
          write(iout,*) ' ?'//trim(slbl)//': inconsistency in array dimensions'
          write(iout,*) '  for step function interpolation (expect idimf=idimx-1):'
          write(iout,*) '     grid dimension         (idimx): ',idimx
          write(iout,*) '     x dimension of f data  (idimf): ',idimf
          ierr=1
       endif
    else
       if(idimf.ne.idimx) then
          write(iout,*) ' ?'//trim(slbl)//': inconsistency in array dimensions'
          if(intrp.eq.0) then
             write(iout,*) '  for piecewise linear interpolation (expect idimf=idimx)'
          else if(intrp.eq.1) then
             write(iout,*) '  for C1 Hermite interpolation (expect idimf=idimx)'
          else if(intrp.eq.2) then
             write(iout,*) '  for C2 Spline interpolation (expect idimf=idimx)'
          else
             write(iout,*) '  for order ',intrp,' interpolation (expect idimf=idimx)'
          endif
          write(iout,*) '     grid dimension         (idimx): ',idimx
          write(iout,*) '     x dimension of f data  (idimf): ',idimf
          ierr=1
       endif
    endif

    if((intrp.le.0).or.(intrp.gt.2)) then
       ideriv_max_ok=0
    else
       ideriv_max_ok=intrp
    endif

    if(ideriv_min.lt.0) then
       write(iout,*) ' ?'//trim(slbl)//': derivative code < 0: ',ideriv_min
       ierr=1
    endif

    if(ideriv_max.gt.ideriv_max_ok) then
       ierr=1
       write(iout,*) ' ?'//trim(slbl)//': derivative code exceeds maximum'
       if(intrp.eq.-1) then
          write(iout,*) '  for stop function (max is 0)'
       else if(intrp.eq.0) then
          write(iout,*) '  for piecewise linear interpolation (max is 0)'
       else if(intrp.eq.1) then
          write(iout,*) '  for C1 Hermite interpolation (max is 1)'
       else if(intrp.eq.2) then
          write(iout,*) '  for C2 Spline interpolation (max is 2)'
       else
          write(iout,*) '  for order ',intrp,' interpolation (max is 0)'
       endif
       write(iout,*) '    maximum derivative code passed: ',ideriv_max
    endif
    
    ixord=0
    do ix=2,idimx
       if(xarray(ix-1).ge.xarray(ix)) then
          ixord=ixord+1
          if(ixord.eq.4) then
             write(iout,*) ' ...and more... '
          else
             write(iout,*) '   ?? x(',ix-1,') = ',xarray(ix-1),' >= ', &
                  ' x(',ix,') = ',xarray(ix)
          endif
       endif
    enddo

    if(ixord.gt.0) then
       write(iout,*) ' ?'//trim(slbl)//': profile x axis out of order.'
       ierr=1
    endif

    if(ierr.ne.0) then
       xpkg = 0.0_rspec
    else
       if(idimx.ge.3) then
          ! psline x axis lookup data prep
          ! assuming: no periodic BC, no out-of-range messages,
          ! default out of range tolerance, index function lookup, stateless
          call genxpkg(idimx,xarray,xpkg,0,0,0,0.0_rspec,3,ierr)
       else
          xpkg=0.0_rspec
          xpkg(1:idimx,1)=xarray
       endif
    endif

    if(ierr.eq.0) then
       do ix=1,idimt
          xtarg(ix)=max(xarray(1),min(xarray(idimx),xtarg(ix)))
       enddo
    endif

  end subroutine chk_xorder

  !-------------------------------------------
  SUBROUTINE ps_get_prof_label(id,ierr,state,label,units,author)

    ! get labeling information on a profile
    ! if no state instance is specified, use "ps".

    integer, intent(in) :: id     ! profile ID (e.g. ss%id_ns(0))
    integer, intent(out) :: ierr  ! return status code: 0=OK

    type (plasma_state), optional, target :: state

    character*(*), intent(out), optional :: label
    character*(*), intent(out), optional :: units
    character*(*), intent(out), optional :: author

    !---------------
    character*32 zunits,zauthor
    character*128 zlabel
    integer :: iout
    TYPE (plasma_state), pointer :: ss
    !---------------

    iout=SWIM_err_out_file

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    call ps_init_tag

    call ps_getlabel(ss,id,zlabel,zunits,zauthor,iout,ierr)

    if(ierr.eq.0) then
       if(present(units)) units=zunits
       if(present(label)) label=zlabel
       if(present(author)) author=zauthor
    else
       if(present(units)) units=' '
       if(present(label)) label=' '
       if(present(author)) author=' '
    endif
       
  end SUBROUTINE ps_get_prof_label

  !-------------------------------------------
  SUBROUTINE ps_rho_metric(rho_name,metric_name,ansv,ierr, &
       state, user_rho_grid)

    ! compute flux surface averaged or integrated metrics over a specified
    ! rho grid.

    ! specify metric_name "DVOL_EQ" to use the Volume(rho) profile employed
    ! in ps_rho_rezone and ps_user_rezone; use "DVOL" to get exact volumes.

    ! specify metric_name "AREA_EQ" to use the Area(rho) profile employed
    ! in ps_rho_rezone and ps_user_rezone; use "DAREA" to get exact cross
    ! sectional areas.

    character*(*), intent(in) :: rho_name     ! name of state rho grid, or
    !    "USER[rho-grid-name]"; interpretation is case-insensitive.  If 
    !    rho_name starts with USER, optional argument user_rho_grid MUST
    !    be provided.

    character*(*), intent(in) :: metric_name  ! name of metric (see partial
    !    table below; interpretation is case-insensitive.

    real*8, dimension(:), intent(out) :: ansv ! result of calculation
    !    SIZE of ansv must match [size of rho grid] or [(size of rho grid) - 1]
    !    depending on the metric_name selected.

    integer, intent(out) :: ierr              ! status code (0=OK)

    !------------------
    !  optional arguments...

    TYPE (plasma_state), optional, target :: state
    !  operate on THIS state, if argument is present;
    !  otherwise use "ps" state.

    real*8, dimension(:), optional, intent(in) :: user_rho_grid
    !  user specified rho grid:  strict ascending, contained in [0:1].
    !  This argument MUST be present if rho_name starts with "USER";
    !  otherwise it must NOT be present.

    !-----------------------------------------
    !  partial table of common metrics
    !
    !      sizes:  nrho = [rho_name grid size]
    !              nrho-1  = [rho_name grid size] - 1
    !
    !  metric_name   size      description
    ! 
    !    "dVol"      nrho-1    zone volumes, m^3
    !    "dArea"     nrho-1    zone cross sectional areas, m^2
    !    "Vol"       nrho      volume enclosed in rho surfaces, m^3
    !    "Area"      nrho      cross section area enclosed by rho surfaces, m^2
    !    "<1/R^2>S"  nrho      differential volume flux surface average of
    !                          (1/R^2) at each surface, m^-2
    !    "<1/R^2>    nrho-1    zonal volume average of (1/R^2), m^-2
    !
    !  in some cases, surface averages are faster to compute than zonal
    !  averages.
    !
    !-----------------------------------------
    ! there are many more; see the xplasma documentation.
    ! in TRANSP this is in source/doc/xplasma.hlp; on the web it is currently
    ! at http://w3.pppl.gov/~pshare/help/xplasma.htm
    !      (or access via the xplasma web page under the catalog at
    !       http://w3.pppl.gov/NTCC).
    !
    !-----------------------------------------

    TYPE (plasma_state), pointer :: ss
    character*32 :: zrho_name,zmet_name
    real(kind=rspec), dimension(:), allocatable :: zwk,zrho

    integer :: idi  ! internal ID for integrator
    integer :: iout
    integer :: ilocal
    integer :: iint
    integer :: isize,isize_expect

    !-----------------------------------------
    !  select state

    ierr = 0
    iout = SWIM_err_out_file

    CALL ps_init_tag

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    !-----------------------------------------
    !  Use ps_xplasma2 routines to find or set up integrator

    zrho_name = rho_name
    call uupper(zrho_name)

    zmet_name = metric_name
    call uupper(zmet_name)

    ilocal=0
    if(zmet_name.eq.'VOL_EQ') then
       ilocal=1
       iint=1
    else if(zmet_name.eq.'DVOL_EQ') then
       ilocal=1
       iint=0
    else if(zmet_name.eq.'AREA_EQ') then
       ilocal=2
       iint=1
    else if(zmet_name.eq.'DAREA_EQ') then
       ilocal=2
       iint=0
    endif

    if(zrho_name(1:4).eq.'USER') then
       if(.not.present(user_rho_grid)) then
          ierr = 999
          write(iout,*) ' ?ps_rho_metric: rho_name = "',trim(rho_name), &
               '" starts with "USER", so,'
          write(iout,*) '  optional argument (user_rho_grid) must be defined.'
       else
          if(ilocal.eq.0) then
             call ps_get_userInteg(ss%iobj,rho_name, &
                  size(user_rho_grid),user_rho_grid,idi,iout,ierr)
          else
             isize=size(user_rho_grid)
             call chksize
             if(ierr.eq.0) then
                allocate(zwk(isize),zrho(isize))
                zrho = user_rho_grid
             endif
          endif
       endif
    else
       if(present(user_rho_grid)) then
          ierr = 999
          write(iout,*) ' ?ps_rho_metric: rho_name = "',trim(rho_name), &
               '" does not start with "USER", so,'
          write(iout,*) '  optional argument (user_rho_grid) must NOT be defined.'
       else
          if(ilocal.eq.0) then
             call ps_get_Integ(ss%iobj,rho_name,idi,iout,ierr)

          else
             call ps_grid_size(ss%iobj,rho_name,isize,iout,ierr)
             call chksize
             if(ierr.eq.0) then
                allocate(zwk(isize),zrho(isize))
                call ps_grid(ss%iobj,rho_name,isize,zrho,iout,ierr)
             endif
          endif
       endif
    endif

    if(ierr.eq.0) then
       if(ilocal.eq.0) then
          ! evaluate integrated metric or field quantity
          call ps_rho_zonint(ss%iobj,idi,metric_name,size(ansv),ansv,iout,ierr)

       else
          if(ilocal.eq.1) then
             call ps_eq_volume(ss%iobj,isize,zrho,zwk,iout,ierr)
          else 
             call ps_eq_area(ss%iobj,isize,zrho,zwk,iout,ierr)
          endif
          if(ierr.eq.0) then
             if(iint.eq.1) then
                ansv = zwk
             else
                ansv = (zwk(2:isize)-zwk(1:isize-1))
             endif
          endif
          deallocate(zwk,zrho)
       endif
    endif

    if(ierr.ne.0) then
       write(iout,*) ' ?ps_rho_metric: integration failed.'
       write(iout,*) '    rho_name:    "',trim(rho_name),'";'
       write(iout,*) '    metric_name: "',trim(metric_name),'".'
    endif

    contains

      subroutine chksize
        isize_expect = isize - (1-iint)
        if(size(ansv).ne.isize_expect) then
           ierr=999
           write(iout,*) ' ?ps_rho_metric: size of output vector not correct:'
           write(iout,*) '  found:    ',size(ansv)
           write(iout,*) '  expected: ',isize_expect
        endif
      end subroutine chksize

  END SUBROUTINE ps_rho_metric

  SUBROUTINE ps_init_tag

    ! publically callable
    ! initialization routine
    !   this establishes the relationship between state objects and
    !   interpolation support and filenames provided by the implementation

    ! execute only ONCE

    !-----------------------------------------
    integer :: iersum,ierr,iout,i
    character*6 :: nam_sss
    !-----------------------------------------

    if(plasma_state_mod_init) return
    plasma_state_mod_init = .TRUE.

    ! register states...
    ! failure to register state is deemed fatal...

    iout=SWIM_err_out_file
    iersum = 0

    write(iout,*) ' ps_init_tag: Plasma State v'//trim(ps_version_id)// &
         ' f90 module initialization.'

    call ps_register_state(ps,"cur",iout,ierr)
    iersum = iersum + ierr

    call ps_register_state(psp,"prev",iout,ierr)
    iersum = iersum + ierr

    call ps_register_state(ps_next,"ps_next",iout,ierr)
    iersum = iersum + ierr

    call ps_register_state(saw0,"saw0",iout,ierr)
    iersum = iersum + ierr

    call ps_register_state(saw1,"saw1",iout,ierr)
    iersum = iersum + ierr

    call ps_register_state(pel0,"pel0",iout,ierr)
    iersum = iersum + ierr

    call ps_register_state(pel1,"pel1",iout,ierr)
    iersum = iersum + ierr

    call ps_register_state(output,"output",iout,ierr)
    iersum = iersum + ierr

    call ps_register_state(aux,"aux",iout,ierr)
    iersum = iersum + ierr

    call ps_register_state(cs,"cs",iout,ierr)
    iersum = iersum + ierr

    do i=1,nsss_max
       nam_sss = ' '
       if(i.le.99) then
          write(nam_sss,'("sss",i2.2)') i
       else
          write(nam_sss,'("sss",i3)') i
       endif
       call ps_register_state(sss(i),nam_sss,iout,ierr)
       iersum = iersum + ierr
    enddo

    if(iersum.gt.0) then

       call ps_bad_exit( &
            ' ?plasm_state_mod: irrecoverable initialization failure!')

    endif

  END SUBROUTINE ps_init_tag

  !---------------------------------------------------------------------  
  ! PRIVATE routine: check initialization status of passed state object

  SUBROUTINE chk_ini(state)

    TYPE (plasma_state) :: state
    integer, save :: auto_init_counter = 0

    integer :: iout,ierr
    character*16 auto_noname
    !-------------------------------
    iout=SWIM_err_out_file

    if(state%itag .eq. ps_unInit) then
       auto_init_counter = auto_init_counter + 1
       write(auto_noname,'("auto_noname_",i4.4)') auto_init_counter
       write(iout,*) &
            ' %plasma_state_mod: warning: auto-initialization of state object.'
       write(iout,*) &
            '  generated tagname & default filename root: '//auto_noname

       call ps_register_state(state,auto_noname,iout,ierr)

       if(ierr.ne.0) then
          write(iout,*) ' ??plasma_state_mod: improperly handled error.'
       endif
    endif

  END SUBROUTINE chk_ini

  !---------------------------------------------------------------------
  SUBROUTINE ps_get_grid_by_name(gname, ierr, state, isize, gdata)

    ! fetch grid data that matches the passed name

    character*(*), intent(in) :: gname  ! name of grid (case insensitive)
    integer, intent(out) :: ierr        ! return status (0 = OK)

    TYPE (plasma_state), OPTIONAL, target :: state  ! use "ps" state if omitted

    integer, intent(out), OPTIONAL :: isize   ! size of grid returned
    REAL(KIND=rspec), dimension(:), OPTIONAL :: gdata   ! grid data returned

    ! if (gdata) is present, its size must match or exceed the size of the
    ! grid.

    !-----------------------------
    !  local:
    integer :: jsize,ksize,id,iout,idum1,idum2
    TYPE (plasma_state), pointer :: ss
    !-----------------------------

    ierr = 0
    if(present(isize)) isize = 0
    if(present(gdata)) gdata = 0.0_rspec

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    iout = SWIM_err_out_file

    call ps_gridId(ss%iobj, iout, gname, id, jsize)
    if(id.eq.0) then
       ierr = 1
       write(iout,*) ' ?ps_get_grid_by_name: grid not found: ',trim(gname)
       return
    endif

    if(present(isize)) isize = jsize

    if(present(gdata)) then
       ksize = size(gdata)
       if(ksize.lt.jsize) then
          ierr = 2
          write(iout,*) &
               ' ?ps_get_grid_by_name: passed array too small to hold grid data.'
          write(iout,*) '  grid data size: ',jsize,'; passed array size: ',ksize
          return
       endif

       call ps_grid_get_data(ss%iobj,iout,id,ksize,gdata,jsize,idum1,idum2,ierr)

       if(ksize.gt.jsize) then
          gdata(jsize+1:ksize) = gdata(jsize)  ! replicate last data point
       endif
    endif

  END SUBROUTINE ps_get_grid_by_name

  SUBROUTINE pscomp_grids(state1, state2, gname1, gname2, match, ierr)

    ! compare the named grid across two states.
    ! return match = .TRUE. if and only if
    !    (a) grid is allocated in both states, AND
    !    (b) grid is the same size in both states, AND
    !    (c) grid is over the same coordinate in both states, AND
    !    (d) grid point values are identical in both states

    type (plasma_state) :: state1
    type (plasma_state) :: state2

    character*(*), intent(in) :: gname1  ! name of grid from state1
    character*(*), intent(in) :: gname2  ! name of grid from state2

    logical, intent(out) :: match

    integer, intent(out) :: ierr

    !---------------------------------
    integer :: iout,id1,inum1,id2,inum2,icoord1,icoord2,idum,ii,iwarn
    real*8, dimension(:), allocatable :: grid1,grid2
    !---------------------------------

    iout = SWIM_err_out_file

    match = .FALSE.
    ierr = 0

    call ps_gridId(state1%iobj, iout, gname1, id1, inum1)
    if((id1.eq.0).or.(inum1.eq.0)) return

    call ps_gridId(state2%iobj, iout, gname2, id2, inum2)
    if((id2.eq.0).or.(inum2.eq.0)) return

    if(inum1.ne.inum2) return

    allocate(grid1(inum1),grid2(inum2))  ! both the same size really...

    call ps_grid_get_data(state1%iobj, iout, id1, inum1, grid1, idum, &
         icoord1, iwarn, ierr)
    if(ierr.ne.0) return

    call ps_grid_get_data(state2%iobj, iout, id2, inum2, grid2, idum, &
         icoord2, iwarn, ierr)
    if(ierr.ne.0) return

    if(icoord1.ne.icoord2) return

    match = .TRUE.
    do ii=1,inum1
       if(grid1(ii).ne.grid2(ii)) match = .FALSE.
    enddo

  END SUBROUTINE pscomp_grids

  SUBROUTINE ps_cclist_add(component_name,cclist,iwarn)

    ! activate a component in a component list

    CHARACTER*(*), INTENT(IN) :: component_name   
                                   ! name of component; "*" to add all

    INTEGER :: cclist(ps_ccount)   ! component activation list
    !            cclist(j)=1 if component j is active, =0 if it is inactive
    
    INTEGER, INTENT(OUT) :: iwarn  ! =0: OK; =1: component_name unrecognized.

    !-------------------------
    CHARACTER*32 :: zcc
    INTEGER :: i
    !-------------------------

    iwarn = 0

    IF(component_name.eq.'*') THEN

       cclist = 1  ! activate all...
       RETURN

    ENDIF

    zcc = component_name
    CALL uupper(zcc)

    iwarn = 1
    DO i=1,ps_ccount
       IF(zcc.eq.ps_cnames(i)) then

          iwarn = 0

          cclist(i) = 1

       ENDIF
    ENDDO

    IF(iwarn.ne.0) then
       WRITE(SWIM_err_out_file,*) &
            ' ?ps_cclist_add: component name not recognized: "'// &
            trim(component_name)//'".'
    ENDIF

  END SUBROUTINE ps_cclist_add

  SUBROUTINE ps_cclist_remove(component_name,cclist,iwarn)

    ! activate a component in a component list

    CHARACTER*(*), INTENT(IN) :: component_name   
                                   ! name of component; "*" to remove all

    INTEGER :: cclist(ps_ccount)   ! component activation list
    !            cclist(j)=1 if component j is active, =0 if it is inactive
    
    INTEGER, INTENT(OUT) :: iwarn  ! =0: OK; =1: component_name unrecognized.

    !-------------------------
    CHARACTER*32 :: zcc
    INTEGER :: i
    !-------------------------

    iwarn = 0

    IF(component_name.eq.'*') THEN

       cclist = 0  ! deactivate all...
       RETURN

    ENDIF

    zcc = component_name
    CALL uupper(zcc)

    iwarn = 1
    DO i=1,ps_ccount
       IF(zcc.eq.ps_cnames(i)) then

          iwarn = 0

          cclist(i) = 0

       ENDIF
    ENDDO

    IF(iwarn.ne.0) then
       WRITE(SWIM_err_out_file,*) &
            ' ?ps_cclist_remove: component name not recognized: "'// &
            trim(component_name)//'".'
    ENDIF

  END SUBROUTINE ps_cclist_remove
       
  !-------------------------------------------
  ! PSW (plasma state window) routines
  ! aquire lists, indicative of plasma state contents,
  ! using the ps_list_window data type, cf plasma_state_definition_mod.f90

  SUBROUTINE psw_components(w)

    !  return list of components, with descriptions

    type (ps_list_window) :: w

    call free_window(w)

    w%list_name = 'Components'
    w%size = ps_ccount

    allocate(w%list_elems(ps_ccount))
    allocate(w%component(ps_ccount))
    allocate(w%elem_descr(ps_ccount))

    w%list_elems = ps_cnames
    w%component = ps_cnames    ! a synonym, in this context
    w%elem_descr = ps_cdescrs

    call psw_order(w)  ! private method -- put elements in alphabetic order

  END SUBROUTINE psw_components

  SUBROUTINE psw_contents(dtype,icomponent,w,ierr, state)

    !  return list of dimensions or data items, with descriptions

    character*1 :: dtype   ! "G" for grids, "S" for species lists, "L" for
    !                other types of lists; "D" for data (not profiles);
    !                "P" for profiles.
    !      new 2008: "M" for MACHINE_DESCRIPTION
    !                "C" for SHOT_CONFIGURATION
    !                "N" for SIMULATION_CONTROL

    integer :: icomponent(ps_ccount)  ! array of flags indicating the
    !                components to be searched for contents

    type (ps_list_window) :: w     ! query results: new list "w" returned.

    integer, intent(out) :: ierr   ! status code, 0=OK


    TYPE (plasma_state), OPTIONAL, TARGET :: state
    !  operate on THIS state, if argument is present;
    !  otherwise use "ps" state.

    !-----------------------------------------------------
    ! local variables
    type (plasma_state), pointer :: ss
    integer :: iout,i,isize,itype
    character*1 ddtype
    !-----------------------------------------------------

    ierr = 0
    iout = SWIM_err_out_file

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    ddtype = dtype
    call uupper(ddtype)

    itype = 0
    if(ddtype.eq."G") then
       itype = ps_grid_dims

    else if(ddtype.eq."S") then
       itype = ps_species_lists

    else if(ddtype.eq."L") then
       itype = ps_item_lists

    else if(ddtype.eq."D") then
       itype = ps_data

    else if(ddtype.eq."P") then
       itype = ps_profiles

    else if(ddtype.eq."M") then
       itype = ps_machine_descr

    else if(ddtype.eq."C") then
       itype = ps_shot_config

    else if(ddtype.eq."N") then
       itype = ps_sim_control

    else if(ddtype.eq."E") then
       itype = ps_enum_dims

    endif

    call ps_contents_alloc(itype,icomponent,w,ierr)
    if(ierr.ne.0) then
       write(iout,*) ' ?psw_contents: ps_contents_alloc ierr code: ',ierr
       return
    endif

    call plasma_state_contents(itype,icomponent,ss,w,ierr)

    call psw_order(w) ! return list sorted in alphabetic order

  END SUBROUTINE psw_contents
    
  SUBROUTINE psw_slist(listname,w,ierr, state, iorder)

    ! return species list

    character*32, intent(in) :: listname  ! name of species list
    type (ps_list_window) :: w    ! species list returned: label, m, q, ...
    integer, intent(out) :: ierr  ! status code, 0=OK


    TYPE (plasma_state), OPTIONAL, TARGET :: state
    !  operate on THIS state, if argument is present;
    !  otherwise use "ps" state.

    logical, intent(in), optional :: iorder
    !  (default = T) return list in order if T
    !    thermal electrons, thermal ions sorted by Z,
    !      various fast ions sorted by Z, fast electrons

    !-----------------------------------------------------
    ! local variables
    type (plasma_state), pointer :: ss
    integer :: iout,i,isize,id
    character*1 ddtype
    character*32 altdummy(1)
    real*8, dimension(:), allocatable :: zqv,zqa
    logical :: jorder
    !-----------------------------------------------------

    ierr = 0
    iout = SWIM_err_out_file

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    call free_window(w)

    w%list_name = trim(listname)//'_species_list'

    call ps_listId(ss%iobj,iout,'PS_SPECIES_'//trim(listname),id,isize)

    w%size = isize
    if(isize.eq.0) return

    allocate(w%list_elems(isize))
    allocate(w%spectype(isize))
    allocate(w%qval(isize))
    allocate(w%qatom(isize))
    allocate(w%awght(isize))

    allocate(zqa(isize),zqv(isize))

    altdummy(1)='alt_dummy'
    call ps_species_list_get(ss%iobj,'PS_SPECIES_'//trim(listname),0,1,isize, &
         w%list_elems, zqa, zqv, w%awght, w%spectype, 1, altdummy, &
         iout,ierr)

    do i=1,isize
       if(zqv(i).lt.0.0d0) then
          w%qval(i) = zqv(i)/ps_xe - 0.5d0
       else
          w%qval(i) = zqv(i)/ps_xe + 0.5d0
       endif

       if(zqa(i).lt.0.0d0) then
          w%qatom(i) = zqa(i)/ps_xe - 0.5d0
       else
          w%qatom(i) = zqa(i)/ps_xe + 0.5d0
       endif

    enddo

    deallocate(zqa,zqv)

    jorder = .TRUE.
    if(present(iorder)) jorder = iorder

    if(jorder) then
       call psw_order(w) ! return list sorted in species list order
    endif

  END SUBROUTINE psw_slist
    
  SUBROUTINE psw_ilist(listname,w,ierr, state, iorder)

    ! return item list

    character*32, intent(in) :: listname  ! name of list
    type (ps_list_window) :: w            ! list returned
    integer, intent(out) :: ierr          ! status code, 0=OK


    TYPE (plasma_state), OPTIONAL, TARGET :: state
    !  operate on THIS state, if argument is present;
    !  otherwise use "ps" state.

    logical, intent(in), optional :: iorder
    !  (default = T) return list in order if T
    !    thermal electrons, thermal ions sorted by Z,
    !      various fast ions sorted by Z, fast electrons

    !-----------------------------------------------------
    ! local variables
    type (plasma_state), pointer :: ss
    integer :: iout,i,isize,id
    character*32 altdummy(1),loc_units
    character*256 label
    logical :: jorder
    !-----------------------------------------------------

    ierr = 0
    iout = SWIM_err_out_file

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    call free_window(w)

    w%list_name = trim(listname)

    call ps_listId(ss%iobj,iout,trim(listname),id,isize)

    w%size = isize
    if(isize.eq.0) return

    allocate(w%list_elems(isize))
    allocate(w%elem_units(isize))
    allocate(w%ival(isize))
    allocate(w%fval(isize))
    allocate(w%c256val(isize))

    altdummy(1)='altdummy'
    loc_units='*'
    w%list_elems(1)='*'

    call ps_list_get('State_Data',ss%iobj,trim(listname), &
         0,loc_units,label,0,altdummy,altdummy, &
         isize, w%list_elems, altdummy, w%ival, w%fval, w%c256val, &
         iout,ierr)

    w%elem_units = loc_units

    jorder = .TRUE.
    if(present(iorder)) jorder = iorder

    if(jorder) then
       call psw_order(w) ! return list sorted in item list (alphabetic) order
    endif

  END SUBROUTINE psw_ilist
    
  SUBROUTINE psw_dlist(data_name,w,xlabel,xunits,ierr, state)

    ! return state data in a list

    character*32, intent(in) :: data_name ! name of data item
    type (ps_list_window) :: w            ! list returned
    character*(*), intent(out) :: xlabel  ! list label returned
    character*(*), intent(out) :: xunits  ! list units returned
    integer, intent(out) :: ierr          ! status code, 0=OK


    TYPE (plasma_state), OPTIONAL, TARGET :: state
    !  operate on THIS state, if argument is present;
    !  otherwise use "ps" state.

    !-----------------------------------------------------
    ! local variables
    type (plasma_state), pointer :: ss
    integer :: iout,i,isize,id,ilenl,idim
    character*32 altdummy(1),loc_units
    character*256 label
    !-----------------------------------------------------

    ierr = 0
    iout = SWIM_err_out_file

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    call free_window(w)

    w%list_name = trim(data_name)

    call ps_listId(ss%iobj,iout,trim(data_name),id,isize)

    w%size = isize
    if(isize.eq.0) return

    allocate(w%list_elems(isize))
    allocate(w%elem_units(isize))
    allocate(w%ival(isize))
    allocate(w%fval(isize))
    allocate(w%c256val(isize))

    altdummy(1)='altdummy'
    loc_units='*'
    w%list_elems(1)='*'

    call ps_list_get('State_Data',ss%iobj,trim(data_name), &
         0,loc_units,label,0,altdummy,altdummy, &
         isize, w%list_elems, altdummy, w%ival, w%fval, w%c256val, &
         iout,ierr)

    w%elem_units = loc_units

    ilenl=min(len(label),len(xlabel))
    xlabel = label(1:ilenl)

    ilenl=min(len(xunits),len(loc_units))
    xunits = loc_units(1:ilenl)

    if(w%c256val(1)(1:16).eq.'__enumeration__ ') then
       allocate(w%vecsizes(isize))
       idim=maxval(w%ival(1:isize))
       allocate(w%fvec(idim,isize))

       if(isize.eq.1) then
          call ps_enum_1d_get(ss%iobj,trim(data_name)//'__Rvec',0, &
               idim, w%vecsizes(1),w%fvec(1:idim,1), iout,ierr)
       else
          call ps_enum_2d_get(ss%iobj,trim(data_name)//'__Rvec',0, &
               isize, w%vecsizes, idim,w%fvec, iout,ierr)
       endif

    else if(w%c256val(1)(1:16).eq.'__inumeration__ ') then
       allocate(w%vecsizes(isize))
       idim=maxval(w%ival(1:isize))
       allocate(w%ivec(idim,isize))

       if(isize.eq.1) then
          call ps_inum_1d_get(ss%iobj,trim(data_name)//'__Ivec',0, &
               idim, w%vecsizes(1),w%ivec(1:idim,1), iout,ierr)
       else
          call ps_inum_2d_get(ss%iobj,trim(data_name)//'__Ivec',0, &
               isize, w%vecsizes, idim,w%ivec, iout,ierr)
       endif

    endif

  END SUBROUTINE psw_dlist
    
  SUBROUTINE psw_plist(prof_name,w,xlabel,xunits,ierr, state)

    ! return state profile information in a list

    character*32, intent(in) :: prof_name ! name of data item
    type (ps_list_window) :: w            ! list returned
    character*(*), intent(out) :: xlabel  ! list label returned
    character*(*), intent(out) :: xunits  ! list units returned
    integer, intent(out) :: ierr          ! status code, 0=OK

    TYPE (plasma_state), OPTIONAL, TARGET :: state
    !  operate on THIS state, if argument is present;
    !  otherwise use "ps" state.

    !-----------------------------------------------------
    ! local variables
    type (plasma_state), pointer :: ss
    integer :: iout,i,isize,id,ilenl
    character*32 altdummy(1),loc_units,list_name
    character*256 label
    !-----------------------------------------------------

    ierr = 0
    iout = SWIM_err_out_file
    xlabel = ' '
    xunits = ' '

    if(present(state)) then
       call chk_ini(state); ss => state
    else
       ss => ps
    endif

    call free_window(w)

    w%list_name = trim(prof_name)

    list_name = trim(prof_name)//'_LIST'
    call ps_listId(ss%iobj,iout,trim(list_name),id,isize)
    if(id.eq.0) then
       list_name = prof_name
       call ps_listId(ss%iobj,iout,trim(list_name),id,isize)
    endif

    w%size = isize
    if(isize.eq.0) return

    allocate(w%list_elems(isize))
    allocate(w%elem_units(isize))
    allocate(w%ival(isize))
    allocate(w%fval(isize))
    allocate(w%c256val(isize))

    altdummy(1)='altdummy'
    loc_units='*'
    w%list_elems(1)='*'

    call ps_list_get('State_Profiles',ss%iobj,trim(list_name), &
         0,loc_units,label,0,altdummy,altdummy, &
         isize, w%list_elems, altdummy, w%ival, w%fval, w%c256val, &
         iout,ierr)

    ilenl=min(len(label),len(xlabel))
    xlabel = label(1:ilenl)

    ilenl=min(len(xunits),len(loc_units))
    xunits = loc_units(1:ilenl)

    w%elem_units = loc_units

  END SUBROUTINE psw_plist

  SUBROUTINE psw_merge(ww,wmerge,ierr, addr_table)

    ! merge elements from an array of list windows into a single list
    ! window, squeezing out duplicates.  Optionally return addr_table
    ! which will have the properties:
    !    addr_table(i,j)=0 if wmerge list element j is absent from ww(i)
    !    addr_table(i,j)=k if wmerge list element j is the k'th list element
    !                      in ww(i).
    ! the first dimension of addr_table should match the dimension of ww.
    ! the 2nd dimension should match or exceed the wmerge list size.

    ! NOTE: if dealing with species lists, atomic charges and weights
    !       are copied; otherwise, only the element names are copied 
    !       into wmerge.

    type (ps_list_window), dimension(:) :: ww  ! lists to merge;
    type (ps_list_window) :: wmerge            ! merged list returned
    integer, intent(out) :: ierr               ! status code, 0=OK

    integer, dimension(:,:), optional :: addr_table

    !------------------------------------------------------
    integer :: inww,ina1,ina2,ii,jj,kk,icur,ickk,ialg,idir,iout
    integer, dimension(:), allocatable :: isize_ww,icur_ww,iijj
    integer :: isize_tot,icount,imatch
    character*32, dimension(:), allocatable :: mlist
    !------------------------------------------------------

    ierr=0
    iout = SWIM_err_out_file

    call free_window(wmerge)

    inww = size(ww)
    if(inww.eq.0) then
       if(present(addr_table)) addr_table = 0
       return
    endif

    ialg = 1         ! retain order of input lists

    if(associated(ww(1)%spectype)) then
       if(associated(ww(1)%qval).and.associated(ww(1)%qatom).and. &
            associated(ww(1)%awght)) then

          ialg = 2   ! specie list sort
       endif
    endif

    if(present(addr_table)) then
       !  check size of table
       addr_table = 0
       ina1 = size(addr_table,1)
       ina2 = size(addr_table,2)
       call schk('psw_merge','size(ww)',inww, &
            'size(addr_table,1)',ina1,ierr)
       if(ierr.ne.0) return
    endif

    wmerge%list_name = ww(1)%list_name

    allocate(isize_ww(inww),icur_ww(inww))

    isize_tot = 0

    do ii=1,inww
       isize_ww(ii) = ww(ii)%size
       isize_tot = isize_tot + isize_ww(ii)
       icur_ww(ii) = 1
    enddo

    allocate(mlist(isize_tot),iijj(isize_tot))

    if(ialg.eq.1) then
       do
          ! add first list (uniqueness of names is assumed within each
          ! individual input list)
          icur=isize_ww(1)
          do jj=1,icur
             if(present(addr_table)) then
                if(jj.gt.ina2) then
                   write(iout,*) ' ?psw_merge: size of 2nd dimension of optional"addr_table" array too small.'
                   write(iout,*) '  size is: ',ina2,'; sum of sizes of input lists: ',isize_tot
                   ierr=1
                   exit
                endif
                addr_table(1,jj)=jj
             endif
             mlist(jj)=ww(1)%list_elems(jj)
          enddo
          if(ierr.ne.0) exit

          ! add remaining lists; check for duplicate names
          ! caution: N**2 algorithm; short list lengths assumed.

          do ii=2,inww
             do jj=1,isize_ww(ii)
                imatch=0
                do kk=1,icur
                   if(ww(ii)%list_elems(jj).eq.mlist(kk)) then
                      imatch=kk
                      exit
                   endif
                enddo

                if(imatch.eq.0) then
                   icur=icur+1
                   if(present(addr_table)) then
                      if(icur.gt.ina2) then
                         write(iout,*) ' ?psw_merge: size of 2nd dimension of optional"addr_table" array too small.'
                         write(iout,*) '  size is: ',ina2,'; sum of sizes of input lists: ',isize_tot
                         ierr=1
                         exit
                      endif
                      addr_table(ii,icur)=jj
                   endif
                   mlist(icur)=ww(ii)%list_elems(jj)
                else
                   if(present(addr_table)) then
                      addr_table(ii,imatch)=jj
                   endif
                endif
             enddo
             if(ierr.ne.0) exit
          enddo
          if(ierr.ne.0) exit

          ! OK...
          
          wmerge%size = icur
          allocate(wmerge%list_elems(icur))
          wmerge%list_elems = mlist(1:icur)

          exit
       enddo
             
    else
       ! ialg=2
       ! form species list ordered merge

       jj=0
       do
          kk=0
          icount=0
          do ii=1,inww
             !  find 1st order element in current row...
             if(icur_ww(ii).gt.isize_ww(ii)) cycle
             icount = icount + 1  ! row is not empty

             if(kk.eq.0) then
                kk=ii
                ickk=icur_ww(kk)
             endif
             if(ii.ne.kk) then
                icur=icur_ww(ii)
                call psw_ordersub2(ww(ii),ww(kk),ialg,icur,ickk,idir)
                if(idir.eq.1) then
                   kk=ii
                   ickk=icur
                endif
             endif
          enddo

          if(icount.eq.0) exit !  all elements have been scanned...

          !  add element

          jj=jj+1
          mlist(jj) = ww(kk)%list_elems(ickk)
          iijj(jj) = kk

          !  increment counters over duplicate elements

          do ii=1,inww
             icur=icur_ww(ii)
             if(icur.gt.isize_ww(ii)) cycle
             if(ww(ii)%list_elems(icur).eq.mlist(jj)) then
                if(present(addr_table)) then
                   addr_table(ii,jj)=icur
                endif
                icur_ww(ii)=icur_ww(ii)+1
             endif
          enddo
       enddo

       if(present(addr_table)) then
          if(jj.gt.ina2) then
             ierr=1
             write(iout,*) &
                  ' ?psw_merge: 2nd dimension too small in "addr_table".'
             write(iout,*) '  received: ',ina2,'; needed: ',jj
          endif
       endif

       ! OK...

       wmerge%size = jj
       allocate(wmerge%list_elems(jj))
       allocate(wmerge%spectype(jj))
       allocate(wmerge%qval(jj))
       allocate(wmerge%qatom(jj))
       allocate(wmerge%awght(jj))

       isize_tot=jj
       do jj=1,isize_tot
          wmerge%list_elems(jj)=mlist(jj)

          ii=iijj(jj)
          do kk=1,isize_ww(ii)
             if(wmerge%list_elems(jj).eq.ww(ii)%list_elems(kk)) then
                wmerge%spectype(jj)=ww(ii)%spectype(kk)
                wmerge%qval(jj)=ww(ii)%qval(kk)
                wmerge%qatom(jj)=ww(ii)%qatom(kk)
                wmerge%awght(jj)=ww(ii)%awght(kk)
             endif
          enddo
       enddo
    endif

    deallocate(icur_ww,isize_ww,mlist,iijj)

  END SUBROUTINE psw_merge

  !-------------------------------------------
  ! private...

  SUBROUTINE schk(sublbl,slab1,isiz1,slab2,isiz2,ierr)

    ! **private**
    ! compare vector size -- match required

    character*(*), intent(in) :: sublbl  ! subroutine label (for message)
    character*(*), intent(in) :: slab1   ! label for 1st vector size
    integer, intent(in) :: isiz1         ! 1st vector size
    character*(*), intent(in) :: slab2   ! label for 2nd vector size
    integer, intent(in) :: isiz2         ! 2nd vector size

    integer, intent(out) :: ierr         ! status code returned, 0=OK

    !----------------------
    integer :: iout
    !----------------------

    ierr = 0
    if(isiz1.eq.isiz2) return

    iout=SWIM_err_out_file
    ierr=999
    write(iout,*) ' ?',trim(sublbl),': vector size mismatch in passed arguments:'
    write(iout,*) '  ',trim(slab1),': ',isiz1, &
         '; ',trim(slab2),': ',isiz2

  END SUBROUTINE schk

  SUBROUTINE psw_order(w)

    ! **private**
    ! re-order elements of w according to rules
    !   species lists:  spectype,qatom,awght,qval
    !   all others:  alphabetic by element name

    type (ps_list_window) :: w

    !----------------------------------
    integer :: iout, idir, inumo, ie, ia, ia1, ia2, ii, ialg
    integer, dimension(:), allocatable :: iorder
    type (ps_list_window) :: wtmp
    character*32 ztmp
    !----------------------------------

    if(w%size.eq.0) return

    iout=SWIM_err_out_file

    if(.not.associated(w%list_elems)) then
       write(iout,*) &
            ' ?plasma_state_mod internal error: psw_order, w not initialized.'
       return
    endif

    ! assure w%list_elem names are all uppercase
    do ie=1,w%size
       ztmp = w%list_elems(ie)
       call uupper(ztmp)
       w%list_elems(ie) = ztmp
    enddo

    ialg = 1         ! assume alphabetic sort initially

    if(associated(w%spectype)) then
       if(associated(w%qval).and.associated(w%qatom).and. &
            associated(w%awght)) then

          ialg = 2   ! specie list sort
       endif
    endif

    call clone_window(w,wtmp)

    allocate(iorder(w%size))

    inumo=1
    iorder(1)=1

    ie=1
    do
       if(inumo.eq.w%size) exit

       ie = ie + 1
       ia = 1
       call psw_ordersub(w,ialg,iorder,1,ie,idir)

       if(idir.gt.0) then
          ia1 = ia
          ia = inumo + 1
          call psw_ordersub(w,ialg,iorder,inumo,ie,idir)

          if(idir.lt.0) then
             ia2=inumo

             !  order requires insertion after (ia1) but before (ia2)
             !  binary search

             do
                if(ia2.eq.ia1+1) then
                   ia = ia2
                   exit
                endif
                ia = max((ia1+1),(ia1+ia2)/2)
                call psw_ordersub(w,ialg,iorder,ia,ie,idir)
                if(idir.gt.0) then
                   ia1=ia
                else
                   ia2=ia
                endif
             enddo

          endif
       endif

       ! insert in iorder
       if(ia.le.inumo) then
          do ii=inumo,ia,-1
             iorder(ii+1)=iorder(ii)
          enddo
       endif
       iorder(ia)=ie
       inumo = inumo + 1

    enddo

    do ie=1,inumo
       ia=iorder(ie)

       if(associated(w%list_elems)) w%list_elems(ie) = wtmp%list_elems(ia)
       if(associated(w%elem_descr)) w%elem_descr(ie) = wtmp%elem_descr(ia)
       if(associated(w%component)) w%component(ie) = wtmp%component(ia)
       if(associated(w%elem_units)) w%elem_units(ie) = wtmp%elem_units(ia)
       if(associated(w%val_field)) w%val_field(ie) = wtmp%val_field(ia)
       if(associated(w%c256val)) w%c256val(ie) = wtmp%c256val(ia)
       if(associated(w%ival)) w%ival(ie) = wtmp%ival(ia)
       if(associated(w%fval)) w%fval(ie) = wtmp%fval(ia)
       if(associated(w%vecsizes)) w%vecsizes(ie) = wtmp%vecsizes(ia)
       if(associated(w%ivec)) w%ivec(:,ie) = wtmp%ivec(:,ia)
       if(associated(w%fvec)) w%fvec(:,ie) = wtmp%fvec(:,ia)
       if(associated(w%spectype)) w%spectype(ie) = wtmp%spectype(ia)
       if(associated(w%qval)) w%qval(ie) = wtmp%qval(ia)
       if(associated(w%qatom)) w%qatom(ie) = wtmp%qatom(ia)
       if(associated(w%awght)) w%awght(ie) = wtmp%awght(ia)
       
    enddo

    ! cleanup...

    call free_window(wtmp)
    deallocate(iorder)

  END SUBROUTINE psw_order

  SUBROUTINE psw_ordersub(w,ialg,iorder,ia,ie,idir)

    !  **private**

    !  order subroutine: if w(ie) > w(iorder(ia)) return idir = 1
    !                    if w(ie) < w(iorder(ia)) return idir = -1
    !                    if w(ie) = w(iorder(ia)) return idir = 0

    !  meaning of <,>,= depends on ialg

    type (ps_list_window) :: w
    integer :: ialg,ia,ie,idir
    integer, dimension(:) :: iorder

    !-----------------------------
    ! local:

    integer :: ii

    !-----------------------------

    ii = iorder(ia)

    !  assume equality if names are equal (regardless of ialg)

    if(w%list_elems(ii).eq.w%list_elems(ie)) then
       idir = 0
       return
    endif

    if(ialg.eq.2) then
       ! order by spectype...
       if(w%spectype(ie).gt.w%spectype(ii)) then
          idir = 1
       else if(w%spectype(ie).lt.w%spectype(ii)) then
          idir = -1

       else
          ! order by qatom (atomic number)
          if(w%qatom(ie).gt.w%qatom(ii)) then
             idir = 1
          else if(w%qatom(ie).lt.w%qatom(ii)) then
             idir = -1

          else
             ! order by awght (atomic weight)
             if(w%awght(ie).gt.w%awght(ii)) then
                idir = 1
             else if(w%awght(ie).lt.w%awght(ii)) then
                idir = -1

             else
                ! (final test) order by qval (ionic charge)
                if(w%awght(ie).gt.w%awght(ii)) then
                   idir = 1
                else
                   idir = -1
                endif
             endif
          endif
       endif

    else
       ! just use alphabetic ordering

       if(w%list_elems(ie).gt.w%list_elems(ii)) then
          idir = 1
       else
          idir = -1
       endif

    endif

  END SUBROUTINE psw_ordersub

  SUBROUTINE psw_ordersub2(w1,w2,ialg,i1,i2,idir)

    !  **private**

    !  order subroutine: if w2(i2) > w1(i1) return idir = 1
    !                    if w2(i2) < w1(i1) return idir = -1
    !                    if w2(i2) = w1(i1) return idir = 0

    !  meaning of <,>,= depends on ialg

    type (ps_list_window) :: w1,w2
    integer :: ialg,i1,i2,idir

    !-----------------------------
    !-----------------------------

    !  assume equality if names are equal (regardless of ialg)

    if(w1%list_elems(i1).eq.w2%list_elems(i2)) then
       idir = 0
       return
    endif

    if(ialg.eq.2) then
       ! order by spectype...
       if(w2%spectype(i2).gt.w1%spectype(i1)) then
          idir = 1
       else if(w2%spectype(i2).lt.w1%spectype(i1)) then
          idir = -1

       else
          ! order by qatom (atomic number)
          if(w2%qatom(i2).gt.w1%qatom(i1)) then
             idir = 1
          else if(w2%qatom(i2).lt.w1%qatom(i1)) then
             idir = -1

          else
             ! order by awght (atomic weight)
             if(w2%awght(i2).gt.w1%awght(i1)) then
                idir = 1
             else if(w2%awght(i2).lt.w1%awght(i1)) then
                idir = -1

             else
                ! (final test) order by qval (ionic charge)
                if(w2%awght(i2).gt.w1%awght(i1)) then
                   idir = 1
                else
                   idir = -1
                endif
             endif
          endif
       endif

    else
       ! just use alphabetic ordering

       if(w2%list_elems(i2).gt.w1%list_elems(i1)) then
          idir = 1
       else
          idir = -1
       endif

    endif

  END SUBROUTINE psw_ordersub2

END MODULE plasma_state_mod
