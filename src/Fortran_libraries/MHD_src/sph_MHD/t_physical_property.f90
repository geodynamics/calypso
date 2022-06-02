!>@file   t_physical_property.f90
!!@brief  module t_physical_property
!!
!!@author H. Matsui
!!@date Programmed in 2001
!!@date Modified in Jan., 2007
!
!>@brief  Coeffiecients of each term
!!
!!@verbatim
!!      subroutine alloc_force_list(num, fl_prop)
!!      subroutine dealloc_force_list(fl_prop)
!!        type(fluid_property), intent(inout) :: fl_prop
!!      subroutine set_reference_scalar_ctl(ref_ctl, scl_prop)
!!        type(reference_temperature_ctl), intent(in) :: ref_ctl
!!        type(scalar_property), intent(inout) :: scl_prop
!!@endverbatim
!
      module t_physical_property
!
      use m_precision
      use m_constants
!
      implicit  none
!
!
!>      Scheme ID for no evolution
      integer (kind=kint), parameter :: id_no_evolution =     0
!>      Scheme ID for explicit Euler scheme
      integer (kind=kint), parameter :: id_explicit_euler =   1
!>      Scheme ID for 2nd order Adams-Bashforth Scheme
      integer (kind=kint), parameter :: id_explicit_adams2 =  2
!>      Scheme ID for Crank-Nicolson Scheme
      integer (kind=kint), parameter :: id_Crank_nicolson =   3
!>      Scheme ID for Crank-Nicolson Scheme with consistent mass matrix
      integer (kind=kint), parameter :: id_Crank_nicolson_cmass = 4
!
!
!>     flag for no gravity
      integer (kind = kint), parameter :: iflag_no_gravity =  -1
!>     flag for constant gravity
      integer (kind = kint), parameter :: iflag_const_g =      0
!>     flag for radial gravity (amplitude is fixed)
      integer (kind = kint), parameter :: iflag_radial_g =     1
!>     flag for self radial gravity
      integer (kind = kint), parameter :: iflag_self_r_g =     2
!
!
!>      Turn ON and evaluate over elements flag
      integer (kind=kint), parameter :: id_FORCE_ele_int =  1
!>      Turn ON and evaluate at node flag
      integer (kind=kint), parameter :: id_FORCE_at_node =  2
!
!   Coefficients
!
!>      Structure for fluid property
      type fluid_property
!>        Time evolution flag for velocity
        integer (kind=kint) :: iflag_scheme = id_no_evolution
!>        Coefficient of implicit term
        real(kind = kreal) :: coef_imp = half
!>        Coefficient of explicit term
        real(kind = kreal) :: coef_exp = half
!
!>        coefficient for time evolution of velocity and advection
        real  (kind=kreal) :: coef_velo
!>       coefficient for advection (-coef_velo)
        real  (kind=kreal) :: coef_nega_v
!>       coefficient for time pressure gradient
        real  (kind=kreal) :: coef_press
!>       1 / coef_press
        real  (kind=kreal) :: acoef_press
!
!>       coefficient for viscous diffusion
        real  (kind=kreal) :: coef_diffuse
!
!>        Force flag for Filtered inertia term
        logical :: iflag_4_inertia = .TRUE.
!>        Force flag for Coriolis force
        logical :: iflag_4_coriolis = .FALSE.
!>        Force flag for Lorentz force
        logical :: iflag_4_lorentz = .FALSE.
!>        Force flag for thermal buoyancy
        logical :: iflag_4_gravity = .FALSE.
!>        Force flag for compositional buoyancy
        logical :: iflag_4_composit_buo = .FALSE.
!
!>        Force flag for Filtered inertia term
        logical :: iflag_4_filter_inertia = .FALSE.
!>        Force flag for Filtered Lorentz force
        logical :: iflag_4_filter_lorentz = .FALSE.
!>        Force flag for filtered thermal buoyancy
        logical :: iflag_4_filter_gravity =  .FALSE.
!>        Force flag for filtered compositional buoyancy
        logical :: iflag_4_filter_comp_buo = .FALSE.
!
!>       coefficient for Coriolis force
        real  (kind=kreal) :: coef_cor = zero
!>       coefficient for chemical Lorentz force
        real  (kind=kreal) :: coef_lor = zero
!>       coefficient for thermal buoyancy
        real  (kind=kreal) :: coef_buo = zero
!>       coefficient for chemical buoyancy
        real  (kind=kreal) :: coef_comp_buo = zero
!
!>       flag for gravity type
        integer (kind=kint) :: i_grav
!>       gravity direction for constant gravity
        real (kind=kreal) :: grav(3) = (/zero, zero, -one/)
!>       rotation vector for Coriolis force
        real (kind=kreal) :: sys_rot(3) = (/zero, zero, one/)
!
!>        Buoyancy model for FEM
        integer(kind = kint) :: iflag_FEM_gravity = id_FORCE_ele_int
!>        Coriolis force model for FEM
        integer(kind = kint) :: iflag_FEM_coriolis = id_FORCE_ele_int
!>        Coriolis force model for FEM
        logical :: iflag_coriolis_implicit = .FALSE.
!
!>        Number of forces
        integer(kind = kint) :: num_force
!>        Name of forces
        character(len=kchara), allocatable :: name_force(:)
      end type fluid_property
!
!>      Structure for manetic property
      type conductive_property
!>        Time evolution flag for magnetic field
        integer (kind=kint) :: iflag_Bevo_scheme = id_no_evolution
!>        Time evolution flag for vector potential
        integer (kind=kint) :: iflag_Aevo_scheme = id_no_evolution
!>        Coefficient of implicit term
        real(kind = kreal) :: coef_imp = half
!>        Coefficient of explicit term
        real(kind = kreal) :: coef_exp = half
!
!>       coefficient for time evolution of magnetic field
        real  (kind=kreal) :: coef_magne
!>       coefficient for time electric potentia
        real  (kind=kreal) :: coef_mag_p
!>       1 / coef_mag_p
        real  (kind=kreal) :: acoef_mag_p
!
!>       coefficient for magnetic diffusion
        real  (kind=kreal) :: coef_diffuse
!>       coefficient for magnetic induction
        real  (kind=kreal) :: coef_induct
!
!>       Ratio of magnetic energy to kinetic energy scale
        real  (kind=kreal) :: me_to_ke_ratio
!
!>        Magneto convectio flag
        integer (kind=kint) :: iflag_magneto_cv = id_turn_OFF
!>       external magnetic field (Constant)
        real (kind=kreal) :: ex_magne(3) = (/zero, zero, zero/)
!
!>        Force flag for induction
        logical :: iflag_4_induction = .FALSE.
!>        Force flag for Filtered induction
        logical :: iflag_4_filter_induction = .FALSE.
      end type conductive_property
!
!>      Structure for thermal property
      type scalar_property
!>        Time evolution flag for velocity
        integer (kind=kint) :: iflag_scheme = id_no_evolution
!>        Coefficient of implicit term
        real(kind = kreal) :: coef_imp = half
!>        Coefficient of explicit term
        real(kind = kreal) :: coef_exp = half
!
!>       coefficient for time evolution of temperature and heat flux
        real(kind = kreal) :: coef_advect
!>       coefficient for heat flux (-coef_advect)
        real(kind = kreal) :: coef_nega_adv
!
!>       coefficient for thermal diffusion
        real(kind = kreal) :: coef_diffuse
!>       coefficient for heat source term
        real(kind = kreal) :: coef_source = zero
!
!>       coefficient for diffusion reduction for ICB
        real(kind = kreal) :: diffusie_reduction_ICB = one
!
!>        Force flag for advection
        logical :: iflag_4_advection = .FALSE.
!>        Force flag for Filtered advection
        logical :: iflag_4_filter_advection = .FALSE.
      end type scalar_property
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_force_list(fl_prop)
!
      type(fluid_property), intent(inout) :: fl_prop
!
!
      allocate(fl_prop%name_force(fl_prop%num_force))
!
      end subroutine alloc_force_list
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_force_list(fl_prop)
!
      type(fluid_property), intent(inout) :: fl_prop
!
!
      deallocate(fl_prop%name_force)
!
      end subroutine dealloc_force_list
!
!  ---------------------------------------------------------------------
!
      subroutine set_filtered_advection_ctl                             &
     &         (filterd_advect_ctl, scl_prop)
!
      use t_control_array_character
      use skip_comment_f
!
      type(read_character_item), intent(in) :: filterd_advect_ctl
      type(scalar_property), intent(inout) :: scl_prop
!
!
      if(scl_prop%iflag_scheme .eq. id_no_evolution) return
      scl_prop%iflag_4_advection = .TRUE.
!
      if(filterd_advect_ctl%iflag .gt. 0                                &
     &   .and. yes_flag(filterd_advect_ctl%charavalue)) then
        scl_prop%iflag_4_advection = .FALSE.
        scl_prop%iflag_4_filter_advection = .TRUE.
      end if
!
      end subroutine set_filtered_advection_ctl
!
! -----------------------------------------------------------------------
!
      subroutine set_filtered_induction_ctl                             &
     &         (filterd_induction_ctl, cd_prop)
!
      use t_control_array_character
      use skip_comment_f
!
      type(read_character_item), intent(in) :: filterd_induction_ctl
      type(conductive_property), intent(inout) :: cd_prop
!
!
      if((cd_prop%iflag_Bevo_scheme .eq. id_no_evolution)               &
     &   .and. (cd_prop%iflag_Aevo_scheme .eq. id_no_evolution)) return
      cd_prop%iflag_4_induction = .TRUE.
!
      if(filterd_induction_ctl%iflag .gt. 0                             &
     &   .and. yes_flag(filterd_induction_ctl%charavalue)) then
        cd_prop%iflag_4_induction = .FALSE.
        cd_prop%iflag_4_filter_induction = .TRUE.
      end if
!
      end subroutine set_filtered_induction_ctl
!
! -----------------------------------------------------------------------
!
      end module t_physical_property
