!>@file   cal_momentum_eq_explicit.f90
!!@brief  module cal_momentum_eq_explicit
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2010
!
!>@brief Time integration for momentum equation by explicit scheme
!!
!!@verbatim
!!      subroutine sel_explicit_sph(i_step, dt, MHD_prop, sph_MHD_bc,   &
!!     &                            sph, ipol, rj_fld)
!!        type(sph_grids), intent(in) ::  sph
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!!
!!@param i_step  time step
!
      module cal_momentum_eq_explicit
!
      use m_precision
!
      use t_control_parameter
      use t_physical_property
      use t_spheric_parameter
!
      use t_phys_address
      use t_phys_data
      use t_fdm_coefs
      use t_schmidt_poly_on_rtm
      use t_boundary_data_sph_MHD
      use t_boundary_params_sph_MHD
!
      implicit  none
!
      private :: cal_explicit_sph_euler, cal_first_prev_step_adams
      private :: cal_explicit_sph_adams
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine sel_explicit_sph(i_step, dt, MHD_prop, sph_MHD_bc,     &
     &                            sph, ipol, rj_fld)
!
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: dt
!
      type(sph_grids), intent(in) ::  sph
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(inout) :: rj_fld
!
!
      if(MHD_prop%iflag_all_scheme .eq. id_explicit_euler) then
        call cal_explicit_sph_euler                                     &
     &     (dt, sph, MHD_prop, sph_MHD_bc, ipol, rj_fld)
      else if(i_step .eq. 1) then
        if(iflag_debug.gt.0) write(*,*) 'cal_explicit_sph_euler'
        call cal_explicit_sph_euler                                     &
     &     (dt, sph, MHD_prop, sph_MHD_bc, ipol, rj_fld)
        call cal_first_prev_step_adams                                  &
     &     (sph, MHD_prop, sph_MHD_bc, ipol, rj_fld)
      else
        if(iflag_debug.gt.0) write(*,*) 'cal_explicit_sph_adams'
        call cal_explicit_sph_adams                                     &
     &     (dt, sph, MHD_prop, sph_MHD_bc, ipol, rj_fld)
      end if
!
      end subroutine sel_explicit_sph
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine cal_explicit_sph_adams                                 &
     &         (dt, sph, MHD_prop, sph_MHD_bc, ipol, rj_fld)
!
      use cal_vorticity_terms_adams
      use cal_nonlinear_sph_MHD
      use cal_explicit_terms
      use explicit_scalars_sph
!
      real(kind = kreal), intent(in) :: dt
!
      type(sph_grids), intent(in) ::  sph
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(inout) :: rj_fld
!
!
      call cal_vorticity_eq_adams                                       &
     &   (sph%sph_rj, MHD_prop%fl_prop, sph_MHD_bc%sph_bc_U,            &
     &    ipol%base, ipol%exp_work, ipol%diffusion,                     &
     &    dt, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      if(MHD_prop%cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &              'cal_diff_induction_MHD_adams'
        call cal_diff_induction_MHD_adams(MHD_prop%cd_prop,             &
     &      ipol%base, ipol%exp_work, ipol%forces, ipol%diffusion,      &
     &      dt, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      call explicit_scalars_sph_adams(dt, sph%sph_params, sph%sph_rj,   &
     &    MHD_prop%ht_prop, MHD_prop%cp_prop,                           &
     &    sph_MHD_bc%sph_bc_T, sph_MHD_bc%sph_bc_C,                     &
     &    ipol%base, ipol%exp_work, ipol%forces, ipol%diffusion,        &
     &    rj_fld)
!
      end subroutine cal_explicit_sph_adams
!
! ----------------------------------------------------------------------
!
      subroutine cal_explicit_sph_euler                                 &
     &         (dt, sph, MHD_prop, sph_MHD_bc, ipol, rj_fld)
!
      use cal_vorticity_terms_adams
      use cal_explicit_terms
      use explicit_scalars_sph
!
      real(kind = kreal), intent(in) :: dt
!
      type(sph_grids), intent(in) ::  sph
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(inout) :: rj_fld
!
!
      call cal_vorticity_eq_euler                                       &
     &   (sph%sph_rj, MHD_prop%fl_prop, sph_MHD_bc%sph_bc_U,            &
     &    ipol%base, ipol%exp_work, ipol%diffusion,                     &
     &    dt, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      if(MHD_prop%cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &                'cal_diff_induction_MHD_euler'
        call cal_diff_induction_MHD_euler(MHD_prop%cd_prop,             &
     &      ipol%base, ipol%forces, ipol%diffusion,                     &
     &      dt, rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      call explicit_scalars_sph_euler                                   &
     &   (dt, sph%sph_rj, MHD_prop%ht_prop, MHD_prop%cp_prop,           &
     &    sph_MHD_bc%sph_bc_T, sph_MHD_bc%sph_bc_C,                     &
     &    ipol%base, ipol%forces, ipol%diffusion, rj_fld)
!
      end subroutine cal_explicit_sph_euler
!
! ----------------------------------------------------------------------
!
      subroutine cal_first_prev_step_adams                              &
     &         (sph, MHD_prop, sph_MHD_bc, ipol, rj_fld)
!
      use cal_vorticity_terms_adams
      use cal_explicit_terms
      use explicit_scalars_sph
!
      type(sph_grids), intent(in) ::  sph
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(inout) :: rj_fld
!
!
      call set_ini_adams_inertia(MHD_prop%fl_prop, ipol%exp_work,       &
     &    rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
!
      if(MHD_prop%cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
        if(iflag_debug .gt. 0) write(*,*)                               &
     &              'set_ini_adams_mag_induct'
        call set_ini_adams_mag_induct(ipol%exp_work, ipol%forces,       &
     &      rj_fld%n_point, rj_fld%ntot_phys, rj_fld%d_fld)
      end if
!
      call first_scalars_prev_step_adams                                &
     &   (sph%sph_rj, MHD_prop%ht_prop, MHD_prop%cp_prop,               &
     &    sph_MHD_bc%sph_bc_T, sph_MHD_bc%sph_bc_C,                     &
     &    ipol%base, ipol%exp_work, ipol%forces, rj_fld)
!
      end subroutine cal_first_prev_step_adams
!
! ----------------------------------------------------------------------
!
      end module cal_momentum_eq_explicit
