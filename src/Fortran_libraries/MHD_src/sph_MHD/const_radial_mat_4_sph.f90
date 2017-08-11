!>@file   const_radial_mat_4_sph.f90
!!@brief  module const_radial_mat_4_sph
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Sep., 2009
!
!>@brief Construct 1D matrices for MHD dynamo simulaiton
!!
!!@verbatim
!!      subroutine const_radial_mat_sph_mhd(dt, MHD_prop, sph_MHD_bc,   &
!!     &          sph_rj, r_2nd, leg, sph_MHD_mat)
!!      subroutine const_radial_mat_sph_snap(MHD_prop, sph_MHD_bc,      &
!!     &          sph_rj, r_2nd, leg, sph_MHD_mat)
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(fdm_matrices), intent(in) :: r_2nd
!!        type(legendre_4_sph_trans), intent(in) :: leg
!!        type(MHD_radial_matrices), intent(inout) :: sph_MHD_mat
!!@endverbatim
!
      module const_radial_mat_4_sph
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      use t_control_parameter
      use t_physical_property
      use t_spheric_rj_data
      use t_fdm_coefs
      use t_schmidt_poly_on_rtm
      use t_physical_property
      use t_boundary_data_sph_MHD
      use t_boundary_params_sph_MHD
      use t_radial_matrices_sph_MHD
      use t_sph_matrices
      use t_sph_matrix
!
      use calypso_mpi
!
      implicit none
!
      private :: const_radial_matrices_sph
      private :: const_radial_mat_sph_w_center
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_radial_mat_sph_mhd(dt, MHD_prop, sph_MHD_bc,     &
     &          sph_rj, r_2nd, leg, sph_MHD_mat)
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(legendre_4_sph_trans), intent(in) :: leg
!
      real(kind = kreal), intent(in) :: dt
!
      type(MHD_radial_matrices), intent(inout) :: sph_MHD_mat
!
!
      call const_radial_matrices_sph(dt, sph_rj, r_2nd,                 &
     &    MHD_prop, sph_MHD_bc, leg%g_sph_rj, sph_MHD_mat)
!
      if(sph_rj%inod_rj_center .gt. 0) then
        call const_radial_mat_sph_w_center                              &
     &     (dt, sph_rj, MHD_prop, sph_MHD_bc, sph_MHD_mat)
      end if
!
      end subroutine const_radial_mat_sph_mhd
!
! -----------------------------------------------------------------------
!
      subroutine const_radial_mat_sph_snap(MHD_prop, sph_MHD_bc,        &
     &          sph_rj, r_2nd, leg, sph_MHD_mat)
!
      use t_boundary_data_sph_MHD
      use const_r_mat_4_scalar_sph
      use const_r_mat_w_center_sph
!
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(legendre_4_sph_trans), intent(in) :: leg
!
      type(MHD_radial_matrices), intent(inout) :: sph_MHD_mat
!
      character(len=kchara) :: mat_name
!
!
      if(MHD_prop%fl_prop%iflag_scheme .lt. id_Crank_nicolson) return
      if(iflag_debug .gt. 0)                                            &
     &          write(*,*) 'const_radial_mat_4_press_sph'
      call const_radial_mat_4_press_sph                                 &
     &   (sph_rj, r_2nd, MHD_prop%fl_prop,                              &
     &    sph_MHD_bc%sph_bc_U, sph_MHD_bc%fdm2_center,                  &
     &    leg%g_sph_rj, sph_MHD_mat%band_p_poisson)
!
      if(sph_rj%inod_rj_center .eq. 0) return
!
      write(mat_name,'(a)') 'average_pressure_w_center'
      call const_radial_mat_press00_sph                                 &
     &   (mat_name, sph_rj, MHD_prop%fl_prop,                           &
     &    sph_MHD_bc%sph_bc_U, sph_MHD_bc%fdm2_center,                  &
     &    sph_MHD_mat%band_p_poisson, sph_MHD_mat%band_p00_poisson)
!
      end subroutine const_radial_mat_sph_snap
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_radial_matrices_sph(dt, sph_rj, r_2nd,           &
     &          MHD_prop, sph_MHD_bc, g_sph_rj, sph_MHD_mat)
!
      use const_r_mat_4_scalar_sph
      use const_r_mat_4_vector_sph
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(fdm_matrices), intent(in) :: r_2nd
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!
      real(kind = kreal), intent(in) :: dt
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),13)
!
      type(MHD_radial_matrices), intent(inout) :: sph_MHD_mat
!
      character(len=kchara) :: mat_name
!
!
      if(MHD_prop%fl_prop%iflag_scheme .ge. id_Crank_nicolson) then
        if(iflag_debug .gt. 0)                                          &
     &          write(*,*) 'const_radial_mat_vort_2step'
        call const_radial_mat_vort_2step                                &
     &     (dt, sph_rj, r_2nd, MHD_prop%fl_prop, sph_MHD_bc%sph_bc_U,   &
     &      sph_MHD_bc%fdm2_center, sph_MHD_bc%fdm2_free_ICB,           &
     &      sph_MHD_bc%fdm2_free_CMB, g_sph_rj,                         &
     &      sph_MHD_mat%band_vs_poisson, sph_MHD_mat%band_vp_evo,       &
     &      sph_MHD_mat%band_vt_evo, sph_MHD_mat%band_wt_evo)
        call const_radial_mat_4_press_sph                               &
     &     (sph_rj, r_2nd, MHD_prop%fl_prop,                            &
     &      sph_MHD_bc%sph_bc_U, sph_MHD_bc%fdm2_center,                &
     &      g_sph_rj, sph_MHD_mat%band_p_poisson)
      end if
!
      write(mat_name,'(a)') 'Temperature_evolution'
      call const_radial_mat_4_scalar_sph                                &
     &   (mat_name, dt, sph_rj, r_2nd, MHD_prop%ht_prop,                &
     &    sph_MHD_bc%sph_bc_T, sph_MHD_bc%fdm2_center,                  &
     &    g_sph_rj, sph_MHD_mat%band_temp_evo)
!
      call const_radial_mat_4_magne_sph                                 &
     &   (dt, sph_rj, r_2nd, MHD_prop%cd_prop,                          &
     &    sph_MHD_bc%sph_bc_B, sph_MHD_bc%fdm2_center,                  &
     &    g_sph_rj, sph_MHD_mat%band_bp_evo, sph_MHD_mat%band_bt_evo)
!
      write(mat_name,'(a)') 'Composition_evolution'
      call const_radial_mat_4_scalar_sph                                &
     &   (mat_name, dt, sph_rj, r_2nd, MHD_prop%cp_prop,                &
     &    sph_MHD_bc%sph_bc_C, sph_MHD_bc%fdm2_center,                  &
     &    g_sph_rj, sph_MHD_mat%band_comp_evo)
!
      end subroutine const_radial_matrices_sph
!
! -----------------------------------------------------------------------
!
      subroutine const_radial_mat_sph_w_center                          &
     &         (dt, sph_rj, MHD_prop, sph_MHD_bc, sph_MHD_mat)
!
      use const_r_mat_w_center_sph
!
      real(kind = kreal), intent(in) :: dt
      type(sph_rj_grid), intent(in) :: sph_rj
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!
      type(MHD_radial_matrices), intent(inout) :: sph_MHD_mat
!
      character(len=kchara) :: mat_name
!
!
      call alloc_average_w_center(sph_rj, sph_MHD_mat)
!
      write(mat_name,'(a)') 'average_pressure_w_center'
      call const_radial_mat_press00_sph                                 &
     &   (mat_name, sph_rj, MHD_prop%fl_prop,                           &
     &    sph_MHD_bc%sph_bc_U, sph_MHD_bc%fdm2_center,                  &
     &    sph_MHD_mat%band_p_poisson, sph_MHD_mat%band_p00_poisson)
!
      write(mat_name,'(a)') 'average_temperature_w_center'
      call const_radial_mat_scalar00_sph                                &
     &   (mat_name, dt, sph_rj, MHD_prop%ht_prop,                       &
     &    sph_MHD_bc%sph_bc_T, sph_MHD_bc%fdm2_center,                  &
     &    sph_MHD_mat%band_temp_evo, sph_MHD_mat%band_temp00_evo)
!
      write(mat_name,'(a)') 'average_composition_w_center'
      call const_radial_mat_scalar00_sph                                &
     &   (mat_name, dt, sph_rj, MHD_prop%cp_prop,                       &
     &    sph_MHD_bc%sph_bc_C, sph_MHD_bc%fdm2_center,                  &
     &    sph_MHD_mat%band_comp_evo, sph_MHD_mat%band_comp00_evo)
!
      end subroutine const_radial_mat_sph_w_center
!
! -----------------------------------------------------------------------
!
      end module const_radial_mat_4_sph
