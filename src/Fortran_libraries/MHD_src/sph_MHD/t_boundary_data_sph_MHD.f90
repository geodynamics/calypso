!>@file   t_boundary_data_sph_MHD.f90
!!@brief  module t_boundary_data_sph_MHD
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Structure for basic boundary conditions for spherical dynamo
!!
!!
!!@verbatim
!!      subroutine check_bc_sph_mhd(id_file, sph_rj,                    &
!!     &                            MHD_prop, sph_MHD_bc)
!!        integer(kind = kint), intent(in) :: id_file
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!@endverbatim
!!
!!@n @param jmax    number of modes for spherical harmonics @f$L*(L+2)@f$
!!@n @param nri     number of radial grid points
!!@n @param radius  radius
!
      module t_boundary_data_sph_MHD
!
      use m_precision
      use t_boundary_params_sph_MHD
      use t_boundary_sph_spectr
      use t_coef_sph_velocity_BCs
!
      use t_coef_fdm2_centre
      use t_coef_fdm4_vpol_centre
      use t_spheric_parameter
      use t_control_parameter
      use t_time_data
!
      implicit none
!
!
!>      Structure for boundary conditions
      type sph_MHD_boundary_data
!>        Structure for basic velocity boundary condition parameters
        type(sph_boundary_type) :: sph_bc_U
!>        Structure for basic magnetic boundary condition parameters
        type(sph_boundary_type) :: sph_bc_B
!>        Structure for basic thermal boundary condition parameters
        type(sph_boundary_type) :: sph_bc_T
!>        Structure for basic compositional boundary condition parameters
        type(sph_boundary_type) :: sph_bc_C
!
!>        Structure for boundary velocity field spectr
        type(sph_vector_boundary_data) :: bcs_U
!>        Structure for boundary magnetic field spectr
        type(sph_vector_boundary_data) :: bcs_B
!>        Structure for boundary temperature spectr
        type(sph_scalar_boundary_data) :: bcs_T
!>        Structure for boundary composition spectr
        type(sph_scalar_boundary_data) :: bcs_C
!
!>        Structure for Additional velocity boundary condition matrices
        type(velocity_boundary_FDMs) :: bc_fdms_U
!>        Structure for FDM matrix of center
        type(fdm2_center_mat) :: fdm2_center
      end type sph_MHD_boundary_data
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine check_bc_sph_mhd(id_file, sph_rj,                      &
     &                            MHD_prop, sph_MHD_bc)
!
      use m_base_field_labels
!
      use t_spheric_rj_data
      use t_coef_fdm1_free_rotate_ICB
      use t_coef_fdm1_free_rotate_CMB
      use set_bc_flag_sph_velo
      use set_bc_sph_scalars
!
      use set_sph_bc_magne_sph
!
      integer(kind = kint), intent(in) :: id_file
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!
!
      if(iflag_debug .gt. 1) then
        write(id_file,*) 'sph_bc_U%iflag_icb',                          &
     &        sph_MHD_bc%sph_bc_U%kr_in,  sph_MHD_bc%sph_bc_U%iflag_icb
        write(id_file,*) 'sph_bc_U%iflag_cmb',                          &
     &        sph_MHD_bc%sph_bc_U%kr_out, sph_MHD_bc%sph_bc_U%iflag_cmb
        write(id_file,*) 'sph_bc_T%iflag_icb',                          &
     &        sph_MHD_bc%sph_bc_T%kr_in,  sph_MHD_bc%sph_bc_T%iflag_icb
        write(id_file,*) 'sph_bc_T%iflag_cmb',                          &
     &        sph_MHD_bc%sph_bc_T%kr_out, sph_MHD_bc%sph_bc_T%iflag_cmb
        write(id_file,*) 'sph_bc_B%iflag_icb',                          &
     &        sph_MHD_bc%sph_bc_B%kr_in,  sph_MHD_bc%sph_bc_B%iflag_icb
        write(id_file,*) 'sph_bc_B%iflag_cmb',                          &
     &        sph_MHD_bc%sph_bc_B%kr_out, sph_MHD_bc%sph_bc_B%iflag_cmb
        write(id_file,*) 'sph_bc_C%iflag_icb',                          &
     &        sph_MHD_bc%sph_bc_C%kr_in,  sph_MHD_bc%sph_bc_C%iflag_icb
        write(id_file,*) 'sph_bc_C%iflag_cmb',                          &
     &        sph_MHD_bc%sph_bc_C%kr_out, sph_MHD_bc%sph_bc_C%iflag_cmb
      end if
!
      if (iflag_debug .eq. iflag_full_msg) then
        if (MHD_prop%fl_prop%iflag_scheme .gt. id_no_evolution) then
          call check_fdm_coefs_4_BC2                                    &
     &       (id_file, velocity%name, sph_MHD_bc%sph_bc_U)
!
          call check_sph_fdm_boundaries(id_file,                        &
     &        sph_MHD_bc%sph_bc_U%kr_in, sph_MHD_bc%sph_bc_U%kr_out,    &
     &        sph_rj, sph_MHD_bc%bc_fdms_U)
        end if
!
        if(MHD_prop%cd_prop%iflag_Bevo_scheme .gt. id_no_evolution)     &
     &   then
          call check_fdm_coefs_4_BC2                                    &
     &       (id_file, magnetic_field%name, sph_MHD_bc%sph_bc_B)
        end if
        if(MHD_prop%ht_prop%iflag_scheme .gt. id_no_evolution) then
          call check_fdm_coefs_4_BC2                                    &
     &       (id_file, temperature%name,  sph_MHD_bc%sph_bc_T)
        end if
        if(MHD_prop%cp_prop%iflag_scheme .gt. id_no_evolution) then
          call check_fdm_coefs_4_BC2                                    &
     &       (id_file, composition%name, sph_MHD_bc%sph_bc_C)
        end if
!
        call check_fdm2_coefs_centre(id_file, sph_MHD_bc%fdm2_center)
      end if
!
      end subroutine check_bc_sph_mhd
!
! -----------------------------------------------------------------------
!
      end module t_boundary_data_sph_MHD
