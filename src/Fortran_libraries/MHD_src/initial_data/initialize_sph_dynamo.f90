!>@file   initialize_sph_dynamo.f90
!!@brief  module initialize_sph_dynamo
!!
!!@author H. Matsui
!!@date Programmed in March, 2008
!
!> @brief Set initial spectrum data for dynamos
!!
!!@verbatim
!!      subroutine sph_initial_data_4_benchmarks(iflag_restart_mode,    &
!!     &                                         sph, ipol, rj_fld)
!!      subroutine sph_initial_data_w_seed_B(sph, sph_MHD_bc, refs,     &
!!     &                                     ipol, rj_fld)
!!        integer(kind = kint), intent(in) :: iflag_restart_mode
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(radial_reference_field), intent(in) :: refs
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(inout) :: rj_fld
!!
!!      subroutine init_sph_scalar_dbench(isig, sph, ipol_scalar,       &
!!     &                                  rj_fld)
!!      subroutine init_sph_scalar_with_noise                           &
!!     &         (sph, iref_scalar, ref_field, ipol_scalar, rj_fld)
!!        integer(kind = kint), intent(in) :: isig
!!        type(sph_grids), intent(in) :: sph
!!        type(phys_data), intent(in) :: ref_field
!!        integer(kind = kint), intent(in) :: iref_scalar
!!        integer(kind = kint), intent(in) :: ipol_scalar
!!        type(phys_data), intent(inout) :: rj_fld
!!@endverbatim
!
      module initialize_sph_dynamo
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_spheric_parameter
      use t_phys_data
      use t_phys_address
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sph_initial_data_4_benchmarks(iflag_restart_mode,      &
     &                                         sph, ipol, rj_fld)
!
      use m_machine_parameter
      use m_initial_field_control
!
      use initial_magne_sph_dynamo
      use calypso_mpi
!
      integer(kind = kint), intent(in) :: iflag_restart_mode
      type(sph_grids), intent(in) :: sph
      type(phys_address), intent(in) :: ipol
!
      type(phys_data), intent(inout) :: rj_fld
!
      integer(kind = kint), parameter :: isig = 400
!
!
      call calypso_mpi_barrier
!$omp parallel workshare
      rj_fld%d_fld(1:rj_fld%n_point,ipol%base%i_velo  ) = 0.0d0
      rj_fld%d_fld(1:rj_fld%n_point,ipol%base%i_velo+1) = 0.0d0
      rj_fld%d_fld(1:rj_fld%n_point,ipol%base%i_velo+2) = 0.0d0
!$omp end parallel workshare
!
!
      if(ipol%base%i_temp .gt. 0) then
        if(iflag_debug.gt.0) write(*,*) 'initilal for temperature'
        call init_sph_scalar_dbench(isig, sph,                          &
     &                              ipol%base%i_temp, rj_fld)
      end if
!
      if(ipol%base%i_light .gt. 0) then
        call init_sph_scalar_dbench(isig, sph,                          &
     &                              ipol%base%i_light, rj_fld)
      end if
!
      if((ipol%base%i_magne*ipol%base%i_current) .gt. 0) then
        call sph_initial_magne_benchmarks(iflag_restart_mode,           &
     &                                    sph, ipol, rj_fld)
      end if
!
      end subroutine sph_initial_data_4_benchmarks
!
!-----------------------------------------------------------------------
!
      subroutine sph_initial_data_w_seed_B(sph, sph_MHD_bc, refs,       &
     &                                     ipol, rj_fld)
!
      use t_boundary_data_sph_MHD
      use t_radial_reference_field
      use initial_magne_sph_dynamo
!
      type(sph_grids), intent(in) :: sph
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(radial_reference_field), intent(in) :: refs
      type(phys_address), intent(in) :: ipol
!
      type(phys_data), intent(inout) :: rj_fld
!
!
!$omp parallel workshare
      rj_fld%d_fld(1:rj_fld%n_point,ipol%base%i_velo  ) = 0.0d0
      rj_fld%d_fld(1:rj_fld%n_point,ipol%base%i_velo+1) = 0.0d0
      rj_fld%d_fld(1:rj_fld%n_point,ipol%base%i_velo+2) = 0.0d0
!$omp end parallel workshare
!
      if((ipol%base%i_temp*refs%iref_base%i_temp) .gt. 0)  then
        call init_sph_scalar_with_noise(sph, refs%iref_base%i_temp,     &
     &      refs%ref_field, ipol%base%i_temp, rj_fld)
      end if
!
      if((ipol%base%i_light*refs%iref_base%i_light) .gt. 0) then
        call init_sph_scalar_with_noise(sph, refs%iref_base%i_light,    &
     &      refs%ref_field, ipol%base%i_light, rj_fld)
      end if
!
      if((ipol%base%i_magne*ipol%base%i_current) .gt. 0) then
        call initial_sph_seed_magne(sph, sph_MHD_bc%sph_bc_B,           &
     &                              ipol, rj_fld)
      end if
!
      end subroutine sph_initial_data_w_seed_B
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine init_sph_scalar_dbench(isig, sph, ipol_scalar,         &
     &                                  rj_fld)
!
      use set_initial_sph_scalars
!
      integer(kind = kint), intent(in) :: isig
      type(sph_grids), intent(in) :: sph
      integer(kind = kint), intent(in) :: ipol_scalar
!
      type(phys_data), intent(inout) :: rj_fld
!
!
!$omp parallel workshare
      rj_fld%d_fld(1:rj_fld%n_point,ipol_scalar) = 0.0d0
!$omp end parallel workshare
!
      call initial_sph_ref_temp_dbench(sph, rj_fld%n_point,             &
     &                                 rj_fld%d_fld(1,ipol_scalar))
      call init_sph_sectorial_temp(isig, sph, rj_fld%n_point,           &
     &                             rj_fld%d_fld(1,ipol_scalar))
!
      end subroutine init_sph_scalar_dbench
!
!-----------------------------------------------------------------------
!
      subroutine init_sph_scalar_with_noise                             &
     &         (sph, iref_scalar, ref_field, ipol_scalar, rj_fld)
!
      use set_initial_sph_scalars
!
      type(sph_grids), intent(in) :: sph
      type(phys_data), intent(in) :: ref_field
      integer(kind = kint), intent(in) :: iref_scalar
      integer(kind = kint), intent(in) :: ipol_scalar
!
      type(phys_data), intent(inout) :: rj_fld
!
!
!$omp parallel workshare
      rj_fld%d_fld(1:rj_fld%n_point,ipol_scalar) = 0.0d0
!$omp end parallel workshare
!
      call initial_sph_reference_scalar(sph,                            &
     &    ref_field%n_point, ref_field%d_fld(1,iref_scalar),            &
     &    rj_fld%n_point, rj_fld%d_fld(1,ipol_scalar))
      call initital_sph_noize_temp(sph, rj_fld%n_point,                 &
     &                             rj_fld%d_fld(1,ipol_scalar))
!
      end subroutine init_sph_scalar_with_noise
!
!-----------------------------------------------------------------------
!
      end module initialize_sph_dynamo
