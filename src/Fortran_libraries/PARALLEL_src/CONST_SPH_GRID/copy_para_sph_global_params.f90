!>@file   copy_para_sph_global_params.f90
!!@brief  module copy_para_sph_global_params
!!
!!@author H. Matsui
!!@date Programmed on July, 2007
!
!>@brief  Set control data for domain decomposition for spherical transform
!!
!!@verbatim
!!      subroutine copy_para_sph_param_from_ctl                         &
!!     &         (sph_org, num_pe, sph_mesh)
!!      subroutine copy_para_global_sph_resolution                      &
!!     &         (sph_org, num_pe, sph_mesh)
!!        integer(kind = kint), intent(in) :: num_pe
!!        type(sph_grids), intent(in) :: sph_org
!!        type(sph_mesh_data), intent(inout) :: sph_mesh(num_pe)
!!@endverbatim
!
      module copy_para_sph_global_params
!
      use m_precision
!
      use t_SPH_mesh_field_data
      use t_spheric_parameter
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine copy_para_sph_param_from_ctl                           &
     &         (sph_org, num_pe, sph_mesh)
!
      integer(kind = kint), intent(in) :: num_pe
      type(sph_grids), intent(in) :: sph_org
!
      type(sph_mesh_data), intent(inout) :: sph_mesh(num_pe)
!
      integer(kind = kint) :: ip
!
!
!$omp parallel do
      do ip = 1, num_pe
        call copy_each_sph_param_from_ctl                               &
     &     (sph_org, sph_mesh(ip)%sph%sph_params,                       &
     &      sph_mesh(ip)%sph%sph_rtp, sph_mesh(ip)%sph%sph_rj)
      end do
!$omp end parallel do
!
      end subroutine copy_para_sph_param_from_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine copy_para_global_sph_resolution                        &
     &         (sph_org, num_pe, sph_mesh)
!
      integer(kind = kint), intent(in) :: num_pe
      type(sph_grids), intent(in) :: sph_org
      type(sph_mesh_data), intent(inout) :: sph_mesh(num_pe)
!
      integer(kind = kint) :: ip
!
!
!$omp parallel do
      do ip = 1, num_pe
        call copy_each_global_sph_resolution(sph_org,                   &
     &     sph_mesh(ip)%sph%sph_rtp, sph_mesh(ip)%sph%sph_rtm,          &
     &     sph_mesh(ip)%sph%sph_rlm, sph_mesh(ip)%sph%sph_rj)
      end do
!$omp end parallel do
!
      end subroutine copy_para_global_sph_resolution
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_each_sph_param_from_ctl                           &
     &         (sph_org, sph_params, sph_rtp, sph_rj)
!
      type(sph_grids), intent(in) :: sph_org
!
      type(sph_shell_parameters), intent(inout) :: sph_params
      type(sph_rtp_grid), intent(inout) :: sph_rtp
      type(sph_rj_grid), intent(inout) :: sph_rj
!
!
        sph_params%iflag_shell_mode                                     &
     &           = sph_org%sph_params%iflag_shell_mode
        sph_params%iflag_radial_grid                                    &
     &           = sph_org%sph_params%iflag_radial_grid
!
        sph_rj%iflag_rj_center = sph_org%sph_rj%iflag_rj_center
!
        sph_params%l_truncation = sph_org%sph_params%l_truncation
        sph_params%m_folding = sph_org%sph_params%m_folding
!
        sph_params%nlayer_2_center = sph_org%sph_params%nlayer_2_center
        sph_params%nlayer_ICB = sph_org%sph_params%nlayer_ICB
        sph_params%nlayer_CMB = sph_org%sph_params%nlayer_CMB
        sph_params%nlayer_mid_OC = sph_org%sph_params%nlayer_mid_OC
!
        sph_params%radius_ICB = sph_org%sph_params%radius_ICB
        sph_params%radius_CMB = sph_org%sph_params%radius_CMB
!
        sph_rtp%nidx_global_rtp(1:2)                                    &
     &           = sph_org%sph_rtp%nidx_global_rtp(1:2)
!
      end subroutine copy_each_sph_param_from_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine copy_each_global_sph_resolution(sph_org,               &
     &          sph_rtp, sph_rtm, sph_rlm, sph_rj)
!
      type(sph_grids), intent(in) :: sph_org
      type(sph_rtp_grid), intent(inout) :: sph_rtp
      type(sph_rtm_grid), intent(inout) :: sph_rtm
      type(sph_rlm_grid), intent(inout) :: sph_rlm
      type(sph_rj_grid), intent(inout) :: sph_rj
!
!
      sph_rtp%nidx_global_rtp(1:3)                                      &
     &      = sph_org%sph_rtp%nidx_global_rtp(1:3)
      sph_rtm%nidx_global_rtm(1:3)                                      &
     &      = sph_org%sph_rtm%nidx_global_rtm(1:3)
      sph_rlm%nidx_global_rlm(1:2)                                      &
     &      = sph_org%sph_rlm%nidx_global_rlm(1:2)
      sph_rj%nidx_global_rj(1:2) = sph_org%sph_rj%nidx_global_rj(1:2)
!
      end subroutine copy_each_global_sph_resolution
!
! -----------------------------------------------------------------------
!
      end module copy_para_sph_global_params
