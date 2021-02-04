!>@file   copy_para_sph_global_params.f90
!!@brief  module copy_para_sph_global_params
!!
!!@author H. Matsui
!!@date Programmed on July, 2007
!
!>@brief  Copy spherical hermonics indexing data
!!
!!@verbatim
!!      subroutine copy_para_sph_param_from_ctl(sph_org, sph_array)
!!      subroutine copy_para_global_sph_resolution(sph_org, sph_array)
!!        type(sph_grids), intent(in) :: sph_org
!!        type(sph_mesh_array), intent(inout) :: sph_array
!!
!!      subroutine copy_each_sph_param_from_ctl                         &
!!     &         (sph_org, sph_params, sph_rtp, sph_rj)
!!        type(sph_grids), intent(in) :: sph_org
!!        type(sph_shell_parameters), intent(inout) :: sph_params
!!        type(sph_rtp_grid), intent(inout) :: sph_rtp
!!        type(sph_rj_grid), intent(inout) :: sph_rj
!!      subroutine copy_each_global_sph_resolution(sph_org,             &
!!     &          sph_rtp, sph_rtm, sph_rlm, sph_rj)
!!        type(sph_grids), intent(in) :: sph_org
!!        type(sph_rtp_grid), intent(inout) :: sph_rtp
!!        type(sph_rtm_grid), intent(inout) :: sph_rtm
!!        type(sph_rlm_grid), intent(inout) :: sph_rlm
!!        type(sph_rj_grid), intent(inout) :: sph_rj
!!@endverbatim
!
      module copy_para_sph_global_params
!
      use m_precision
!
      use t_SPH_mesh_field_array
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
      subroutine copy_para_sph_param_from_ctl(sph_org, sph_array)
!
      type(sph_grids), intent(in) :: sph_org
      type(sph_mesh_array), intent(inout) :: sph_array
!
      integer :: ip
!
!
!$omp parallel do
      do ip = 1, sph_array%num_pe
        call copy_each_sph_param_from_ctl                               &
     &     (sph_org, sph_array%sph(ip)%sph_params,                      &
     &      sph_array%sph(ip)%sph_rtp, sph_array%sph(ip)%sph_rj)
      end do
!$omp end parallel do
!
      end subroutine copy_para_sph_param_from_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine copy_para_global_sph_resolution(sph_org, sph_array)
!
      type(sph_grids), intent(in) :: sph_org
      type(sph_mesh_array), intent(inout) :: sph_array
!
      integer(kind = kint) :: ip
!
!
!$omp parallel do
      do ip = 1, sph_array%num_pe
        call copy_each_global_sph_resolution(sph_org,                   &
     &     sph_array%sph(ip)%sph_rtp, sph_array%sph(ip)%sph_rtm,        &
     &     sph_array%sph(ip)%sph_rlm, sph_array%sph(ip)%sph_rj)
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
