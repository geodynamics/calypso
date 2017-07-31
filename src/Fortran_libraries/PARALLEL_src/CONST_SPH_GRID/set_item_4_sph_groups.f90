!>@file   set_item_4_sph_groups.f90
!!@brief  module set_item_4_sph_groups
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Make group informations for spherical shell model
!!
!!
!!@verbatim
!!      subroutine set_item_rtp_radial_grp(sph_params, sph_rtp,         &
!!     &          r_layer_grp, added_radial_grp, radial_rtp_grp)
!!      subroutine set_item_rj_radial_grp                               &
!!     &         (sph_params, sph_rj, added_radial_grp, radial_rj_grp)
!!      subroutine set_item_rtp_meridional_grp                          &
!!     &         (sph_rtp, med_layer_grp, theta_rtp_grp)
!!      subroutine set_item_rj_spectr_grp(sph_rj, sphere_rj_grp)
!!        type(sph_shell_parameters) :: sph_params
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(layering_group_list), intent(in) :: r_layer_grp
!!        type(layering_group_list), intent(in) :: added_radial_grp
!!        type(layering_group_list), intent(in) :: med_layer_grp
!!        type(group_data), intent(inout) :: radial_rtp_grp
!!        type(group_data), intent(inout) :: theta_rtp_grp
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(group_data), intent(inout) :: radial_rj_grp
!!        type(group_data), intent(inout) :: sphere_rj_grp
!!@endverbatim
!
      module set_item_4_sph_groups
!
      use m_precision
      use m_constants
!
      use t_spheric_parameter
      use t_spheric_rtp_data
      use t_spheric_rtm_data
      use t_spheric_rlm_data
      use t_spheric_rj_data
      use t_group_data
      use t_control_1D_layering
!
      implicit none
!
      private :: set_item_sph_grp_by_list, set_item_sph_grp_direct
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_item_rtp_radial_grp(sph_params, sph_rtp,           &
     &          r_layer_grp, added_radial_grp, radial_rtp_grp)
!
      type(sph_shell_parameters) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(layering_group_list), intent(in) :: r_layer_grp
      type(layering_group_list), intent(in) :: added_radial_grp
!
      type(group_data), intent(inout) :: radial_rtp_grp
!
      integer(kind = kint) :: icou, nlayer_ed, inum
!
      icou = 0
      call set_item_sph_grp_by_list                                     &
     &   (icou, sph_params%nlayer_ICB, sph_params%nlayer_ICB,           &
     &    sph_rtp%nidx_rtp(1), sph_rtp%idx_gl_1d_rtp_r,                 &
     &    radial_rtp_grp%num_item, radial_rtp_grp%item_grp)
!
      call set_item_sph_grp_by_list                                     &
     &   (icou, sph_params%nlayer_CMB, sph_params%nlayer_CMB,           &
     &    sph_rtp%nidx_rtp(1), sph_rtp%idx_gl_1d_rtp_r,                 &
     &    radial_rtp_grp%num_item, radial_rtp_grp%item_grp)
!
      call set_item_sph_grp_by_list(icou,                               &
     &    sph_params%nlayer_2_center, sph_params%nlayer_2_center,       &
     &    sph_rtp%nidx_rtp(1), sph_rtp%idx_gl_1d_rtp_r,                 &
     &    radial_rtp_grp%num_item, radial_rtp_grp%item_grp)
!
      if (sph_rtp%nidx_global_rtp(1) .gt. sph_params%nlayer_CMB) then
        call set_item_sph_grp_by_list(icou,                             &
     &      sph_rtp%nidx_global_rtp(1), sph_rtp%nidx_global_rtp(1),     &
     &      sph_rtp%nidx_rtp(1), sph_rtp%idx_gl_1d_rtp_r,               &
     &      radial_rtp_grp%num_item, radial_rtp_grp%item_grp)
      end if
!
      if (sph_params%nlayer_mid_OC .gt. 0) then
        call set_item_sph_grp_by_list(icou,                             &
     &      sph_params%nlayer_mid_OC, sph_params%nlayer_mid_OC,         &
     &      sph_rtp%nidx_rtp(1), sph_rtp%idx_gl_1d_rtp_r,               &
     &      radial_rtp_grp%num_item, radial_rtp_grp%item_grp)
      end if
!
      nlayer_ed = sph_params%nlayer_ICB-1
      call set_item_sph_grp_by_list                                     &
     &   (icou, sph_params%nlayer_2_center, nlayer_ed,                  &
     &    sph_rtp%nidx_rtp(1), sph_rtp%idx_gl_1d_rtp_r,                 &
     &    radial_rtp_grp%num_item, radial_rtp_grp%item_grp)
!
      call set_item_sph_grp_by_list                                     &
     &   (icou, sph_params%nlayer_ICB, sph_params%nlayer_CMB,           &
     &    sph_rtp%nidx_rtp(1), sph_rtp%idx_gl_1d_rtp_r,                 &
     &    radial_rtp_grp%num_item, radial_rtp_grp%item_grp)
!
!
      do inum = 1, r_layer_grp%nlayer
        call set_item_sph_grp_by_list(icou,                             &
     &      r_layer_grp%istart(inum), r_layer_grp%iend(inum),           &
     &      sph_rtp%nidx_rtp(1), sph_rtp%idx_gl_1d_rtp_r,               &
     &      radial_rtp_grp%num_item, radial_rtp_grp%item_grp)
      end do
!
      do inum = 1, added_radial_grp%nlayer
        call set_item_sph_grp_by_list(icou,                             &
     &      added_radial_grp%istart(inum), added_radial_grp%iend(inum), &
     &      sph_rtp%nidx_rtp(1), sph_rtp%idx_gl_1d_rtp_r,               &
     &      radial_rtp_grp%num_item, radial_rtp_grp%item_grp)
      end do
!
      end subroutine set_item_rtp_radial_grp
!
! -----------------------------------------------------------------------
!
      subroutine set_item_rj_radial_grp                                 &
     &         (sph_params, sph_rj, added_radial_grp, radial_rj_grp)
!
      type(sph_shell_parameters) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(layering_group_list), intent(in) :: added_radial_grp
      type(group_data), intent(inout) :: radial_rj_grp
!
      integer(kind = kint) :: icou, nlayer_ed, inum
!
!
      icou = 0
      call set_item_sph_grp_direct                                      &
     &   (icou, sph_params%nlayer_ICB, sph_params%nlayer_ICB,           &
     &    sph_rj%ist_rj(1), sph_rj%ied_rj(1),                           &
     &    radial_rj_grp%num_item, radial_rj_grp%item_grp)
!
      call set_item_sph_grp_direct                                      &
     &   (icou, sph_params%nlayer_CMB, sph_params%nlayer_CMB,           &
     &    sph_rj%ist_rj(1), sph_rj%ied_rj(1),                           &
     &    radial_rj_grp%num_item, radial_rj_grp%item_grp)
!
      call set_item_sph_grp_direct(icou,                                &
     &    sph_params%nlayer_2_center, sph_params%nlayer_2_center,       &
     &    sph_rj%ist_rj(1), sph_rj%ied_rj(1),                           &
     &    radial_rj_grp%num_item, radial_rj_grp%item_grp)
!
      if (sph_rj%nidx_global_rj(1) .gt. sph_params%nlayer_CMB) then
        call set_item_sph_grp_direct(icou, sph_rj%nidx_global_rj(1),    &
     &      sph_rj%nidx_global_rj(1), sph_rj%ist_rj(1),                 &
     &      sph_rj%ied_rj(1), radial_rj_grp%num_item,                   &
     &      radial_rj_grp%item_grp)
      end if
!
      if (sph_params%nlayer_mid_OC .gt. 0) then
        call set_item_sph_grp_direct(icou,                              &
     &      sph_params%nlayer_mid_OC, sph_params%nlayer_mid_OC,         &
     &      sph_rj%ist_rj(1), sph_rj%ied_rj(1),                         &
     &      radial_rj_grp%num_item, radial_rj_grp%item_grp)
      end if
!
      nlayer_ed = sph_params%nlayer_ICB-1
      call set_item_sph_grp_direct                                      &
     &   (icou, sph_params%nlayer_2_center, nlayer_ed,                  &
     &    sph_rj%ist_rj(1), sph_rj%ied_rj(1),                           &
     &    radial_rj_grp%num_item, radial_rj_grp%item_grp)
!
      call set_item_sph_grp_direct                                      &
     &   (icou, sph_params%nlayer_ICB, sph_params%nlayer_CMB,           &
     &    sph_rj%ist_rj(1), sph_rj%ied_rj(1),                           &
     &    radial_rj_grp%num_item, radial_rj_grp%item_grp)
!
      do inum = 1, added_radial_grp%nlayer
        call set_item_sph_grp_direct(icou,                              &
     &      added_radial_grp%istart(inum), added_radial_grp%iend(inum), &
     &      sph_rj%ist_rj(1), sph_rj%ied_rj(1),                         &
     &      radial_rj_grp%num_item, radial_rj_grp%item_grp)
      end do
!
      end subroutine set_item_rj_radial_grp
!
! -----------------------------------------------------------------------
!
      subroutine set_item_rtp_meridional_grp                            &
     &         (sph_rtp, med_layer_grp, theta_rtp_grp)
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(layering_group_list), intent(in) :: med_layer_grp
      type(group_data), intent(inout) :: theta_rtp_grp
!
      integer(kind = kint) :: inum, icou
!
      icou = 0
      do inum = 1, med_layer_grp%nlayer
        call set_item_sph_grp_direct                                    &
     &     (icou, med_layer_grp%istart(inum), med_layer_grp%iend(inum), &
     &      sph_rtp%ist_rtp(2), sph_rtp%ied_rtp(2),                     &
     &      theta_rtp_grp%num_item, theta_rtp_grp%item_grp)
      end do
!
      end subroutine set_item_rtp_meridional_grp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_item_rj_spectr_grp(sph_rj, sphere_rj_grp)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(group_data), intent(inout) :: sphere_rj_grp
!
      integer(kind = kint) :: icou
!
      icou = 0
      call set_item_sph_grp_direct(icou, izero, izero,                  &
     &    sph_rj%ist_rj(2), sph_rj%ied_rj(2),                           &
     &    sphere_rj_grp%num_item, sphere_rj_grp%item_grp)
!
      call set_item_sph_grp_direct(icou, ione, ione,                    &
     &    sph_rj%ist_rj(2), sph_rj%ied_rj(2),                           &
     &    sphere_rj_grp%num_item, sphere_rj_grp%item_grp)
!
      call set_item_sph_grp_direct(icou, itwo, itwo,                    &
     &    sph_rj%ist_rj(2), sph_rj%ied_rj(2),                           &
     &    sphere_rj_grp%num_item, sphere_rj_grp%item_grp)
!
      call set_item_sph_grp_direct(icou, ithree, ithree,                &
     &    sph_rj%ist_rj(2), sph_rj%ied_rj(2),                           &
     &    sphere_rj_grp%num_item, sphere_rj_grp%item_grp)
!
      end subroutine set_item_rj_spectr_grp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_item_sph_grp_by_list(icou, nlayer_st, nlayer_ed,   &
     &          nidx, idx_gl_1d, ntot_grp, item_grp)
!
      integer(kind = kint), intent(in) :: nlayer_st, nlayer_ed
      integer(kind = kint), intent(in) :: nidx, idx_gl_1d(nidx)
      integer(kind = kint), intent(in) :: ntot_grp
!
      integer(kind = kint), intent(inout) :: icou
      integer(kind = kint), intent(inout) :: item_grp(ntot_grp)
!
      integer(kind = kint) :: j
!
!
      if (nlayer_st .ge. 0) then
        do j = 1, nidx
          if (idx_gl_1d(j) .ge. nlayer_st                               &
     &      .and. idx_gl_1d(j) .le. nlayer_ed ) then
            icou = icou + 1
            item_grp(icou) = j
          end if
        end do
      end if
!
      end subroutine set_item_sph_grp_by_list
!
! ----------------------------------------------------------------------
!
      subroutine set_item_sph_grp_direct(icou, nlayer_st, nlayer_ed,    &
     &          ist_domain, ied_domain, ntot_grp, item_grp)
!
      integer(kind = kint), intent(in) :: nlayer_st, nlayer_ed
      integer(kind = kint), intent(in) :: ist_domain, ied_domain
      integer(kind = kint), intent(in) :: ntot_grp
!
      integer(kind = kint), intent(inout) :: icou
      integer(kind = kint), intent(inout) :: item_grp(ntot_grp)
!
      integer(kind = kint) :: j
!
!
      if (nlayer_st .ge. 0) then
        do j = nlayer_st, nlayer_ed
          if (j.ge.ist_domain .and. j.le.ied_domain ) then
            icou = icou + 1
            item_grp(icou) = j - ist_domain + 1
          end if
        end do
      end if
!
      end subroutine set_item_sph_grp_direct
!
! ----------------------------------------------------------------------
!
      end module set_item_4_sph_groups
