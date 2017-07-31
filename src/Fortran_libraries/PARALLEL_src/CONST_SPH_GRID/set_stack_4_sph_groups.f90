!>@file   set_stack_4_sph_groups.f90
!!@brief  module set_stack_4_sph_groups
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Make group informations for spherical shell model
!!
!!
!!@verbatim
!!      subroutine set_stack_rtp_radial_grp(sph_param, sph_rtp,         &
!!     &          r_layer_grp, added_radial_grp, radial_rtp_grp)
!!      subroutine set_stack_rtp_meridional_grp                         &
!!     &         (sph_rtp, med_layer_grp, theta_rtp_grp)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(layering_group_list), intent(in) :: med_layer_grp
!!        type(group_data), intent(inout) :: theta_rtp_grp
!!      subroutine set_stack_rj_radial_grp                              &
!!     &        (sph_param, sph_rj, added_radial_grp, radial_rj_grp)
!!      subroutine set_stack_rj_spectr_grp(sph_rj, sphere_rj_grp)
!!        type(sph_shell_parameters), intent(in) :: sph_param
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(layering_group_list), intent(in) :: added_radial_grp
!!        type(group_data), intent(inout) :: radial_rtp_grp
!!        type(group_data), intent(inout) :: radial_rj_grp
!!        type(group_data), intent(inout) :: sphere_rj_grp
!!        type(group_data), intent(inout) :: theta_rtp_grp
!!@endverbatim
!
      module set_stack_4_sph_groups
!
      use m_precision
      use m_constants
!
      use t_spheric_parameter
      use t_group_data
      use t_control_1D_layering
!
      implicit none
!
      character(len = kchara), parameter :: name_mid =     'Mid_OC'
      character(len = kchara), parameter :: name_ED = 'Outmost_of_Shell'
!
      character(len = kchara), parameter :: name_y00 =  'Y_0_0'
      character(len = kchara), parameter :: name_y10 =  'Y_1_0'
      character(len = kchara), parameter :: name_y11s = 'Y_1_1s'
      character(len = kchara), parameter :: name_y11c = 'Y_1_1c'
!
      private :: name_mid
      private :: name_y00, name_y10, name_y11s, name_y11c
      private :: set_stack_sph_grp_by_list, set_stack_sph_grp_direct
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_stack_rtp_radial_grp(sph_param, sph_rtp,           &
     &          r_layer_grp, added_radial_grp, radial_rtp_grp)
!
      type(sph_shell_parameters), intent(in) :: sph_param
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(layering_group_list), intent(in) :: r_layer_grp
      type(layering_group_list), intent(in) :: added_radial_grp
      type(group_data), intent(inout) :: radial_rtp_grp
!
      integer(kind = kint) :: icou, nlayer_ed, inum
!
!
      icou = 0
      call set_stack_sph_grp_by_list                                    &
     &   (icou, sph_param%nlayer_ICB, sph_param%nlayer_ICB,             &
     &    sph_rtp%nidx_rtp(1), sph_rtp%idx_gl_1d_rtp_r,                 &
     &    ICB_nod_grp_name, radial_rtp_grp%num_grp,                     &
     &    radial_rtp_grp%istack_grp, radial_rtp_grp%grp_name)
!
      call set_stack_sph_grp_by_list                                    &
     &   (icou, sph_param%nlayer_CMB, sph_param%nlayer_CMB,             &
     &    sph_rtp%nidx_rtp(1), sph_rtp%idx_gl_1d_rtp_r,                 &
     &    CMB_nod_grp_name, radial_rtp_grp%num_grp,                     &
     &    radial_rtp_grp%istack_grp, radial_rtp_grp%grp_name)
!
      call set_stack_sph_grp_by_list(icou,                              &
     &    sph_param%nlayer_2_center, sph_param%nlayer_2_center,         &
     &    sph_rtp%nidx_rtp(1), sph_rtp%idx_gl_1d_rtp_r,                 &
     &    CTR_nod_grp_name, radial_rtp_grp%num_grp,                     &
     &    radial_rtp_grp%istack_grp, radial_rtp_grp%grp_name)
!
      if (sph_rtp%nidx_global_rtp(1) .gt. sph_param%nlayer_CMB) then
        call set_stack_sph_grp_by_list(icou,                            &
     &      sph_rtp%nidx_global_rtp(1), sph_rtp%nidx_global_rtp(1),     &
     &      sph_rtp%nidx_rtp(1), sph_rtp%idx_gl_1d_rtp_r, name_ED,      &
     &      radial_rtp_grp%num_grp, radial_rtp_grp%istack_grp,          &
     &      radial_rtp_grp%grp_name)
      end if
!
      if (sph_param%nlayer_mid_OC .gt. 0) then
        call set_stack_sph_grp_by_list(icou,                            &
     &     sph_param%nlayer_mid_OC, sph_param%nlayer_mid_OC,            &
     &     sph_rtp%nidx_rtp(1), sph_rtp%idx_gl_1d_rtp_r,                &
     &     name_mid, radial_rtp_grp%num_grp,                            &
     &     radial_rtp_grp%istack_grp, radial_rtp_grp%grp_name)
      end if
!
      nlayer_ed = sph_param%nlayer_ICB-1
      call set_stack_sph_grp_by_list                                    &
     &   (icou, sph_param%nlayer_2_center, nlayer_ed,                   &
     &    sph_rtp%nidx_rtp(1), sph_rtp%idx_gl_1d_rtp_r,                 &
     &    IC_ele_grp_name, radial_rtp_grp%num_grp,                      &
     &    radial_rtp_grp%istack_grp, radial_rtp_grp%grp_name)
!
      call set_stack_sph_grp_by_list                                    &
     &   (icou, sph_param%nlayer_ICB, sph_param%nlayer_CMB,             &
     &    sph_rtp%nidx_rtp(1), sph_rtp%idx_gl_1d_rtp_r,                 &
     &    OC_ele_grp_name, radial_rtp_grp%num_grp,                      &
     &    radial_rtp_grp%istack_grp, radial_rtp_grp%grp_name)
!
!
      do inum = 1, r_layer_grp%nlayer
        call set_stack_sph_grp_by_list(icou,                            &
     &      r_layer_grp%istart(inum), r_layer_grp%iend(inum),           &
     &      sph_rtp%nidx_rtp(1), sph_rtp%idx_gl_1d_rtp_r,               &
     &      r_layer_grp%name(inum), radial_rtp_grp%num_grp,             &
     &      radial_rtp_grp%istack_grp, radial_rtp_grp%grp_name)
      end do
!
      do inum = 1, added_radial_grp%nlayer
        call set_stack_sph_grp_by_list(icou,                            &
     &      added_radial_grp%istart(inum), added_radial_grp%iend(inum), &
     &      sph_rtp%nidx_rtp(1), sph_rtp%idx_gl_1d_rtp_r,               &
     &      added_radial_grp%name(inum), radial_rtp_grp%num_grp,        &
     &      radial_rtp_grp%istack_grp, radial_rtp_grp%grp_name)
      end do
!
      radial_rtp_grp%num_item                                          &
     &      = radial_rtp_grp%istack_grp(radial_rtp_grp%num_grp)
!
      end subroutine set_stack_rtp_radial_grp
!
! -----------------------------------------------------------------------
!
      subroutine set_stack_rj_radial_grp                                &
     &        (sph_param, sph_rj, added_radial_grp, radial_rj_grp)
!
      type(sph_shell_parameters), intent(in) :: sph_param
      type(sph_rj_grid), intent(in) :: sph_rj
      type(layering_group_list), intent(in) :: added_radial_grp
      type(group_data), intent(inout) :: radial_rj_grp
!
      integer(kind = kint) :: icou, nlayer_ed, inum
!
!
      icou = 0
      call set_stack_sph_grp_direct                                     &
     &   (icou, sph_param%nlayer_ICB, sph_param%nlayer_ICB,             &
     &    sph_rj%ist_rj(1), sph_rj%ied_rj(1), ICB_nod_grp_name,         &
     &    radial_rj_grp%num_grp, radial_rj_grp%istack_grp,              &
     &    radial_rj_grp%grp_name)
!
      call set_stack_sph_grp_direct                                     &
     &   (icou, sph_param%nlayer_CMB, sph_param%nlayer_CMB,             &
     &    sph_rj%ist_rj(1), sph_rj%ied_rj(1), CMB_nod_grp_name,         &
     &    radial_rj_grp%num_grp, radial_rj_grp%istack_grp,              &
     &    radial_rj_grp%grp_name)
!
      call set_stack_sph_grp_direct                                     &
     &   (icou, sph_param%nlayer_2_center, sph_param%nlayer_2_center,   &
     &    sph_rj%ist_rj(1), sph_rj%ied_rj(1), CTR_nod_grp_name,         &
     &    radial_rj_grp%num_grp, radial_rj_grp%istack_grp,              &
     &    radial_rj_grp%grp_name)
!
      if (sph_rj%nidx_global_rj(1) .gt. sph_param%nlayer_CMB) then
        call set_stack_sph_grp_direct                                   &
     &     (icou, sph_rj%nidx_global_rj(1), sph_rj%nidx_global_rj(1),   &
     &      sph_rj%ist_rj(1), sph_rj%ied_rj(1), name_ED,                &
     &      radial_rj_grp%num_grp,  radial_rj_grp%istack_grp,           &
     &      radial_rj_grp%grp_name)
      end if
!
      if (sph_param%nlayer_mid_OC .gt. 0) then
        call set_stack_sph_grp_direct                                   &
     &    (icou, sph_param%nlayer_mid_OC, sph_param%nlayer_mid_OC,      &
     &     sph_rj%ist_rj(1), sph_rj%ied_rj(1), name_mid,                &
     &     radial_rj_grp%num_grp, radial_rj_grp%istack_grp,             &
     &     radial_rj_grp%grp_name)
      end if
!
      nlayer_ed = sph_param%nlayer_ICB-1
      call set_stack_sph_grp_direct                                     &
     &   (icou, sph_param%nlayer_2_center, nlayer_ed,                   &
     &    sph_rj%ist_rj(1), sph_rj%ied_rj(1), IC_ele_grp_name,          &
     &    radial_rj_grp%num_grp, radial_rj_grp%istack_grp,              &
     &    radial_rj_grp%grp_name)
!
      call set_stack_sph_grp_direct                                     &
     &   (icou, sph_param%nlayer_ICB, sph_param%nlayer_CMB,             &
     &    sph_rj%ist_rj(1), sph_rj%ied_rj(1), OC_ele_grp_name,          &
     &    radial_rj_grp%num_grp, radial_rj_grp%istack_grp,              &
     &    radial_rj_grp%grp_name)
!
      do inum = 1, added_radial_grp%nlayer
        call set_stack_sph_grp_direct(icou,                             &
     &      added_radial_grp%istart(inum), added_radial_grp%iend(inum), &
     &      sph_rj%ist_rj(1), sph_rj%ied_rj(1),                         &
     &      added_radial_grp%name(inum), radial_rj_grp%num_grp,         &
     &      radial_rj_grp%istack_grp, radial_rj_grp%grp_name)
      end do
!
      radial_rj_grp%num_item                                            &
     &      = radial_rj_grp%istack_grp(radial_rj_grp%num_grp)
!
      end subroutine set_stack_rj_radial_grp
!
! -----------------------------------------------------------------------
!
      subroutine set_stack_rtp_meridional_grp                           &
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
        call set_stack_sph_grp_direct                                   &
     &     (icou, med_layer_grp%istart(inum), med_layer_grp%iend(inum), &
     &      sph_rtp%ist_rtp(2), sph_rtp%ied_rtp(2),                     &
     &      med_layer_grp%name(inum), theta_rtp_grp%num_grp,            &
     &      theta_rtp_grp%istack_grp, theta_rtp_grp%grp_name)
      end do
!
      theta_rtp_grp%num_item                                            &
     &     = theta_rtp_grp%istack_grp(theta_rtp_grp%num_grp)
!
      end subroutine set_stack_rtp_meridional_grp
!
! -----------------------------------------------------------------------
!
      subroutine set_stack_rj_spectr_grp(sph_rj, sphere_rj_grp)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(group_data), intent(inout) :: sphere_rj_grp
!
      integer(kind = kint) :: icou
!
      icou = 0
      call set_stack_sph_grp_direct(icou, izero, izero,                 &
     &    sph_rj%ist_rj(2), sph_rj%ied_rj(2),                           &
     &    name_y00, sphere_rj_grp%num_grp,                              &
     &    sphere_rj_grp%istack_grp, sphere_rj_grp%grp_name)
!
      call set_stack_sph_grp_direct(icou, ione, ione,                   &
     &    sph_rj%ist_rj(2), sph_rj%ied_rj(2),                           &
     &    name_y11s, sphere_rj_grp%num_grp,                             &
     &    sphere_rj_grp%istack_grp, sphere_rj_grp%grp_name)
!
      call set_stack_sph_grp_direct(icou, itwo, itwo,                   &
     &    sph_rj%ist_rj(2), sph_rj%ied_rj(2),                           &
     &    name_y10, sphere_rj_grp%num_grp,                              &
     &    sphere_rj_grp%istack_grp, sphere_rj_grp%grp_name)
!
      call set_stack_sph_grp_direct(icou, ithree, ithree,               &
     &    sph_rj%ist_rj(2), sph_rj%ied_rj(2),                           &
     &     name_y11c, sphere_rj_grp%num_grp,                            &
     &    sphere_rj_grp%istack_grp, sphere_rj_grp%grp_name)
!
      sphere_rj_grp%num_item                                            &
     &     = sphere_rj_grp%istack_grp(sphere_rj_grp%num_grp)
!
      end subroutine set_stack_rj_spectr_grp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_stack_sph_grp_by_list(icou, nlayer_st, nlayer_ed,  &
     &          nidx, idx_gl_1d, grp_name, num_grp, istack_grp,         &
     &          name_grp)
!
      integer(kind = kint), intent(in) :: nlayer_st, nlayer_ed
      integer(kind = kint), intent(in) :: nidx, idx_gl_1d(nidx)
      integer(kind = kint), intent(in) :: num_grp
      character(len = kchara), intent(in) :: grp_name
!
      integer(kind = kint), intent(inout) :: icou
      integer(kind = kint), intent(inout) :: istack_grp(0:num_grp)
      character(len = kchara), intent(inout) :: name_grp(num_grp)
!
      integer(kind = kint) :: j
!
!
      if (nlayer_st .ge. 0) then
        icou = icou + 1
        name_grp(icou) =   grp_name
        istack_grp(icou) = istack_grp(icou-1)
        do j = 1, nidx
          if (idx_gl_1d(j) .ge. nlayer_st                               &
     &      .and. idx_gl_1d(j) .le. nlayer_ed ) then
            istack_grp(icou) = istack_grp(icou) + 1
          end if
        end do
      end if
!
      end subroutine set_stack_sph_grp_by_list
!
! -----------------------------------------------------------------------
!
      subroutine set_stack_sph_grp_direct(icou, nlayer_st, nlayer_ed,   &
     &          ist_domain, ied_domain, grp_name, num_grp, istack_grp,  &
     &          name_grp)
!
      integer(kind = kint), intent(in) :: nlayer_st, nlayer_ed
      integer(kind = kint), intent(in) :: ist_domain, ied_domain
      integer(kind = kint), intent(in) :: num_grp
      character(len = kchara), intent(in) :: grp_name
!
      integer(kind = kint), intent(inout) :: icou
      integer(kind = kint), intent(inout) :: istack_grp(0:num_grp)
      character(len = kchara), intent(inout) :: name_grp(num_grp)
!
      integer(kind = kint) :: j
!
!
      if (nlayer_st .ge. 0) then
        icou = icou + 1
        name_grp(icou) =   grp_name
        istack_grp(icou) = istack_grp(icou-1)
        do j = nlayer_st, nlayer_ed
          if (j.ge.ist_domain .and. j.le.ied_domain ) then
            istack_grp(icou) = istack_grp(icou) + 1
          end if
        end do
      end if
!
      end subroutine set_stack_sph_grp_direct
!
! -----------------------------------------------------------------------
!
      end module set_stack_4_sph_groups
