!set_stack_4_sph_groups.f90
!      module set_stack_4_sph_groups
!
!     Written by H. Matsui on July, 2007
!
!      subroutine set_stack_rtp_radial_grp
!      subroutine set_stack_rj_radial_grp
!      subroutine set_stack_rj_spectr_grp
!
      module set_stack_4_sph_groups
!
      use m_precision
      use m_constants
!
      implicit none
!
      character(len = kchara), parameter :: name_ICB =     'ICB'
      character(len = kchara), parameter :: name_CMB =     'CMB'
      character(len = kchara), parameter :: name_2center = 'to_Center'
      character(len = kchara), parameter :: name_mid =     'Mid_OC'
      character(len = kchara), parameter :: name_IC =      'inner_core'
      character(len = kchara), parameter :: name_OC =      'outer_core'
      character(len = kchara), parameter :: name_ED = 'Outmost_of_Shell'
!
      character(len = kchara), parameter :: name_y00 =  'Y_0_0'
      character(len = kchara), parameter :: name_y10 =  'Y_1_0'
      character(len = kchara), parameter :: name_y11s = 'Y_1_1s'
      character(len = kchara), parameter :: name_y11c = 'Y_1_1c'
!
      private :: name_ICB, name_CMB, name_mid, name_OC
      private :: name_y00, name_y10, name_y11s, name_y11c
      private :: izero, ione, itwo, ithree
      private :: set_stack_sph_grp_by_list, set_stack_sph_grp_by_rng
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_stack_rtp_radial_grp
!
      use m_spheric_parameter
      use m_group_data_sph_specr
!
      integer(kind = kint) :: icou, nlayer_ed
!
!
      icou = 0
      call set_stack_sph_grp_by_list(icou, nlayer_ICB, nlayer_ICB,      &
     &    nidx_rtp(1), idx_gl_1d_rtp_r(1), name_ICB,                    &
     &    num_radial_grp_rtp, istack_radial_grp_rtp,                    &
     &    name_radial_grp_rtp)
!
      call set_stack_sph_grp_by_list(icou, nlayer_CMB, nlayer_CMB,      &
     &    nidx_rtp(1), idx_gl_1d_rtp_r(1), name_CMB,                    &
     &    num_radial_grp_rtp, istack_radial_grp_rtp,                    &
     &    name_radial_grp_rtp)
!
      call set_stack_sph_grp_by_list(icou,                              &
     &    nlayer_2_center, nlayer_2_center,  nidx_rtp(1),               &
     &    idx_gl_1d_rtp_r(1), name_2center, num_radial_grp_rtp,         &
     &    istack_radial_grp_rtp, name_radial_grp_rtp)
!
      if (nidx_global_rtp(1) .gt. nlayer_CMB) then
        call set_stack_sph_grp_by_list(icou,                            &
     &      nidx_global_rtp(1), nidx_global_rtp(1), nidx_rtp(1),        &
     &      idx_gl_1d_rtp_r(1), name_ED, num_radial_grp_rtp,            &
     &      istack_radial_grp_rtp, name_radial_grp_rtp)
      end if
!
      if (nlayer_mid_OC .gt. 0) then
        call set_stack_sph_grp_by_list(icou,                            &
     &      nlayer_mid_OC, nlayer_mid_OC, nidx_rtp(1),                  &
     &      idx_gl_1d_rtp_r(1), name_mid, num_radial_grp_rtp,           &
     &      istack_radial_grp_rtp, name_radial_grp_rtp)
      end if
!
      nlayer_ed = nlayer_ICB-1
      call set_stack_sph_grp_by_list(icou, nlayer_2_center, nlayer_ed,  &
     &    nidx_rtp(1), idx_gl_1d_rtp_r(1), name_IC, num_radial_grp_rtp, &
     &    istack_radial_grp_rtp, name_radial_grp_rtp)
!
      call set_stack_sph_grp_by_list(icou, nlayer_ICB, nlayer_CMB,      &
     &    nidx_rtp(1), idx_gl_1d_rtp_r(1), name_OC, num_radial_grp_rtp, &
     &    istack_radial_grp_rtp, name_radial_grp_rtp)
!
      ntot_radial_grp_rtp = istack_radial_grp_rtp(num_radial_grp_rtp)
!
      end subroutine set_stack_rtp_radial_grp
!
! -----------------------------------------------------------------------
!
      subroutine set_stack_rj_radial_grp
!
      use m_spheric_parameter
      use m_group_data_sph_specr
!
      integer(kind = kint) :: icou, nlayer_ed
!
!
      icou = 0
      call set_stack_sph_grp_by_rng(icou, nlayer_ICB, nlayer_ICB,       &
     &    ist_rj(1), ied_rj(1), name_ICB, num_radial_grp_rj,            &
     &    istack_radial_grp_rj, name_radial_grp_rj)
!
      call set_stack_sph_grp_by_rng(icou, nlayer_CMB, nlayer_CMB,       &
     &    ist_rj(1), ied_rj(1), name_CMB, num_radial_grp_rj,            &
     &    istack_radial_grp_rj, name_radial_grp_rj)
!
      call set_stack_sph_grp_by_rng(icou, nlayer_2_center,              &
     &    nlayer_2_center, ist_rj(1), ied_rj(1), name_2center,          &
     &    num_radial_grp_rj, istack_radial_grp_rj, name_radial_grp_rj)
!
      if (nidx_global_rtp(1) .gt. nlayer_CMB) then
        call set_stack_sph_grp_by_rng(icou, nidx_global_rtp(1),         &
     &     nidx_global_rtp(1), ist_rj(1), ied_rj(1), name_ED,           &
     &     num_radial_grp_rj, istack_radial_grp_rj, name_radial_grp_rj)
      end if
!
      if (nlayer_mid_OC .gt. 0) then
        call set_stack_sph_grp_by_rng(icou, nlayer_mid_OC,              &
     &     nlayer_mid_OC, ist_rj(1), ied_rj(1), name_mid,               &
     &     num_radial_grp_rj, istack_radial_grp_rj, name_radial_grp_rj)
      end if
!
      nlayer_ed = nlayer_ICB-1
      call set_stack_sph_grp_by_rng(icou, nlayer_2_center, nlayer_ed,   &
     &    ist_rj(1), ied_rj(1), name_IC, num_radial_grp_rj,             &
     &    istack_radial_grp_rj, name_radial_grp_rj)
!
      call set_stack_sph_grp_by_rng(icou, nlayer_ICB, nlayer_CMB,       &
     &    ist_rj(1), ied_rj(1), name_OC, num_radial_grp_rj,             &
     &    istack_radial_grp_rj, name_radial_grp_rj)
!
      ntot_radial_grp_rj = istack_radial_grp_rj(num_radial_grp_rj)
!
      end subroutine set_stack_rj_radial_grp
!
! -----------------------------------------------------------------------
!
      subroutine set_stack_rj_spectr_grp
!
      use m_spheric_parameter
      use m_group_data_sph_specr
!
      integer(kind = kint) :: icou
!
      icou = 0
      call set_stack_sph_grp_by_rng(icou, izero, izero,                 &
     &    ist_rj(2), ied_rj(2), name_y00, num_sphere_grp_rj,            &
     &    istack_sphere_grp_rj, name_sphere_grp_rj)
!
      call set_stack_sph_grp_by_rng(icou, ione, ione,                   &
     &    ist_rj(2), ied_rj(2), name_y11s, num_sphere_grp_rj,           &
     &    istack_sphere_grp_rj, name_sphere_grp_rj)
!
      call set_stack_sph_grp_by_rng(icou, itwo, itwo,                   &
     &    ist_rj(2), ied_rj(2), name_y10, num_sphere_grp_rj,            &
     &    istack_sphere_grp_rj, name_sphere_grp_rj)
!
      call set_stack_sph_grp_by_rng(icou, ithree, ithree,               &
     &    ist_rj(2), ied_rj(2), name_y11c, num_sphere_grp_rj,           &
     &    istack_sphere_grp_rj, name_sphere_grp_rj)
!
      ntot_sphere_grp_rj = istack_sphere_grp_rj(num_sphere_grp_rj)
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
      subroutine set_stack_sph_grp_by_rng(icou, nlayer_st, nlayer_ed,   &
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
      end subroutine set_stack_sph_grp_by_rng
!
! -----------------------------------------------------------------------
!
      end module set_stack_4_sph_groups
