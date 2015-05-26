!set_item_4_sph_groups.f90
!      module set_item_4_sph_groups
!
      module set_item_4_sph_groups
!
!     Written by H. Matsui on July, 2007
!
      use m_precision
      use m_constants
!
      implicit none
!
      private :: set_item_sph_grp_by_list, set_item_sph_grp_by_rng
!
!      subroutine set_item_rtp_radial_grp
!      subroutine set_item_rj_radial_grp
!      subroutine set_item_rj_spectr_grp
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_item_rtp_radial_grp
!
      use m_sph_1d_global_index
      use m_spheric_parameter
      use m_group_data_sph_specr
!
      integer(kind = kint) :: icou, nlayer_ed, inum
!
      icou = 0
      call set_item_sph_grp_by_list(icou, nlayer_ICB, nlayer_ICB,       &
     &    nidx_rtp(1), idx_gl_1d_rtp_r(1), ntot_radial_grp_rtp,         &
     &    item_radial_grp_rtp)
!
      call set_item_sph_grp_by_list(icou, nlayer_CMB, nlayer_CMB,       &
     &    nidx_rtp(1), idx_gl_1d_rtp_r(1), ntot_radial_grp_rtp,         &
     &    item_radial_grp_rtp)
!
      call set_item_sph_grp_by_list(icou,                               &
     &    nlayer_2_center, nlayer_2_center, nidx_rtp(1),                &
     &    idx_gl_1d_rtp_r(1), ntot_radial_grp_rtp, item_radial_grp_rtp)
!
      if (nidx_global_rtp(1) .gt. nlayer_CMB) then
        call set_item_sph_grp_by_list(icou,                             &
     &    nidx_global_rtp(1), nidx_global_rtp(1), nidx_rtp(1),          &
     &    idx_gl_1d_rtp_r(1), ntot_radial_grp_rtp, item_radial_grp_rtp)
      end if
!
      if (nlayer_mid_OC .gt. 0) then
        call set_item_sph_grp_by_list(icou,                             &
     &    nlayer_mid_OC, nlayer_mid_OC, nidx_rtp(1),                    &
     &    idx_gl_1d_rtp_r(1), ntot_radial_grp_rtp, item_radial_grp_rtp)
      end if
!
      nlayer_ed = nlayer_ICB-1
      call set_item_sph_grp_by_list(icou, nlayer_2_center, nlayer_ed,   &
     &    nidx_rtp(1), idx_gl_1d_rtp_r(1), ntot_radial_grp_rtp,         &
     &    item_radial_grp_rtp)
!
      call set_item_sph_grp_by_list(icou, nlayer_ICB, nlayer_CMB,       &
     &    nidx_rtp(1), idx_gl_1d_rtp_r(1), ntot_radial_grp_rtp,         &
     &    item_radial_grp_rtp)
!
!
      do inum = 1, numlayer_sph_bc
        call set_item_sph_grp_by_list(icou,                             &
     &      kr_sph_boundary(inum), kr_sph_boundary(inum),               &
     &      nidx_rtp(1), idx_gl_1d_rtp_r(1), ntot_radial_grp_rtp,       &
     &      item_radial_grp_rtp)
      end do
!
      end subroutine set_item_rtp_radial_grp
!
! -----------------------------------------------------------------------
!
      subroutine set_item_rj_radial_grp
!
      use m_sph_1d_global_index
      use m_spheric_parameter
      use m_group_data_sph_specr
!
      integer(kind = kint) :: icou, nlayer_ed, inum
!
!
      icou = 0
      call set_item_sph_grp_by_rng(icou, nlayer_ICB, nlayer_ICB,        &
     &    ist_rj(1), ied_rj(1), ntot_radial_grp_rj, item_radial_grp_rj)
!
      call set_item_sph_grp_by_rng(icou, nlayer_CMB, nlayer_CMB,        &
     &    ist_rj(1), ied_rj(1), ntot_radial_grp_rj, item_radial_grp_rj)
!
      call set_item_sph_grp_by_rng(icou,                                &
     &    nlayer_2_center, nlayer_2_center, ist_rj(1), ied_rj(1),       &
     &    ntot_radial_grp_rj, item_radial_grp_rj)
!
      if (nidx_global_rtp(1) .gt. nlayer_CMB) then
        call set_item_sph_grp_by_rng(icou, nidx_global_rtp(1),          &
     &      nidx_global_rtp(1), ist_rj(1), ied_rj(1),                   &
     &      ntot_radial_grp_rj, item_radial_grp_rj)
      end if
!
      if (nlayer_mid_OC .gt. 0) then
        call set_item_sph_grp_by_rng(icou,                              &
     &      nlayer_mid_OC, nlayer_mid_OC, ist_rj(1), ied_rj(1),         &
     &      ntot_radial_grp_rj, item_radial_grp_rj)
      end if
!
      nlayer_ed = nlayer_ICB-1
      call set_item_sph_grp_by_rng(icou, nlayer_2_center, nlayer_ed,    &
     &    ist_rj(1), ied_rj(1), ntot_radial_grp_rj,                     &
     &    item_radial_grp_rj)
!
      call set_item_sph_grp_by_rng(icou, nlayer_ICB, nlayer_CMB,        &
     &    ist_rj(1), ied_rj(1), ntot_radial_grp_rj, item_radial_grp_rj)
!
      do inum = 1, numlayer_sph_bc
        call set_item_sph_grp_by_rng(icou,                              &
     &      kr_sph_boundary(inum), kr_sph_boundary(inum),               &
     &      ist_rj(1), ied_rj(1), ntot_radial_grp_rj,                   &
     &      item_radial_grp_rj)
      end do
!
      end subroutine set_item_rj_radial_grp
!
! -----------------------------------------------------------------------
!
      subroutine set_item_rj_spectr_grp
!
      use m_spheric_parameter
      use m_group_data_sph_specr
!
      integer(kind = kint) :: icou
!
      icou = 0
      call set_item_sph_grp_by_rng(icou, izero, izero,                  &
     &    ist_rj(2), ied_rj(2), ntot_sphere_grp_rj, item_sphere_grp_rj)
!
      call set_item_sph_grp_by_rng(icou, ione, ione,                    &
     &    ist_rj(2), ied_rj(2), ntot_sphere_grp_rj, item_sphere_grp_rj)
!
      call set_item_sph_grp_by_rng(icou, itwo, itwo,                    &
     &    ist_rj(2), ied_rj(2), ntot_sphere_grp_rj, item_sphere_grp_rj)
!
      call set_item_sph_grp_by_rng(icou, ithree, ithree,                &
     &    ist_rj(2), ied_rj(2), ntot_sphere_grp_rj, item_sphere_grp_rj)
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
      subroutine set_item_sph_grp_by_rng(icou, nlayer_st, nlayer_ed,    &
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
      end subroutine set_item_sph_grp_by_rng
!
! ----------------------------------------------------------------------
!
      end module set_item_4_sph_groups
