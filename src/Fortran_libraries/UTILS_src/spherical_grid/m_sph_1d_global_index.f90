!>@file   m_sph_1d_global_index.f90
!!@brief  module m_sph_1d_global_index
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in July, 2007
!
!>@brief  Global addresses for soherical harmonics indices
!!
!!@verbatim
!!      subroutine allocate_sph_1d_global_stack
!!      subroutine allocate_sph_1d_global_idx
!!
!!      subroutine deallocate_sph_1d_global_stack
!!      subroutine deallocate_sph_1d_global_idx
!!
!!      subroutine allocate_sph_radial_group
!!      subroutine deallocate_sph_radial_group
!!
!!      subroutine check_spheric_global_stack(ip_rank)
!!@endverbatim
!
!
      module m_sph_1d_global_index
!
      use m_precision
!
      implicit none
!
!>      Global radial address for f(r,t,p)
      integer(kind = kint), allocatable :: istack_idx_local_rtp_r(:)
!>      Global radial address for f(r,t,p)
      integer(kind = kint), allocatable :: istack_idx_local_rtp_t(:)
!>      Global zonal grid address for f(r,t,p)
      integer(kind = kint), allocatable :: istack_idx_local_rtp_p(:)
!>      Global radial address for f(r,t,m)
      integer(kind = kint), allocatable :: istack_idx_local_rtm_r(:)
!>      Global meridional grid address for f(r,t,m)
      integer(kind = kint), allocatable :: istack_idx_local_rtm_t(:)
!>      Global zonal mode address for f(r,t,m)
      integer(kind = kint), allocatable :: istack_idx_local_rtm_m(:)
!>      Global radial address for f(r,l,m)
      integer(kind = kint), allocatable :: istack_idx_local_rlm_r(:)
!>      Global spherical harmonics mode address for f(r,l,m)
      integer(kind = kint), allocatable :: istack_idx_local_rlm_j(:)
!>      Global radial address for f(r,j)
      integer(kind = kint), allocatable :: istack_idx_local_rj_r(:)
!>      Global spherical harmonics mode address for f(r,j)
      integer(kind = kint), allocatable :: istack_idx_local_rj_j(:)
!
!
!>      Global radial address for f(r,t,p)
      integer(kind = kint), allocatable :: idx_global_rtp_r(:)
!>      Global radial address for f(r,t,p)
      integer(kind = kint), allocatable :: idx_global_rtp_t(:)
!>      Global zonal grid address for f(r,t,p)
      integer(kind = kint), allocatable :: idx_global_rtp_p(:,:)
!>      Global radial address for f(r,t,m)
      integer(kind = kint), allocatable :: idx_global_rtm_r(:)
!>      Global meridional grid address for f(r,t,m)
      integer(kind = kint), allocatable :: idx_global_rtm_t(:)
!>      Global zonal mode address for f(r,t,m)
      integer(kind = kint), allocatable :: idx_global_rtm_m(:,:)
!>      Global radial address for f(r,l,m)
      integer(kind = kint), allocatable :: idx_global_rlm_r(:)
!>      Global spherical harmonics mode address for f(r,l,m)
      integer(kind = kint), allocatable :: idx_global_rlm_j(:,:)
!>      Global radial address for f(r,j)
      integer(kind = kint), allocatable :: idx_global_rj_r(:)
!>      Global spherical harmonics mode address for f(r,j)
      integer(kind = kint), allocatable :: idx_global_rj_j(:,:)
!
!
!>      Number of radial group from control
      integer(kind = kint) :: numlayer_sph_bc
!>      global radial address for each group
      integer(kind = kint), allocatable :: kr_sph_boundary(:)
!>      name of radial group
      character(len = kchara), allocatable :: sph_bondary_name(:)
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_sph_1d_global_stack
!
      use m_spheric_parameter
!
      integer(kind = kint) :: num
!
!
      num = ndomain_rtp(1)
      allocate(istack_idx_local_rtp_r(0:num))
      num = ndomain_rtp(2)
      allocate(istack_idx_local_rtp_t(0:num))
      num = ndomain_rtp(3)
      allocate(istack_idx_local_rtp_p(0:num))
!
      num = ndomain_rtm(1)
      allocate(istack_idx_local_rtm_r(0:num))
      num = ndomain_rtm(2)
      allocate(istack_idx_local_rtm_t(0:num))
      num = ndomain_rtm(3)
      allocate(istack_idx_local_rtm_m(0:num))
!
      num = ndomain_rlm(1)
      allocate(istack_idx_local_rlm_r(0:num))
      num = ndomain_rlm(2)
      allocate(istack_idx_local_rlm_j(0:num))
!
      num = ndomain_rj(1)
      allocate(istack_idx_local_rj_r(0:num))
      num = ndomain_rj(2)
      allocate(istack_idx_local_rj_j(0:num))
!
      istack_idx_local_rtp_r = 0
      istack_idx_local_rtp_t = 0
      istack_idx_local_rtp_p = 0
      istack_idx_local_rtm_r = 0
      istack_idx_local_rtm_t = 0
      istack_idx_local_rtm_m = 0
      istack_idx_local_rlm_r = 0
      istack_idx_local_rlm_j = 0
      istack_idx_local_rj_r =  0
      istack_idx_local_rj_j =  0
!
      end subroutine allocate_sph_1d_global_stack
!
! -----------------------------------------------------------------------
!
      subroutine allocate_sph_1d_global_idx
!
      use m_spheric_parameter
!
      integer(kind = kint) :: n1, n2, n3
      integer(kind = kint) :: ist_1, ist_2, ist_3
      integer(kind = kint) :: ied_1, ied_2, ied_3
!
!
      n1 = ndomain_rtp(1)
      n2 = ndomain_rtp(2)
      n3 = ndomain_rtp(3)
      ist_1 = istack_idx_local_rtp_r(0) + 1
      ied_1 = istack_idx_local_rtp_r(n1)
      ist_2 = istack_idx_local_rtp_t(0) + 1
      ied_2 = istack_idx_local_rtp_t(n2)
      ist_3 = istack_idx_local_rtp_p(0) + 1
      ied_3 = istack_idx_local_rtp_p(n3)
!
      allocate( idx_global_rtp_r(ist_1:ied_1) )
      allocate( idx_global_rtp_t(ist_2:ied_2) )
      allocate( idx_global_rtp_p(ist_3:ied_3,2) )
!
      idx_global_rtp_r = 0
      idx_global_rtp_t = 0
      idx_global_rtp_p = 0
!
!
      n1 = ndomain_rtm(1)
      n2 = ndomain_rtm(2)
      n3 = ndomain_rtm(3)
      ist_1 = istack_idx_local_rtm_r(0) + 1
      ied_1 = istack_idx_local_rtm_r(n1)
      ist_2 = istack_idx_local_rtm_t(0) + 1
      ied_2 = istack_idx_local_rtm_t(n2)
      ist_3 = istack_idx_local_rtm_m(0) + 1
      ied_3 = istack_idx_local_rtm_m(n3)
!
      allocate( idx_global_rtm_r(ist_1:ied_1) )
      allocate( idx_global_rtm_t(ist_2:ied_2) )
      allocate( idx_global_rtm_m(ist_3:ied_3,2) )
!
      idx_global_rtm_r = 0
      idx_global_rtm_t = 0
      idx_global_rtm_m = 0
!
!
      n1 = ndomain_rlm(1)
      n2 = ndomain_rlm(2)
      ist_1 = istack_idx_local_rlm_r(0) + 1
      ied_1 = istack_idx_local_rlm_r(n1)
      ist_2 = istack_idx_local_rlm_j(0) + 1
      ied_2 = istack_idx_local_rlm_j(n2)
!
      allocate( idx_global_rlm_r(ist_1:ied_1) )
      allocate( idx_global_rlm_j(ist_2:ied_2,3) )
!
      idx_global_rlm_r = 0
      idx_global_rlm_j = 0
!
!
      n1 = ndomain_rj(1)
      n2 = ndomain_rj(2)
      ist_1 = istack_idx_local_rj_r(0) + 1
      ied_1 = istack_idx_local_rj_r(n1)
      ist_2 = istack_idx_local_rj_j(0) + 1
      ied_2 = istack_idx_local_rj_j(n2)
!
      allocate( idx_global_rj_r(ist_1:ied_1) )
      allocate( idx_global_rj_j(ist_2:ied_2,3) )
!
      idx_global_rj_r = 0
      idx_global_rj_j = 0
!
      end subroutine allocate_sph_1d_global_idx
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine deallocate_sph_1d_global_stack
!
!
      deallocate(istack_idx_local_rtp_r)
      deallocate(istack_idx_local_rtp_t)
      deallocate(istack_idx_local_rtp_p)
!
      deallocate(istack_idx_local_rtm_r)
      deallocate(istack_idx_local_rtm_t)
      deallocate(istack_idx_local_rtm_m)
!
      deallocate(istack_idx_local_rlm_r)
      deallocate(istack_idx_local_rlm_j)
!
      deallocate(istack_idx_local_rj_r)
      deallocate(istack_idx_local_rj_j)
!
      end subroutine deallocate_sph_1d_global_stack
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_sph_1d_global_idx
!
!
      deallocate( idx_global_rtp_r, idx_global_rtp_t, idx_global_rtp_p)
      deallocate( idx_global_rtm_r, idx_global_rtm_t, idx_global_rtm_m)
      deallocate( idx_global_rlm_r, idx_global_rlm_j)
      deallocate( idx_global_rj_r, idx_global_rj_j)
!
      end subroutine deallocate_sph_1d_global_idx
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine allocate_sph_radial_group
!
!
      allocate( kr_sph_boundary(numlayer_sph_bc))
      allocate( sph_bondary_name(numlayer_sph_bc))
      if(numlayer_sph_bc .gt. 0) kr_sph_boundary = 0
!
      end subroutine allocate_sph_radial_group
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_sph_radial_group
!
!
      deallocate( kr_sph_boundary, sph_bondary_name)
!
      end subroutine deallocate_sph_radial_group
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_spheric_global_stack(my_rank)
!
      use m_spheric_parameter
!
!
      integer(kind = kint), intent(in) :: my_rank
!
      write(my_rank+50,*) 'istack_idx_local_rtp_r',                     &
     &     istack_idx_local_rtp_r
      write(my_rank+50,*) 'istack_idx_local_rtp_t',                     &
     &     istack_idx_local_rtp_t
      write(my_rank+50,*) 'istack_idx_local_rtp_p',                     &
     &     istack_idx_local_rtp_p
!
      write(my_rank+50,*) 'istack_idx_local_rtm_r',                     &
     &     istack_idx_local_rtm_r
      write(my_rank+50,*) 'istack_idx_local_rtm_t',                     &
     &     istack_idx_local_rtm_t
      write(my_rank+50,*) 'istack_idx_local_rtm_m',                     &
     &     istack_idx_local_rtm_m
!
      write(my_rank+50,*) 'istack_idx_local_rlm_r',                     &
     &     istack_idx_local_rlm_r
      write(my_rank+50,*) 'istack_idx_local_rlm_j',                     &
     &     istack_idx_local_rlm_j
!
      write(my_rank+50,*) 'istack_idx_local_rj_r',                      &
     &     istack_idx_local_rj_r
      write(my_rank+50,*) 'istack_idx_local_rj_j',                      &
     &     istack_idx_local_rj_j
!
      end subroutine check_spheric_global_stack
!
! -----------------------------------------------------------------------
!
      end module m_sph_1d_global_index
