!>@file   pickup_sph_spectr
!!@brief      module pickup_sph_spectr
!!
!!@author H. Matsui
!!@date Programmed in  Dec., 2012
!
!> @brief Make spectrum data list
!!
!!@verbatim
!!      subroutine allocate_iflag_pick_sph(l_truncation)
!!      subroutine deallocate_iflag_pick_sph
!!      subroutine count_picked_sph_adrress                             &
!!     &     (num_pick_sph, num_pick_sph_l, num_pick_sph_m,             &
!!     &      idx_pick_sph, idx_pick_sph_l, idx_pick_sph_m, ntot_pickup)
!!      subroutine set_picked_sph_address                               &
!!     &         (num_pick_sph, num_pick_sph_l, num_pick_sph_m,         &
!!     &          idx_pick_sph, idx_pick_sph_l, idx_pick_sph_m,         &
!!     &          ntot_pickup, num_pickup, idx_pick_gl, idx_pick_lc)
!!      subroutine set_scale_4_vect_l0(num_pickup,                      &
!!     &          idx_pick_gl, scale_for_zelo)
!!@endverbatim
!
      module pickup_sph_spectr
!
      use m_precision
      use m_constants
!
      implicit  none
!
      integer(kind = kint), allocatable :: iflag_picked_sph(:)
      private :: iflag_picked_sph
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_iflag_pick_sph(l_truncation)
!
      integer(kind = kint), intent(in) :: l_truncation
      integer(kind = kint) :: num
!
      num = l_truncation*(l_truncation+2)
      allocate( iflag_picked_sph(0:num) )
!
      iflag_picked_sph = 0
!
      end subroutine allocate_iflag_pick_sph
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_iflag_pick_sph
!
      deallocate( iflag_picked_sph )
!
      end subroutine deallocate_iflag_pick_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine count_picked_sph_adrress                               &
     &     (num_pick_sph, num_pick_sph_l, num_pick_sph_m,               &
     &      idx_pick_sph, idx_pick_sph_l, idx_pick_sph_m, ntot_pickup)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: num_pick_sph
      integer(kind = kint), intent(in) :: num_pick_sph_l
      integer(kind = kint), intent(in) :: num_pick_sph_m
      integer(kind = kint), intent(in) :: idx_pick_sph(num_pick_sph,2)
      integer(kind = kint), intent(in) :: idx_pick_sph_l(num_pick_sph_l)
      integer(kind = kint), intent(in) :: idx_pick_sph_m(num_pick_sph_m)
!
      integer(kind = kint), intent(inout) :: ntot_pickup
!
      integer(kind = kint) :: inum, mm
!
!
      ntot_pickup = 0
      do inum = 1, num_pick_sph
        if(idx_pick_sph(inum,1) .le. l_truncation) then
          ntot_pickup = ntot_pickup + 1
        end if
      end do
      do inum = 1, num_pick_sph_l
        if(idx_pick_sph_l(inum) .le. l_truncation) then
          ntot_pickup = ntot_pickup + 2*idx_pick_sph_l(inum) + 1
        end if
      end do
      do inum = 1, num_pick_sph_m
        mm = abs(idx_pick_sph_m(inum))
        if(mm .le. l_truncation) then
          ntot_pickup = ntot_pickup + l_truncation + 1 - mm
        end if
      end do
!
      end subroutine count_picked_sph_adrress
!
! -----------------------------------------------------------------------
!
      subroutine set_picked_sph_address                                 &
     &         (num_pick_sph, num_pick_sph_l, num_pick_sph_m,           &
     &          idx_pick_sph, idx_pick_sph_l, idx_pick_sph_m,           &
     &          ntot_pickup, num_pickup, idx_pick_gl, idx_pick_lc)
!
      use m_spheric_parameter
      use spherical_harmonics
      use quicksort
!
      integer(kind = kint), intent(in) :: num_pick_sph
      integer(kind = kint), intent(in) :: num_pick_sph_l
      integer(kind = kint), intent(in) :: num_pick_sph_m
      integer(kind = kint), intent(in) :: idx_pick_sph(num_pick_sph,2)
      integer(kind = kint), intent(in) :: idx_pick_sph_l(num_pick_sph_l)
      integer(kind = kint), intent(in) :: idx_pick_sph_m(num_pick_sph_m)
!
      integer(kind = kint), intent(in) :: ntot_pickup
!
      integer(kind = kint), intent(inout) :: num_pickup
      integer(kind = kint), intent(inout) :: idx_pick_gl(ntot_pickup,3)
      integer(kind = kint), intent(inout) :: idx_pick_lc(ntot_pickup)
!
      integer(kind = kint) :: l, m, mm, j, icou, inum
      integer(kind = 4) :: l4, m4
!
!
      icou = 0
      do inum = 1, num_pick_sph
        l = idx_pick_sph(inum,1)
        m = idx_pick_sph(inum,2)
        l4 = int(l)
        m4 = int(m)
        j = get_idx_by_full_degree_order(l,m)
        if(l .le. l_truncation) then
          icou = icou + 1
          idx_pick_gl(icou,1) = j
          idx_pick_lc(icou) = find_local_sph_mode_address(l4, m4)
          iflag_picked_sph(j)  = icou
        end if
      end do
!
      do inum = 1, num_pick_sph_l
        l = idx_pick_sph_l(inum)
        if(l .le. l_truncation) then
          do m = -l, l
           l4 = int(l)
           m4 = int(m)
           j = get_idx_by_full_degree_order(l,m)
            if(iflag_picked_sph(j) .le. izero) then
              icou = icou + 1
              idx_pick_gl(icou,1) = j
              idx_pick_lc(icou) = find_local_sph_mode_address(l4, m4)
              iflag_picked_sph(j)  = icou
            end if
          end do
        end if
      end do
!
      do inum = 1, num_pick_sph_m
        m = idx_pick_sph_m(inum)
        mm = abs(m)
        if(mm .le. l_truncation) then
          do l = mm, l_truncation
            l4 = int(l)
            m4 = int(m)
            j = get_idx_by_full_degree_order(l,m)
            if(iflag_picked_sph(j) .le. izero) then
              icou = icou + 1
              idx_pick_gl(icou,1) = j
              idx_pick_lc(icou) = find_local_sph_mode_address(l4, m4)
              iflag_picked_sph(j)  = icou
            end if
          end do
        end if
      end do
      num_pickup = icou
!
      call quicksort_w_index(num_pickup, idx_pick_gl(1,1),              &
     &    ione, num_pickup, idx_pick_lc(1))
!
      do icou = 1, num_pickup
        call get_dgree_order_by_full_j(idx_pick_gl(icou,1),             &
     &      idx_pick_gl(icou,2), idx_pick_gl(icou,3))
      end do
!
      end subroutine set_picked_sph_address
!
! -----------------------------------------------------------------------
!
      subroutine set_scale_4_vect_l0(num_pickup,                        &
     &          idx_pick_gl, scale_for_zelo)
!
      integer(kind = kint), intent(in) :: num_pickup
      integer(kind = kint), intent(in) :: idx_pick_gl(num_pickup,3)
      real(kind = kreal), intent(inout) :: scale_for_zelo(num_pickup)
!
      integer(kind = kint) :: inum
!
!
      do inum = 1, num_pickup
        if(idx_pick_gl(inum,1) .eq. 0) then
          scale_for_zelo(inum) = half
        else
          scale_for_zelo(inum) = one
        end if
      end do
!
      end subroutine set_scale_4_vect_l0
!
! -----------------------------------------------------------------------
!
      end module pickup_sph_spectr
