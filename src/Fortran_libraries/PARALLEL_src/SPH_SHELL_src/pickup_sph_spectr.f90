!>@file   pickup_sph_spectr
!!@brief      module pickup_sph_spectr
!!
!!@author H. Matsui
!!@date Programmed in  Dec., 2012
!
!> @brief Make spectrum data list
!!
!!@verbatim
!!      subroutine const_picked_sph_address                             &
!!     &         (l_truncation, sph_rj, pick_list, picked)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(pickup_mode_list), intent(inout) :: pick_list
!!        type(picked_spectrum_data), intent(inout) :: picked
!!
!!      subroutine allocate_iflag_pick_sph(l_truncation)
!!      subroutine deallocate_iflag_pick_sph
!!      subroutine count_picked_sph_adrress(l_truncation,               &
!!     &          num_pick_sph, num_pick_sph_l, num_pick_sph_m,         &
!!     &          idx_pick_sph, idx_pick_sph_l, idx_pick_sph_m,         &
!!     &          num_pickup)
!!      subroutine set_picked_sph_address(l_truncation, sph_rj,         &
!!     &          num_pick_sph, num_pick_sph_l, num_pick_sph_m,         &
!!     &          idx_pick_sph, idx_pick_sph_l, idx_pick_sph_m,         &
!!     &          num_pickup, idx_pick_gl, idx_pick_lc)
!!        type(sph_rj_grid), intent(in) :: sph_rj
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
      subroutine const_picked_sph_address                               &
     &         (l_truncation, sph_rj, pick_list, picked)
!
      use t_spheric_rj_data
      use t_pickup_sph_spectr_data
!
      integer(kind = kint), intent(in) ::l_truncation
      type(sph_rj_grid), intent(in) :: sph_rj
!
      type(pickup_mode_list), intent(inout) :: pick_list
      type(picked_spectrum_data), intent(inout) :: picked
!
!
      call allocate_iflag_pick_sph(l_truncation)
!
      call count_picked_sph_adrress(l_truncation,                       &
     &    pick_list%num_modes, pick_list%num_degree,                    &
     &    pick_list%num_order, pick_list%idx_pick_mode,                 &
     &    pick_list%idx_pick_l, pick_list%idx_pick_m,                   &
     &    picked%num_sph_mode)
!
      call alloc_pick_sph_monitor(picked)
!
      call set_picked_sph_address(l_truncation, sph_rj,                 &
     &    pick_list%num_modes, pick_list%num_degree,                    &
     &    pick_list%num_order, pick_list%idx_pick_mode,                 &
     &    pick_list%idx_pick_l, pick_list%idx_pick_m,                   &
     &    picked%num_sph_mode, picked%idx_gl, picked%idx_lc)
!
      call deallocate_iflag_pick_sph
      call dealloc_pick_sph_mode(pick_list)
!
      end subroutine const_picked_sph_address
!
! -----------------------------------------------------------------------
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
      iflag_picked_sph = -1
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
      subroutine count_picked_sph_adrress(l_truncation,                 &
     &          num_pick_sph, num_pick_sph_l, num_pick_sph_m,           &
     &          idx_pick_sph, idx_pick_sph_l, idx_pick_sph_m,           &
     &          num_pickup)
!
      use spherical_harmonics
!
      integer(kind = kint), intent(in) :: l_truncation
!
      integer(kind = kint), intent(in) :: num_pick_sph
      integer(kind = kint), intent(in) :: num_pick_sph_l
      integer(kind = kint), intent(in) :: num_pick_sph_m
      integer(kind = kint), intent(in) :: idx_pick_sph(num_pick_sph,2)
      integer(kind = kint), intent(in)                                  &
     &                     :: idx_pick_sph_l(num_pick_sph_l)
      integer(kind = kint), intent(in)                                  &
     &                     :: idx_pick_sph_m(num_pick_sph_m)
!
      integer(kind = kint), intent(inout) :: num_pickup
!
      integer(kind = kint) :: l, m, mm, j, inum
!
!
      iflag_picked_sph = -1
      num_pickup = 0
      do inum = 1, num_pick_sph
        l = idx_pick_sph(inum,1)
        m = idx_pick_sph(inum,2)
        if(l .le. l_truncation) then
          j = get_idx_by_full_degree_order(l,m)
          num_pickup = num_pickup + 1
          iflag_picked_sph(j)  = num_pickup
        end if
      end do
!
      do inum = 1, num_pick_sph_l
        l = idx_pick_sph_l(inum)
        if(l .le. l_truncation) then
          do m = -l, l
            j = get_idx_by_full_degree_order(l,m)
            if(iflag_picked_sph(j) .lt. izero) then
              num_pickup = num_pickup + 1
              iflag_picked_sph(j)  = num_pickup
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
            j = get_idx_by_full_degree_order(l,m)
            if(iflag_picked_sph(j) .lt. izero) then
              num_pickup = num_pickup + 1
              iflag_picked_sph(j)  = num_pickup
            end if
          end do
        end if
      end do
!
      end subroutine count_picked_sph_adrress
!
! -----------------------------------------------------------------------
!
      subroutine set_picked_sph_address(l_truncation, sph_rj,           &
     &          num_pick_sph, num_pick_sph_l, num_pick_sph_m,           &
     &          idx_pick_sph, idx_pick_sph_l, idx_pick_sph_m,           &
     &          num_pickup, idx_pick_gl, idx_pick_lc)
!
      use t_spheric_rj_data
      use spherical_harmonics
      use quicksort
!
      integer(kind = kint), intent(in) :: l_truncation
      type(sph_rj_grid), intent(in) :: sph_rj
!
      integer(kind = kint), intent(in) :: num_pick_sph
      integer(kind = kint), intent(in) :: num_pick_sph_l
      integer(kind = kint), intent(in) :: num_pick_sph_m
      integer(kind = kint), intent(in) :: idx_pick_sph(num_pick_sph,2)
      integer(kind = kint), intent(in)                                  &
     &                     :: idx_pick_sph_l(num_pick_sph_l)
      integer(kind = kint), intent(in)                                  &
     &                     :: idx_pick_sph_m(num_pick_sph_m)
!
      integer(kind = kint), intent(in) :: num_pickup
!
      integer(kind = kint), intent(inout) :: idx_pick_gl(num_pickup,3)
      integer(kind = kint), intent(inout) :: idx_pick_lc(num_pickup)
!
      integer(kind = kint) :: l, m, mm, j, icou, inum
      integer(kind = 4) :: l4, m4
!
!
      iflag_picked_sph = -1
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
          idx_pick_lc(icou) = find_local_sph_address(sph_rj, l4, m4)
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
            if(iflag_picked_sph(j) .lt. izero) then
              icou = icou + 1
              idx_pick_gl(icou,1) = j
              idx_pick_lc(icou)                                         &
     &           = find_local_sph_address(sph_rj, l4, m4)
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
            if(iflag_picked_sph(j) .lt. izero) then
              icou = icou + 1
              idx_pick_gl(icou,1) = j
              idx_pick_lc(icou)                                         &
     &              = find_local_sph_address(sph_rj, l4, m4)
              iflag_picked_sph(j)  = icou
            end if
          end do
        end if
      end do
!
      call quicksort_w_index(num_pickup, idx_pick_gl(1,1),              &
     &    ione, num_pickup, idx_pick_lc(1))
!
      do icou = 1, num_pickup
        call get_degree_order_by_full_j(idx_pick_gl(icou,1),            &
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
