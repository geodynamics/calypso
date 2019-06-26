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
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!      subroutine set_scale_4_vect_l0(num_pickup,                      &
!!     &          idx_pick_gl, scale_for_zero)
!!@endverbatim
!
      module pickup_sph_spectr
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use t_spheric_rj_data
!
      implicit  none
!
      integer(kind = kint), allocatable :: iflag_picked_sph(:)
      private :: iflag_picked_sph
!
      private :: count_picked_sph_adrress, set_picked_sph_address
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_picked_sph_address                               &
     &         (iflag_center, l_truncation, sph_rj, pick_list, picked)
!
      use t_spheric_rj_data
      use t_pickup_sph_spectr_data
!
      integer(kind = kint), intent(in) :: iflag_center
      integer(kind = kint), intent(in) :: l_truncation
      type(sph_rj_grid), intent(in) :: sph_rj
!
      type(pickup_mode_list), intent(inout) :: pick_list
      type(picked_spectrum_data), intent(inout) :: picked
!
      integer :: i
!
!
      call allocate_iflag_pick_sph(l_truncation)
!
      call count_picked_sph_adrress(l_truncation, sph_rj,               &
     &    pick_list%num_modes, pick_list%num_degree,                    &
     &    pick_list%num_order, pick_list%idx_pick_mode,                 &
     &    pick_list%idx_pick_l, pick_list%idx_pick_m,                   &
     &    picked%num_sph_mode, picked%num_sph_mode_lc)
!
      call alloc_pickup_sph_spec_local(nprocs, picked)
      call alloc_pick_sph_monitor(picked)
!
      call set_picked_sph_address(iflag_center, l_truncation, sph_rj,   &
     &    pick_list%num_modes, pick_list%num_degree,                    &
     &    pick_list%num_order, pick_list%idx_pick_mode,                 &
     &    pick_list%idx_pick_l, pick_list%idx_pick_m,                   &
     &    picked%num_sph_mode_lc, picked%idx_out)
!
      if(picked%idx_out(0,4) .gt. 0) then 
        picked%ntot_pick_spectr_lc = picked%ntot_pick_spectr_lc + 1
      end if
      call MPI_Allgather                                                &
     &   (picked%ntot_pick_spectr_lc, 1, CALYPSO_INTEGER,               &
     &    picked%istack_picked_spec_lc(1), 1, CALYPSO_INTEGER,          &
     &    CALYPSO_COMM, ierr_MPI)
!
      picked%istack_picked_spec_lc(0) = 0
      do i = 1, nprocs
        picked%istack_picked_spec_lc(i)                                 &
     &       =  picked%istack_picked_spec_lc(i-1)                       &
     &        + picked%istack_picked_spec_lc(i)
      end do
      picked%ntot_pick_spectr = picked%istack_picked_spec_lc(nprocs) 
!
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
      subroutine count_picked_sph_adrress(l_truncation, sph_rj,         &
     &          num_pick_sph, num_pick_sph_l, num_pick_sph_m,           &
     &          idx_pick_sph, idx_pick_sph_l, idx_pick_sph_m,           &
     &          num_pickup, num_pickup_lc)
!
      use spherical_harmonics
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
      integer(kind = kint), intent(inout) :: num_pickup
      integer(kind = kint), intent(inout) :: num_pickup_lc
!
      integer(kind = kint) :: l, m, mm, j_gl, j_lc, inum
!
!
      iflag_picked_sph = -1
      num_pickup = 0
      num_pickup_lc = 0
      do inum = 1, num_pick_sph
        l = idx_pick_sph(inum,1)
        m = idx_pick_sph(inum,2)
        if(l .le. l_truncation) then
          j_gl = get_idx_by_full_degree_order(l,m)
          j_lc = find_local_sph_address(sph_rj, int(l), int(m))
          num_pickup = num_pickup + 1
          iflag_picked_sph(j_gl)  = num_pickup
          if(j_lc .gt. 0) num_pickup_lc = num_pickup_lc + 1
        end if
      end do
!
      do inum = 1, num_pick_sph_l
        l = idx_pick_sph_l(inum)
        if(l .le. l_truncation) then
          do m = -l, l
            j_gl = get_idx_by_full_degree_order(l,m)
            j_lc = find_local_sph_address(sph_rj, int(l), int(m))
            if(iflag_picked_sph(j_gl) .lt. izero) then
              num_pickup = num_pickup + 1
              iflag_picked_sph(j_gl)  = num_pickup
              if(j_lc .gt. 0) num_pickup_lc = num_pickup_lc + 1
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
            j_gl = get_idx_by_full_degree_order(l,m)
            j_lc = find_local_sph_address(sph_rj, int(l), int(m))
            if(iflag_picked_sph(j_gl) .lt. izero) then
              num_pickup = num_pickup + 1
              iflag_picked_sph(j_gl)  = num_pickup
              if(j_lc .gt. 0) num_pickup_lc = num_pickup_lc + 1
            end if
          end do
        end if
      end do
!
      end subroutine count_picked_sph_adrress
!
! -----------------------------------------------------------------------
!
      subroutine set_picked_sph_address                                 &
     &         (iflag_center, l_truncation, sph_rj,                     &
     &          num_pick_sph, num_pick_sph_l, num_pick_sph_m,           &
     &          idx_pick_sph, idx_pick_sph_l, idx_pick_sph_m,           &
     &          num_pickup_lc, idx_pickup)
!
      use spherical_harmonics
      use quicksort
!
      integer(kind = kint), intent(in) :: iflag_center, l_truncation
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
      integer(kind = kint), intent(in) :: num_pickup_lc
!
      integer(kind = kint), intent(inout)                               &
     &                     :: idx_pickup(0:num_pickup_lc,4)
!
      integer(kind = kint) :: l, m, mm, j_lc, j_gl, icou, inum, jcou
!
!
      iflag_picked_sph = -1
      icou = 0
      jcou = 0
      do inum = 1, num_pick_sph
        l = idx_pick_sph(inum,1)
        m = idx_pick_sph(inum,2)
        j_gl = get_idx_by_full_degree_order(l,m)
        j_lc = find_local_sph_address(sph_rj, int(l), int(m))
        if(l .le. l_truncation) then
          icou = icou + 1
          iflag_picked_sph(j_gl)  = icou
!
          if(j_lc .gt. 0) then
            jcou = jcou + 1
            idx_pickup(jcou,3) = j_gl
            idx_pickup(jcou,4) = j_lc
          end if
        end if
      end do
!
      do inum = 1, num_pick_sph_l
        l = idx_pick_sph_l(inum)
        if(l .le. l_truncation) then
          do m = -l, l
            j_gl = get_idx_by_full_degree_order(l,m)
            j_lc = find_local_sph_address(sph_rj, int(l), int(m))
            if(iflag_picked_sph(j_gl) .lt. izero) then
              icou = icou + 1
              iflag_picked_sph(j_gl)  = icou
!
              if(j_lc .gt. 0) then
                jcou = jcou + 1
                idx_pickup(jcou,3) = j_gl
                idx_pickup(jcou,4) = j_lc
              end if
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
            j_gl = get_idx_by_full_degree_order(l,m)
            j_lc = find_local_sph_address(sph_rj, int(l), int(m))
            if(iflag_picked_sph(j_gl) .lt. izero) then
              icou = icou + 1
              iflag_picked_sph(j_gl)  = icou
!
              if(j_lc .gt. 0) then
                jcou = jcou + 1
                idx_pickup(jcou,3) = j_gl
                idx_pickup(jcou,4) = j_lc
              end if
            end if
          end do
        end if
      end do
!
      if(num_pickup_lc .le. 0) return
!
      call quicksort_w_index(num_pickup_lc, idx_pickup(1,3),            &
     &    ione, num_pickup_lc, idx_pickup(1,4))
!
      do jcou = 1, num_pickup_lc
        call get_degree_order_by_full_j(idx_pickup(jcou,3),             &
     &      idx_pickup(jcou,1), idx_pickup(jcou,2))
      end do
!
      if(iflag_center .le. 0) return
      if(idx_pickup(1,3) .eq. 0) then
        idx_pickup(0,1:3) = 0
        idx_pickup(0,4) = sph_rj%inod_rj_center
      end if
!
      end subroutine set_picked_sph_address
!
! -----------------------------------------------------------------------
!
      subroutine set_scale_4_vect_l0(num_pickup,                        &
     &          idx_pick_gl, scale_for_zero)
!
      integer(kind = kint), intent(in) :: num_pickup
      integer(kind = kint), intent(in) :: idx_pick_gl(num_pickup,3)
      real(kind = kreal), intent(inout) :: scale_for_zero(num_pickup)
!
      integer(kind = kint) :: inum
!
!
      do inum = 1, num_pickup
        if(idx_pick_gl(inum,1) .eq. 0) then
          scale_for_zero(inum) = half
        else
          scale_for_zero(inum) = one
        end if
      end do
!
      end subroutine set_scale_4_vect_l0
!
! -----------------------------------------------------------------------
!
      end module pickup_sph_spectr
