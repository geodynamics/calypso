!pickup_sph_spectr.f90
!      module pickup_sph_spectr
!
!        programmed by H.Matsui on Dec., 2012
!
!      subroutine allocate_iflag_pick_sph(l_truncation)
!      subroutine deallocate_iflag_pick_sph
!
!!      subroutine count_picked_sph_adrress(l_truncation,               &
!!     &          num_pick_sph, num_pick_sph_l, num_pick_sph_m,         &
!!     &          idx_pick_sph_l, idx_pick_sph_m, ntot_pickup)
!!      subroutine set_picked_sph_adrress(l_truncation, jmax, idx_gl_j, &
!!     &          num_pick_sph, num_pick_sph_l, num_pick_sph_m,         &
!!     &          idx_pick_sph, idx_pick_sph_l, idx_pick_sph_m,         &
!!     &          ntot_pickup, num_pickup, idx_pick_gl, idx_pick_lc)
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
      subroutine count_picked_sph_adrress(l_truncation,                 &
     &          num_pick_sph, num_pick_sph_l, num_pick_sph_m,           &
     &          idx_pick_sph_l, idx_pick_sph_m, ntot_pickup)
!
      integer(kind = kint), intent(in) :: l_truncation
!
      integer(kind = kint), intent(in) :: num_pick_sph
      integer(kind = kint), intent(in) :: num_pick_sph_l
      integer(kind = kint), intent(in) :: num_pick_sph_m
      integer(kind = kint), intent(in) :: idx_pick_sph_l(num_pick_sph_l)
      integer(kind = kint), intent(in) :: idx_pick_sph_m(num_pick_sph_m)
!
      integer(kind = kint), intent(inout) :: ntot_pickup
!
      integer(kind = kint) :: inum, mm
!
!
      ntot_pickup = num_pick_sph
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
      subroutine set_picked_sph_adrress(l_truncation, jmax, idx_gl_j,   &
     &          num_pick_sph, num_pick_sph_l, num_pick_sph_m,           &
     &          idx_pick_sph, idx_pick_sph_l, idx_pick_sph_m,           &
     &          ntot_pickup, num_pickup, idx_pick_gl, idx_pick_lc)
!
      integer(kind = kint), intent(in) :: l_truncation, jmax
      integer(kind = kint), intent(in) :: idx_gl_j(jmax)
!
      integer(kind = kint), intent(in) :: num_pick_sph
      integer(kind = kint), intent(in) :: num_pick_sph_l
      integer(kind = kint), intent(in) :: num_pick_sph_m
      integer(kind = kint), intent(in) :: idx_pick_sph(num_pick_sph)
      integer(kind = kint), intent(in) :: idx_pick_sph_l(num_pick_sph_l)
      integer(kind = kint), intent(in) :: idx_pick_sph_m(num_pick_sph_m)
!
      integer(kind = kint), intent(in) :: ntot_pickup
!
      integer(kind = kint), intent(inout) :: num_pickup
      integer(kind = kint), intent(inout) :: idx_pick_gl(ntot_pickup)
      integer(kind = kint), intent(inout) :: idx_pick_lc(ntot_pickup)
!
      integer(kind = kint) :: l, m, mm, j, icou, inum
!
!
      icou = 0
      do inum = 1, num_pick_sph
        j = idx_pick_sph(inum)
        if(j .le. l_truncation*(l_truncation+2)) then
          icou = icou + 1
          idx_pick_gl(icou) = j
          iflag_picked_sph(j)  = icou
        end if
      end do
!
      do inum = 1, num_pick_sph_l
        l = idx_pick_sph_l(inum)
        if(l .le. l_truncation) then
          do m = -l, l
            j = l*(l+1) + m
            if(iflag_picked_sph(j) .le. izero) then
              icou = icou + 1
              idx_pick_gl(icou) = j
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
            j = l*(l+1) + m
            if(iflag_picked_sph(j) .le. izero) then
              icou = icou + 1
              idx_pick_gl(icou) = j
              iflag_picked_sph(j)  = icou
            end if
          end do
        end if
      end do
      num_pickup = icou
!
      do inum = 1, num_pickup
        idx_pick_lc(inum) = 0
        do j = 1, jmax
          if (idx_gl_j(j) .eq. idx_pick_gl(inum)) then
            idx_pick_lc(inum) = j
            exit
          end if
        end do
      end do
!
      end subroutine set_picked_sph_adrress
!
! -----------------------------------------------------------------------
!
      end module pickup_sph_spectr
