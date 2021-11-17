!
!      module set_integration_indices
!
!       Written by H. Matsui on March. 2006
!
!!      subroutine set_integrate_indices_3d                             &
!!     &         (ntot_int_3d, max_int_point, l_int)
!!      subroutine set_integrate_indices_2d                             &
!!     &         (ntot_int_2d, max_int_point, l_int2d)
!!      subroutine set_integrate_indices_1d                             &
!!     &         (ntot_int_1d, max_int_point, l_int1d)
!!
!!      subroutine set_integration_indices_3d_mesh                      &
!!     &         (ntot_int_3d, max_int_point, l_int)
!!      subroutine set_integration_indices_2d_mesh                      &
!!     &         (ntot_int_2d, max_int_point, l_int2d)
!!      subroutine set_integration_indices_1d_mesh                      &
!!     &         (ntot_int_1d, max_int_point, l_int1d)
!
      module set_integration_indices
!
      use m_precision
      use m_gauss_int_parameters
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
!
!  ---------------------------------------------------------------------
!
      subroutine set_integrate_indices_3d                               &
     &         (ntot_int_3d, max_int_point, l_int)
!
      integer(kind = kint), intent(in) :: ntot_int_3d
      integer(kind = kint), intent(in) :: max_int_point
      integer(kind = kint), intent(inout)                               &
     &                     :: l_int(3,ntot_int_3d,max_int_point)
!
      integer(kind = kint) :: n, i, kx, ky, kz
!
!
      do n = 1, max_int_point
        do kz = 1, n
          do ky = 1, n
            do kx = 1, n
              i = kx + n*(ky-1) + n*n*(kz-1)
              l_int(1,i,n) = kx
              l_int(2,i,n) = ky
              l_int(3,i,n) = kz
            end do
          end do
        end do
      end do
!
      end subroutine set_integrate_indices_3d
!
!  ---------------------------------------------------------------------
!
      subroutine set_integrate_indices_2d                               &
     &         (ntot_int_2d, max_int_point, l_int2d)
!
      integer(kind = kint), intent(in) :: ntot_int_2d
      integer(kind = kint), intent(in) :: max_int_point
      integer(kind = kint), intent(inout)                               &
     &                     :: l_int2d(2,ntot_int_2d,max_int_point)
!
      integer(kind = kint) :: n, i, kx, ky
!
!
      do n = 1, max_int_point
        do ky = 1, n
          do kx = 1, n
            i = kx + n*(ky-1)
            l_int2d(1,i,n) = kx
            l_int2d(2,i,n) = ky
          end do
        end do
      end do
!
      end subroutine set_integrate_indices_2d
!
!  ---------------------------------------------------------------------
!
      subroutine set_integrate_indices_1d                               &
     &         (ntot_int_1d, max_int_point, l_int1d)
!
      integer(kind = kint), intent(in) :: ntot_int_1d
      integer(kind = kint), intent(in) :: max_int_point
      integer(kind = kint), intent(inout)                               &
     &                     :: l_int1d(1,ntot_int_1d,max_int_point)
!
      integer(kind = kint) :: n, kx
!
      do n = 1, max_int_point
        do kx = 1, n
          l_int1d(1,kx,n) = kx
        end do
      end do
!
      end subroutine set_integrate_indices_1d
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_integration_indices_3d_mesh                        &
     &         (ntot_int_3d, max_int_point, l_int)
!
      integer(kind = kint), intent(in) :: ntot_int_3d
      integer(kind = kint), intent(in) :: max_int_point
      integer(kind = kint), intent(inout)                               &
     &                     :: l_int(3,ntot_int_3d,max_int_point)
!
      integer(kind = kint) :: nd, ii, j
!
!
      do nd = 1, 3
          l_int(nd,1,1) = int_position_1(nd)
      end do
!
      do ii = 1, 8
        do nd = 1, 3
          j = nd + 3*(ii-1)
          l_int(nd,ii,2) = int_position_8(j)
        end do
      end do
!
      do ii = 1, 27
        do nd = 1, 3
          j = nd + 3*(ii-1)
          l_int(nd,ii,3) = int_position_27(j)
        end do
      end do
!
      do ii = 1, 64
        do nd = 1, 3
          j = nd + 3*(ii-1)
          l_int(nd,ii,4) = int_position_64(j)
        end do
      end do
!
      end subroutine set_integration_indices_3d_mesh
!
!  ---------------------------------------------------------------------
!
      subroutine set_integration_indices_2d_mesh                        &
     &         (ntot_int_2d, max_int_point, l_int2d)
!
      integer(kind = kint), intent(in) :: ntot_int_2d
      integer(kind = kint), intent(in) :: max_int_point
      integer(kind = kint), intent(inout)                               &
     &                     :: l_int2d(2,ntot_int_2d,max_int_point)
!
      integer(kind = kint) :: nd, ii, j
!
!
      do nd = 1, 2
          l_int2d(nd,1,1) = int_posi_2d_1(nd)
      end do
!
      do ii = 1, 4
        do nd = 1, 2
          j = nd + 2*(ii-1)
          l_int2d(nd,ii,2) = int_posi_2d_4(j)
        end do
      end do
!
      do ii = 1, 9
        do nd = 1, 2
          j = nd + 2*(ii-1)
          l_int2d(nd,ii,3) = int_posi_2d_9(j)
        end do
      end do
!
      do ii = 1, 16
        do nd = 1, 2
          j = nd + 2*(ii-1)
          l_int2d(nd,ii,4) = int_posi_2d_16(j)
        end do
      end do
!
      end subroutine set_integration_indices_2d_mesh
!
!  ---------------------------------------------------------------------
!
      subroutine set_integration_indices_1d_mesh                        &
     &         (ntot_int_1d, max_int_point, l_int1d)
!
      integer(kind = kint), intent(in) :: ntot_int_1d
      integer(kind = kint), intent(in) :: max_int_point
      integer(kind = kint), intent(inout)                               &
     &                     :: l_int1d(1,ntot_int_1d,max_int_point)
!
      integer(kind = kint) :: ii
!
!
      l_int1d(1,1,1) = int_posi_1d_1(1)
!
      do ii = 1, 2
        l_int1d(1,ii,2) = int_posi_1d_2(ii)
      end do
!
      do ii = 1, 3
        l_int1d(1,ii,3) = int_posi_1d_3(ii)
      end do
!
      do ii = 1, 4
        l_int1d(1,ii,4) = int_posi_1d_4(ii)
      end do
!
      end subroutine set_integration_indices_1d_mesh
!
!  ---------------------------------------------------------------------
!
      end module set_integration_indices
