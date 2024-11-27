!
!     module cal_local_position_by_tetra
!
!     Written by H. Matsui on Aug., 2006
!
!      subroutine s_cal_local_position_by_tetra(nnod_4_ele_l, xi,       &
!     &          coefs_by_tet)
!
      module cal_local_position_by_tetra
!
      use m_precision
!
      implicit none
!
      private :: cal_position_in_ele, cal_position_in_ele_quad
      private :: cal_position_in_ele_lag
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_cal_local_position_by_tetra(nnod_4_ele_l, xi,        &
     &          coefs_by_tet)
!
      use m_geometry_constants
!
      integer(kind = kint), intent(in) :: nnod_4_ele_l
      real(kind = kreal), intent(in) ::   coefs_by_tet(nnod_4_ele_l)
      real(kind = kreal), intent(inout) :: xi(3)
!
!
      if (nnod_4_ele_l .eq. num_t_linear) then
        call cal_position_in_ele(xi, coefs_by_tet)
      else if (nnod_4_ele_l .eq. num_t_quad) then
        call cal_position_in_ele_quad(xi, coefs_by_tet)
      else if (nnod_4_ele_l .eq. num_t_lag) then
        call cal_position_in_ele_lag(xi, coefs_by_tet)
      end if
!
      end subroutine s_cal_local_position_by_tetra
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_position_in_ele(xi, coefs_by_tet)
!
      real(kind = kreal), intent(in) ::   coefs_by_tet(8)
      real(kind = kreal), intent(inout) :: xi(3)
!
!
         xi(1) =- coefs_by_tet(1) + coefs_by_tet(2)                     &
     &          + coefs_by_tet(3) - coefs_by_tet(4)                     &
     &          - coefs_by_tet(5) + coefs_by_tet(6)                     &
     &          + coefs_by_tet(7) - coefs_by_tet(8)
!
         xi(2) =- coefs_by_tet(1) - coefs_by_tet(2)                     &
     &          + coefs_by_tet(3) + coefs_by_tet(4)                     &
     &          - coefs_by_tet(5) - coefs_by_tet(6)                     &
     &          + coefs_by_tet(7) + coefs_by_tet(8)
!
         xi(3) =- coefs_by_tet(1) - coefs_by_tet(2)                     &
     &          - coefs_by_tet(3) - coefs_by_tet(4)                     &
     &          + coefs_by_tet(5) + coefs_by_tet(6)                     &
     &          + coefs_by_tet(7) + coefs_by_tet(8)
!
      end subroutine cal_position_in_ele
!
!-----------------------------------------------------------------------
!
      subroutine cal_position_in_ele_quad(xi, coefs_by_tet)
!
      real(kind = kreal), intent(in) ::   coefs_by_tet(20)
      real(kind = kreal), intent(inout) :: xi(3)
!
!
         xi(1) =- coefs_by_tet( 1) + coefs_by_tet( 2)                   &
     &          + coefs_by_tet( 3) - coefs_by_tet( 4)                   &
     &          - coefs_by_tet( 5) + coefs_by_tet( 6)                   &
     &          + coefs_by_tet( 7) - coefs_by_tet( 8)                   &
     &                             + coefs_by_tet(10)                   &
     &                             - coefs_by_tet(12)                   &
     &                             + coefs_by_tet(14)                   &
     &                             - coefs_by_tet(16)                   &
     &          - coefs_by_tet(17) + coefs_by_tet(18)                   &
     &          + coefs_by_tet(19) - coefs_by_tet(20)
!
         xi(2) =- coefs_by_tet( 1) - coefs_by_tet( 2)                   &
     &          + coefs_by_tet( 3) + coefs_by_tet( 4)                   &
     &          - coefs_by_tet( 5) - coefs_by_tet( 6)                   &
     &          + coefs_by_tet( 7) + coefs_by_tet( 8)                   &
     &          - coefs_by_tet( 9)                                      &
     &          + coefs_by_tet(11)                                      &
     &          - coefs_by_tet(13)                                      &
     &          + coefs_by_tet(15)                                      &
     &          - coefs_by_tet(17) - coefs_by_tet(18)                   &
     &          + coefs_by_tet(19) + coefs_by_tet(20)
!
         xi(3) =- coefs_by_tet( 1) - coefs_by_tet( 2)                   &
     &          - coefs_by_tet( 3) - coefs_by_tet( 4)                   &
     &          + coefs_by_tet( 5) + coefs_by_tet( 6)                   &
     &          + coefs_by_tet( 7) + coefs_by_tet( 8)                   &
     &          - coefs_by_tet( 9) - coefs_by_tet(10)                   &
     &          - coefs_by_tet(11) - coefs_by_tet(12)                   &
     &          + coefs_by_tet(13) + coefs_by_tet(14)                   &
     &          + coefs_by_tet(15) + coefs_by_tet(16)
!
      end subroutine cal_position_in_ele_quad
!
!-----------------------------------------------------------------------
!
      subroutine cal_position_in_ele_lag(xi, coefs_by_tet)
!
      real(kind = kreal), intent(in) ::   coefs_by_tet(27)
      real(kind = kreal), intent(inout) :: xi(3)
!
!
         xi(1) =- coefs_by_tet( 1) + coefs_by_tet( 2)                   &
     &          + coefs_by_tet( 3) - coefs_by_tet( 4)                   &
     &          - coefs_by_tet( 5) + coefs_by_tet( 6)                   &
     &          + coefs_by_tet( 7) - coefs_by_tet( 8)                   &
     &                             + coefs_by_tet(10)                   &
     &                             - coefs_by_tet(12)                   &
     &                             + coefs_by_tet(14)                   &
     &                             - coefs_by_tet(16)                   &
     &          - coefs_by_tet(17) + coefs_by_tet(18)                   &
     &          + coefs_by_tet(19) - coefs_by_tet(20)                   &
     &          - coefs_by_tet(21) + coefs_by_tet(22)
!
         xi(2) =- coefs_by_tet( 1) - coefs_by_tet( 2)                   &
     &          + coefs_by_tet( 3) + coefs_by_tet( 4)                   &
     &          - coefs_by_tet( 5) - coefs_by_tet( 6)                   &
     &          + coefs_by_tet( 7) + coefs_by_tet( 8)                   &
     &          - coefs_by_tet( 9)                                      &
     &          + coefs_by_tet(11)                                      &
     &          - coefs_by_tet(13)                                      &
     &          + coefs_by_tet(15)                                      &
     &          - coefs_by_tet(17) - coefs_by_tet(18)                   &
     &          + coefs_by_tet(19) + coefs_by_tet(20)                   &
     &          - coefs_by_tet(23) + coefs_by_tet(24)
!
         xi(3) =- coefs_by_tet( 1) - coefs_by_tet( 2)                   &
     &          - coefs_by_tet( 3) - coefs_by_tet( 4)                   &
     &          + coefs_by_tet( 5) + coefs_by_tet( 6)                   &
     &          + coefs_by_tet( 7) + coefs_by_tet( 8)                   &
     &          - coefs_by_tet( 9) - coefs_by_tet(10)                   &
     &          - coefs_by_tet(11) - coefs_by_tet(12)                   &
     &          + coefs_by_tet(13) + coefs_by_tet(14)                   &
     &          + coefs_by_tet(15) + coefs_by_tet(16)                   &
     &          - coefs_by_tet(25) + coefs_by_tet(26)
!
      end subroutine cal_position_in_ele_lag
!
!-----------------------------------------------------------------------
!
      end module cal_local_position_by_tetra
