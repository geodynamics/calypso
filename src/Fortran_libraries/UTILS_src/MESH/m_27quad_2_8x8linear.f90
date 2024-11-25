!>@file   m_27quad_2_8x8linear.f90
!!@brief  module m_27quad_2_8x8linear
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2006
!!
!>@brief Make linear elements from quad mesh
!!
!!@verbatim
!!      subroutine set_27quad_2_8x8linear_1ele(ie1_q27, ie1_l)
!!        integer(kind = kint), intent(in) :: ie1_q27(27)
!!        integer(kind = kint), intent(inout) :: ie1_l(num_t_linear,8)
!!           Caution!: i is connectivity and j is divided element
!!                    in ie1_l(i,j)
!!      subroutine set_27quad_2_8x8linear(numele, numele8, ie_l, ie_q27)
!!      subroutine gen_connect_quad27_from_quad20(nnod332, nele332,     &
!!     &          nsurf332, ie_332, isf_ele_332, ie_333)
!!@endverbatim
!
      module m_27quad_2_8x8linear
!
      use m_precision
      use m_geometry_constants
!
      implicit  none
!
      integer(kind = kint), parameter :: id_quad27_8linear8(8,8)        &
     &     = reshape((/  1,  9, 25, 12, 17, 23, 27, 21,                 &
     &                   9,  2, 10, 25, 23, 18, 22, 27,                 &
     &                  25, 10,  3, 11, 27, 22, 19, 24,                 &
     &                  12, 25, 11,  4, 21, 27, 24, 20,                 &
     &                  17, 23, 27, 21,  5, 13, 26, 16,                 &
     &                  23, 18, 22, 27, 13,  6, 14, 26,                 &
     &                  27, 22, 19, 24, 26, 14,  7, 15,                 &
     &                  21, 27, 24, 20, 16, 26, 15,  8/),shape=(/8,8/))
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_27quad_2_8x8linear_1ele(ie1_q27, ie1_l)
!
      integer(kind = kint), intent(in) :: ie1_q27(27)
      integer(kind = kint), intent(inout) :: ie1_l(num_t_linear,8)
!
!
      ie1_l(1:8,1) = ie1_q27(id_quad27_8linear8(1:8,1))
      ie1_l(1:8,2) = ie1_q27(id_quad27_8linear8(1:8,2))
      ie1_l(1:8,3) = ie1_q27(id_quad27_8linear8(1:8,3))
      ie1_l(1:8,4) = ie1_q27(id_quad27_8linear8(1:8,4))
      ie1_l(1:8,5) = ie1_q27(id_quad27_8linear8(1:8,5))
      ie1_l(1:8,6) = ie1_q27(id_quad27_8linear8(1:8,6))
      ie1_l(1:8,7) = ie1_q27(id_quad27_8linear8(1:8,7))
      ie1_l(1:8,8) = ie1_q27(id_quad27_8linear8(1:8,8))
!
      end subroutine set_27quad_2_8x8linear_1ele
!
!  ---------------------------------------------------------------------
!
      subroutine set_27quad_2_8x8linear(nele_q, ie_q27, nele_l, ie_l)
!
      integer(kind = kint), intent(in) :: nele_q, nele_l
      integer(kind = kint), intent(in) :: ie_q27(nele_q,27)
      integer(kind = kint), intent(inout) :: ie_l(nele_l,8)
!
      integer(kind = kint) :: iele
      integer(kind = kint) :: iele1, iele2, iele3, iele4
      integer(kind = kint) :: iele5, iele6, iele7, iele8
!
!
!$omp parallel do &
!$omp&  private(iele1,iele2,iele3,iele4,iele5,iele6,iele7,iele8)
!$cdir parallel do &
!$cdir& private(iele1,iele2,iele3,iele4,iele5,iele6,iele7,iele8)
!poption parallel
        do iele = 1, nele_q
          iele1 = 8*(iele-1) + 1
          iele2 = 8*(iele-1) + 2
          iele3 = 8*(iele-1) + 3
          iele4 = 8*(iele-1) + 4
          iele5 = 8*(iele-1) + 5
          iele6 = 8*(iele-1) + 6
          iele7 = 8*(iele-1) + 7
          iele8 = 8*(iele-1) + 8
!
          ie_l(iele1,1) = ie_q27(iele,id_quad27_8linear8(1,1))
          ie_l(iele1,2) = ie_q27(iele,id_quad27_8linear8(2,1))
          ie_l(iele1,3) = ie_q27(iele,id_quad27_8linear8(3,1))
          ie_l(iele1,4) = ie_q27(iele,id_quad27_8linear8(4,1))
          ie_l(iele1,5) = ie_q27(iele,id_quad27_8linear8(5,1))
          ie_l(iele1,6) = ie_q27(iele,id_quad27_8linear8(6,1))
          ie_l(iele1,7) = ie_q27(iele,id_quad27_8linear8(7,1))
          ie_l(iele1,8) = ie_q27(iele,id_quad27_8linear8(8,1))
!
          ie_l(iele2,1) = ie_q27(iele,id_quad27_8linear8(1,2))
          ie_l(iele2,2) = ie_q27(iele,id_quad27_8linear8(2,2))
          ie_l(iele2,3) = ie_q27(iele,id_quad27_8linear8(3,2))
          ie_l(iele2,4) = ie_q27(iele,id_quad27_8linear8(4,2))
          ie_l(iele2,5) = ie_q27(iele,id_quad27_8linear8(5,2))
          ie_l(iele2,6) = ie_q27(iele,id_quad27_8linear8(6,2))
          ie_l(iele2,7) = ie_q27(iele,id_quad27_8linear8(7,2))
          ie_l(iele2,8) = ie_q27(iele,id_quad27_8linear8(8,2))
!
          ie_l(iele3,1) = ie_q27(iele,id_quad27_8linear8(1,3))
          ie_l(iele3,2) = ie_q27(iele,id_quad27_8linear8(2,3))
          ie_l(iele3,3) = ie_q27(iele,id_quad27_8linear8(3,3))
          ie_l(iele3,4) = ie_q27(iele,id_quad27_8linear8(4,3))
          ie_l(iele3,5) = ie_q27(iele,id_quad27_8linear8(5,3))
          ie_l(iele3,6) = ie_q27(iele,id_quad27_8linear8(6,3))
          ie_l(iele3,7) = ie_q27(iele,id_quad27_8linear8(7,3))
          ie_l(iele3,8) = ie_q27(iele,id_quad27_8linear8(8,3))
!
          ie_l(iele4,1) = ie_q27(iele,id_quad27_8linear8(1,4))
          ie_l(iele4,2) = ie_q27(iele,id_quad27_8linear8(2,4))
          ie_l(iele4,3) = ie_q27(iele,id_quad27_8linear8(3,4))
          ie_l(iele4,4) = ie_q27(iele,id_quad27_8linear8(4,4))
          ie_l(iele4,5) = ie_q27(iele,id_quad27_8linear8(5,4))
          ie_l(iele4,6) = ie_q27(iele,id_quad27_8linear8(6,4))
          ie_l(iele4,7) = ie_q27(iele,id_quad27_8linear8(7,4))
          ie_l(iele4,8) = ie_q27(iele,id_quad27_8linear8(8,4))
!
          ie_l(iele5,1) = ie_q27(iele,id_quad27_8linear8(1,5))
          ie_l(iele5,2) = ie_q27(iele,id_quad27_8linear8(2,5))
          ie_l(iele5,3) = ie_q27(iele,id_quad27_8linear8(3,5))
          ie_l(iele5,4) = ie_q27(iele,id_quad27_8linear8(4,5))
          ie_l(iele5,5) = ie_q27(iele,id_quad27_8linear8(5,5))
          ie_l(iele5,6) = ie_q27(iele,id_quad27_8linear8(6,5))
          ie_l(iele5,7) = ie_q27(iele,id_quad27_8linear8(7,5))
          ie_l(iele5,8) = ie_q27(iele,id_quad27_8linear8(8,5))
!
          ie_l(iele6,1) = ie_q27(iele,id_quad27_8linear8(1,6))
          ie_l(iele6,2) = ie_q27(iele,id_quad27_8linear8(2,6))
          ie_l(iele6,3) = ie_q27(iele,id_quad27_8linear8(3,6))
          ie_l(iele6,4) = ie_q27(iele,id_quad27_8linear8(4,6))
          ie_l(iele6,5) = ie_q27(iele,id_quad27_8linear8(5,6))
          ie_l(iele6,6) = ie_q27(iele,id_quad27_8linear8(6,6))
          ie_l(iele6,7) = ie_q27(iele,id_quad27_8linear8(7,6))
          ie_l(iele6,8) = ie_q27(iele,id_quad27_8linear8(8,6))
!
          ie_l(iele7,1) = ie_q27(iele,id_quad27_8linear8(1,7))
          ie_l(iele7,2) = ie_q27(iele,id_quad27_8linear8(2,7))
          ie_l(iele7,3) = ie_q27(iele,id_quad27_8linear8(3,7))
          ie_l(iele7,4) = ie_q27(iele,id_quad27_8linear8(4,7))
          ie_l(iele7,5) = ie_q27(iele,id_quad27_8linear8(5,7))
          ie_l(iele7,6) = ie_q27(iele,id_quad27_8linear8(6,7))
          ie_l(iele7,7) = ie_q27(iele,id_quad27_8linear8(7,7))
          ie_l(iele7,8) = ie_q27(iele,id_quad27_8linear8(8,7))
!
          ie_l(iele8,1) = ie_q27(iele,id_quad27_8linear8(1,8))
          ie_l(iele8,2) = ie_q27(iele,id_quad27_8linear8(2,8))
          ie_l(iele8,3) = ie_q27(iele,id_quad27_8linear8(3,8))
          ie_l(iele8,4) = ie_q27(iele,id_quad27_8linear8(4,8))
          ie_l(iele8,5) = ie_q27(iele,id_quad27_8linear8(5,8))
          ie_l(iele8,6) = ie_q27(iele,id_quad27_8linear8(6,8))
          ie_l(iele8,7) = ie_q27(iele,id_quad27_8linear8(7,8))
          ie_l(iele8,8) = ie_q27(iele,id_quad27_8linear8(8,8))
        end do
!$omp end parallel do
!
      end subroutine set_27quad_2_8x8linear
!
!  ---------------------------------------------------------------------
!
      subroutine gen_connect_quad27_from_quad20(nnod332, nele332,       &
     &          nsurf332, ie_332, isf_ele_332, ie_333)
!
      integer(kind = kint), intent(in) :: nnod332, nele332, nsurf332
      integer(kind = kint), intent(in) :: ie_332(nele332,20)
      integer(kind = kint), intent(in) :: isf_ele_332(nele332,6)
!
      integer(kind = kint), intent(inout) :: ie_333(nele332,27)
!
      integer(kind = kint) :: iele
!
!
!$omp parallel do
        do iele = 1, nele332
          ie_333(iele, 1) = ie_332(iele, 1)
          ie_333(iele, 2) = ie_332(iele, 2)
          ie_333(iele, 3) = ie_332(iele, 3)
          ie_333(iele, 4) = ie_332(iele, 4)
          ie_333(iele, 5) = ie_332(iele, 5)
          ie_333(iele, 6) = ie_332(iele, 6)
          ie_333(iele, 7) = ie_332(iele, 7)
          ie_333(iele, 8) = ie_332(iele, 8)
          ie_333(iele, 9) = ie_332(iele, 9)
          ie_333(iele,10) = ie_332(iele,10)
          ie_333(iele,11) = ie_332(iele,11)
          ie_333(iele,12) = ie_332(iele,12)
          ie_333(iele,13) = ie_332(iele,13)
          ie_333(iele,14) = ie_332(iele,14)
          ie_333(iele,15) = ie_332(iele,15)
          ie_333(iele,16) = ie_332(iele,16)
          ie_333(iele,17) = ie_332(iele,17)
          ie_333(iele,18) = ie_332(iele,18)
          ie_333(iele,19) = ie_332(iele,19)
          ie_333(iele,20) = ie_332(iele,20)
          ie_333(iele,21) = abs(isf_ele_332(iele,1)) + nnod332
          ie_333(iele,22) = abs(isf_ele_332(iele,2)) + nnod332
          ie_333(iele,23) = abs(isf_ele_332(iele,3)) + nnod332
          ie_333(iele,24) = abs(isf_ele_332(iele,4)) + nnod332
          ie_333(iele,25) = abs(isf_ele_332(iele,5)) + nnod332
          ie_333(iele,26) = abs(isf_ele_332(iele,6)) + nnod332
          ie_333(iele,27) = iele + nnod332 + nsurf332
        end do
!$omp end parallel do
!
      end subroutine gen_connect_quad27_from_quad20
!
!  ---------------------------------------------------------------------
!
      end module m_27quad_2_8x8linear
