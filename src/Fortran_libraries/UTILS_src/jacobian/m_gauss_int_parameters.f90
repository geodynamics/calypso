!
!     module m_gauss_int_parameters
!
!     Written by H. Matsui on March. 2006
!
!      subroutine init_gauss_int_parameters
!
      module m_gauss_int_parameters
!
      use m_precision
      use m_constants
!
      implicit none
!
      real(kind = kreal) :: sqrt165
!
!   Base of Gauss points and their coefficients
!
      real(kind = kreal) :: gauss_2p
!
      real(kind = kreal) :: gauss_3pc
      real(kind = kreal) :: gauss_3ps
      real(kind = kreal) :: gauss_3wc
      real(kind = kreal) :: gauss_3ws
!
      real(kind = kreal) :: gauss_4pi
      real(kind = kreal) :: gauss_4po
      real(kind = kreal) :: gauss_4wi
      real(kind = kreal) :: gauss_4wo
!
!   Gauss points and their coefficients
!
      real(kind = kreal), parameter :: pt1d_1g(1) = (/zero/)
      real(kind = kreal), parameter :: wt1d_1g(1) = (/two /)
!
      real(kind = kreal) :: pt1d_2g(2)
      real(kind = kreal) :: wt1d_2g(2)
!
      real(kind = kreal) :: pt1d_3g(3)
      real(kind = kreal) :: wt1d_3g(3)
!
      real(kind = kreal) :: pt1d_4g(4)
      real(kind = kreal) :: wt1d_4g(4)
!
!   Specital integration points and their coefficients by Irons(71)
!
      real(kind = kreal) :: pt3d_q27_c
      real(kind = kreal) :: pt3d_q27_s
      real(kind = kreal) :: pt3d_q27_n
      real(kind = kreal) :: pt3d_q27_e
      real(kind = kreal) :: wt3d_q27_c
      real(kind = kreal) :: wt3d_q27_s
      real(kind = kreal) :: wt3d_q27_n
      real(kind = kreal) :: wt3d_q27_e
!
!  Integration points ID for 3-dimension
!
      integer(kind = kint), parameter :: int_position_1(3)              &
     &     = (/ 1, 1, 1/)

      integer(kind = kint), parameter :: int_position_8(24)             &
     &     = (/ 1, 1, 1,    2, 1, 1,                                    &
     &          1, 2, 1,    2, 2, 1,                                    &
     &          1, 1, 2,    2, 1, 2,                                    &
     &          1, 2, 2,    2, 2, 2 /)

      integer(kind = kint), parameter :: int_position_27(81)            &
     &     = (/ 1, 1, 1,    2, 1, 1,    3, 1, 1,                        &
     &          1, 2, 1,    2, 2, 1,    3, 2, 1,                        &
     &          1, 3, 1,    2, 3, 1,    3, 3, 1,                        &
     &          1, 1, 2,    2, 1, 2,    3, 1, 2,                        &
     &          1, 2, 2,    2, 2, 2,    3, 2, 2,                        &
     &          1, 3, 2,    2, 3, 2,    3, 3, 2,                        &
     &          1, 1, 3,    2, 1, 3,    3, 1, 3,                        &
     &          1, 2, 3,    2, 2, 3,    3, 2, 3,                        &
     &          1, 3, 3,    2, 3, 3,    3, 3, 3 /)

      integer(kind = kint), parameter :: int_position_64(192)           &
     &     = (/ 1, 1, 1,    2, 1, 1,    3, 1, 1,    4, 1, 1,            &
     &          1, 2, 1,    2, 2, 1,    3, 2, 1,    4, 2, 1,            &
     &          1, 3, 1,    2, 3, 1,    3, 3, 1,    4, 3, 1,            &
     &          1, 4, 1,    2, 4, 1,    3, 4, 1,    4, 4, 1,            &
     &          1, 1, 2,    2, 1, 2,    3, 1, 2,    4, 1, 2,            &
     &          1, 2, 2,    2, 2, 2,    3, 2, 2,    4, 2, 2,            &
     &          1, 3, 2,    2, 3, 2,    3, 3, 2,    4, 3, 2,            &
     &          1, 4, 2,    2, 4, 2,    3, 4, 2,    4, 4, 2,            &
     &          1, 1, 3,    2, 1, 3,    3, 1, 3,    4, 1, 3,            &
     &          1, 2, 3,    2, 2, 3,    3, 2, 3,    4, 2, 3,            &
     &          1, 3, 3,    2, 3, 3,    3, 3, 3,    4, 3, 3,            &
     &          1, 4, 3,    2, 4, 3,    3, 4, 3,    4, 4, 3,            &
     &          1, 1, 4,    2, 1, 4,    3, 1, 4,    4, 1, 4,            &
     &          1, 2, 4,    2, 2, 4,    3, 2, 4,    4, 2, 4,            &
     &          1, 3, 4,    2, 3, 4,    3, 3, 4,    4, 3, 4,            &
     &          1, 4, 4,    2, 4, 4,    3, 4, 4,    4, 4, 4/)
!
!  Integration points ID for 2-dimension
!
      integer(kind = kint), parameter :: int_posi_2d_1(2)               &
     &     = (/ 1, 1/)

      integer(kind = kint), parameter :: int_posi_2d_4(8)               &
     &     = (/ 1, 1,    2, 1,                                          &
     &          1, 2,    2, 2/)

      integer(kind = kint), parameter :: int_posi_2d_9(18)              &
     &     = (/ 1, 1,    2, 1,    3, 1,                                 &
     &          1, 2,    2, 2,    3, 2,                                 &
     &          1, 3,    2, 3,    3, 3/)

      integer(kind = kint), parameter :: int_posi_2d_16(32)             &
     &     = (/ 1, 1,    2, 1,    3, 1,    4, 1,                        &
     &          1, 2,    2, 2,    3, 2,    4, 2,                        &
     &          1, 3,    2, 3,    3, 3,    4, 3,                        &
     &          1, 4,    2, 4,    3, 4,    4, 4/)
!
!
!  Integration points ID for 1-dimension
!
      integer(kind = kint), parameter :: int_posi_1d_1(1)               &
     &     = (/1/)

      integer(kind = kint), parameter :: int_posi_1d_2(2)               &
     &     = (/ 1, 2/)

      integer(kind = kint), parameter :: int_posi_1d_3(3)               &
     &     = (/ 1, 2, 3/)

      integer(kind = kint), parameter :: int_posi_1d_4(4)               &
     &     = (/ 1, 2, 3, 4/)
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_gauss_int_parameters
!
!
      gauss_2p = one/sqrt(three)
!
      gauss_3pc = zero
      gauss_3ps = sqrt(3.0d00/5.0d00)
      gauss_3wc = eight / dnine
      gauss_3ws = five / dnine
!
      gauss_4pi = sqrt( three/seven - two*sqrt(thirty)/(seven*five) )
      gauss_4po = sqrt( three/seven + two*sqrt(thirty)/(seven*five) )
!
      gauss_4wi = one/two + sqrt(thirty) / (dnine*four)
      gauss_4wo = one/two - sqrt(thirty) / (dnine*four)
!
      pt1d_2g(1) = -gauss_2p
      pt1d_2g(2) =  gauss_2p
      wt1d_2g(1:2) = one
!
      pt1d_3g(1) = -gauss_3ps
      pt1d_3g(2) =  gauss_3pc
      pt1d_3g(3) =  gauss_3ps
      wt1d_3g(1) =  gauss_3ws
      wt1d_3g(2) =  gauss_3wc
      wt1d_3g(3) =  gauss_3ws
!
      pt1d_4g(1) = -gauss_4po
      pt1d_4g(2) = -gauss_4pi
      pt1d_4g(3) =  gauss_4pi
      pt1d_4g(4) =  gauss_4po
      wt1d_4g(1) =  gauss_4wo
      wt1d_4g(2) =  gauss_4wi
      wt1d_4g(3) =  gauss_4wi
      wt1d_4g(4) =  gauss_4wo
!
!
      sqrt165 = sqrt(165.0d0)
!
      pt3d_q27_c = zero
      pt3d_q27_s = sqrt( ( 33.0d0 -      sqrt165) / 28.0d0  )
      pt3d_q27_n = sqrt( (195.0d0 - four*sqrt165) / 337.0d0 )
      pt3d_q27_e = sqrt( ( 30.0d0 +      sqrt165) / 35.0d0  )
!
      wt3d_q27_c = (157.0d0 -   (sqrt165 * 557.0d0 / 495.0d0) )         &
     &            * 256.0d0 / 46305.0d0
      wt3d_q27_s = (    two +   (sqrt165 * 104.0d0 /  99.0d0) )         &
     &            * 128.0d0 / 945.0d0
      wt3d_q27_n = (13273.0d0 + (sqrt165 * 31124.0d0 /  five) )         &
     &            * one / 46305.0d0
      wt3d_q27_e = ( 2790.0d0 - (sqrt165 * 191.0d0) )                   &
     &            * eight / 83349.0d0
!
!
      end subroutine init_gauss_int_parameters
!
! ----------------------------------------------------------------------
!
      end module m_gauss_int_parameters
