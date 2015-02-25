!>@file   m_precision.f90
!!@brief  module m_precision
!!
!!@author H. Matsui and H.Okuda
!!@date Programmed in July 2000
!!@n    Modified in Sep., 2005
!
!> @brief Define byte length for number and characters
!
      module m_precision
!
      implicit none
!
!>  kind parameter for integer type variable (  4 byte )
      integer, parameter   ::  kint  =       4
!>  kind parameter for real    type variable (  8 byte )
      integer, parameter   ::  kreal =       8
!>  character length for name               (  255 byte )
      integer, parameter   ::  kchara =    255
!>  character length for file name          ( 2047 byte )
      integer, parameter   ::  len_fname = 2047
!
!>  kind parameter for long integer for global index (  8 byte )
      integer, parameter   ::  kint_gl = 8
!
      end module m_precision

