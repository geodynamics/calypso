!m_precision.f90
!      module m_precision
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modieied by H. Matsui on Sep., 2005
!
!> @brief Define byte length for number and characters
!
      module m_precision
!
      implicit none
!
      integer, parameter   ::  kint  =       4
!<  kind parameter for integer type variable (  4 byte )
      integer, parameter   ::  kreal =       8
!<  kind parameter for real    type variable (  8 byte )
      integer, parameter   ::  kchara =    255
!<  character length for name               (  255 byte )
      integer, parameter   ::  len_fname = 2047
!<  character length for file name          ( 2047 byte )
!
      end module m_precision

