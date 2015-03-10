!
!     module m_intersection_data_0
!
      module m_intersection_data_0
!
!      Written by H. Matsui on june, 2006
!
      use m_precision
!
      implicit none
!
!       12345678: boundary...3 of 12
!   0:  00000000: none :  none
!
!       12345678: boundary...3 of 12
! 255:  11111111: none :  none
!
      integer(kind = kint), parameter :: nkind_etype_0 = 2
      integer(kind = kint), parameter                                   &
     &   :: iflag_psf_etype_0(2) = (/0, 255/)
!
      integer(kind = kint), parameter :: num_triangle_0 = 0
      integer(kind = kint), parameter :: num_patch_0 = 0
!
      integer(kind = kint), parameter                                   &
     &        :: iedge_4_patch_0(1,1) = reshape( (/0/), shape=(/1,1/))
!
      end module m_intersection_data_0
