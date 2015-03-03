!
!     module m_intersection_data_1
!
      module m_intersection_data_1
!
!      Written by H. Matsui on june, 2006
!
      use m_precision
!
      implicit none
!
!   1 node has differrent sign
!
!     flag = 1: c-c_ref >= 0
!     flag = 1: c-c_ref < 0
!
!
!       12345678: boundary...3 of 12
!   1:  10000000: 4-1, 1-2, 1-5  :  4-1- 9
!   2:  01000000: 1-2, 2-3, 2-6  :  1-2-10
!   4:  00100000: 2-3, 3-4, 3-7  :  2-3-11
!   8:  00010000: 3-4, 4-1, 4-8  :  3-4-12
!  16:  00001000: 5-6, 8-5, 1-5  :  5-8- 9
!  32:  00000100: 6-7, 5-6, 2-6  :  6-5-10
!  64:  00000010: 7-8, 6-7, 3-7  :  7-6-11
! 128:  00000001: 8-1, 7-8, 4-8  :  8-7-12
!
!
!       12345678: boundary...3 of 12
! 254:  01111111: 1-2, 4-1, 1-5  :  1-4- 9
! 253:  10111111: 2-3, 1-2, 2-6  :  2-1-10
! 251:  11011111: 3-4, 2-3, 3-7  :  3-2-11
! 247:  11101111: 4-1, 3-4, 4-8  :  4-3-12
! 239:  11110111: 8-5, 5-6, 1-5  :  8-5- 9
! 223:  11111011: 5-6, 6-7, 2-6  :  5-6-10
! 191:  11111101: 6-7, 7-8, 3-7  :  6-7-11
! 127:  11111110: 7-8, 8-5, 4-8  :  7-8-12
!
      integer(kind = kint), parameter, private :: nnod_tri = 3
!
      integer(kind = kint), parameter :: nkind_etype_1 = 8
      integer(kind = kint), parameter :: num_triangle_1 = 1
      integer(kind = kint), parameter :: num_patch_1 = 1
      integer(kind = kint), parameter :: itri_2_patch_1(3)              &
     &     = (/1, 2, 3/)
!
      integer(kind = kint), parameter                                   &
     &   :: iflag_psf_etype_1p(nkind_etype_1)                           &
     &     = (/1, 2, 4, 8, 16, 32, 64, 128/)
      integer(kind = kint), parameter                                   &
     &   :: iflag_psf_etype_1n(nkind_etype_1)                           &
     &     = (/254, 253, 251, 247, 239, 223, 191, 127/)
!
!
      integer(kind = kint), parameter                                   &
     &        :: iedge_triangle_1p(nnod_tri,nkind_etype_1)              &
     &      = reshape(                                                  &
     &       (/  4, 1,  9,   1, 2, 10,   2, 3, 11,   3, 4, 12,          &
     &           5, 8,  9,   6, 5, 10,   7, 6, 11,   8, 7, 12/),        &
     &       shape=(/nnod_tri,nkind_etype_1/) )
!
      integer(kind = kint), parameter                                   &
     &        :: iedge_triangle_1n(nnod_tri,nkind_etype_1)              &
     &      = reshape(                                                  &
     &       (/  1, 4,  9,   2, 1, 10,   3, 2, 11,   4, 3, 12,          &
     &           8, 5,  9,   5, 6, 10,   6, 7, 11,   7, 8, 12/),        &
     &       shape=(/nnod_tri,nkind_etype_1/) )
!
!
      integer(kind = kint), parameter                                   &
     &        :: iedge_4_patch_1p(nnod_tri,nkind_etype_1)               &
     &      = reshape(                                                  &
     &       (/  4, 1,  9,   1, 2, 10,   2, 3, 11,   3, 4, 12,          &
     &           5, 8,  9,   6, 5, 10,   7, 6, 11,   8, 7, 12/),        &
     &       shape=(/nnod_tri,nkind_etype_1/) )
!
      integer(kind = kint), parameter                                   &
     &        :: iedge_4_patch_1n(nnod_tri,nkind_etype_1)               &
     &      = reshape(                                                  &
     &       (/  1, 4,  9,   2, 1, 10,   3, 2, 11,   4, 3, 12,          &
     &           8, 5,  9,   5, 6, 10,   6, 7, 11,   7, 8, 12/),        &
     &       shape=(/nnod_tri,nkind_etype_1/) )
!
      end module m_intersection_data_1
