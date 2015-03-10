!
!     module m_intersection_data_9
!
      module m_intersection_data_9
!
!      Written by H. Matsui on june, 2006
!
      use m_precision
!
      implicit none
!
!
!   4 nodes which have differrent sign making four triangles
!      (one hexagonal ... 4 triangles)
!
!     flag = 1: c-c_ref >= 0
!     flag = 1: c-c_ref < 0
!
!       12345678: boundary...6 of 12: hexagonal: 
!                  (2-3-4), (2-4-5), (2-5-1), (1-5-6)
!  27:  11011000: 2-3, 2-6, 5-6, 8-5, 4-8, 3-4: 2-10-5-8-12-3: 
!                  10-5-8, 10-8-12, 10-12-2, 2-12-3
!  39:  11100100: 3-4, 3-7, 6-7, 5-6, 1-5, 4-1: 3-11-6-5- 9-4:
!                  11-6-5, 11-5- 9, 11- 9-3, 3- 9-4
!  78:  01110010: 4-1, 4-8, 7-8, 6-7, 2-6, 1-2: 4-12-7-6-10-1:
!                  12-7-6, 12-6-10, 12-10-4, 4-10-1
! 141:  10110001: 1-2, 1-5, 8-5, 3-7, 7-8, 2-3: 1- 9-8-7-11-2:
!                   9-8-7,  9-7-11,  9-11-1, 1-11-2
! 177:  10001101: 7-8, 4-8, 4-1, 1-2, 2-6, 6-7: 7-12-4-1-10-6:
!                  12-4-1, 12-1-10, 12-10-7, 7-10-6
! 114:  01001110: 8-5, 1-5, 1-2, 2-3, 3-7, 7-8: 8- 9-1-2-11-7:
!                   9-1-2,  9-2-11,  9-11-8, 8-11-7
! 228:  00100111: 5-6, 2-6, 2-3, 3-4, 4-8, 8-5: 5-10-2-3-12-8:
!                  10-2-3, 10-3-12, 10-12-5, 5-12-8
! 216:  00011011: 6-7, 3-7, 3-4, 4-1, 1-5, 5-6: 6-11-3-4- 9-5:
!                  11-3-4, 11-4- 9, 11- 9-6, 6- 9-5
!
      integer(kind = kint), parameter, private :: nnod_tri = 3
      integer(kind = kint), parameter, private :: nnod_hex = 6
!
      integer(kind = kint), parameter :: nkind_etype_9 = 8
      integer(kind = kint), parameter :: num_patch_9 =   4
      integer(kind = kint), parameter :: itri_2_patch_9(12)             &
     &     = (/2, 3, 4,   2, 4, 5,   2, 5, 1,    1, 5, 6/)
!
      integer(kind = kint), parameter                                   &
     &   :: iflag_psf_etype_9(nkind_etype_9)                            &
     &     = (/27, 39, 78, 141, 177, 114, 228, 216/)
!
!
      integer(kind = kint), parameter                                   &
     &        :: iedge_hex_9(nnod_hex, nkind_etype_9)                   &
     &      = reshape(                                                  &
     &       (/ 2, 10, 5, 8, 12, 3,   3, 11, 6, 5,  9, 4,               &
     &          4, 12, 7, 6, 10, 1,   1,  9, 8, 7, 11, 2,               &
     &          7, 12, 4, 1, 10, 6,   8,  9, 1, 2, 11, 7,               &
     &          5, 10, 2, 3, 12, 8,   6, 11, 3, 4,  9, 5/),             &
     &       shape=(/nnod_hex, nkind_etype_9/) )
!
      integer(kind = kint), parameter                                   &
     &        :: iedge_4_patch_9(nnod_tri,num_patch_9,nkind_etype_9)    &
     &      = reshape(                                                  &
     &       (/10, 5, 8,   10, 8, 12,   10, 12, 2,   2, 12, 3,          &
     &         11, 6, 5,   11, 5,  9,   11,  9, 3,   3,  9, 4,          &
     &         12, 7, 6,   12, 6, 10,   12, 10, 4,   4, 10, 1,          &
     &          9, 8, 7,    9, 7, 11,    9, 11, 1,   1, 11, 2,          &
     &         12, 4, 1,   12, 1, 10,   12, 10, 7,   7, 10, 6,          &
     &          9, 1, 2,    9, 2, 11,    9, 11, 8,   8, 11, 7,          &
     &         10, 2, 3,   10, 3, 12,   10, 12, 5,   5, 12, 8,          &
     &         11, 3, 4,   11, 4,  9,   11,  9, 6,   6,  9, 5/),        &
     &       shape=(/nnod_tri,num_patch_9,nkind_etype_9/) )
!
!
      end module m_intersection_data_9
