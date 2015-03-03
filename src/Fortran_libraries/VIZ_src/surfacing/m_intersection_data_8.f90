!
!     module m_intersection_data_8
!
      module m_intersection_data_8
!
!      Written by H. Matsui on june, 2006
!
      use m_precision
!
      implicit none
!
!
!   4 nodes which have differrent sign on a surface
!      (one quad ... 2 triangles)
!
!     flag = 1: c-c_ref >= 0
!     flag = 1: c-c_ref < 0
!
!       12345678: boundary...4 of 12: quad: (2-3-1), (1-3-4)
! 153:  10011001:  1-2, 5-6, 7-8, 3-4:  1-5-7-3;      5-7-1, 1-7-3
! 102:  01100110:  1-2, 3-4, 7-8, 5-6:  1-3-7-5:      3-7-1, 1-7-5
!  51:  11001100:  4-1, 2-3, 6-7, 8-5:  4-2-6-8:      2-6-4, 4-6-8
! 204:  00110011:  4-1, 8-5, 6-7, 2-3:  4-8-6-2;      8-6-4, 4-6-2
!  15:  11110000:  1-5, 4-8, 3-7, 2-6:  9-12-11-10: 12-11-9, 9-11-10
! 240:  00001111:  1-5, 2-6, 3-7, 4-8:  9-10-11-12; 10-11-9, 9-11-12
!
      integer(kind = kint), parameter, private :: nnod_tri = 3
      integer(kind = kint), parameter, private :: nnod_quad = 4
!
      integer(kind = kint), parameter :: nkind_etype_8 = 6
      integer(kind = kint), parameter :: num_patch_8 =   2
      integer(kind = kint), parameter :: itri_2_patch_8(6)              &
     &     = (/2, 3, 1,   1, 3, 4/)
!
      integer(kind = kint), parameter                                   &
     &   :: iflag_psf_etype_8(nkind_etype_8)                            &
     &     = (/153, 102,  51, 204, 15, 240/)
!
!
      integer(kind = kint), parameter                                   &
     &        :: iedge_quad_8(nnod_quad, nkind_etype_8)                 &
     &      = reshape(                                                  &
     &       (/1, 5, 7, 3,    1,  3,  7,  5,   4,  2,  6,  8,           &
     &         4, 8, 6, 2,    9, 12, 11, 10,   9, 10, 11, 12/),         &
     &       shape=(/nnod_quad, nkind_etype_8/) )
!
      integer(kind = kint), parameter                                   &
     &        :: iedge_4_patch_8(nnod_tri,num_patch_8,nkind_etype_8)    &
     &      = reshape(                                                  &
     &       (/ 5,  7, 1,  1,  7,  3,    3,  7, 1,  1,  7,  5,          &
     &          2,  6, 4,  4,  6,  8,    8,  6, 4,  4,  6,  2,          &
     &         12, 11, 9,  9, 11, 10,   10, 11, 9,  9, 11, 12/),        &
     &       shape=(/nnod_tri,num_patch_8,nkind_etype_8/) )
!
      end module m_intersection_data_8

