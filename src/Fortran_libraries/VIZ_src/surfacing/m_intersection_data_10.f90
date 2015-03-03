!
!     module m_intersection_data_10
!
      module m_intersection_data_10
!
!      Written by H. Matsui on june, 2006
!
      use m_precision
!
      implicit none
!
!
!   4 nodes which have differrent sign on 2 parallel edges
!      (two quads ... 4 triangles)
!
!     flag = 1: c-c_ref >= 0
!     flag = 1: c-c_ref < 0
!
!       12345678: boundary...8 of 12: 2 quads; 
!                (1-2-4), (3-4-2); (1-2-4), (3-4-2)
! 195:  11000011:  4-1, 2-3, 2-6, 1-5; 3-7, 4-8, 8-5, 6-7:
!                 4-2-10- 9; 11-12-8-6: 4-2- 9, 10- 9-2; 11-12-6, 8-6-12
! 150:  01101001:  1-2, 3-4, 3-7, 2-6; 4-8, 1-5, 5-6, 7-8
!                 1-3-11-10; 12- 9-5-7: 1-3-10, 11-10-3; 12- 9-7, 5-7- 9
!  60:  00111100:  2-3, 4-1, 4-8, 3-7; 1-5, 2-6, 6-7, 8-5
!                 2-4-12-11; 9-10-6-8: 2-4-11, 12-11-4; 9-10-8, 6-8-10
! 105:  10010110:  3-4, 1-2, 1-5, 4-8; 2-6, 3-7, 7-8, 5-6
!                 3-1- 9-12; 10-11-7-5: 3-1-12,  9-12-1; 10-11-5, 7-5-11
!  85:  10101010:  8-5, 4-1, 1-2, 5-6; 6-7, 2-3, 3-4, 7-8
!                 8-4-1-5; 6-2-3-7: 8-4-5,  1-5-4; 6-2-7,  3-7-2
! 170:  01010101:  5-6, 1-2, 2-3, 6-7; 7-8, 3-4, 4-1, 8-5
!                 5-1-2-6; 7-3-4-8:  5-1-6,  2-6-1; 7-3-8,  4-8-3
!
      integer(kind = kint), parameter, private :: nnod_tri = 3
      integer(kind = kint), parameter, private :: nnod_quad = 4
!
      integer(kind = kint), parameter :: nkind_etype_10 = 6
      integer(kind = kint), parameter :: num_quad_10 =    2
      integer(kind = kint), parameter :: num_patch_10 =    4
      integer(kind = kint), parameter :: itri_2_patch_10(6)             &
     &     = (/1, 2, 4,   3, 4, 2/)
!
      integer(kind = kint), parameter                                   &
     &   :: iflag_psf_etype_10(nkind_etype_10)                          &
     &     = (/195, 150, 60, 105, 85, 170/)
!
!
      integer(kind = kint), parameter                                   &
     &        :: iedge_quads_10(nnod_quad, num_quad_10, nkind_etype_10) &
     &      = reshape(                                                  &
     &       (/ 4, 2, 10,  9,    11, 12, 8, 6,                          &
     &          1, 3, 11, 10,    12,  9, 5, 7,                          &
     &          2, 4, 12, 11,     9, 10, 6, 8,                          &
     &          3, 1,  9, 12,    10, 11, 7, 5,                          &
     &          8, 4,  1,  5,     6,  2, 3, 7,                          &
     &          5, 1,  2,  6,     7,  3, 4, 8/),                        &
     &       shape=(/nnod_quad, num_quad_10, nkind_etype_10/) )
!
      integer(kind = kint), parameter                                   &
     &        :: iedge_4_patch_10(nnod_tri,num_patch_10,nkind_etype_10) &
     &      = reshape(                                                  &
     &       (/ 4, 2,  9,   10,  9, 2,     11, 12, 6,   8, 6, 12,       &
     &          1, 3, 10,   11, 10, 3,     12,  9, 7,   5, 7,  9,       &
     &          2, 4, 11,   12, 11, 4,      9, 10, 8,   6, 8, 10,       &
     &          3, 1, 12,    9, 12, 1,     10, 11, 5,   7, 5, 11,       &
     &          8, 4,  5,    1,  5, 4,      6,  2, 7,   3, 7,  2,       &
     &          5, 1,  6,    2,  6, 1,      7,  3, 8,   4, 8,  3/),     &
     &       shape=(/nnod_tri,num_patch_10,nkind_etype_10/) )
!
      end module m_intersection_data_10
