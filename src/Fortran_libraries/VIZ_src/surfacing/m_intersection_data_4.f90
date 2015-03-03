!
!     module m_intersection_data_4
!
      module m_intersection_data_4
!
!      Written by H. Matsui on june, 2006
!
      use m_precision
!
      implicit none
!
!
!   2 nodes which have differrent sign at opposite side of cube
!      (2 triangles)
!
!     flag = 1: c-c_ref >= 0
!     flag = 1: c-c_ref < 0
!
      integer(kind = kint), parameter, private :: nnod_tri = 3
!
!
!       12345678: boundary...6 of 12
!  65:  10000010: 4-1, 1-2, 1-5, 7-8, 6-7, 3-7; 4-1- 9, 7-6-11
! 130:  01000001: 1-2, 2-3, 2-6, 8-1, 7-8, 4-8; 1-2-10, 8-7-12
!  20:  00101000: 2-3, 3-4, 3-7, 5-6, 8-5, 1-5; 2-3-11, 5-8- 9
!  40:  00010100: 3-4, 4-1, 4-8, 6-7, 5-6, 2-6; 3-4-12, 6-5-10
!
!       12345678: boundary...6 of 12
! 190:  01111101: 1-2, 4-1, 1-5, 6-7, 7-8, 3-7;  1-4- 9,  6-7-11
! 125:  10111110: 2-3, 1-2, 2-6, 7-8, 8-5, 4-8;  2-1-10,  7-8-12
! 235:  11010111: 3-4, 2-3, 3-7, 8-5, 5-6, 1-5;  3-2-11,  8-5- 9
! 215:  11101011: 4-1, 3-4, 4-8, 5-6, 6-7, 2-6;  4-3-12,  5-6-10
!
      integer(kind = kint), parameter :: nkind_etype_4 = 4
      integer(kind = kint), parameter :: num_tri_4 =     2
      integer(kind = kint), parameter :: num_patch_4 =   2
      integer(kind = kint), parameter :: itri_2_patch_3(3)              &
     &     = (/1, 2, 3/)
!
      integer(kind = kint), parameter                                   &
     &   :: iflag_psf_etype_4p(nkind_etype_4) = (/ 65, 130,  20,  40/)
      integer(kind = kint), parameter                                   &
     &   :: iflag_psf_etype_4n(nkind_etype_4) = (/190, 125, 235, 215/)
!
!
      integer(kind = kint), parameter                                   &
     &        :: iedge_tri_4p(nnod_tri,num_tri_4, nkind_etype_4)        &
     &      = reshape(                                                  &
     &       (/ 4, 1,  9,   7, 6, 11,      1, 2, 10,    8, 7, 12,       &
     &          2, 3, 11,   5, 8,  9,      3, 4, 12,    6, 5, 10 /),    &
     &       shape=(/nnod_tri,num_tri_4, nkind_etype_4/) )
!
      integer(kind = kint), parameter                                   &
     &        :: iedge_tri_4n(nnod_tri,num_tri_4, nkind_etype_4)        &
     &      = reshape(                                                  &
     &       (/ 1, 4,  9,   6, 7, 11,      2, 1, 10,    7, 8, 12,       &
     &          3, 2, 11,   8, 5,  9,      4, 3, 12,    5, 6, 10/),     &
     &       shape=(/nnod_tri,num_tri_4, nkind_etype_4/) )
!
!
      integer(kind = kint), parameter                                   &
     &        :: iedge_4_patch_4p(nnod_tri,num_patch_4,nkind_etype_4)   &
     &      = reshape(                                                  &
     &       (/ 4, 1,  9,   7, 6, 11,      1, 2, 10,    8, 7, 12,       &
     &          2, 3, 11,   5, 8,  9,      3, 4, 12,    6, 5, 10 /),    &
     &       shape=(/nnod_tri,num_patch_4,nkind_etype_4/) )
!
      integer(kind = kint), parameter                                   &
     &        :: iedge_4_patch_4n(nnod_tri,num_patch_4,nkind_etype_4)   &
     &      = reshape(                                                  &
     &       (/ 1, 4,  9,   6, 7, 11,      2, 1, 10,    7, 8, 12,       &
     &          3, 2, 11,   8, 5,  9,      4, 3, 12,    5, 6, 10/),     &
     &       shape=(/nnod_tri,num_patch_4,nkind_etype_4/) )
!
      end module m_intersection_data_4
