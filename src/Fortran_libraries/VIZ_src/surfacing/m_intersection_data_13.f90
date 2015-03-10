!
!     module m_intersection_data_13
!
      module m_intersection_data_13
!
!      Written by H. Matsui on june, 2006
!
      use m_precision
!
      implicit none
!
!
!   4 nodes which have differrent sign remote each other
!      (cover negative markers 4 triangles)
!
!     flag = 1: c-c_ref >= 0
!     flag = 1: c-c_ref < 0
!
!        12345678: boundary..all(12): 4triangles
!  165:  10100101: 3-4,4-8,4-1; 1-2,2-6,2-3; 1-5,8-5,5-6; 6-7,7-8,3-7
!                    3-12-4, 1-10-2, 9-8-5, 6-7-11
!  90:   01011010: 1-2,4-1,1-5; 5-6,6-7,2-6; 3-4,2-3,3-7; 4-8,7-8,8-5
!                    1-4-9, 5-6-10, 3-2-11, 12-7-8
!
      integer(kind = kint), parameter, private :: nnod_tri = 3
!
      integer(kind = kint), parameter :: nkind_etype_13 = 2
      integer(kind = kint), parameter :: num_tri_13 =     4
      integer(kind = kint), parameter :: num_patch_13 =   4
      integer(kind = kint), parameter :: itri_2_patch_13(3)             &
     &     = (/1, 2, 3/)
!
      integer(kind = kint), parameter                                   &
     &   :: iflag_psf_etype_13(nkind_etype_13) = (/165, 90/)
!
      integer(kind = kint), parameter                                   &
     &        :: iedge_tri_13(nnod_tri,num_tri_13,nkind_etype_13)       &
     &      = reshape(                                                  &
     &       (/ 3, 12, 4,    1, 10,  2,    9, 8,  5,     6, 7, 11,      &
                1,  4, 9,    5,  6, 10,    3, 2, 11,    12, 7,  8/),    &
     &       shape=(/nnod_tri,num_tri_13,nkind_etype_13/) )
!
      integer(kind = kint), parameter                                   &
     &        :: iedge_4_patch_13(nnod_tri,num_patch_13,nkind_etype_13) &
     &      = reshape(                                                  &
     &       (/ 3, 12, 4,    1, 10,  2,    9, 8,  5,     6, 7, 11,      &
                1,  4, 9,    5,  6, 10,    3, 2, 11,    12, 7,  8/),    &
     &       shape=(/nnod_tri,num_patch_13,nkind_etype_13/) )
!
      end module m_intersection_data_13
