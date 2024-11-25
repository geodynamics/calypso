!>@file   set_iflag_for_used_ele.f90
!!@brief  module set_iflag_for_used_ele
!!
!!@author  H. Matsui
!!@date Programmed in May. 2006
!
!>@brief Mark used elements for field line and PVR
!!
!!@verbatim
!!      subroutine s_set_iflag_for_used_ele                             &
!!     &         (ele, ele_grp, ngrp_ele, id_ele_grp, iflag_used_ele)
!!      subroutine set_iflag_used_ele_w_overlap(ele, ele_grp,           &
!!     &          ngrp_ele, id_ele_grp, iflag_used_ele)
!!        type(element_data), intent(in) :: ele
!!        type(group_data), intent(in) :: ele_grp
!!@endverbatim
!
      module set_iflag_for_used_ele
!
      use m_precision
      use t_geometry_data
      use t_group_data
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_iflag_for_used_ele                               &
     &         (ele, ele_grp, ngrp_ele, id_ele_grp, iflag_used_ele)
!
      type(element_data), intent(in) :: ele
      type(group_data), intent(in) :: ele_grp
!
      integer(kind = kint), intent(in) :: ngrp_ele
      integer(kind = kint), intent(in) :: id_ele_grp(ngrp_ele)
      integer(kind = kint), intent(inout) :: iflag_used_ele(ele%numele)
!
      integer(kind = kint) :: jgrp, jnum, jele, jg, jst, jed
!
!
!$omp parallel workshare
      iflag_used_ele(1:ele%numele) = 0
!$omp end parallel workshare
!
      do jgrp = 1, ngrp_ele
        jg = id_ele_grp(jgrp)
        if(jg .le. 0) then
!$omp parallel do
          do jele = 1, ele%numele
            if(ele%interior_ele(jele) .gt. 0) iflag_used_ele(jele) = 1
          end do
!$omp end parallel do
        else
          jst = ele_grp%istack_grp(jg-1) + 1
          jed = ele_grp%istack_grp(jg)
!$omp parallel do private(jnum,jele)
          do jnum = jst, jed
            jele = ele_grp%item_grp(jnum)
            if(ele%interior_ele(jele) .gt. 0) iflag_used_ele(jele) = 1
          end do
!$omp end parallel do
        end if
      end do
!
      end subroutine s_set_iflag_for_used_ele
!
!  ---------------------------------------------------------------------
!
      subroutine set_iflag_used_ele_w_overlap(ele, ele_grp,             &
     &          ngrp_ele, id_ele_grp, iflag_used_ele)
!
      type(element_data), intent(in) :: ele
      type(group_data), intent(in) :: ele_grp
!
      integer(kind = kint), intent(in) :: ngrp_ele
      integer(kind = kint), intent(in) :: id_ele_grp(ngrp_ele)
      integer(kind = kint), intent(inout) :: iflag_used_ele(ele%numele)
!
      integer(kind = kint) :: jgrp, jnum, jele, jg, jst, jed
!
!
!$omp parallel workshare
      iflag_used_ele(1:ele%numele) = 0
!$omp end parallel workshare
!
      do jgrp = 1, ngrp_ele
        jg = id_ele_grp(jgrp)
        if(jg .le. 0) then
!$omp parallel do
          do jele = 1, ele%numele
            iflag_used_ele(jele) = 1
          end do
!$omp end parallel do
        else
          jst = ele_grp%istack_grp(jg-1) + 1
          jed = ele_grp%istack_grp(jg)
!$omp parallel do private(jnum,jele)
          do jnum = jst, jed
            jele = ele_grp%item_grp(jnum)
            iflag_used_ele(jele) = 1
          end do
!$omp end parallel do
        end if
      end do
!
      end subroutine set_iflag_used_ele_w_overlap
!
!  ---------------------------------------------------------------------
!
      end module set_iflag_for_used_ele
