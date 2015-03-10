!
!      module set_area_4_viz
!
!        programmed by H.Matsui on May, 2006
!
!      subroutine count_area_4_viz(num_mat, mat_name,                   &
!     &          num_area_grp, area_ele_grp, ngrp_area)
!      subroutine s_set_area_4_viz(num_mat, mat_name,                   &
!     &          num_area_grp, area_ele_grp, ngrp_area, id_ele_grp_psf)
!
!      subroutine set_surf_grp_id_4_viz(num_surf, surf_name,            &
!     &          chosen_surf_grp, id_surf_grp)
!
      module set_area_4_viz
!
      use m_precision
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_area_4_viz(num_mat, mat_name,                    &
     &          num_area_grp, area_ele_grp, ngrp_area)
!
      integer(kind = kint), intent(in) :: num_mat
      character(len=kchara), intent(in) :: mat_name(num_mat)
!
      integer(kind = kint), intent(in) :: num_area_grp
      character(len=kchara), intent(in) :: area_ele_grp(num_area_grp)
!
      integer(kind = kint), intent(inout) :: ngrp_area
!
      integer(kind = kint) :: i, id
!
!
      ngrp_area = 0
      if (num_area_grp .eq. 0) then
        ngrp_area = 1
      else if ( area_ele_grp(1).eq.'all' .or. area_ele_grp(1).eq.'All'  &
     &     .or. area_ele_grp(1).eq.'ALL') then
        ngrp_area = 1
      else
        do i = 1, num_area_grp
          do id = 1, num_mat
            if ( area_ele_grp(i) .eq. mat_name(id) ) then
              ngrp_area = ngrp_area + 1
            end if
          end do
        end do
      end if
!
      end subroutine count_area_4_viz
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_area_4_viz(num_mat, mat_name,                    &
     &          num_area_grp, area_ele_grp, ngrp_area, id_ele_grp_psf)
!
      integer(kind = kint), intent(in) :: num_mat
      character(len=kchara), intent(in) :: mat_name(num_mat)
!
      integer(kind = kint), intent(in) :: num_area_grp
      integer(kind = kint), intent(in) :: ngrp_area
      character(len=kchara), intent(in) :: area_ele_grp(num_area_grp)
!
      integer(kind = kint), intent(inout) :: id_ele_grp_psf(ngrp_area)
!
      integer(kind = kint) :: i, id, icou
!
!
      icou = 0
      if (num_area_grp .eq. 0) then
        id_ele_grp_psf(1) = 0
      else if ( area_ele_grp(1).eq.'all' .or. area_ele_grp(1).eq.'All'  &
     &     .or. area_ele_grp(1).eq.'ALL') then
        id_ele_grp_psf(1) = 0
      else
        do i = 1, num_area_grp
          do id = 1, num_mat
            if ( area_ele_grp(i) .eq. mat_name(id) ) then
              icou = icou + 1
              id_ele_grp_psf(icou) = id
            end if
          end do
        end do
      end if
!
      end subroutine s_set_area_4_viz
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_surf_grp_id_4_viz(num_surf, surf_name,             &
     &          chosen_surf_grp, id_surf_grp)
!
      integer(kind = kint), intent(in) :: num_surf
      character(len=kchara), intent(in) :: surf_name(num_surf)
!
      character(len=kchara), intent(in) :: chosen_surf_grp
      integer(kind = kint), intent(inout) :: id_surf_grp
!
      integer(kind = kint) :: id
!
!
      do id = 1, num_surf
        if ( chosen_surf_grp .eq. surf_name(id)) then
          id_surf_grp = id
          exit
        end if
      end do
!
      end subroutine set_surf_grp_id_4_viz
!
!  ---------------------------------------------------------------------
!
      end module set_area_4_viz
