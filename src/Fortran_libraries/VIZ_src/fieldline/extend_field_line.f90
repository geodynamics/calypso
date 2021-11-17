!>@file  extend_field_line.f90
!!       module extend_field_line
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2011
!
!> @brief extend field line in each domain
!!
!!@verbatim
!!      subroutine s_extend_field_line(node, ele, surf,                 &
!!     &          max_line_step, iflag_used_ele, iflag_dir,             &
!!     &          vect_nod, color_nod, isurf_org, x4_start, v4_start,   &
!!     &          c_field, icount_line, iflag_comm, fline_lc)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        integer(kind = kint), intent(in) :: iflag_dir, max_line_step
!!        integer(kind = kint), intent(in) :: iflag_used_ele(ele%numele)
!!        real(kind = kreal), intent(in) :: vect_nod(node%numnod,3)
!!        real(kind = kreal), intent(in) :: color_nod(node%numnod)
!!        integer(kind = kint), intent(inout) :: isurf_org(3)
!!        integer(kind = kint), intent(inout) :: icount_line, iflag_comm
!!        real(kind = kreal), intent(inout) ::   v4_start(4), x4_start(4)
!!        real(kind = kreal), intent(inout) ::   c_field(1)
!!        type(local_fieldline), intent(inout) :: fline_lc
!!@endverbatim
!
      module extend_field_line
!
      use m_precision
!
      use m_constants
      use m_geometry_constants
      use calypso_mpi
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_extend_field_line(node, ele, surf,                   &
     &          max_line_step, iflag_used_ele, iflag_dir,               &
     &          vect_nod, color_nod, isurf_org, x4_start, v4_start,     &
     &          c_field, icount_line, iflag_comm, fline_lc)
!
      use t_geometry_data
      use t_surface_data
      use t_local_fline
      use cal_field_on_surf_viz
      use cal_fline_in_cube
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
!
      integer(kind = kint), intent(in) :: iflag_dir, max_line_step
      integer(kind = kint), intent(in) :: iflag_used_ele(ele%numele)
      real(kind = kreal), intent(in) :: vect_nod(node%numnod,3)
      real(kind = kreal), intent(in) :: color_nod(node%numnod)
!
      integer(kind = kint), intent(inout) :: isurf_org(3)
      integer(kind = kint), intent(inout) :: icount_line, iflag_comm
      real(kind = kreal), intent(inout) ::   v4_start(4), x4_start(4)
      real(kind = kreal), intent(inout) ::   c_field(1)
!
      type(local_fieldline), intent(inout) :: fline_lc
!
      integer(kind = kint) :: isf_tgt, isurf_end, iele, isf_org
      real(kind = kreal) :: x4_tgt(4), v4_tgt(4), c_tgt(1)
      real(kind = kreal) :: xi(2), flux
      real(kind = kreal) :: xx4_ele_surf(4,num_linear_sf,nsurf_4_ele)
!
!
      if(isurf_org(1) .eq. 0) then
        iflag_comm = 0
        return
      end if
!
      call add_fline_start(x4_start, c_field(1), fline_lc)
!
       do
        icount_line = icount_line + 1
        iele =    isurf_org(1)
        isf_org = isurf_org(2)
!
!   extend in the middle of element
        call position_on_each_ele_surfs                                 &
     &     (surf, node%numnod, node%xx, iele, xx4_ele_surf)
        call find_line_end_in_1ele(iflag_dir,                           &
     &      isf_org, v4_start, x4_start, xx4_ele_surf,                  &
     &      isf_tgt, x4_tgt, xi)
!
        if(isf_tgt .eq. 0) then
          iflag_comm = -1
          exit
        end if
!
        isurf_end = abs(surf%isf_4_ele(iele,isf_tgt))
        call cal_field_on_surf_vect4                                    &
     &     (node%numnod, surf%numsurf, surf%nnod_4_surf,                &
     &      surf%ie_surf, isurf_end, xi, vect_nod, v4_tgt)
        call cal_field_on_surf_scalar                                   &
     &     (node%numnod, surf%numsurf, surf%nnod_4_surf,                &
     &      surf%ie_surf, isurf_end, xi, color_nod, c_tgt(1))
!
        isf_org =  0
        x4_start(1:4) = half * (x4_start(1:4) + x4_tgt(1:4))
        v4_start(1:4) = half * (v4_start(1:4) + v4_tgt(1:4))
        c_field(1) =   half * (c_field(1) + c_tgt(1))
!
        call add_fline_list(x4_start, c_field(1), fline_lc)
!
!   extend to surface of element
        call position_on_each_ele_surfs                                 &
     &     (surf, node%numnod, node%xx, iele, xx4_ele_surf)
        call find_line_end_in_1ele(iflag_dir,                           &
     &      isf_org, v4_start, x4_start, xx4_ele_surf,                  &
     &      isf_tgt, x4_tgt, xi)
!
        if(isf_tgt .eq. 0) then
          iflag_comm = -1
          exit
        end if
!
        isurf_end = abs(surf%isf_4_ele(iele,isf_tgt))
        call cal_field_on_surf_vect4                                    &
     &     (node%numnod, surf%numsurf, surf%nnod_4_surf,                &
     &      surf%ie_surf, isurf_end, xi, vect_nod, v4_start)
        call cal_field_on_surf_scalar                                   &
     &     (node%numnod, surf%numsurf, surf%nnod_4_surf,                &
     &      surf%ie_surf, isurf_end, xi, color_nod, c_field(1))
        x4_start(1:4) =  x4_tgt(1:4)
!
        call add_fline_list(x4_start, c_field(1), fline_lc)
!
        flux = (v4_start(1) * surf%vnorm_surf(isurf_end,1)              &
     &        + v4_start(2) * surf%vnorm_surf(isurf_end,2)              &
     &        + v4_start(3) * surf%vnorm_surf(isurf_end,3))             &
     &         * dble(surf%isf_4_ele(iele,isf_tgt) / isurf_end)         &
     &         *(-one)**iflag_dir
!
!         write(60+my_rank,'(a6,i8,1p4e16.7)')  'x_tgt: ', icount_line, &
!     &          v4_start(1:4), flux
!
        if(surf%interior_surf(isurf_end) .eq. izero) then
          isurf_org(1) = iele
          isurf_org(2) = isf_tgt
          isurf_org(3) = surf%ie_surf(isurf_end,1)
          iflag_comm = 1
          exit
        end if
!
!   set backside element and surface 
!
        if(flux.ge.zero) then
          if(surf%isf_4_ele(iele,isf_tgt) .lt. 0) then
            isurf_org(1) = surf%iele_4_surf(isurf_end,1,1)
            isurf_org(2) = surf%iele_4_surf(isurf_end,1,2)
          else
            isurf_org(1) = surf%iele_4_surf(isurf_end,2,1)
            isurf_org(2) = surf%iele_4_surf(isurf_end,2,2)
          end if
        else
          iflag_comm = -2
          exit
        end if
!
!         write(70+my_rank,*) 'isurf_end', icount_line, iele, isf_tgt,  &
!     &                        surf%isf_4_ele(iele,isf_tgt)
!         write(70+my_rank,*) 'isurf_nxt', icount_line, isurf_org(1:2), &
!     &                        surf%isf_4_ele(isurf_org(1),isurf_org(2))
!
        if(isurf_org(1).eq.0 .or.  iflag_used_ele(iele).eq.0            &
     &     .or. icount_line.gt.max_line_step) then
          iflag_comm = 0
          exit
        end if
      end do
!
      end subroutine s_extend_field_line
!
!  ---------------------------------------------------------------------
!
      end module extend_field_line

