!>@file   set_control_each_fline.f90
!!@brief  module set_control_each_fline
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Set seed points on surface
!!
!!@verbatim
!!      integer(kind = kint) function count_fline_start_surf            &
!!     &                            (node, ele, surf, isf_4_ele_dbl,    &
!!     &                             nod_fld, fln_prm, fln_src)
!!      subroutine set_fline_start_surf(node, ele, surf, isf_4_ele_dbl, &
!!     &          nod_fld, fln_prm, fln_src, fln_tce)
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(phys_data), intent(in) :: nod_fld
!!        integer(kind = kint), intent(in)                              &
!!     &               :: isf_4_ele_dbl(ele%numele,nsurf_4_ele,2)
!!        type(fieldline_paramter), intent(in) :: fln_prm
!!        type(each_fieldline_source), intent(in) :: fln_src
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
!!@endverbatim
!
      module set_fline_start_surface
!
      use m_precision
      use m_geometry_constants
      use t_geometry_data
      use t_surface_data
      use t_tracing_data
      use calypso_mpi
!
      implicit  none
!
      private :: check_fline_start_surf, choose_fline_start_surf
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function count_fline_start_surf              &
     &                            (node, ele, surf, isf_4_ele_dbl,      &
     &                             nod_fld, fln_prm, fln_src)
!
      use m_constants
      use m_geometry_constants
      use t_phys_data
      use t_control_params_4_fline
      use t_source_of_filed_line
      use cal_field_on_surf_viz
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      integer(kind = kint), intent(in)                                  &
     &               :: isf_4_ele_dbl(ele%numele,nsurf_4_ele,2)
      type(phys_data), intent(in) :: nod_fld
      type(fieldline_paramter), intent(in) :: fln_prm
      type(each_fieldline_source), intent(inout) :: fln_src
!
      integer(kind = kint)  :: icou
      integer(kind = kint)  :: i, iele, isf_1ele, isurf
      real(kind = kreal), parameter :: xi(2) = (/zero, zero/)
      real(kind = kreal) :: vec_surf(3), flux
!
!
      icou = 0
      do i = 1, fln_src%num_line_local
        iele =     fln_prm%id_surf_start_fline(1,i)
        isf_1ele = fln_prm%id_surf_start_fline(2,i)
        isurf = abs(surf%isf_4_ele(iele,isf_1ele))
        fln_src%xx4_initial_fline(1:3,i) = surf%x_surf(isurf,1:3)
        fln_src%xx4_initial_fline(4,i) =   1.0d0
!
        call cal_field_on_surf_vector                                   &
     &     (node%numnod, surf%numsurf, surf%nnod_4_surf, surf%ie_surf,  &
     &      isurf, xi, nod_fld%d_fld(1,fln_prm%iphys_4_fline),          &
     &      vec_surf)
!
        flux = (vec_surf(1) * surf%vnorm_surf(isurf,1)                  &
     &        + vec_surf(2) * surf%vnorm_surf(isurf,2)                  &
     &        + vec_surf(3) * surf%vnorm_surf(isurf,3))                 &
     &         * dble(surf%isf_4_ele(iele,isf_1ele) / isurf)
        if(flux .eq. zero) then
           fln_src%iflag_outward_flux_fline(i) =  1
        else
           fln_src%iflag_outward_flux_fline(i) =  int(flux / abs(flux))
        end if
!
        if(fln_prm%id_fline_direction .ne. iflag_both_trace) then
          icou = icou + check_fline_start_surf                          &
     &                       (fln_src%iflag_outward_flux_fline(i),      &
     &                       iele, isf_1ele, isurf, ele, surf,          &
     &                       isf_4_ele_dbl, fln_prm%id_fline_direction)
        else
          icou = icou + check_fline_start_surf                          &
     &                      (fln_src%iflag_outward_flux_fline(i),       &
     &                       iele, isf_1ele, isurf, ele, surf,          &
     &                       isf_4_ele_dbl, iflag_forward_trace)
          icou = icou + check_fline_start_surf                          &
     &                      (fln_src%iflag_outward_flux_fline(i),       &
     &                       iele, isf_1ele, isurf, ele, surf,          &
     &                       isf_4_ele_dbl, iflag_backward_trace)
        end if
      end do
      count_fline_start_surf = icou
!
      end function count_fline_start_surf
!
!  ---------------------------------------------------------------------
!
      subroutine set_fline_start_surf(node, ele, surf, isf_4_ele_dbl,   &
     &          nod_fld, fln_prm, fln_src, fln_tce)
!
      use m_constants
      use m_geometry_constants
      use t_phys_data
      use t_control_params_4_fline
      use t_source_of_filed_line
!
      use cal_field_on_surf_viz
      use trace_in_element
      use tracer_field_interpolate
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      integer(kind = kint), intent(in)                                  &
     &               :: isf_4_ele_dbl(ele%numele,nsurf_4_ele,2)
      type(phys_data), intent(in) :: nod_fld
      type(fieldline_paramter), intent(in) :: fln_prm
      type(each_fieldline_source), intent(in) :: fln_src
!
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
      integer(kind = kint)  :: icou
!
      integer(kind = kint)  :: i, iele, isf_1ele, isurf
      real(kind = kreal) :: xyz_surf(4), vec_surf(4)
      real(kind = kreal), parameter :: xi(2) = (/zero, zero/)
!
      integer(kind = kint)  :: isf_dbl_st_tmp(2)
!
!
      icou = 0
      do i = 1, fln_src%num_line_local
        iele =     fln_prm%id_surf_start_fline(1,i)
        isf_1ele = fln_prm%id_surf_start_fline(2,i)
        isurf = abs(surf%isf_4_ele(iele,isf_1ele))
!
        xyz_surf(1:3) = surf%x_surf(isurf,1:3)
        xyz_surf(4) =   1.0d0
        call cal_field_on_surf_vector                                   &
     &     (node%numnod, surf%numsurf, surf%nnod_4_surf, surf%ie_surf,  &
     &      isurf, xi, nod_fld%d_fld(1,fln_prm%iphys_4_fline),          &
     &      vec_surf(1))
        vec_surf(4) = one
!
        if(fln_prm%id_fline_direction .ne. iflag_both_trace) then
          call choose_fline_start_surf                                  &
     &       (fln_src%iflag_outward_flux_fline(i),                      &
     &        iele, isf_1ele, isurf, ele, surf, isf_4_ele_dbl,          &
     &        fln_prm%id_fline_direction, isf_dbl_st_tmp)
          if(isf_dbl_st_tmp(2) .le. 0) cycle
!
          icou = icou + 1
          fln_tce%iline_original(icou) = i                              &
     &          + fln_tce%istack_current_fline(my_rank)
          fln_tce%iflag_direction(icou) = fln_prm%id_fline_direction
          fln_tce%isf_dbl_start(1,icou) = my_rank
          fln_tce%isf_dbl_start(2,icou) = isf_dbl_st_tmp(1)
          fln_tce%isf_dbl_start(3,icou) = isf_dbl_st_tmp(2)
          fln_tce%xx_fline_start(1:4,icou) = xyz_surf(1:4)
          fln_tce%v_fline_start(1:4,icou) =  vec_surf(1:4)
          fln_tce%trace_length(icou) = 0.0d0
          fln_tce%icount_fline(icou) = 0
!
          call cal_fields_on_line(isurf, xi, xyz_surf(1),               &
     &                          surf, nod_fld, fln_prm%fline_fields,    &
     &                          fln_tce%c_fline_start(1,icou))
        else
          fln_tce%iflag_direction(icou) = iflag_forward_trace
!
          call choose_fline_start_surf                                  &
     &       (fln_src%iflag_outward_flux_fline(i),                      &
     &        iele, isf_1ele, isurf, ele, surf, isf_4_ele_dbl,          &
     &        iflag_forward_trace, isf_dbl_st_tmp)
!
          if(isf_dbl_st_tmp(2) .gt. 0) then
            icou = icou + 1
            fln_tce%iline_original(icou) = i                            &
     &            + fln_tce%istack_current_fline(my_rank)
            fln_tce%iflag_direction(icou) = iflag_backward_trace
            fln_tce%isf_dbl_start(1,icou) = my_rank
            fln_tce%isf_dbl_start(2,icou) = isf_dbl_st_tmp(1)
            fln_tce%isf_dbl_start(3,icou) = isf_dbl_st_tmp(2)
            fln_tce%xx_fline_start(1:4,icou) = xyz_surf(1:4)
            fln_tce%v_fline_start(1:4,icou) =  vec_surf(1:4)
            fln_tce%trace_length(icou) = 0.0d0
            fln_tce%icount_fline(icou) = 0
!
            call cal_fields_on_line(isurf, xi, xyz_surf(1),             &
     &                          surf, nod_fld, fln_prm%fline_fields,    &
     &                          fln_tce%c_fline_start(1,icou))
          end if
!
          call choose_fline_start_surf                                  &
     &        (fln_src%iflag_outward_flux_fline(i),                     &
     &         iele, isf_1ele, isurf, ele, surf, isf_4_ele_dbl,         &
     &         iflag_backward_trace, isf_dbl_st_tmp)
          if(isf_dbl_st_tmp(2) .le. 0) cycle
!
          icou = icou + 1
          fln_tce%iflag_direction(icou) = iflag_backward_trace
          fln_tce%isf_dbl_start(1,icou) = my_rank
          fln_tce%isf_dbl_start(2,icou) = isf_dbl_st_tmp(1)
          fln_tce%isf_dbl_start(3,icou) = isf_dbl_st_tmp(2)
          fln_tce%trace_length(icou) = 0.0d0
          fln_tce%icount_fline(icou) = 0
          call copy_global_start_fline(icou, (icou-1),                  &
     &                                 fln_prm%fline_fields, fln_tce)
        end if
      end do
!
      end subroutine set_fline_start_surf
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(kind = kint) function check_fline_start_surf              &
     &         (iflag_outward_flux, iele, isf_1ele, isurf, ele, surf,   &
     &          isf_4_ele_dbl, iflag_direction)
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      integer(kind = kint), intent(in)                                  &
     &               :: isf_4_ele_dbl(ele%numele,nsurf_4_ele,2)
      integer(kind = kint), intent(in) :: iflag_outward_flux
      integer(kind = kint), intent(in) :: iele, isf_1ele, isurf
      integer(kind = kint), intent(in) :: iflag_direction
!
      integer(kind = kint) :: iflag
!
!
      check_fline_start_surf = 1
      if((iflag_direction*iflag_outward_flux) .le. 0) then
        iflag = isf_1ele
      else if(isf_4_ele_dbl(iele,isf_1ele,2) .le. 0) then
        iflag = surf%iele_4_surf(isurf,1,2)
      else
        iflag = surf%iele_4_surf(isurf,2,2)
      end if
      if(iflag .eq. 0) check_fline_start_surf = 0
!
      end function check_fline_start_surf
!
!  ---------------------------------------------------------------------
!
      subroutine choose_fline_start_surf                                &
     &         (iflag_outward_flux, iele, isf_1ele, isurf, ele, surf,   &
     &          isf_4_ele_dbl, iflag_direction, isf_dbl_start)
!
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      integer(kind = kint), intent(in)                                  &
     &               :: isf_4_ele_dbl(ele%numele,nsurf_4_ele,2)
      integer(kind = kint), intent(in) :: iflag_outward_flux
      integer(kind = kint), intent(in) :: iele, isf_1ele, isurf
!
      integer(kind = kint), intent(in) :: iflag_direction
      integer(kind = kint), intent(inout) :: isf_dbl_start(2)
!
!
      if((iflag_direction*iflag_outward_flux) .le. 0) then
        isf_dbl_start(1) = iele
        isf_dbl_start(2) = isf_1ele
      else if(isf_4_ele_dbl(iele,isf_1ele,2) .le. 0) then
        isf_dbl_start(1) = surf%iele_4_surf(isurf,1,1)
        isf_dbl_start(2) = surf%iele_4_surf(isurf,1,2)
      else
        isf_dbl_start(1) = surf%iele_4_surf(isurf,2,1)
        isf_dbl_start(2) = surf%iele_4_surf(isurf,2,2)
      end if
!
      end subroutine choose_fline_start_surf
!
!  ---------------------------------------------------------------------
!
      end module set_fline_start_surface
