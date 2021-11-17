!set_fline_start_surface.f90
!
!      module set_fline_start_surface
!
!      Written by H. Matsui on Aug., 2011
!
!!      subroutine set_fline_start_surf(id_rank, numnod, numele,        &
!!     &          numsurf, nnod_4_surf, ie_surf, isf_4_ele, iele_4_surf,&
!!     &          fln_prm, fln_src, fln_tce)
!!        type(fieldline_paramter), intent(in) :: fln_prm
!!        type(each_fieldline_source), intent(in) :: fln_src
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
!
      module set_fline_start_surface
!
      use m_precision
!
      implicit  none
!
      private :: set_forward_fline_start_surf
      private :: set_backward_fline_start_surf
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_fline_start_surf(id_rank, numnod, numele,          &
     &          numsurf, nnod_4_surf, ie_surf, isf_4_ele, iele_4_surf,  &
     &          fln_prm, fln_src, fln_tce)
!
      use m_constants
      use m_geometry_constants
      use t_control_params_4_fline
      use t_source_of_filed_line
!
      use cal_field_on_surf_viz
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: numnod, numele, numsurf
      integer(kind = kint), intent(in) :: nnod_4_surf
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: isf_4_ele(numele,nsurf_4_ele)
      integer(kind = kint), intent(in) :: iele_4_surf(numsurf,2,2)
!
      type(fieldline_paramter), intent(in) :: fln_prm
      type(each_fieldline_source), intent(in) :: fln_src
!
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
      integer(kind = kint)  :: i, iele, isf_1ele, isurf
      integer(kind = kint)  :: inum1, inum2
      real(kind = kreal), parameter :: xi(2) = (/zero, zero/)
!
!
      do i = 1, fln_src%num_line_local
        inum1 = i + fln_tce%istack_current_fline(id_rank)
        iele =     fln_prm%id_surf_start_fline(1,i)
        isf_1ele = fln_prm%id_surf_start_fline(2,i)
        isurf = abs(isf_4_ele(iele,isf_1ele))
!
        fln_tce%xx_fline_start(1:4,inum1)                               &
     &       = fln_src%xx4_initial_fline(1:4,i)
!
        call cal_field_on_surf_vect4(numnod, numsurf, nnod_4_surf,      &
     &      ie_surf, isurf, xi, fln_src%vector_nod_fline,               &
     &      fln_tce%v_fline_start(1,inum1))
        call cal_field_on_surf_scalar(numnod, numsurf, nnod_4_surf,     &
     &      ie_surf, isurf, xi, fln_src%color_nod_fline,                &
     &      fln_tce%c_fline_start(inum1))
!
        if(fln_prm%id_fline_direction .eq. iflag_forward_trace) then
           call set_forward_fline_start_surf                            &
     &        (fln_prm%iflag_outward_flux_fline(i),                     &
     &         iele, isf_1ele, isurf,                                   &
     &         numsurf, nnod_4_surf, ie_surf, iele_4_surf,              &
     &         fln_tce%iflag_fline(inum1),                              &
     &         fln_tce%isf_fline_start(1,inum1))
!
        else if(fln_prm%id_fline_direction .eq. iflag_backward_trace)   &
     &      then
           call set_backward_fline_start_surf                           &
     &         (fln_prm%iflag_outward_flux_fline(i),                    &
     &          iele, isf_1ele, isurf,                                  &
     &          numsurf, nnod_4_surf, ie_surf, iele_4_surf,             &
     &          fln_tce%iflag_fline(inum1),                             &
     &          fln_tce%isf_fline_start(1,inum1))
!
        else
           call set_forward_fline_start_surf                            &
     &         (fln_prm%iflag_outward_flux_fline(i),                    &
     &          iele, isf_1ele, isurf,                                  &
     &          numsurf, nnod_4_surf, ie_surf, iele_4_surf,             &
     &          fln_tce%iflag_fline(inum1),                             &
     &          fln_tce%isf_fline_start(1,inum1))
!
          inum2 = inum1 + fln_src%num_line_local
          fln_tce%xx_fline_start(1:4,inum2)                             &
     &          = fln_tce%xx_fline_start(1:4,inum1)
          fln_tce%v_fline_start(1:4,inum2)                              &
     &          = fln_tce%v_fline_start(1:4,inum1)
          fln_tce%c_fline_start(inum2) = fln_tce%c_fline_start(inum1)
!
           call set_backward_fline_start_surf                           &
     &         (fln_prm%iflag_outward_flux_fline(i),                    &
     &          iele, isf_1ele, isurf,                                  &
     &          numsurf, nnod_4_surf, ie_surf, iele_4_surf,             &
     &          fln_tce%iflag_fline(inum2),                             &
     &          fln_tce%isf_fline_start(1,inum2))
        end if
      end do
!
      end subroutine set_fline_start_surf
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_forward_fline_start_surf(iflag_outward_flux,       &
     &          iele, isf_1ele, isurf, numsurf, nnod_4_surf,            &
     &          ie_surf, iele_4_surf, iflag_fline, isf_fline_start)
!
      integer(kind = kint), intent(in) :: iflag_outward_flux
      integer(kind = kint), intent(in) :: iele, isf_1ele, isurf
      integer(kind = kint), intent(in) :: numsurf, nnod_4_surf
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: iele_4_surf(numsurf,2,2)
!
      integer(kind = kint), intent(inout) :: iflag_fline
      integer(kind = kint), intent(inout) :: isf_fline_start(3)
!
      integer(kind = kint) :: inod
!
!
      inod =  ie_surf(isurf,1)
!
      iflag_fline = 0
      if(iflag_outward_flux .eq. 0) then
        isf_fline_start(1) = iele
        isf_fline_start(2) = isf_1ele
        isf_fline_start(3) = inod
      else
        isf_fline_start(1) = iele_4_surf(isurf,2,1)
        isf_fline_start(2) = iele_4_surf(isurf,2,2)
        isf_fline_start(3) = inod
      end if
!
      end subroutine set_forward_fline_start_surf
!
!  ---------------------------------------------------------------------
!
      subroutine set_backward_fline_start_surf(iflag_outward_flux,      &
     &          iele, isf_1ele, isurf, numsurf, nnod_4_surf,            &
     &          ie_surf, iele_4_surf, iflag_fline, isf_fline_start)
!
      integer(kind = kint), intent(in) :: iflag_outward_flux
      integer(kind = kint), intent(in) :: iele, isf_1ele, isurf
      integer(kind = kint), intent(in) :: numsurf, nnod_4_surf
      integer(kind = kint), intent(in) :: ie_surf(numsurf,nnod_4_surf)
      integer(kind = kint), intent(in) :: iele_4_surf(numsurf,2,2)
!
      integer(kind = kint), intent(inout) :: iflag_fline
      integer(kind = kint), intent(inout) :: isf_fline_start(3)
!
      integer(kind = kint) :: inod
!
!
      inod =  ie_surf(isurf,1)
!
      iflag_fline = 1
!
      if(iflag_outward_flux .eq. 1) then
        isf_fline_start(1) = iele
        isf_fline_start(2) = isf_1ele
        isf_fline_start(3) = inod
      else
        isf_fline_start(1) = iele_4_surf(isurf,2,1)
        isf_fline_start(2) = iele_4_surf(isurf,2,2)
        isf_fline_start(3) = inod
      end if
!
      end subroutine set_backward_fline_start_surf
!
!  ---------------------------------------------------------------------
!
      end module set_fline_start_surface
