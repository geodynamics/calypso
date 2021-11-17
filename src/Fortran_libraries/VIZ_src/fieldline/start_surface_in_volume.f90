!start_surface_in_volume.f90
!
!      module start_surface_in_volume
!
!      Written by H. Matsui on Aug., 2011
!
!!      subroutine s_start_surface_by_volume                            &
!!     &         (ele, ele_grp, fln_prm, fln_src, fln_tce)
!!        type(group_data), intent(in) :: ele_grp
!!        type(fieldline_paramter), intent(inout) :: fln_prm
!!        type(each_fieldline_source), intent(inout) :: fln_src
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
!
      module start_surface_in_volume
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_machine_parameter
!
      use t_phys_data
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_control_params_4_fline
      use t_source_of_filed_line
!
      implicit  none
!
      private :: set_start_surface_in_domain, cal_volume_for_fline_area
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_start_surface_by_volume                              &
     &         (ele, ele_grp, fln_prm, fln_src, fln_tce)
!
      use calypso_mpi_real
      use extend_field_line
      use cal_field_on_surf_viz
      use set_fline_start_surface
!
      type(element_data), intent(in) :: ele
      type(group_data), intent(in) :: ele_grp
!
      type(fieldline_paramter), intent(inout) :: fln_prm
      type(each_fieldline_source), intent(inout) :: fln_src
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
      integer(kind = kint) :: ip
!
      real(kind = kreal) :: volume_local, total_volume, volume_start_l
!
      integer(kind = kint), allocatable :: iflag_ele(:)
!
!
      allocate(iflag_ele(ele%numele))
!
      if(iflag_debug .gt. 0) write(*,*) 'cal_volume_for_fline_area'
      call cal_volume_for_fline_area(ele, ele_grp,                      &
     &     fln_prm%nele_grp_area_fline, fln_prm%id_ele_grp_area_fline,  &
     &     iflag_ele, volume_local)
!
      call calypso_mpi_allgather_one_real(volume_local,                 &
     &                                    fln_tce%flux_stack_fline(1))
!
      fln_tce%flux_stack_fline(0) = 0.0d0
      do ip = 1, nprocs
        fln_tce%flux_stack_fline(ip)                                    &
     &                         = fln_tce%flux_stack_fline(ip-1)         &
     &                          + fln_tce%flux_stack_fline(ip)
      end do
      total_volume = fln_tce%flux_stack_fline(nprocs)
      volume_start_l = total_volume / dble(fln_prm%num_each_field_line)
!
      do ip = 1, nprocs
        fln_tce%num_current_fline(ip)                                   &
     &     = nint((fln_tce%flux_stack_fline(ip)                         &
     &      - fln_tce%flux_stack_fline(ip-1)) / volume_start_l)
      end do
      fln_src%num_line_local = fln_tce%num_current_fline(my_rank+1)
!
      if(i_debug .gt. 0) then
        write(my_rank+50,*)  'total_volume',                            &
     &                      volume_start_l, total_volume
        write(my_rank+50,*)  'original num_each_field_line',            &
     &                      fln_src%num_line_local
        write(my_rank+50,*)  'volume_start_l', volume_start_l
      end if
!
      if(iflag_debug .gt. 0) write(*,*) 'set_start_surface_in_domain'
      call set_start_surface_in_domain                                  &
     &   (ele, fln_src%num_line_local, volume_start_l, iflag_ele,       &
     &    fln_prm%num_each_field_line, fln_prm%id_surf_start_fline)
!
      deallocate(iflag_ele)
      call calypso_mpi_barrier
!
      end subroutine s_start_surface_by_volume
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_start_surface_in_domain                            &
     &         (ele, num_line_local, vol_4_start, iflag_ele,            &
     &          num_line, id_surf_start_fline)
!
      use extend_field_line
      use cal_field_on_surf_viz
      use set_fline_start_surface
!
      type(element_data), intent(in) :: ele
      integer(kind = kint), intent(in) :: num_line_local
      real(kind = kreal), intent(in) :: vol_4_start
      integer(kind = kint), intent(in) :: iflag_ele(ele%numele)
!
      integer(kind = kint), intent(in) :: num_line
!
      integer(kind = kint), intent(inout)                               &
     &                          :: id_surf_start_fline(2,num_line)
!
      integer(kind = kint) :: icou, iele
      real(kind = kreal) :: flux, ref_flux
!
!
      ref_flux = vol_4_start                                            &
     &          * dble(num_line_local) / dble(num_line_local+1)
      icou = 0
      if(num_line_local .gt. 0) then
        flux = 0.0d0
        do iele = 1, ele%numele
          flux = flux + ele%volume_ele(iele) * dble(iflag_ele(iele))
          if(flux .gt. ref_flux) then
            icou = icou + 1
            id_surf_start_fline(1,icou) = iele
            id_surf_start_fline(2,icou) = 5
            flux = 0.0d0
          end if
          if(icou .ge. num_line_local) exit
        end do
      end if
!
      end subroutine set_start_surface_in_domain
!
!  ---------------------------------------------------------------------
!
      subroutine cal_volume_for_fline_area(ele, ele_grp,                &
     &          num_area_grp, id_ele_grp_fline, iflag_ele, vol_l)
!
      type(element_data), intent(in) :: ele
      type(group_data), intent(in) :: ele_grp
!
      integer(kind = kint), intent(in) :: num_area_grp
      integer(kind = kint), intent(in)                                  &
     &                      :: id_ele_grp_fline(num_area_grp)
!
      integer(kind = kint), intent(inout) :: iflag_ele(ele%numele)
      real(kind = kreal), intent(inout) :: vol_l
!
      integer(kind = kint) :: i, igrp, ist, ied, inum, iele
!
!
!$omp parallel workshare
      iflag_ele(1:ele%numele) = 0
!$omp end parallel workshare
!
      do i = 1, num_area_grp
        igrp = id_ele_grp_fline(i)
        if(igrp .eq. 0) then
!$omp parallel workshare
          iflag_ele(1:ele%numele) = ele%interior_ele(1:ele%numele)
!$omp end parallel workshare
        else
          ist = ele_grp%istack_grp(igrp-1) + 1
          ied = ele_grp%istack_grp(igrp  )
!$omp parallel do private(inum,iele)
          do inum = ist, ied
            iele = ele_grp%item_grp(inum)
            iflag_ele(iele) = ele%interior_ele(iele)
          end do
!$omp end parallel do
        end if
      end do
!
      vol_l = 0.0d0
!$omp parallel do reduction(+:vol_l)
      do iele = 1, ele%numele
        vol_l = vol_l + ele%volume_ele(iele) * dble(iflag_ele(iele))
      end do
!$omp end parallel do
!
      end subroutine cal_volume_for_fline_area
!
!  ---------------------------------------------------------------------
!
      end module start_surface_in_volume
