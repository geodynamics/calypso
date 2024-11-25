!>@file   t_control_params_4_fline.f90
!!@brief  module t_control_params_4_fline
!!
!!@date  Programmed by H.Matsui in Aug. 2011
!
!>@brief control parameters for each field line
!!
!!@verbatim
!!      subroutine alloc_fline_starts_ctl(fln_prm)
!!      subroutine alloc_iflag_fline_used_ele(ele, fln_prm)
!!        type(element_data), intent(in) :: ele
!!        type(fieldline_paramter), intent(inout) :: fln_prm
!!
!!      subroutine dealloc_fline_starts_ctl(fln_prm)
!!      subroutine dealloc_iflag_fline_used_ele(fln_prm)
!!        type(fieldline_paramter), intent(inout) :: fln_prm
!!
!!      subroutine check_control_params_fline(fln_prm)
!!        type(fieldline_paramter), intent(in) :: fln_prm
!!@endverbatim
!
      module t_control_params_4_fline
!
      use m_precision
      use t_file_IO_parameter
      use t_ctl_params_viz_fields
!
      implicit  none
!
!
      integer(kind = kint), parameter :: id_fline_data_code = 11
      character(len=kchara), parameter                                  &
     &                      :: default_tracer_prefix = 'tracer'
!
!        integer(kind = kint) :: num_fline
!
      type fieldline_paramter
!>        File parameters for field line data file
        type(field_IO_params) :: fline_file_IO
!>        File parameters for tracer restart file
        type(field_IO_params) :: fline_rst_IO
!
!>        flag to use MPI_Bcast for data communication
        logical :: flag_use_broadcast
!
!>        Area of seed point
        integer(kind = kint) :: id_fline_seed_type = 0
!>        Direction of field line tracing
        integer(kind = kint) :: id_fline_direction = 0
!>        Distoribution of seed point
        integer(kind = kint) :: id_seed_distribution = 0
!
!>        Surface group ID for seed points
        integer(kind = kint) :: igrp_start_fline_surf_grp = 0
!
!>        Element group ID for seed points
        integer(kind = kint) :: igrp_start_fline_ele_grp = 0
!>        field ID to find reference density for seed points
        integer(kind = kint) :: ifield_4_density = 0
!>        field ID to find reference component for seed points
        integer(kind = kint) :: icomp_4_density = 0
!
!>        Element group ID for seed points
        integer(kind = kint) :: id_tracer_for_seed = 0
!
!>        Maximum step length for line tracing
        integer(kind = kint) :: max_line_stepping = 1000
!>        Maximum trace length for line tracing
        real(kind = kreal) :: max_trace_length =  1.0d30
!
!>        start address for of field data for fieldline
        integer(kind = kint) :: iphys_4_fline = 0
!
!>        control parameter for vizulization field output
        type(ctl_params_viz_fields) :: fline_fields
!
!
!>        Number of element group to use in fieldline
        integer(kind = kint) :: nele_grp_area_fline = 0
!>        Element group list to use in fieldline
        integer(kind = kint), allocatable :: id_ele_grp_area_fline(:)
!>        Element flag to use in fieldline
        integer(kind = kint), allocatable :: iflag_fline_used_ele(:)
!
!>        number of seed points
        integer(kind = kint) :: num_each_field_line = 0
!>        local surface ID for seed points
        integer(kind = kint), allocatable :: id_surf_start_fline(:,:)
!>        global surface ID for seed points
        integer(kind = kint), allocatable                               &
     &                       :: id_gl_surf_start_fline(:,:)

!>        Position list of seed point
        real(kind = kreal), allocatable :: xx_surf_start_fline(:,:)
      end type fieldline_paramter
!
!
      integer(kind = kint), parameter :: iflag_surface_group =   0
      integer(kind = kint), parameter :: iflag_surface_list =    1
      integer(kind = kint), parameter :: iflag_position_list =   2
      integer(kind = kint), parameter :: iflag_spray_in_domain = 3
      integer(kind = kint), parameter :: iflag_read_reastart =  10
      integer(kind = kint), parameter :: iflag_tracer_seeds =   20
!
!
      integer(kind = kint), parameter :: iflag_backward_trace = -1
      integer(kind = kint), parameter :: iflag_both_trace =      0
      integer(kind = kint), parameter :: iflag_forward_trace =   1
!
      integer(kind = kint), parameter :: iflag_random_by_amp =   0
      integer(kind = kint), parameter :: iflag_random_by_area =  1
      integer(kind = kint), parameter :: iflag_no_random =       2
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_fline_starts_ctl(fln_prm)
!
      type(fieldline_paramter), intent(inout) :: fln_prm
!
      integer(kind = kint) :: num
!
!
      num = fln_prm%nele_grp_area_fline
      allocate(fln_prm%id_ele_grp_area_fline(num))
      if(num .gt. 0) fln_prm%id_ele_grp_area_fline = 0
!
      num = fln_prm%num_each_field_line
      allocate(fln_prm%id_surf_start_fline(2,num))
      allocate(fln_prm%id_gl_surf_start_fline(2,num))
!
      allocate(fln_prm%xx_surf_start_fline(3,num))
!
      if(num .gt. 0) then
        fln_prm%id_surf_start_fline(1:2,1:num) =    0
        fln_prm%id_gl_surf_start_fline(1:2,1:num) = 0
        fln_prm%xx_surf_start_fline(1:3,1:num) =    0.0d0
      end if
!
      end subroutine alloc_fline_starts_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_iflag_fline_used_ele(ele, fln_prm)
!
      use t_geometry_data
!
      type(element_data), intent(in) :: ele
      type(fieldline_paramter), intent(inout) :: fln_prm
!
!
      allocate(fln_prm%iflag_fline_used_ele(ele%numele))
!
!$omp parallel workshare
      fln_prm%iflag_fline_used_ele = 0
!$omp end parallel workshare
!
      end subroutine alloc_iflag_fline_used_ele
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine dealloc_fline_starts_ctl(fln_prm)
!
      type(fieldline_paramter), intent(inout) :: fln_prm
!
!
      call dealloc_ctl_params_viz_fields(fln_prm%fline_fields)
!
      deallocate(fln_prm%id_ele_grp_area_fline)
!
      deallocate(fln_prm%id_surf_start_fline)
      deallocate(fln_prm%id_gl_surf_start_fline)

      deallocate(fln_prm%xx_surf_start_fline)
!
      end subroutine dealloc_fline_starts_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_iflag_fline_used_ele(fln_prm)
!
      type(fieldline_paramter), intent(inout) :: fln_prm
!
!
      deallocate(fln_prm%iflag_fline_used_ele)
!
      end subroutine dealloc_iflag_fline_used_ele
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine check_control_params_fline(fln_prm)
!
      type(fieldline_paramter), intent(in) :: fln_prm
!
      integer(kind = kint) :: i
!
!
        write(*,*) 'fline_header: ',                                    &
     &            trim(fln_prm%fline_file_IO%file_prefix)
        write(*,*) 'file format: ', fln_prm%fline_file_IO%iflag_format
        write(*,*) 'id_fline_direction: ', fln_prm%id_fline_direction
        write(*,*) 'id_fline_seed_type: ', fln_prm%id_fline_seed_type
        write(*,*) 'id_seed_distribution: ',                            &
     &            fln_prm%id_seed_distribution
        write(*,*) 'max_line_stepping: ', fln_prm%max_line_stepping
        write(*,*) 'max_trace_length: ',  fln_prm%max_trace_length
!
        write(*,*) 'nele_grp_area_fline: ',                             &
     &            fln_prm%nele_grp_area_fline
!
        write(*,*) 'num_each_field_line: ',                             &
     &            fln_prm%num_each_field_line
        if     (fln_prm%id_fline_seed_type                              &
     &                          .eq. iflag_surface_group) then
          write(*,*) 'igrp_start_fline_surf_grp: ',                     &
     &              fln_prm%igrp_start_fline_surf_grp
        else if(fln_prm%id_fline_seed_type                              &
     &                          .eq. iflag_surface_list) then
          do i = 1, fln_prm%num_each_field_line
            write(*,*) i, fln_prm%id_gl_surf_start_fline(1:2,i)
          end do
        else if(fln_prm%id_fline_seed_type                              &
     &                          .eq. iflag_position_list) then
          do i = 1, fln_prm%num_each_field_line
            write(*,*) i, fln_prm%xx_surf_start_fline(1:3,i)
          end do
        end if
!
      end subroutine check_control_params_fline
!
!  ---------------------------------------------------------------------
!
      end module t_control_params_4_fline
