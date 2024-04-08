!>@file   t_volume_rendering.f90
!!@brief  module t_volume_rendering
!!
!!@date  Programmed by H.Matsui in May. 2006
!
!>@brief Main routines for volume renderings
!!
!!@verbatim
!!      subroutine set_from_PVR_control(geofem, nod_fld, pvr_ctls, pvr)
!!        type(mesh_data), intent(in) :: geofem
!!        type(phys_data), intent(in) :: nod_fld
!!        type(volume_rendering_controls), intent(inout) :: pvr_ctls
!!        type(volume_rendering_module), intent(inout) :: pvr
!!      subroutine check_PVR_update                                     &
!!     &         (id_control, pvr_ctls, pvr, iflag_redraw)
!!      subroutine read_ctl_pvr_files_4_update(id_control,              &
!!     &                                       pvr_ctls, iflag_failed)
!!      subroutine alloc_pvr_data(pvr)
!!
!!      subroutine dealloc_pvr_data(pvr)
!!      subroutine alloc_pvr_images(pvr)
!!      subroutine dealloc_pvr_and_lic_data(pvr)
!!        type(mesh_data), intent(in) :: geofem
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(phys_data), intent(in) :: nod_fld
!!        type(jacobians_type), intent(in) :: jacs
!!        type(volume_rendering_controls), intent(inout) :: pvr_ctls
!!        type(volume_rendering_module), intent(inout) :: pvr
!!@endverbatim
!
      module t_volume_rendering
!
      use m_precision
      use calypso_mpi
!
      use m_constants
      use m_machine_parameter
      use m_geometry_constants
      use m_work_time
      use m_elapsed_labels_4_VIZ
!
      use t_mesh_data
      use t_phys_data
      use t_jacobians
!
      use t_surf_grp_list_each_surf
      use t_rendering_vr_image
      use t_control_params_4_pvr
      use t_surf_grp_4_pvr_domain
      use t_pvr_ray_startpoints
      use t_pvr_image_array
      use t_pvr_field_data
      use t_geometries_in_pvr_screen
      use t_control_data_pvrs
      use t_sort_PVRs_by_type
!
      use each_volume_rendering
!
      implicit  none
!
!
      integer(kind = kint), parameter :: IFLAG_THROUGH =    1
      integer(kind = kint), parameter :: IFLAG_DRAW =       0
      integer(kind = kint), parameter :: IFLAG_TERMINATE = -1
!
!
      type volume_rendering_module
!>        Character flag to update volume rendering
        character(len=kchara) :: cflag_update
!
!>        Structure of surface group list for each surface
        type(sf_grp_list_each_surf) :: sf_grp_4_sf
!
!>        Number of volume rendering
        integer(kind = kint) :: num_pvr = 0
!>        Structure of PVR control parameters
        type(PVR_control_params), allocatable :: pvr_param(:)
!>        Structure of field for PVRs
        type(pvr_field_data), allocatable :: field_pvr(:)
!>        Domain boundary information
        type(pvr_bounds_surf_ctl), allocatable :: pvr_bound(:)
!
!>        Number of image files for volume rendering
        integer(kind = kint) :: num_pvr_images =    0
!>        Structure for projection data
        type(PVR_projection_data), allocatable :: pvr_proj(:)
!>        Structure for PVR images
        type(pvr_image_type), allocatable :: pvr_rgb(:)
!
!>        Structure for PVR images
        type(sort_PVRs_by_type) :: PVR_sort
      end type volume_rendering_module
!
      character(len=kchara), parameter                                  &
     &             :: hd_pvr_ctl = 'volume_rendering'
      private :: hd_pvr_ctl
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_from_PVR_control(geofem, nod_fld, pvr_ctls, pvr)
!
      use t_control_data_pvr_sections
      use set_pvr_control
      use rendering_and_image_nums
!
      type(mesh_data), intent(in) :: geofem
      type(phys_data), intent(in) :: nod_fld
      type(volume_rendering_controls), intent(inout) :: pvr_ctls
      type(volume_rendering_module), intent(inout) :: pvr
!
      integer(kind = kint) :: i_ctl, i_pvr
!
!
      call alloc_pvr_data(pvr)
      call s_sort_PVRs_by_type(pvr%num_pvr, pvr_ctls%pvr_ctl_type,      &
     &                         pvr%PVR_sort)
!
      if(iflag_debug .gt. 0) then
        do i_pvr = 0, 6
          write(*,*) i_pvr, 'pvr%istack_PVR_modes',                     &
    &       pvr%PVR_sort%istack_PVR_modes(i_pvr)
        end do
        do i_pvr = 1, pvr%num_pvr
          write(*,*) i_pvr, trim(pvr_ctls%fname_pvr_ctl(i_pvr)), ' ',   &
    &    yes_flag(pvr_ctls%pvr_ctl_type(i_pvr)%anaglyph_ctl%charavalue),&
    &    ' ', pvr_ctls%pvr_ctl_type(i_pvr)%movie%movie_mode_ctl%iflag,  &
    &    yes_flag(pvr_ctls%pvr_ctl_type(i_pvr)%quilt_ctl%charavalue),   &
    &    ' ', pvr%PVR_sort%ipvr_sorted(i_pvr)
        end do
      end if
!
      do i_pvr = 1, pvr%num_pvr
        call alloc_nod_data_4_pvr                                       &
     &     (geofem%mesh%node%numnod, geofem%mesh%ele%numele,            &
     &      pvr%field_pvr(i_pvr))
        call alloc_iflag_pvr_boundaries(geofem%group%surf_grp,          &
     &      pvr%pvr_param(i_pvr)%draw_param)
      end do
!
      do i_ctl = 1, pvr%num_pvr
        i_pvr = pvr%PVR_sort%ipvr_sorted(i_ctl)
        call s_set_pvr_controls(geofem%group, nod_fld,                  &
     &      pvr_ctls%pvr_ctl_type(i_ctl), pvr%pvr_param(i_pvr))
      end do
!
      call count_num_rendering_and_images(pvr%num_pvr, pvr%pvr_param,   &
     &    pvr%num_pvr_images, pvr%PVR_sort%istack_pvr_images)
      call alloc_pvr_images(pvr)
!
      call set_rendering_and_image_pes                                  &
     &   (nprocs, pvr%num_pvr, pvr_ctls%pvr_ctl_type, pvr%PVR_sort,     &
     &    pvr%num_pvr_images, pvr%pvr_rgb)
      call dealloc_sort_PVRs_list(pvr%PVR_sort)
!
      end subroutine set_from_PVR_control
!
!  ---------------------------------------------------------------------
!
      subroutine check_PVR_update                                       &
     &         (id_control, pvr_ctls, pvr, iflag_redraw)
!
      use calypso_mpi_int
      use bcast_control_data_4_pvr
      use ctl_file_each_pvr_IO
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_control
      type(volume_rendering_controls), intent(inout) :: pvr_ctls
      type(volume_rendering_module), intent(inout) :: pvr
      integer(kind = kint), intent(inout) :: iflag_redraw
!
      character(len = kchara) :: tmpchara
!
!
      if(my_rank .eq. izero) then
        call read_control_pvr_update                                    &
     &     (id_control, pvr_ctls%fname_pvr_ctl(1),                      &
     &      hd_pvr_ctl, pvr_ctls%pvr_ctl_type(1))
!
        iflag_redraw = IFLAG_THROUGH
        if(pvr_ctls%pvr_ctl_type(1)%updated_ctl%iflag .gt. 0) then
          tmpchara = pvr_ctls%pvr_ctl_type(1)%updated_ctl%charavalue
          if(cmp_no_case(tmpchara, 'end')) then
            iflag_redraw = IFLAG_TERMINATE
          else if(pvr%cflag_update .ne. tmpchara) then
            iflag_redraw = IFLAG_DRAW
            pvr%cflag_update = tmpchara
          end if
        end if
        call reset_pvr_update_flags(pvr_ctls%pvr_ctl_type(1))
      end if
!
      call bcast_pvr_update_flag(pvr_ctls%pvr_ctl_type(1))
      if(pvr_ctls%pvr_ctl_type(1)%i_pvr_ctl .lt. 0) then
        call calypso_MPI_abort(pvr_ctls%pvr_ctl_type(1)%i_pvr_ctl,      &
     &                           'control file is broken')
      end if
!
      call calypso_mpi_bcast_one_int(iflag_redraw, 0)
      call calypso_mpi_barrier
!
      end subroutine check_PVR_update
!
!  ---------------------------------------------------------------------
!
      subroutine read_ctl_pvr_files_4_update(id_control,                &
     &                                       pvr_ctls, iflag_failed)
!
      use t_read_control_elements
      use skip_comment_f
      use ctl_file_each_pvr_IO
!
      integer(kind = kint), intent(in) :: id_control
      type(volume_rendering_controls), intent(inout) :: pvr_ctls
      integer(kind = kint), intent(inout) :: iflag_failed
!
      integer(kind = kint) :: i_pvr
      type(buffer_for_control) :: c_buf1
!
!
      iflag_failed = 0
      if(my_rank .ne. 0) return
      c_buf1%level = 0
      do i_pvr = 1, pvr_ctls%num_pvr_ctl
        if(.not. no_file_flag(pvr_ctls%fname_pvr_ctl(i_pvr))) then
          call read_control_pvr_file                                    &
     &     (id_control, pvr_ctls%fname_pvr_ctl(i_pvr), hd_pvr_ctl,      &
     &      pvr_ctls%pvr_ctl_type(i_pvr), c_buf1)
          iflag_failed = pvr_ctls%pvr_ctl_type(i_pvr)%i_pvr_ctl
        end if
      end do
!
      end subroutine read_ctl_pvr_files_4_update
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine alloc_pvr_data(pvr)
!
      type(volume_rendering_module), intent(inout) :: pvr
!
!
      allocate(pvr%pvr_param(pvr%num_pvr))
      allocate(pvr%field_pvr(pvr%num_pvr))
      allocate(pvr%pvr_bound(pvr%num_pvr))
!
      call alloc_sort_PVRs_by_type(pvr%num_pvr, pvr%PVR_sort)
!
      end subroutine alloc_pvr_data
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_pvr_images(pvr)
!
      type(volume_rendering_module), intent(inout) :: pvr
!
!
      allocate(pvr%pvr_proj(pvr%num_pvr_images))
      allocate(pvr%pvr_rgb(pvr%num_pvr_images))
!
      end subroutine alloc_pvr_images
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_pvr_data(pvr)
!
      type(volume_rendering_module), intent(inout) :: pvr
      integer(kind = kint) :: i_pvr
!
!
      if(pvr%num_pvr.le.0) return
      do i_pvr = 1, pvr%num_pvr
        call dealloc_nod_data_4_pvr(pvr%field_pvr(i_pvr))
      end do
      call dealloc_pvr_and_lic_data(pvr)
!
      end subroutine dealloc_pvr_data
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_pvr_and_lic_data(pvr)
!
      use set_pvr_control
!
      type(volume_rendering_module), intent(inout) :: pvr
      integer(kind = kint) :: i_pvr
!
!
      call dealloc_sort_PVRs_list(pvr%PVR_sort)
!
      do i_pvr = 1, pvr%num_pvr
        call dealloc_iflag_pvr_boundaries                               &
     &     (pvr%pvr_param(i_pvr)%draw_param)
        call dealloc_iflag_pvr_boundaries                               &
     &     (pvr%pvr_param(i_pvr)%draw_param)
        call dealloc_iflag_pvr_used_ele                                 &
     &     (pvr%pvr_param(i_pvr)%draw_param)
        call dealloc_pvr_surf_domain_item(pvr%pvr_bound(i_pvr))
      end do
      deallocate(pvr%pvr_bound, pvr%pvr_param)
!
      call dealloc_num_sf_grp_each_surf(pvr%sf_grp_4_sf)
!
!
      do i_pvr = 1, pvr%num_pvr_images
        call dealloc_pvr_image_array(pvr%pvr_rgb(i_pvr))
        call flush_rendering_4_fixed_view(pvr%pvr_proj(i_pvr))
      end do
      deallocate(pvr%pvr_rgb, pvr%pvr_proj)
!
      end subroutine dealloc_pvr_and_lic_data
!
!  ---------------------------------------------------------------------
!
      end module t_volume_rendering
