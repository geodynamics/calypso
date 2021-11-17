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
!!      subroutine read_ctl_pvr_files_4_update(id_control, pvr_ctls)
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
!>        Number of rendering for volume rendering
        integer(kind = kint) :: num_pvr_rendering = 0
!>        Structure for projection data
        type(PVR_projection_data), allocatable :: pvr_proj(:)
!
!>        Number of image files for volume rendering
        integer(kind = kint) :: num_pvr_images =    0
!>        Number of image files for volume rendering
        integer(kind = kint), allocatable :: istack_pvr_images(:)
!>        Structure for PVR images
        type(pvr_image_type), allocatable :: pvr_rgb(:)
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
      integer(kind = kint) :: i_pvr
!
!
      if(iflag_PVR_time) call start_elapsed_time(ist_elapsed_PVR+5)
      call bcast_pvr_controls(pvr%num_pvr,                              &
     &    pvr_ctls%pvr_ctl_type, pvr%cflag_update)
      if(iflag_PVR_time) call end_elapsed_time(ist_elapsed_PVR+5)
!
      call alloc_pvr_data(pvr)
      do i_pvr = 1, pvr%num_pvr
        call alloc_nod_data_4_pvr                                       &
     &     (geofem%mesh%node%numnod, geofem%mesh%ele%numele,            &
     &      pvr%field_pvr(i_pvr))
        call alloc_iflag_pvr_boundaries(geofem%group%surf_grp,          &
     &      pvr%pvr_param(i_pvr)%draw_param)
      end do
!
      call s_set_pvr_controls(geofem%group, nod_fld, pvr%num_pvr,       &
     &    pvr_ctls%pvr_ctl_type, pvr%pvr_param)
!
      end subroutine set_from_PVR_control
!
!  ---------------------------------------------------------------------
!
      subroutine check_PVR_update                                       &
     &         (id_control, pvr_ctls, pvr, iflag_redraw)
!
      use calypso_mpi_int
      use set_pvr_control
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
      call calypso_mpi_bcast_one_int(iflag_redraw, 0)
      call calypso_mpi_barrier
!
      end subroutine check_PVR_update
!
!  ---------------------------------------------------------------------
!
      subroutine read_ctl_pvr_files_4_update(id_control, pvr_ctls)
!
      use t_read_control_elements
      use skip_comment_f
      use set_pvr_control
!
      integer(kind = kint), intent(in) :: id_control
      type(volume_rendering_controls), intent(inout) :: pvr_ctls
!
      integer(kind = kint) :: i_pvr
!
!
      if(my_rank .ne. 0) return
      do i_pvr = 1, pvr_ctls%num_pvr_ctl
        if(pvr_ctls%fname_pvr_ctl(i_pvr) .ne. 'NO_FILE') then
          call read_control_pvr_file                                    &
     &     (id_control, pvr_ctls%fname_pvr_ctl(i_pvr), hd_pvr_ctl,      &
     &      pvr_ctls%pvr_ctl_type(i_pvr))
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
      allocate(pvr%istack_pvr_images(0:pvr%num_pvr))
      pvr%istack_pvr_images = 0
!
      allocate(pvr%pvr_param(pvr%num_pvr))
      allocate(pvr%field_pvr(pvr%num_pvr))
      allocate(pvr%pvr_bound(pvr%num_pvr))
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
      allocate(pvr%pvr_proj(pvr%num_pvr_rendering))
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
      end do
      deallocate(pvr%pvr_rgb)
!
!
      do i_pvr = 1, pvr%num_pvr_rendering
        call flush_rendering_4_fixed_view(pvr%pvr_proj(i_pvr))
      end do
      deallocate(pvr%pvr_proj)
      deallocate(pvr%istack_pvr_images)
!
      end subroutine dealloc_pvr_and_lic_data
!
!  ---------------------------------------------------------------------
!
      end module t_volume_rendering
