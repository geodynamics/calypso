!>@file   t_psf_results.f90
!!@brief  module t_psf_results
!!
!!@author H. Matsui
!!@date Programmed in ????
!
!>@brief set edge information for PSF results
!!
!!@verbatim
!!      subroutine load_psf_data_to_link_IO                             &
!!     &         (istep, psf_file_param, np_ucd, t_IO, psf_dat, psf_ucd)
!!      subroutine load_psf_data(istep, psf_file_param,                 &
!!     &                         np_ucd, t_IO, psf_dat)
!!
!!      subroutine dealloc_psf_results(psf_nod, psf_ele, psf_phys)
!!
!!      integer(kind = kint) function compare_psf_data                  &
!!     &                   (istep, psf1_file_param, psf2_file_param)
!!        integer(kind = kint), intent(in) :: istep
!!        type(field_IO_params), intent(in) :: psf1_file_param
!!        type(field_IO_params), intent(in) :: psf2_file_param
!!@endverbatim
!
      module t_psf_results
!
      use m_precision
      use m_field_file_format
!
      use t_geometry_data
      use t_phys_data
      use t_time_data
      use t_ucd_data
      use t_file_IO_parameter
!
      implicit none
!
!
!>       structure for section data
      type psf_results
!>       structure for sectioned nodes
        type(node_data) :: psf_nod
!>       structure for sectioned element
        type(element_data) :: psf_ele
!>       structure for sectioned field
        type(phys_data) :: psf_phys
      end type psf_results
!
      private :: set_psf_udt_mesh, set_psf_udt_data
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine load_psf_data_to_link_IO                               &
     &         (istep, psf_file_param, np_ucd, t_IO, psf_dat, psf_ucd)
!
      use set_ucd_data_to_type
!
      integer(kind = kint), intent(in) :: istep
      type(field_IO_params), intent(in) :: psf_file_param
!
      integer, intent(inout) :: np_ucd
      type(time_data), intent(inout) :: t_IO
      type(psf_results), intent(inout) :: psf_dat
      type(ucd_data), intent(inout) :: psf_ucd
!
!
      call load_psf_data(istep, psf_file_param, np_ucd, t_IO, psf_dat)
!
      call link_node_data_2_ucd(psf_dat%psf_nod, psf_ucd)
      call link_ele_data_2_ucd(psf_dat%psf_ele, psf_ucd)
      call link_field_data_to_ucd(psf_dat%psf_phys, psf_ucd)
!
      end subroutine load_psf_data_to_link_IO
!
!-----------------------------------------------------------------------
!
      subroutine load_psf_data(istep, psf_file_param,                   &
     &                         np_ucd, t_IO, psf_dat)
!
      use ucd_IO_select
!
      integer(kind = kint), intent(in) :: istep
      type(field_IO_params), intent(in) :: psf_file_param
!
      integer, intent(inout) :: np_ucd
      type(time_data), intent(inout) :: t_IO
      type(psf_results), intent(inout) :: psf_dat
!
      type(ucd_data) :: read_psf
!
!
      call sel_read_alloc_ucd_file                                      &
     &   (-1, np_ucd, istep, psf_file_param, t_IO, read_psf)
!
      call set_psf_udt_mesh                                             &
     &   (read_psf, psf_dat%psf_nod, psf_dat%psf_ele, psf_dat%psf_phys)
      call set_psf_udt_data                                             &
     &   (read_psf, psf_dat%psf_nod, psf_dat%psf_phys)
!
      end subroutine load_psf_data
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine dealloc_psf_results(psf_dat)
!
      type(psf_results), intent(inout) :: psf_dat
!
!
      call dealloc_phys_data(psf_dat%psf_phys)
      call dealloc_phys_name(psf_dat%psf_phys)
      call dealloc_ele_connect(psf_dat%psf_ele)
      call dealloc_node_geometry_w_sph(psf_dat%psf_nod)
!
      end subroutine dealloc_psf_results
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_psf_udt_mesh(ucd, psf_nod, psf_ele, psf_phys)
!
      use m_geometry_constants
      use m_phys_constants
      use cal_mesh_position
!
      type(ucd_data), intent(inout) :: ucd
      type(node_data), intent(inout) :: psf_nod
      type(element_data), intent(inout) :: psf_ele
      type(phys_data), intent(inout) :: psf_phys
!
!
      psf_nod%numnod =     int(ucd%nnod, KIND(psf_nod%numnod))
      psf_nod%internal_node = psf_nod%numnod
      psf_ele%numele =     int(ucd%nele, KIND(psf_ele%numele))
      psf_ele%nnod_4_ele = num_triangle
      psf_phys%ntot_phys = ucd%ntot_comp
      call alloc_node_geometry_w_sph(psf_nod)
      call alloc_ele_connect(psf_ele)
!
      psf_nod%inod_global(1:psf_nod%numnod)                             &
     &      = ucd%inod_global(1:psf_nod%numnod)
      psf_ele%iele_global(1:psf_ele%numele)                             &
     &      = ucd%iele_global(1:psf_ele%numele)
      psf_nod%xx(1:psf_nod%numnod,1:n_vector)                           &
     &      = ucd%xx(1:psf_nod%numnod,1:n_vector)
      psf_ele%ie(1:psf_ele%numele,1:ithree)                             &
     &      = int(ucd%ie(1:psf_ele%numele,1:ithree),                    &
     &            KIND(psf_ele%ie(1,1)))
!
      call deallocate_ucd_node(ucd)
      call deallocate_ucd_ele(ucd)
!
      call set_spherical_position(psf_nod)
!
      end subroutine set_psf_udt_mesh
!
!-----------------------------------------------------------------------
!
      subroutine set_psf_udt_data(ucd, psf_nod, psf_phys)
!
      use set_ucd_data_to_type
!
      type(ucd_data), intent(inout) :: ucd
      type(node_data), intent(in) :: psf_nod
      type(phys_data), intent(inout) :: psf_phys
!
!
      call alloc_phys_data_type_by_output(ucd, psf_nod, psf_phys)
!
      psf_phys%d_fld(1:psf_nod%numnod,1:psf_phys%ntot_phys)             &
     &   = ucd%d_ucd(1:psf_nod%numnod,1:psf_phys%ntot_phys)
!
      call deallocate_ucd_data(ucd)
!
      end subroutine set_psf_udt_data
!
!-----------------------------------------------------------------------
!
      end module  t_psf_results
