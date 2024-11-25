!>@file   collect_fline_data.f90
!!@brief  module collect_fline_data
!!
!!@author  H. Matsui
!!@date Programmed on Aug., 2011
!
!> @brief MPI communication To collect field line data
!!
!!@verbatim
!!      subroutine copy_local_fieldline_to_IO(viz_fields, fline_lc, ucd)
!!      subroutine copy_local_particles_to_IO(viz_fields, fline_lc, ucd)
!!        type(ctl_params_viz_fields), intent(in) :: viz_fields
!!        type(local_fieldline), intent(in) :: fline_lc
!!        type(ucd_data), intent(inout) :: ucd
!!@endverbatim
!
      module collect_fline_data
!
      use m_precision
!
      use calypso_mpi
      use m_constants
      use m_geometry_constants
      use t_control_params_4_fline
      use t_local_fline
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine copy_local_fieldline_to_IO(viz_fields, fline_lc, ucd)
!
      use t_ucd_data
      use const_global_element_ids
!
      type(ctl_params_viz_fields), intent(in) :: viz_fields
      type(local_fieldline), intent(in) :: fline_lc
!
      type(ucd_data), intent(inout) :: ucd
!
      integer(kind = kint_gl) :: i, nd
!
!
      ucd%nnod = fline_lc%nnod_line_l
      ucd%nele = fline_lc%nele_line_l
      ucd%nnod_4_ele = num_linear_edge
!
      call alloc_merged_ucd_nod_stack(nprocs, ucd)
      call alloc_merged_ucd_ele_stack(nprocs, ucd)
      call count_number_of_node_stack(fline_lc%nnod_line_l,             &
     &                                ucd%istack_merged_nod)
      call count_number_of_node_stack(fline_lc%nele_line_l,             &
     &                                ucd%istack_merged_ele)
      write(*,*) 'ucd%istack_merged_nod', ucd%istack_merged_nod
      write(*,*) 'ucd%istack_merged_ele', ucd%istack_merged_ele
!
!$omp parallel workshare
      ucd%istack_merged_intnod(0:nprocs)                                &
     &                  = ucd%istack_merged_nod(0:nprocs)
!$omp end parallel workshare
!
      call allocate_ucd_node(ucd)
!$omp parallel do
      do i = 1, ucd%nnod
        ucd%inod_global(i) = fline_lc%iglobal_fline(i)
        ucd%xx(i,1) = fline_lc%xx_line_l(1,i)
        ucd%xx(i,2) = fline_lc%xx_line_l(2,i)
        ucd%xx(i,3) = fline_lc%xx_line_l(3,i)
      end do
!$omp end parallel do

      call allocate_ucd_ele(ucd)
!$omp parallel do
      do i = 1, ucd%nele
        ucd%iele_global(i) = i + ucd%istack_merged_ele(my_rank)
        ucd%ie(i,1) = fline_lc%iedge_line_l(1,i)                        &
     &               + ucd%istack_merged_nod(my_rank)
        ucd%ie(i,2) = fline_lc%iedge_line_l(2,i)                        &
     &               + ucd%istack_merged_nod(my_rank)
      end do
!$omp end parallel do
      
      ucd%num_field = viz_fields%num_color_fields
      call allocate_ucd_phys_name(ucd)
!$omp parallel workshare
      ucd%phys_name(1:ucd%num_field)                                    &
     &     = viz_fields%color_field_name(1:ucd%num_field)
      ucd%num_comp(1:ucd%num_field)                                     &
     &     = viz_fields%ncomp_color_field(1:ucd%num_field)
!$omp end parallel workshare

      ucd%ntot_comp = viz_fields%ntot_color_comp
      call allocate_ucd_phys_data(ucd)
      do nd = 1, ucd%ntot_comp
!$omp parallel workshare
        ucd%d_ucd(1:ucd%nnod,nd) = fline_lc%col_line_l(nd,1:ucd%nnod)
!$omp end parallel workshare
      end do
!
      end subroutine copy_local_fieldline_to_IO
!
!  ---------------------------------------------------------------------
!
      subroutine copy_local_particles_to_IO(viz_fields, fline_lc, ucd)
!
      use t_ucd_data
      use t_source_of_filed_line
      use const_global_element_ids
!
      type(ctl_params_viz_fields), intent(in) :: viz_fields
      type(local_fieldline), intent(in) :: fline_lc
!
      type(ucd_data), intent(inout) :: ucd
!
      integer(kind = kint_gl) :: i, nd
!
!
      ucd%nnod = fline_lc%nnod_line_l
      ucd%nele = ucd%nnod
      ucd%nnod_4_ele = num_linear_point
!
      call alloc_merged_ucd_nod_stack(nprocs, ucd)
      call alloc_merged_ucd_ele_stack(nprocs, ucd)
      call count_number_of_node_stack(fline_lc%nnod_line_l,             &
     &                                ucd%istack_merged_nod)
!
!$omp parallel workshare
      ucd%istack_merged_ele(0:nprocs)                                   &
     &                  = ucd%istack_merged_nod(0:nprocs)
      ucd%istack_merged_intnod(0:nprocs)                                &
     &                  = ucd%istack_merged_nod(0:nprocs)
!$omp end parallel workshare
!
      call allocate_ucd_node(ucd)
!$omp parallel do
      do i = 1, ucd%nnod
        ucd%inod_global(i) = fline_lc%iglobal_fline(i)
        ucd%xx(i,1) = fline_lc%xx_line_l(1,i)
        ucd%xx(i,2) = fline_lc%xx_line_l(2,i)
        ucd%xx(i,3) = fline_lc%xx_line_l(3,i)
      end do
!$omp end parallel do

      call allocate_ucd_ele(ucd)
!$omp parallel do
      do i = 1, ucd%nele
        ucd%iele_global(i) = ucd%inod_global(i)
        ucd%ie(i,1) =        fline_lc%iglobal_fline(i)
      end do
!$omp end parallel do
      
      ucd%num_field = viz_fields%num_color_fields
      call allocate_ucd_phys_name(ucd)
!$omp parallel workshare
      ucd%phys_name(1:ucd%num_field)                                    &
     &     = viz_fields%color_field_name(1:ucd%num_field)
      ucd%num_comp(1:ucd%num_field)                                     &
     &     = viz_fields%ncomp_color_field(1:ucd%num_field)
!$omp end parallel workshare

      ucd%ntot_comp = viz_fields%ntot_color_comp
      call allocate_ucd_phys_data(ucd)
      do nd = 1, ucd%ntot_comp
!$omp parallel workshare
        ucd%d_ucd(1:ucd%nnod,nd) = fline_lc%col_line_l(nd,1:ucd%nnod)
!$omp end parallel workshare
      end do
!
      end subroutine copy_local_particles_to_IO
!
!  ---------------------------------------------------------------------
!
      end module collect_fline_data
