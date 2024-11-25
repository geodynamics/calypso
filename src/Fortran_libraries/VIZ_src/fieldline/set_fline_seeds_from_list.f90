!>@file   set_fline_seeds_from_list.f90
!!@brief  module set_fline_seeds_from_list
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Main routine for field line module
!!
!!@verbatim
!!      subroutine alloc_FLINE_element_size(ele, fln_dist)
!!      subroutine dealloc_FLINE_element_size(fln_dist)
!!        type(element_data), intent(in) :: ele
!!        type(FLINE_element_size), intent(inout) :: fln_dist
!!        type(FLINE_element_size), intent(inout) :: fln_dist
!!
!!      subroutine cal_FLINE_element_size(node, ele, fln_dist)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(FLINE_element_size), intent(inout) :: fln_dist
!!      subroutine init_FLINE_seed_from_list(node, ele,                 &
!!     &                                     fln_prm, fln_src, fln_dist)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(fieldline_paramter), intent(inout) :: fln_prm
!!        type(each_fieldline_source), intent(inout) :: fln_src
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
!!        type(FLINE_element_size), intent(inout) :: fln_dist
!!      subroutine set_FLINE_seed_field_from_list                       &
!!     &         (node, ele, nod_fld, fln_prm, fln_src, fln_tce)
!!         type(node_data), intent(in) :: node
!!         type(element_data), intent(in) :: ele
!!         type(phys_data), intent(in) :: nod_fld
!!         type(fieldline_paramter), intent(in) :: fln_prm
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
!!
!!      subroutine count_FLINE_seed_from_list(fln_prm, fln_src, fln_tce)
!!        type(fieldline_paramter), intent(in) :: fln_prm
!!        type(each_fieldline_source), intent(in) :: fln_src
!!        type(each_fieldline_trace), intent(inout) :: fln_tce
!!
!!      subroutine set_field_at_each_seed_point(node, ele, nod_fld,     &
!!     &          fline_fields, iphys_4_fline, iele_seed, x4_seed,      &
!!     &          v_fline_start, c_fline_start)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(phys_data), intent(in) :: nod_fld
!!        type(ctl_params_viz_fields), intent(in) :: fline_fields
!!        integer(kind = kint), intent(in) :: iphys_4_fline
!!        integer(kind = kint), intent(in) :: iele_seed(1)
!!        real(kind = kreal), intent(in) :: x4_seed(4)
!!        real(kind = kreal), intent(inout) :: v_fline_start(4)
!!        real(kind = kreal), intent(inout)                             &
!!     &         :: c_fline_start(fline_fields%ntot_color_comp)
!!@endverbatim
!
      module set_fline_seeds_from_list
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_geometry_constants
      use t_geometry_data
      use t_phys_data
      use t_control_params_4_fline
      use t_source_of_filed_line
      use t_tracing_data
!
      implicit none
!
      integer(kind = kint), parameter, private :: maxitr = 20
      real(kind = kreal), parameter, private ::   eps_iter = 1.0d-9
      integer(kind = kint), parameter, private :: iflag_nomessage = 0
      real(kind = kreal), parameter, private ::   error_level = 1.0d-9
!
      type FLINE_element_size
        real(kind = kreal), allocatable :: ele_size(:)
        real(kind = kreal), allocatable :: distance(:)
        integer(kind = kint), allocatable :: index(:)
      end type FLINE_element_size
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine alloc_FLINE_element_size(ele, fln_dist)
      type(element_data), intent(in) :: ele
      type(FLINE_element_size), intent(inout) :: fln_dist
!
      allocate(fln_dist%ele_size(ele%numele))
      allocate(fln_dist%distance(ele%numele))
      allocate(fln_dist%index(ele%numele))
!
      if(ele%numele .le. 0) return
!$omp parallel workshare 
      fln_dist%ele_size(1:ele%numele) = 0.0d0
      fln_dist%distance(1:ele%numele) = 0.0d0
      fln_dist%index(1:ele%numele) =    0
!$omp end parallel workshare 
!
      end subroutine alloc_FLINE_element_size
!
!  ---------------------------------------------------------------------
!
      subroutine dealloc_FLINE_element_size(fln_dist)
      type(FLINE_element_size), intent(inout) :: fln_dist
!
      if(allocated(fln_dist%ele_size) .eqv. .FALSE.) return
      deallocate(fln_dist%ele_size)
      deallocate(fln_dist%distance, fln_dist%index)
!
      end subroutine dealloc_FLINE_element_size
!
!  ---------------------------------------------------------------------
!
      subroutine cal_FLINE_element_size(node, ele, fln_dist)
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(FLINE_element_size), intent(inout) :: fln_dist
!
      real(kind = kreal) :: x(ele%nnod_4_ele)
      real(kind = kreal) :: y(ele%nnod_4_ele)
      real(kind = kreal) :: z(ele%nnod_4_ele)
      real(kind = kreal) :: size_max(3)
      integer(kind = kint) :: inod, iele, k1
!
!$omp parallel do private(iele,k1,inod,x,y,z)
      do iele = 1, ele%numele
        do k1 = 1, ele%nnod_4_ele
          inod = ele%ie(iele,k1)
          x(k1) = node%xx(inod,1)
          y(k1) = node%xx(inod,2)
          z(k1) = node%xx(inod,3)
        end do
        size_max(1) = maxval(x) - minval(x)
        size_max(2) = maxval(y) - minval(y)
        size_max(3) = maxval(z) - minval(z)
        fln_dist%ele_size(iele) = sqrt(size_max(1)*size_max(1)          &
     &                               + size_max(2)*size_max(2)          &
     &                               + size_max(3)*size_max(3))
      end do
!$omp end parallel do
!
      end subroutine cal_FLINE_element_size
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine init_FLINE_seed_from_list(node, ele,                   &
     &                                     fln_prm, fln_src, fln_dist)
!
      use calypso_mpi_int
      use t_control_data_flines
      use t_find_interpolate_in_ele
      use set_fline_control
      use quicksort
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(fieldline_paramter), intent(inout) :: fln_prm
      type(each_fieldline_source), intent(inout) :: fln_src
      type(FLINE_element_size), intent(inout) :: fln_dist
!
      type(cal_interpolate_coefs_work), save :: itp_ele_work_f
      integer(kind = kint) :: ierr_inter
!
      real(kind = kreal) :: x, y, z
      real(kind = kreal) :: dist_tmp
      real(kind = kreal) :: xi(3)
      integer(kind = kint) :: i, iele, inum
      integer(kind = kint) :: num_search
!      integer(kind = kint) :: ip, i_fln

!
      call alloc_work_4_interpolate(ele%nnod_4_ele, itp_ele_work_f)
!
      do i = 1, fln_prm%num_each_field_line
        x = fln_prm%xx_surf_start_fline(1,i)
        y = fln_prm%xx_surf_start_fline(2,i)
        z = fln_prm%xx_surf_start_fline(3,i)
        num_search = 0
        do iele = 1, ele%numele
          dist_tmp = sqrt ((x - ele%x_ele(iele,1))**2                   &
     &                   + (y - ele%x_ele(iele,2))**2                   &
     &                   + (z - ele%x_ele(iele,3))**2)
          if(dist_tmp .le. fln_dist%ele_size(iele)) then
            num_search = num_search + 1
            fln_dist%index(num_search) =    iele
            fln_dist%distance(num_search) = dist_tmp
          end if
        end do

        if(num_search .gt. 1) then
          call quicksort_real_w_index(ele%numele,                       &
    &         fln_dist%distance(1), ione, num_search, fln_dist%index(1))
        end if
!
        fln_src%ip_surf_start_fline(i) = -1
        fln_src%iele_surf_start_fline(i) = 0
        fln_src%xi_surf_start_fline(1:3,i) = -2.0
        do inum = 1, num_search
          iele = fln_dist%index(inum)
          if(ele%interior_ele(iele) .le. 0) cycle
          ierr_inter = 0
          xi(1:3) = -2.0
          call find_interpolate_in_ele                                  &
     &       (fln_prm%xx_surf_start_fline(1,i), maxitr, eps_iter,       &
     &        my_rank, iflag_nomessage, error_level,                    &
     &        node, ele, iele, itp_ele_work_f, xi, ierr_inter)  
          if(ierr_inter.gt.1 .and. ierr_inter.le.maxitr) then
            fln_src%ip_surf_start_fline(i) = my_rank
            fln_src%iele_surf_start_fline(i) = iele
            fln_src%xi_surf_start_fline(1:3,i) = xi(1:3)
            exit
          end if
        end do
      end do
      call dealloc_work_4_interpolate(itp_ele_work_f)
        
!      do ip = 1, nprocs
!        call calypso_mpi_barrier
!        if(my_rank .ne. ip-1) cycle
!        do i = 1, fln_prm%num_each_field_line
!          if(fln_src%ip_surf_start_fline(i) .ge. 0) then
!          write(*,*) my_rank, i_fln, i, 'fln_prm',                     &
!     &              fln_src%ip_surf_start_fline(i),                    &
!     &              fln_src%iele_surf_start_fline(i),                  &
!     &              fln_src%xi_surf_start_fline(1:3,i),                &
!     &              ele%numele, ierr_inter
!          end if
!        end do
!      end do
!
!        call calypso_mpi_barrier
      fln_src%num_line_local = 0
      do i = 1, fln_prm%num_each_field_line
      if(fln_src%ip_surf_start_fline(i) .eq. my_rank)                   &
        fln_src%num_line_local = fln_src%num_line_local + 1
      end do
!        call calypso_mpi_barrier
!        write(*,*) my_rank, 'fln_src%num_line_local',                  &
!     &            fln_src%num_line_local
!
      end subroutine init_FLINE_seed_from_list
!
!  ---------------------------------------------------------------------
!
      subroutine count_FLINE_seed_from_list(fln_prm, fln_src, fln_tce)
!
      use calypso_mpi_int
!
      type(fieldline_paramter), intent(in) :: fln_prm
      type(each_fieldline_source), intent(in) :: fln_src
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
      integer(kind = kint) :: i
!
!
      fln_tce%num_current_fline = fln_src%num_line_local
      if(fln_prm%id_fline_direction .eq. iflag_both_trace) then
        fln_tce%num_current_fline = 2 * fln_tce%num_current_fline
      end if
      call resize_line_start_fline(fln_tce%num_current_fline,           &
     &                             fln_prm%fline_fields, fln_tce)
!
      fln_tce%istack_current_fline(0) = 0
      call calypso_mpi_allgather_one_int(fln_tce%num_current_fline,     &
     &                                 fln_tce%istack_current_fline(1))
      do i = 1, nprocs
        fln_tce%istack_current_fline(i)                                 &
     &     = fln_tce%istack_current_fline(i-1)                          &
     &      + fln_tce%istack_current_fline(i)
      end do
!
      end subroutine count_FLINE_seed_from_list
!
!  ---------------------------------------------------------------------
!
      subroutine set_FLINE_seed_field_from_list                         &
     &         (node, ele, nod_fld, fln_prm, fln_src, fln_tce)
!
      use sel_interpolate_scalar
      use extend_field_line
      use trace_in_element
      use tracer_field_interpolate
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
!
      type(fieldline_paramter), intent(in) :: fln_prm
      type(each_fieldline_source), intent(in) :: fln_src
      type(each_fieldline_trace), intent(inout) :: fln_tce
!
      integer(kind = kint) :: icou, inum
!
      icou = 0
      do inum = 1, fln_prm%num_each_field_line
          if(fln_src%ip_surf_start_fline(inum) .ne. my_rank) cycle
          icou = icou + 1
!
          call cal_each_seed_field_in_ele(node, ele, nod_fld,           &
     &        fln_prm%fline_fields, fln_prm%iphys_4_fline,              &
     &        fln_src%iele_surf_start_fline(inum),                      &
     &        fln_src%xi_surf_start_fline(1,inum),                      &
     &        fln_prm%xx_surf_start_fline(1,inum),                      &
     &        fln_tce%v_fline_start(1,icou),                            &
     &        fln_tce%c_fline_start(1,icou))
!
!
          fln_tce%isf_dbl_start(1,icou) = my_rank
          fln_tce%isf_dbl_start(2,icou)                                 &
     &      = fln_src%iele_surf_start_fline(inum)

          fln_tce%isf_dbl_start(3,icou) = 0
          if(abs(fln_src%xi_surf_start_fline(1,inum)+one)               &
     &              .lt. error_level) fln_tce%isf_dbl_start(3,icou) = 1
          if(abs(fln_src%xi_surf_start_fline(1,inum)-one)               &
     &              .lt. error_level) fln_tce%isf_dbl_start(3,icou) = 2
          if(abs(fln_src%xi_surf_start_fline(2,inum)+one)               &
     &              .lt. error_level) fln_tce%isf_dbl_start(3,icou) = 3
          if(abs(fln_src%xi_surf_start_fline(2,inum)-one)               &
     &              .lt. error_level) fln_tce%isf_dbl_start(3,icou) = 4
          if(abs(fln_src%xi_surf_start_fline(3,inum)+one)               &
     &              .lt. error_level) fln_tce%isf_dbl_start(3,icou) = 5
          if(abs(fln_src%xi_surf_start_fline(3,inum)-one)               &
     &              .lt. error_level) fln_tce%isf_dbl_start(3,icou) = 6
          
          fln_tce%iline_original(icou) = inum
          fln_tce%xx_fline_start(1:3,icou)                              &
     &         = fln_prm%xx_surf_start_fline(1:3,inum)
          fln_tce%xx_fline_start(4,icou) = one
          fln_tce%trace_length(icou) = 0.0d0
          fln_tce%icount_fline(icou) = 0
          
          if     (fln_prm%id_fline_direction                            &
     &                  .eq. iflag_forward_trace) then
           fln_tce%iflag_direction(icou) = 1
          else if(fln_prm%id_fline_direction                            &
     &                  .eq. iflag_backward_trace) then

            fln_tce%iflag_direction(icou) = -1
          else
            fln_tce%iflag_direction(icou) = 1
!
            icou = icou + 1
            fln_tce%iflag_direction(icou) = -1
            fln_tce%isf_dbl_start(1,icou) = my_rank
            fln_tce%isf_dbl_start(2,icou)                               &
     &            = fln_src%iele_surf_start_fline(inum)
!
            fln_tce%isf_dbl_start(3,icou) = 0
            if(abs(fln_src%xi_surf_start_fline(1,inum)+one)             &
     &              .lt. error_level) fln_tce%isf_dbl_start(3,icou) = 1
            if(abs(fln_src%xi_surf_start_fline(1,inum)-one)             &
     &              .lt. error_level) fln_tce%isf_dbl_start(3,icou) = 2
            if(abs(fln_src%xi_surf_start_fline(2,inum)+one)             &
     &              .lt. error_level) fln_tce%isf_dbl_start(3,icou) = 3
            if(abs(fln_src%xi_surf_start_fline(2,inum)-one)             &
     &              .lt. error_level) fln_tce%isf_dbl_start(3,icou) = 4
            if(abs(fln_src%xi_surf_start_fline(3,inum)+one)             &
     &              .lt. error_level) fln_tce%isf_dbl_start(3,icou) = 5
            if(abs(fln_src%xi_surf_start_fline(3,inum)-one)             &
     &              .lt. error_level) fln_tce%isf_dbl_start(3,icou) = 6
!
            fln_tce%trace_length(icou) = 0.0d0
            fln_tce%icount_fline(icou) = 0
            call copy_global_start_fline(icou, (icou-1),                &
     &                                   fln_prm%fline_fields, fln_tce)

          end if
        end do
!
      end subroutine set_FLINE_seed_field_from_list
!
!  ---------------------------------------------------------------------
!
      subroutine cal_each_seed_field_in_ele                             &
     &         (node, ele, nod_fld, fline_fields, iphys_4_fline,        &
     &          iele_surf_start_fline, xi_surf_start_fline,             &
     &          xx_surf_start_fline, v_fline_start, c_fline_start)
!
      use sel_interpolate_scalar
      use extend_field_line
      use trace_in_element
      use tracer_field_interpolate
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
!
      type(ctl_params_viz_fields), intent(in) :: fline_fields
      integer(kind = kint), intent(in) :: iphys_4_fline
!
      integer(kind = kint), intent(in) :: iele_surf_start_fline(1)
      real(kind = kreal), intent(in) :: xi_surf_start_fline(3)
      real(kind = kreal), intent(in) :: xx_surf_start_fline(3)
!
      real(kind = kreal), intent(inout) :: v_fline_start(4)
      real(kind = kreal), intent(inout)                                 &
     &         :: c_fline_start(fline_fields%ntot_color_comp)
!
!      real(kind = kreal) :: position_check(3)
! 
      call sel_sgl_interpolate_scalar_ele                               &
     &   (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie,              &
     &    nod_fld%d_fld(1,iphys_4_fline), iele_surf_start_fline(1),     &
     &    xi_surf_start_fline, v_fline_start(1))
      call sel_sgl_interpolate_scalar_ele                               &
     &   (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie,              &
     &    nod_fld%d_fld(1,iphys_4_fline+1), iele_surf_start_fline(1),   &
     &    xi_surf_start_fline, v_fline_start(2))
      call sel_sgl_interpolate_scalar_ele                               &
     &   (node%numnod, ele%numele, ele%nnod_4_ele, ele%ie,              &
     &    nod_fld%d_fld(1,iphys_4_fline+2), iele_surf_start_fline(1),   &
     &    xi_surf_start_fline, v_fline_start(3))
      v_fline_start(4) = one
!
      call cal_fields_in_element(iele_surf_start_fline,                 &
     &    xi_surf_start_fline, xx_surf_start_fline,                     &
     &    ele, nod_fld, fline_fields, c_fline_start(1))
!
      end subroutine cal_each_seed_field_in_ele
!
!  ---------------------------------------------------------------------
!
      subroutine set_field_at_each_seed_point(node, ele, nod_fld,       &
     &          fline_fields, iphys_4_fline, iele_seed, x4_seed,        &
     &          v_fline_start, c_fline_start)
!
      use t_find_interpolate_in_ele
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_data), intent(in) :: nod_fld
!
      type(ctl_params_viz_fields), intent(in) :: fline_fields
      integer(kind = kint), intent(in) :: iphys_4_fline
!
      integer(kind = kint), intent(in) :: iele_seed(1)
      real(kind = kreal), intent(in) :: x4_seed(4)
!
      real(kind = kreal), intent(inout) :: v_fline_start(4)
      real(kind = kreal), intent(inout)                                 &
     &         :: c_fline_start(fline_fields%ntot_color_comp)
!
      type(cal_interpolate_coefs_work), save :: itp_ele_work_f
      integer(kind = kint) :: ierr_inter
      real(kind = kreal) :: xi_in_ele(3)
!
      call alloc_work_4_interpolate(ele%nnod_4_ele, itp_ele_work_f)
      xi_in_ele(1:3) = -2.0
      call find_interpolate_in_ele(x4_seed, maxitr, eps_iter,           &
     &    my_rank, iflag_nomessage, error_level, node, ele,             &
     &    iele_seed(1), itp_ele_work_f, xi_in_ele, ierr_inter)
      call dealloc_work_4_interpolate(itp_ele_work_f)
!
      call cal_each_seed_field_in_ele                                   &
     &   (node, ele, nod_fld, fline_fields, iphys_4_fline,              &
     &    iele_seed, xi_in_ele, x4_seed, v_fline_start, c_fline_start)
!
      end subroutine set_field_at_each_seed_point
!
!  ---------------------------------------------------------------------
!
      end module set_fline_seeds_from_list
