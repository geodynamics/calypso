!>@file   t_spheric_rj_data.f90
!!@brief  module t_spheric_rj_data
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Structure for indexing table of speherical harmonic oefficients
!!
!!@verbatim
!!      subroutine alloc_type_spheric_param_rj(rj)
!!      subroutine alloc_type_sph_1d_index_rj(rj)
!!      subroutine alloc_rj_param_smp(rj)
!!        type(sph_rj_grid), intent(inout) :: rj
!!
!!      subroutine dealloc_spheric_param_rj(rj)
!!      subroutine dealloc_type_sph_1d_index_rj(rj)
!!      subroutine dealloc_rj_param_smp(rj)
!!        type(sph_rj_grid), intent(inout) :: rj
!!
!!      subroutine copy_spheric_rj_data                                 &
!!     &         (ltr_org, rj_org, ltr_new, rj_new)
!!        type(sph_rj_grid), intent(in) :: rj_org
!!        type(sph_rj_grid), intent(inout) :: rj_new
!!
!!      subroutine check_type_spheric_param_rj(id_rank, rj)
!!        integer, intent(in) :: id_rank
!!        type(sph_rj_grid), intent(in) :: rj
!!
!!      integer(kind = kint) function find_local_sph_address(rj, l, m)
!!        type(sph_rj_grid), intent(in) :: rj
!!        integer(kind = 4), intent(in) :: l, m
!!      integer(kind = kint) function local_sph_node_address            &
!!     &                            (rj, kr, j_lc)
!!        type(sph_rj_grid), intent(in) :: rj
!!@endverbatim
!!
!!@n @param  id_rank     Running rank ID
!!
      module t_spheric_rj_data
!
      use m_precision
      use m_constants
      use m_spheric_constants
!
      implicit none
!
!>        structure of index table for @f$ f(r,j) @f$
      type sph_rj_grid
!>        integer flag for center point in spectr data
!!@n      This flag should have same value for all processes
!!@n      0: No center point
!!@n      1: Center point is there
        integer (kind=kint) :: iflag_rj_center =  0
!
!>        number of global 1d data points for @f$ f(r,j) @f$
        integer(kind = kint) :: nidx_global_rj(2)
!>        1d subdomain ID for @f$ f(r,j) @f$ (start from 0)
        integer(kind = kint) :: irank_sph_rj(2)
!
!>        local spectr index for @f$ l = m = 0 @f$
!!@n      If @f$ l = m = 0 @f$ mode does not exist in subdomain, 
!!@n      idx_rj_degree_zero = 0.
        integer (kind=kint) :: idx_rj_degree_zero
!>        local spectr index for @f$ l = 1@f$ and  @f$ m = -1, 0, 1@f$.
!!        for @f$ f(r,j) @f$
!!@n        If spectr data do not exist in subdomain,
!!@n        idx_rj_degree_one(m) = 0.
        integer (kind=kint) :: idx_rj_degree_one(-1:1)
!
!>        local spectr index for @f$ l = m = 0 @f$ at center
!!@n      if center does not exist in subdomain, inod_rj_center = 0.
        integer (kind=kint) :: inod_rj_center =   0
!
!
!>        number of data points for @f$ f(r,j) @f$
        integer(kind = kint) :: nnod_rj
!>        number of 1d data points for @f$ f(r,j) @f$
        integer(kind = kint) :: nidx_rj(2)
!>        number of increments for @f$ f(r,j) @f$
        integer(kind = kint) :: istep_rj(2)
!>        1d start address of global data for @f$ f(r,j) @f$
        integer(kind = kint) :: ist_rj(2)
!>        1d end address of global data for @f$ f(r,j) @f$
        integer(kind = kint) :: ied_rj(2)
!
!>        SMP stack for spectr data @f$ f(r,j) @f$
      integer(kind = kint), allocatable :: istack_inod_rj_smp(:)
!
!>        SMP stacks for indexing @f$ (r,j) @f$
      integer(kind = kint), allocatable :: istack_rj_kr_smp(:)
!>        SMP stacks for indexing @f$ (r,j) @f$
      integer(kind = kint), allocatable :: istack_rj_j_smp(:)
!
!>        global address for each direction @f$ f(r,j) @f$
        integer(kind = kint), allocatable :: idx_global_rj(:,:)
!
!>        radial global address @f$ f(r,j) @f$
        integer(kind = kint), allocatable :: idx_gl_1d_rj_r(:)
!>        spherical harmonics mode for  @f$ f(r,j) @f$
!!@n        idx_gl_1d_rj_j(j,1): global ID for spherical harmonics
!!@n        idx_gl_1d_rj_j(j,2): spherical hermonincs degree
!!@n        idx_gl_1d_rj_j(j,3): spherical hermonincs order
        integer(kind = kint), allocatable :: idx_gl_1d_rj_j(:,:)
!
!>        1d radius data for @f$ f(r,j) @f$
        real(kind = kreal), allocatable :: radius_1d_rj_r(:)
!>        1d @f$1 / r @f$ for @f$ f(r,j) @f$
        real(kind = kreal), allocatable :: a_r_1d_rj_r(:)
!
!>        1d @f$1 / r @f$ for @f$ f(r,j) @f$
!!@n       see set_radius_func_cheby or set_radius_func_cheby
        real(kind = kreal), allocatable :: ar_1d_rj(:,:)
!
!>        1d radius between grids for @f$ f(r,j) @f$
        real(kind = kreal), allocatable :: r_ele_rj(:)
!>        1d @f$1 / r @f$ between grids for @f$ f(r,j) @f$
        real(kind = kreal), allocatable :: ar_ele_rj(:,:)
      end type sph_rj_grid
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_type_spheric_param_rj(rj)
!
      type(sph_rj_grid), intent(inout) :: rj
!
      allocate(rj%idx_global_rj(rj%nnod_rj,2))
!
      if(rj%nnod_rj .gt. 0) rj%idx_global_rj =  0
!
      end subroutine alloc_type_spheric_param_rj
!
! ----------------------------------------------------------------------
!
      subroutine alloc_type_sph_1d_index_rj(rj)
!
      type(sph_rj_grid), intent(inout) :: rj
      integer(kind = kint) :: num
!
      num = rj%nidx_rj(1)
      allocate(rj%idx_gl_1d_rj_r(num))
      allocate(rj%radius_1d_rj_r(num))
      allocate(rj%a_r_1d_rj_r(num))
!
      allocate(rj%ar_1d_rj(num,3))
      allocate(rj%r_ele_rj(num))
      allocate(rj%ar_ele_rj(num,3))
!
      num = rj%nidx_rj(2)
      allocate(rj%idx_gl_1d_rj_j(num,3))
!
      rj%idx_rj_degree_zero = 0
      rj%idx_rj_degree_one(-1:1) = 0
!
      if(rj%nidx_rj(2) .gt. 0) rj%idx_gl_1d_rj_j = 0
      if(rj%nidx_rj(1) .gt. 0) then
        rj%idx_gl_1d_rj_r = 0
        rj%radius_1d_rj_r = 0.0d0
        rj%a_r_1d_rj_r = 0.0d0
!
        rj%ar_1d_rj = 0.0d0
        rj%r_ele_rj = 0.0d0
        rj%ar_ele_rj = 0.0d0
      end if
!
      end subroutine alloc_type_sph_1d_index_rj
!
! ----------------------------------------------------------------------
!
      subroutine alloc_rj_param_smp(rj)
!
      use m_machine_parameter
!
      type(sph_rj_grid), intent(inout) :: rj
!
!
      allocate(rj%istack_inod_rj_smp(0:np_smp))
!
      allocate(rj%istack_rj_kr_smp(0:np_smp))
      allocate(rj%istack_rj_j_smp(0:np_smp))
!
      rj%istack_inod_rj_smp = 0
!
      rj%istack_rj_kr_smp =  0
      rj%istack_rj_j_smp =  0
!
      end subroutine alloc_rj_param_smp
!
! -----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_spheric_param_rj(rj)
!
      type(sph_rj_grid), intent(inout) :: rj
!
      deallocate(rj%idx_global_rj)
!
      end subroutine dealloc_spheric_param_rj
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_type_sph_1d_index_rj(rj)
!
      type(sph_rj_grid), intent(inout) :: rj
!
!
      deallocate(rj%radius_1d_rj_r, rj%a_r_1d_rj_r)
      deallocate(rj%ar_1d_rj, rj%r_ele_rj, rj%ar_ele_rj)
      deallocate(rj%idx_gl_1d_rj_r, rj%idx_gl_1d_rj_j)
!
      end subroutine dealloc_type_sph_1d_index_rj
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_rj_param_smp(rj)
!
      type(sph_rj_grid), intent(inout) :: rj
!
!
      deallocate(rj%istack_inod_rj_smp)
      deallocate(rj%istack_rj_kr_smp, rj%istack_rj_j_smp)
!
      end subroutine dealloc_rj_param_smp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_spheric_rj_data                                   &
     &         (ltr_org, rj_org, ltr_new, rj_new)
!
      type(sph_rj_grid), intent(in) :: rj_org
      integer(kind = kint), intent(in) :: ltr_org
!
      type(sph_rj_grid), intent(inout) :: rj_new
      integer(kind = kint), intent(inout) :: ltr_new
!
      integer(kind = kint) :: i
!
      rj_new%irank_sph_rj(1:itwo) =     rj_org%irank_sph_rj(1:itwo)
!
      rj_new%nidx_global_rj(1:itwo) = rj_org%nidx_global_rj(1:itwo)
      ltr_new =   ltr_org
!
      rj_new%nnod_rj =         rj_org%nnod_rj
      rj_new%nidx_rj(1:itwo) = rj_org%nidx_rj(1:itwo)
      rj_new%ist_rj(1:itwo) =  rj_org%ist_rj(1:itwo)
      rj_new%ied_rj(1:itwo) =  rj_org%ied_rj(1:itwo)
!
      call alloc_type_spheric_param_rj(rj_new)
      call alloc_type_sph_1d_index_rj(rj_new)
!
      do i = 1, itwo
        rj_new%idx_global_rj(1:rj_new%nnod_rj,i)                        &
     &       = rj_org%idx_global_rj(1:rj_new%nnod_rj,i)
      end do
!
      rj_new%radius_1d_rj_r(1:rj_new%nidx_rj(1))                        &
     &       = rj_org%radius_1d_rj_r(1:rj_new%nidx_rj(1))
      rj_new%idx_gl_1d_rj_r(1:rj_new%nidx_rj(1))                        &
     &       = rj_org%idx_gl_1d_rj_r(1:rj_new%nidx_rj(1))
      rj_new%idx_gl_1d_rj_j(1:rj_new%nidx_rj(2),1)                      &
     &       = rj_org%idx_gl_1d_rj_j(1:rj_new%nidx_rj(2),1)
      rj_new%idx_gl_1d_rj_j(1:rj_new%nidx_rj(2),2)                      &
     &       = rj_org%idx_gl_1d_rj_j(1:rj_new%nidx_rj(2),2)
      rj_new%idx_gl_1d_rj_j(1:rj_new%nidx_rj(2),3)                      &
     &      = rj_org%idx_gl_1d_rj_j(1:rj_new%nidx_rj(2),3)
!
!      call dealloc_type_sph_1d_index_rj(rj_org)
!      call dealloc_spheric_param_rj(rj_org)
!
      end subroutine copy_spheric_rj_data
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_type_spheric_param_rj(id_rank, rj)
!
      integer, intent(in) :: id_rank
      type(sph_rj_grid), intent(in) :: rj
      integer(kind = kint) :: i
!
!
      write(id_rank+50,*) 'irank_sph_rj ',  rj%irank_sph_rj(1:2)
      write(id_rank+50,*) 'nidx_rj  ',  rj%nidx_rj(1:2)
      write(id_rank+50,*) 'nnod_rj ',  rj%nnod_rj
!
      write(id_rank+50,*) 'i, idx_global_rj(r,j)'
      do i = 1, rj%nnod_rj
        write(id_rank+50,*) i, rj%idx_global_rj(i,1:2)
      end do
!
      end subroutine check_type_spheric_param_rj
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      integer(kind = kint) function find_local_sph_address(rj, l, m)
!
      type(sph_rj_grid), intent(in) :: rj
      integer, intent(in) :: l, m
!
      integer(kind = kint) :: j
!
!
      find_local_sph_address = 0
      do j = 1, rj%nidx_rj(2)
        if (   rj%idx_gl_1d_rj_j(j,2) .eq. l                            &
     &   .and. rj%idx_gl_1d_rj_j(j,3) .eq. m) then
          find_local_sph_address = j
          return
        end if
      end do
!
      end function find_local_sph_address
!
!-----------------------------------------------------------------------
!
      integer(kind = kint) function local_sph_node_address              &
     &                            (rj, kr, j_lc)
!
      type(sph_rj_grid), intent(in) :: rj
      integer, intent(in) :: kr, j_lc
!
!
      local_sph_node_address = j_lc + (kr-1)*rj%nidx_rj(2)
!
      end function local_sph_node_address
!
!-----------------------------------------------------------------------
!
      end module t_spheric_rj_data
