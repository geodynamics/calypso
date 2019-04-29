!>@file   t_spheric_rlm_data.f90
!!@brief  module t_spheric_rlm_data
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Structure for indexing table of speherical harmonics transform
!!
!!@verbatim
!!      subroutine alloc_type_spheric_param_rlm(sph_rlm)
!!      subroutine alloc_type_sph_1d_index_rlm(sph_rlm)
!!      subroutine alloc_rlm_param_smp(sph_rlm)
!!        type(sph_rlm_grid), intent(inout) :: sph_rlm
!!
!!      subroutine dealloc_type_spheric_param_rlm(sph_rlm)
!!      subroutine dealloc_rlm_param_smp(sph_rlm)
!!        type(sph_rlm_grid), intent(inout) :: sph_rlm
!!
!!      subroutine copy_spheric_rlm_data                                &
!!     &         (ltr_org, rlm_org, ltr_new, rlm_new)
!!        type(sph_rlm_grid), intent(in) :: rlm_org
!!        type(sph_rlm_grid), intent(inout) :: rlm_new
!!
!!      subroutine check_type_spheric_param_rlm(id_rank, sph_rlm)
!!        integer, intent(in) :: id_rank
!!        type(sph_rlm_grid), intent(in) :: sph_rlm
!!@endverbatim
!!
!!@n @param  id_rank     Running rank ID
!!
      module t_spheric_rlm_data
!
      use m_precision
      use m_constants
      use m_spheric_constants
!
      implicit none
!
!
!>        structure of index table for @f$ f(r,l,m) @f$
      type sph_rlm_grid
!>        number of global 1d data points for @f$ f(r,l,m) @f$
        integer(kind = kint) :: nidx_global_rlm(2)
!>        1d subdomain ID for @f$ f(r,l,m) @f$ (start from 0)
        integer(kind = kint) :: irank_sph_rlm(2)
!
!>        number of data points for @f$ f(r,l,m) @f$
        integer(kind = kint) :: nnod_rlm
!>        number of 1d data points for @f$ f(r,l,m) @f$
        integer(kind = kint) :: nidx_rlm(2)
!>        number of increments for @f$ f(r,l,m) @f$
        integer(kind = kint) :: istep_rlm(2)
!>        1d start address of global data for @f$ f(r,l,m) @f$
        integer(kind = kint) :: ist_rlm(2)
!>        1d end address of global data for @f$ f(r,l,m) @f$
        integer(kind = kint) :: ied_rlm(2)
!
!>        SMP stack for spectr data @f$ f(r,l,m) @f$
        integer(kind = kint), allocatable :: istack_inod_rlm_smp(:)
!
!>        SMP stacks for indexing @f$ r@f$
        integer(kind = kint), allocatable :: istack_rlm_kr_smp(:)
!>        SMP stacks for indexing @f$ j @f$
        integer(kind = kint), allocatable :: istack_rlm_j_smp(:)
!
!>        global address for each direction @f$ f(r,l,m) @f$
        integer(kind = kint), allocatable :: idx_global_rlm(:,:)
!
!>        radial global address for @f$ f(r,l,m) @f$
        integer(kind = kint), allocatable :: idx_gl_1d_rlm_r(:)
!>        spherical harmonics mode for  @f$ f(r,l,m) @f$
!!@n        idx_gl_1d_rj_j(j,1): global ID for spherical harmonics
!!@n        idx_gl_1d_rj_j(j,2): spherical hermonincs degree
!!@n        idx_gl_1d_rj_j(j,3): spherical hermonincs order
        integer(kind = kint), allocatable :: idx_gl_1d_rlm_j(:,:)
!
!>        1d radius data for @f$ f(r,l,m) @f$
        real(kind = kreal), allocatable :: radius_1d_rlm_r(:)
!>        1 / radius_1d_rlm_r
        real(kind = kreal), allocatable :: a_r_1d_rlm_r(:)
      end type sph_rlm_grid
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_type_spheric_param_rlm(sph_rlm)
!
      type(sph_rlm_grid), intent(inout) :: sph_rlm
!
      allocate(sph_rlm%idx_global_rlm(sph_rlm%nnod_rlm,2))
!
      if(sph_rlm%nnod_rlm .gt. 0) then
        sph_rlm%idx_global_rlm = 0
      end if
!
      end subroutine alloc_type_spheric_param_rlm
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine alloc_type_sph_1d_index_rlm(sph_rlm)
!
      type(sph_rlm_grid), intent(inout) :: sph_rlm
      integer(kind = kint) :: num
!
      num = sph_rlm%nidx_rlm(1)
      allocate(sph_rlm%idx_gl_1d_rlm_r(num))
      allocate(sph_rlm%radius_1d_rlm_r(num))
      allocate(sph_rlm%a_r_1d_rlm_r(num))
      num = sph_rlm%nidx_rlm(2)
      allocate(sph_rlm%idx_gl_1d_rlm_j(num,3))
!
      if(sph_rlm%nidx_rlm(2) .gt. 0) sph_rlm%idx_gl_1d_rlm_j = 0
      if(sph_rlm%nidx_rlm(1) .gt. 0) then
        sph_rlm%idx_gl_1d_rlm_r = 0
        sph_rlm%radius_1d_rlm_r = 0.0d0
        sph_rlm%a_r_1d_rlm_r    = 0.0d0
      end if
!
      end subroutine alloc_type_sph_1d_index_rlm
!
! ----------------------------------------------------------------------
!
      subroutine alloc_rlm_param_smp(sph_rlm)
!
      use m_machine_parameter
!
      type(sph_rlm_grid), intent(inout) :: sph_rlm
!
!
      allocate(sph_rlm%istack_inod_rlm_smp(0:np_smp))
!
      allocate(sph_rlm%istack_rlm_kr_smp(0:np_smp))
      allocate(sph_rlm%istack_rlm_j_smp(0:np_smp))
!
      sph_rlm%istack_inod_rlm_smp = 0
!
      sph_rlm%istack_rlm_kr_smp = 0
      sph_rlm%istack_rlm_j_smp = 0
!
      end subroutine alloc_rlm_param_smp
!
! -----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_type_spheric_param_rlm(sph_rlm)
!
      type(sph_rlm_grid), intent(inout) :: sph_rlm
!
      deallocate(sph_rlm%idx_global_rlm)
!
      end subroutine dealloc_type_spheric_param_rlm
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_type_sph_1d_index_rlm(sph_rlm)
!
      type(sph_rlm_grid), intent(inout) :: sph_rlm
!
!
      deallocate(sph_rlm%radius_1d_rlm_r)
      deallocate(sph_rlm%a_r_1d_rlm_r   )
      deallocate(sph_rlm%idx_gl_1d_rlm_r)
      deallocate(sph_rlm%idx_gl_1d_rlm_j)
!
      end subroutine dealloc_type_sph_1d_index_rlm
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_rlm_param_smp(sph_rlm)
!
      type(sph_rlm_grid), intent(inout) :: sph_rlm
!
!
      deallocate(sph_rlm%istack_inod_rlm_smp)
      deallocate(sph_rlm%istack_rlm_kr_smp, sph_rlm%istack_rlm_j_smp)
!
      end subroutine dealloc_rlm_param_smp
!
! -----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine copy_spheric_rlm_data                                  &
     &         (ltr_org, rlm_org, ltr_new, rlm_new)
!
      type(sph_rlm_grid), intent(in) :: rlm_org
      integer(kind = kint), intent(in) :: ltr_org
!
      type(sph_rlm_grid), intent(inout) :: rlm_new
      integer(kind = kint), intent(inout) :: ltr_new
!
      integer(kind = kint) :: i
!
      rlm_new%irank_sph_rlm(1:itwo) =   rlm_org%irank_sph_rlm(1:itwo)
!
      rlm_new%nidx_global_rlm(1:itwo) = rlm_org%nidx_global_rlm(1:itwo)
      ltr_new =   ltr_org
!
      rlm_new%nnod_rlm =         rlm_org%nnod_rlm
      rlm_new%nidx_rlm(1:itwo) = rlm_org%nidx_rlm(1:itwo)
      rlm_new%ist_rlm(1:itwo) =  rlm_org%ist_rlm(1:itwo)
      rlm_new%ied_rlm(1:itwo) =  rlm_org%ied_rlm(1:itwo)
!
      call alloc_type_spheric_param_rlm(rlm_new)
      call alloc_type_sph_1d_index_rlm(rlm_new)
!
      do i = 1, itwo
        rlm_new%idx_global_rlm(1:rlm_new%nnod_rlm,i)                    &
     &      = rlm_org%idx_global_rlm(1:rlm_new%nnod_rlm,i)
      end do
!
      rlm_new%radius_1d_rlm_r(1:rlm_new%nidx_rlm(1))                    &
     &       =   rlm_org%radius_1d_rlm_r(1:rlm_new%nidx_rlm(1))
      rlm_new%idx_gl_1d_rlm_r(1:rlm_new%nidx_rlm(1))                    &
     &       =   rlm_org%idx_gl_1d_rlm_r(1:rlm_new%nidx_rlm(1))
      rlm_new%idx_gl_1d_rlm_j(1:rlm_new%nidx_rlm(2),1)                  &
     &       = rlm_org%idx_gl_1d_rlm_j(1:rlm_new%nidx_rlm(2),1)
      rlm_new%idx_gl_1d_rlm_j(1:rlm_new%nidx_rlm(2),2)                  &
     &       = rlm_org%idx_gl_1d_rlm_j(1:rlm_new%nidx_rlm(2),2)
      rlm_new%idx_gl_1d_rlm_j(1:rlm_new%nidx_rlm(2),3)                  &
     &       = rlm_org%idx_gl_1d_rlm_j(1:rlm_new%nidx_rlm(2),3)
!
!      call dealloc_type_sph_1d_index_rlm(rlm_org)
!      call dealloc_type_spheric_param_rlm(rlm_org)
!
      end subroutine copy_spheric_rlm_data
!
! ----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_type_spheric_param_rlm(id_rank, sph_rlm)
!
      integer, intent(in) :: id_rank
      type(sph_rlm_grid), intent(in) :: sph_rlm
      integer(kind = kint) :: i
!
!
      write(id_rank+50,*) 'irank_sph_rlm ', sph_rlm%irank_sph_rlm(1:2)
      write(id_rank+50,*) 'nidx_rlm ', sph_rlm%nidx_rlm(1:2)
      write(id_rank+50,*) 'nnod_rlm ', sph_rlm%nnod_rlm
!
      write(id_rank+50,*) 'i, idx_global_rlm(r,j)'
      do i = 1, sph_rlm%nnod_rlm
        write(id_rank+50,*) i, sph_rlm%idx_global_rlm(i,1:2)
      end do
!
      end subroutine check_type_spheric_param_rlm
!
! -----------------------------------------------------------------------
!
      end module t_spheric_rlm_data
