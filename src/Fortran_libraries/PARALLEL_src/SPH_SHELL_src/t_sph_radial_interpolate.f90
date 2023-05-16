!>@file   t_sph_radial_interpolate.f90
!!@brief  module t_sph_radial_interpolate
!!
!!@author H. Matsui
!!@date Programmed in March, 2012
!
!>@brief Interpolate spectr data
!!
!!@verbatim
!!      subroutine alloc_org_radius_interpolate(nri_source, r_itp)
!!      subroutine alloc_radial_interpolate(nri_target, r_itp)
!!      subroutine alloc_original_sph_data(n_rj_org, r_itp)
!!      subroutine dealloc_org_radius_interpolate(r_itp)
!!      subroutine dealloc_radial_interpolate(r_itp)
!!      subroutine dealloc_original_sph_data(r_itp)
!!        integer(kind = kint), intent(in) :: nri_source
!!        integer(kind = kint), intent(in) :: nri_target
!!        integer(kind = kint), intent(in) :: n_rj_org
!!        type(sph_radial_interpolate), intent(inout) :: r_itp
!!
!!      subroutine share_r_interpolation_tbl(new_nri, r_itp)
!!        integer(kind = kint), intent(in) :: new_nri
!!        type(sph_radial_interpolate), intent(inout) :: r_itp
!!
!!      subroutine check_sph_radial_interpolate                         &
!!     &         (nri_org, r_org, nri_new, r_new, r_itp)
!!        integer(kind = kint), intent(in) :: nri_org, nri_new
!!        real(kind = kreal), intent(in) :: r_org(nri_org)
!!        real(kind = kreal), intent(in) :: r_new(nri_org)
!!        type(sph_radial_interpolate), intent(inout) :: r_itp
!!@endverbatim
!
      module t_sph_radial_interpolate
!
      use m_precision
      use m_constants
!
!      use calypso_mpi
!      use m_machine_parameter
!
!      use t_spheric_rj_data
!      use t_phys_address
!      use t_phys_data
!
      implicit  none
!
!
!>      Structure for radial interpolation
      type sph_radial_interpolate
!>        Logical flag if radial grid is same
        logical :: flag_same_rgrid =  .TRUE.
!
!>        Inner boundary address
        integer(kind = kint) :: kr_target_inside
!>        Outer boundary address
        integer(kind = kint) :: kr_target_outside
!
!>        Innermost new radial ID within the original domain
        integer(kind = kint) :: kr_inner_source = 0
!>        Outmost new radial ID within the original domain
        integer(kind = kint) :: kr_outer_source = 0
!
!>        Radial data number for target data
        integer(kind = kint) :: nri_target
!>      Inner radial grid ID for interpolation
        integer(kind = kint), allocatable :: k_old2new_in(:)
!>      Outer radial grid ID for interpolation
        integer(kind = kint), allocatable :: k_old2new_out(:)
!>        interpolation coefficients for interpolation
        real(kind = kreal), allocatable :: coef_old2new_in(:)
!
!>        Radial data number for target data
        integer(kind = kint) :: nri_source
!>        Original radius
        real(kind = kreal), allocatable :: source_radius(:)
!
!>        Total data number for original data
        integer(kind = kint) :: n_rj_org
!>        Original work area for fields
        real(kind = kreal), allocatable :: d_rj_org(:,:)
      end type sph_radial_interpolate
!
!  -------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_org_radius_interpolate(nri_source, r_itp)
!
      integer(kind = kint), intent(in) :: nri_source
      type(sph_radial_interpolate), intent(inout) :: r_itp
!
!
      r_itp%nri_source = nri_source
      allocate(r_itp%source_radius(r_itp%nri_source))
      if(r_itp%nri_source .gt. 0) r_itp%source_radius = zero
!
      end subroutine alloc_org_radius_interpolate
!
!  -------------------------------------------------------------------
!
      subroutine alloc_radial_interpolate(nri_target, r_itp)
!
      integer(kind = kint), intent(in) :: nri_target
      type(sph_radial_interpolate), intent(inout) :: r_itp
!
!
      r_itp%nri_target = nri_target
      allocate(r_itp%k_old2new_in(r_itp%nri_target))
      allocate(r_itp%k_old2new_out(r_itp%nri_target))
      allocate(r_itp%coef_old2new_in(r_itp%nri_target))
!
      if(r_itp%nri_target .gt. 0) then
        r_itp%k_old2new_in = izero
        r_itp%k_old2new_out = izero
        r_itp%coef_old2new_in = zero
      end if
!
      end subroutine alloc_radial_interpolate
!
!  -------------------------------------------------------------------
!
      subroutine alloc_original_sph_data(n_rj_org, r_itp)
!
      integer(kind = kint), intent(in) :: n_rj_org
      type(sph_radial_interpolate), intent(inout) :: r_itp
!
      r_itp%n_rj_org = n_rj_org
      allocate(r_itp%d_rj_org(r_itp%n_rj_org,6))
      if(r_itp%n_rj_org .gt. 0) r_itp%d_rj_org = zero
!
      end subroutine alloc_original_sph_data
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine dealloc_org_radius_interpolate(r_itp)
!
      type(sph_radial_interpolate), intent(inout) :: r_itp
!
      deallocate(r_itp%source_radius)
!
      end subroutine dealloc_org_radius_interpolate
!
!  -------------------------------------------------------------------
!
      subroutine dealloc_radial_interpolate(r_itp)
!
      type(sph_radial_interpolate), intent(inout) :: r_itp
!
      deallocate(r_itp%coef_old2new_in)
      deallocate(r_itp%k_old2new_in, r_itp%k_old2new_out)
!
      end subroutine dealloc_radial_interpolate
!
!  -------------------------------------------------------------------
!
      subroutine dealloc_original_sph_data(r_itp)
!
      type(sph_radial_interpolate), intent(inout) :: r_itp
!
      deallocate(r_itp%d_rj_org)
!
      end subroutine dealloc_original_sph_data
!
!  -------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine share_r_interpolation_tbl(new_nri, r_itp)
!
      use calypso_mpi_real
      use calypso_mpi_int
      use calypso_mpi_logical
      use transfer_to_long_integers
!
      integer(kind = kint), intent(in) :: new_nri
      type(sph_radial_interpolate), intent(inout) :: r_itp
!
!
      call calypso_mpi_bcast_one_logical(r_itp%flag_same_rgrid, 0)
      if(my_rank .eq. 0) write(*,*) 'flag_same_rgrid: ',                &
     &            r_itp%flag_same_rgrid, new_nri
!
      if(r_itp%flag_same_rgrid) return
      if(my_rank .ne. 0)  call alloc_radial_interpolate(new_nri, r_itp)
!
        call calypso_mpi_bcast_one_int(r_itp%nri_target, 0)
        call calypso_mpi_bcast_one_int(r_itp%kr_inner_source, 0)
        call calypso_mpi_bcast_one_int(r_itp%kr_outer_source, 0)
!
        call calypso_mpi_bcast_int                                      &
     &     (r_itp%k_old2new_in, cast_long(r_itp%nri_target), 0)
        call calypso_mpi_bcast_int                                      &
     &     (r_itp%k_old2new_out, cast_long(r_itp%nri_target), 0)
        call calypso_mpi_bcast_real                                     &
     &     (r_itp%coef_old2new_in, cast_long(r_itp%nri_target), 0)
!
      end subroutine share_r_interpolation_tbl
!
! -----------------------------------------------------------------------
!
      subroutine check_sph_radial_interpolate                           &
     &         (nri_org, r_org, nri_new, r_new, r_itp)
!
      integer(kind = kint), intent(in) :: nri_org, nri_new
      real(kind = kreal), intent(in) :: r_org(nri_org)
      real(kind = kreal), intent(in) :: r_new(nri_org)
      type(sph_radial_interpolate), intent(in) :: r_itp
!
      integer(kind = kint) :: k
!
!
      write(*,*) 'r_itp%kr_inner_source', r_itp%kr_inner_source
      write(*,*) 'r_itp%kr_outer_source', r_itp%kr_outer_source
      do k = 1, nri_new
        write(*,'(i5,1pe16.8,2i5,1p3e16.8)') k, r_new(k),               &
     &         r_itp%k_old2new_in(k), r_itp%k_old2new_out(k),           &
     &         r_org(r_itp%k_old2new_in(k)),                            &
     &         r_org(r_itp%k_old2new_out(k)),                           &
     &         r_itp%coef_old2new_in(k)
      end do
!
      end subroutine check_sph_radial_interpolate
!
! -----------------------------------------------------------------------
!
      end module t_sph_radial_interpolate
