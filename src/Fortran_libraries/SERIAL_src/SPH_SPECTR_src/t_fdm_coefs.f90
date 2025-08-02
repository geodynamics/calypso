!>@file   t_fdm_coefs.f90
!!@brief  module t_fdm_coefs
!!
!!@author H. Matsui
!!@date Programmed in Jan, 2010
!
!>@brief  Coefficients to obtain radial derivatives
!!        by finite difference method
!!
!!@verbatim
!! ----------------------------------------------------------------------
!!      Coeeficients for derivatives by 1d finite difference method
!!
!!     define of elemental field
!!       r_ele(k) = half *(r_nod(k-1) + r_nod(k))
!!       d_ele(k) = half *(d_nod(k-1) + d_nod(k))
!!
!!    2nd order derivatives on node by nodal field
!!      dfdr =    r_fdm%dmat(-1,k,1) * d_nod(k-1)
!!              + r_fdm%dmat( 0,k,1) * d_nod(k  )
!!              + r_fdm%dmat( 1,k,1) * d_nod(k+1)
!!      d2fdr2 =  r_fdm%dmat(-1,k,2) * d_nod(k-1)
!!              + r_fdm%dmat( 0,k,2) * d_nod(k  )
!!              + r_fdm%dmat( 1,k,2) * d_nod(k+1)
!! ----------------------------------------------------------------------
!!
!!      subroutine alloc_nod_fdm_matrices                               &
!!     &         (nri, ist_order, num_order, n_minus, n_plus, r_fdm)
!!      subroutine dealloc_nod_fdm_matrices(r_fdm)
!!        integer(kind = kint), intent(in) :: nri, num_order
!!        integer(kind = kint), intent(in) :: n_minus, n_plus
!!        type(fdm_matrices), intent(inout) :: r_fdm
!!
!!      subroutine check_fdm_coefs(id_file, nri, r, r_fdm)
!!        integer(kind = kint), intent(in) :: id_file
!!        integer(kind = kint), intent(in) :: nri
!!        real(kind = kreal), intent(in) :: r(nri)
!!        type(fdm_matrices), intent(in) :: r_fdm
!!@endverbatim
!!
!!@n @param nri    number of radial grid points
!!@n @param r(nri) radius
!
      module t_fdm_coefs
!
      use m_precision
!
      implicit none
!
!
!>        Structure of FDM matrices
      type fdm_matrices
!>        Minimum order for derivative
        integer(kind = kint) :: ist_order
!>        Maximum order for derivative
        integer(kind = kint) :: n_order
!
!>        Number of radial points
        integer(kind = kint) :: nri_mat
!>        Width of matrix (positive side)
        integer(kind = kint) :: n_plus
!>        Width of matrix (negative side)
        integer(kind = kint) :: n_minus
!>        Coefficients to evaluate radial derivative
!!        from nodal field by FDM
        real(kind = kreal), allocatable :: dmat(:,:,:)
      end type fdm_matrices
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_nod_fdm_matrices                                 &
     &         (nri, ist_order, num_order, n_minus, n_plus, r_fdm)
!
      integer(kind = kint), intent(in) :: nri, ist_order, num_order
      integer(kind = kint), intent(in) :: n_minus, n_plus
      type(fdm_matrices), intent(inout) :: r_fdm
!
      integer(kind = kint) :: i
!
!
      r_fdm%ist_order = ist_order
      r_fdm%n_order =   num_order
      r_fdm%nri_mat =  nri
      r_fdm%n_plus =   n_plus
      r_fdm%n_minus = -n_minus
      allocate(r_fdm%dmat(-n_minus:n_plus,1:nri,0:num_order))
!
      if(size(r_fdm%dmat) .gt. 0) then
!$omp parallel workshare
        r_fdm%dmat(-n_minus:n_plus,1:nri,0:num_order) = 0.0d0
!$omp end parallel workshare
      end if
!
      end subroutine alloc_nod_fdm_matrices
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_nod_fdm_matrices(r_fdm)
!
      type(fdm_matrices), intent(inout) :: r_fdm
!
!
      deallocate(r_fdm%dmat)
!
      end subroutine dealloc_nod_fdm_matrices
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_fdm_coefs(id_file, nri, r, r_fdm)
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: r(nri)
      type(fdm_matrices), intent(in) :: r_fdm
!
      integer(kind = kint) :: i, kr
!
!
      do i = 0, r_fdm%n_order
        write(id_file,*) 'Matrix for differences: ', i
        write(id_file,*) 'r, kr, coefficients'
        do kr = 1, nri
          write(id_file,'(1pe20.12,i5,1p40e20.12)') r(kr), kr,          &
     &          r_fdm%dmat(r_fdm%n_minus:r_fdm%n_plus,kr,i)
        end do
      end do
!
      end subroutine check_fdm_coefs
!
! -----------------------------------------------------------------------
!
      end module t_fdm_coefs
