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
!!      dfdr =    fdmn_nod%fdm(1)%dmat(k,-1) * d_nod(k-1)
!!              + fdmn_nod%fdm(1)%dmat(k, 0) * d_nod(k  )
!!              + fdmn_nod%fdm(1)%dmat(k, 1) * d_nod(k+1)
!!      d2fdr2 =  fdmn_nod%fdm(2)%dmat(k,-1) * d_nod(k-1)
!!              + fdmn_nod%fdm(2)%dmat(k, 0) * d_nod(k  )
!!              + fdmn_nod%fdm(2)%dmat(k, 1) * d_nod(k+1)
!! ----------------------------------------------------------------------
!!
!!      subroutine alloc_nod_fdm_matrices                               &
!!     &         (nri, ist_order, num_order, n_minus, n_plus, fdmn_nod)
!!      subroutine dealloc_nod_fdm_matrices(fdmn_nod)
!!        integer(kind = kint), intent(in) :: nri, num_order
!!        integer(kind = kint), intent(in) :: n_minus, n_plus
!!        type(fdm_matrices), intent(inout) :: fdmn_nod
!!
!!      subroutine check_fdm_coefs(nri, r, fdmn_nod)
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
!>        Structure of FDM matrix
      type fdm_matrix
!>        Number of radial points
        integer(kind = kint) :: nri_mat
!>        Width of matrix (positive side)
        integer(kind = kint) :: n_plus
!>        Width of matrix (negative side)
        integer(kind = kint) :: n_minus
!>        Coefficients to evaluate radial derivative
!!        from nodal field by FDM
        real(kind = kreal), allocatable :: dmat(:,:)
      end type fdm_matrix
!
!>        Structure of FDM matrices
      type fdm_matrices
!>        Minimum order for derivative
        integer(kind = kint) :: ist_order
!>        Maximum order for derivative
        integer(kind = kint) :: n_order
!>        Structure of FDM matrix
        type(fdm_matrix), allocatable :: fdm(:)
      end type fdm_matrices
!
      private :: alloc_fdm_matrix, dealloc_fdm_matrix
      private :: check_fdm_coef
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_nod_fdm_matrices                                 &
     &         (nri, ist_order, num_order, n_minus, n_plus, fdmn_nod)
!
      integer(kind = kint), intent(in) :: nri, ist_order, num_order
      integer(kind = kint), intent(in) :: n_minus, n_plus
      type(fdm_matrices), intent(inout) :: fdmn_nod
!
      integer(kind = kint) :: i
!
!
      fdmn_nod%ist_order = ist_order
      fdmn_nod%n_order =   num_order
      allocate( fdmn_nod%fdm(ist_order:num_order) )
!
      do i = fdmn_nod%ist_order, fdmn_nod%n_order
        call alloc_fdm_matrix(nri, n_minus, n_plus, fdmn_nod%fdm(i))
      end do
!
      end subroutine alloc_nod_fdm_matrices
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_nod_fdm_matrices(fdmn_nod)
!
      type(fdm_matrices), intent(inout) :: fdmn_nod
!
      integer(kind = kint) :: i
!
!
      do i = fdmn_nod%ist_order, fdmn_nod%n_order
        call dealloc_fdm_matrix(fdmn_nod%fdm(i))
      end do
!
      deallocate(fdmn_nod%fdm)
!
      end subroutine dealloc_nod_fdm_matrices
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_fdm_coefs(nri, r, fdmn_nod)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: r(nri)
      type(fdm_matrices), intent(in) :: fdmn_nod
!
      integer(kind = kint) :: i
!
!
      do i = fdmn_nod%ist_order, fdmn_nod%n_order
        write(50,*) 'Matrix for differences: ', i
        call check_fdm_coef(nri, r, fdmn_nod%fdm(i))
      end do
!
      end subroutine check_fdm_coefs
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_fdm_matrix(nri, n_minus, n_plus, fdm)
!
      integer(kind = kint), intent(in) :: nri, n_minus, n_plus
      type(fdm_matrix), intent(inout) :: fdm
!
!
      fdm%nri_mat = nri
      fdm%n_plus =  n_plus
      fdm%n_minus = n_minus
      allocate( fdm%dmat(fdm%nri_mat,-fdm%n_minus:fdm%n_plus) )
!
      if(nri .gt. 0) fdm%dmat = 0.0d0
!
      end subroutine alloc_fdm_matrix
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_fdm_matrix(fdm)
!
      type(fdm_matrix), intent(inout) :: fdm
!
      deallocate(fdm%dmat)
!
      end subroutine dealloc_fdm_matrix
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_fdm_coef(nri, r, fdm)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: r(nri)
      type(fdm_matrix), intent(in) :: fdm
!
      integer(kind = kint) :: kr
!
      write(50,*) 'kr, r, coefficients'
      do kr = 1, nri
        write(50,'(i5,1p40e20.12)')                                     &
     &       kr, r(kr), fdm%dmat(kr,-fdm%n_minus:fdm%n_plus)
      end do
!
      end subroutine check_fdm_coef
!
! -----------------------------------------------------------------------
!
      end module t_fdm_coefs
