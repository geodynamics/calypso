!>@file   m_fdm_coefs.f90
!!@brief  module m_fdm_coefs
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
!!    2nd order derivatives on node by nodal field
!!      dfdr =    r_2nd%fdm(1)%dmat(-1) * d_nod(k-1)
!!              + r_2nd%fdm(1)%dmat( 0) * d_nod(k  )
!!              + r_2nd%fdm(1)%dmat( 1) * d_nod(k+1)
!!      d2fdr2 =  r_2nd%fdm(2)%dmat(-1) * d_nod(k-1)
!!              + r_2nd%fdm(2)%dmat( 0) * d_nod(k  )
!!              + r_2nd%fdm(2)%dmat( 1) * d_nod(k+1)
!!
!!       r_2nd%fdm(1)%dmat = d1nod_mat_fdm_2
!!       r_2nd%fdm(2)%dmat = d2nod_mat_fdm_2
!!
!! ----------------------------------------------------------------------
!!      Coeeficients for derivatives by 1d finite difference method
!!
!!    4th order derivatives on node by nodal field
!!      dfdr =    r_4th%fdm(1)%dmat(-2,1) *  d_nod(k-2)
!!              + r_4th%fdm(1)%dmat(-1,2) *  d_nod(k-1)
!!              + r_4th%fdm(1)%dmat( 0,3) *  d_nod(k  )
!!              + r_4th%fdm(1)%dmat( 1,3) *  d_nod(k+1)
!!              + r_4th%fdm(1)%dmat( 2,4) *  d_nod(k+2)
!!      d2fdr2 =  r_4th%fdm(2)%dmat(-2,1) *  d_nod(k-2)
!!              + r_4th%fdm(2)%dmat(-1,2) *  d_nod(k-1)
!!              + r_4th%fdm(2)%dmat( 1,3) *  d_nod(k  )
!!              + r_4th%fdm(2)%dmat( 0,3) *  d_nod(k+1)
!!              + r_4th%fdm(2)%dmat( 2,4) *  d_nod(k+2)
!!      d3fdr3 =  r_4th%fdm(3)%dmat(-2,1) *  d_nod(k-2)
!!              + r_4th%fdm(3)%dmat(-1,2) *  d_nod(k-1)
!!              + r_4th%fdm(3)%dmat( 0,3) *  d_nod(k  )
!!              + r_4th%fdm(3)%dmat( 1,3) *  d_nod(k+1)
!!              + r_4th%fdm(3)%dmat( 2,4) *  d_nod(k+2)
!!      d4fdr4 =  r_4th%fdm(4)%dmat(-2,1) *  d_nod(k-2)
!!              + r_4th%fdm(4)%dmat(-1,2) *  d_nod(k-1)
!!              + r_4th%fdm(4)%dmat( 0,3) *  d_nod(k  )
!!              + r_4th%fdm(4)%dmat( 1,3) *  d_nod(k+1)
!!              + r_4th%fdm(4)%dmat( 2,4) *  d_nod(k+2)
!!
!!       r_4th%fdm(1)%dmat = d1nod_mat_fdm_4
!!       r_4th%fdm(2)%dmat = d2nod_mat_fdm_4
!!       r_4th%fdm(3)%dmat = d3nod_mat_fdm_4
!!       r_4th%fdm(4)%dmat = d4nod_mat_fdm_4
!!
!! ----------------------------------------------------------------------
!!      Coeeficients for derivatives by 1d finite difference method
!!
!!     define of elemental field
!!       r_ele(k) = half *(r_nod(k+1) + r_nod(k))
!!       d_ele(k) = half *(d_nod(k+1) + d_nod(k))
!!
!!    derivatives on node by element field
!!      dfdr =    r_2nd_ele%fdm(1)%dmat(0) * d_ele(k  )
!!              + r_2nd_ele%fdm(1)%dmat(1) * d_ele(k+1)
!!
!!    r_2nd_ele%fdm(1)%dmat = d1nod_mat_fdm_2e
!!
!! ----------------------------------------------------------------------
!!@endverbatim
!!
      module m_fdm_coefs
!
      use m_precision
      use m_constants
      use t_fdm_coefs
!
      implicit none
!
!
!>        Structure of 2nd order FDM matrices
      type(fdm_matrices), save :: r_2nd
!
!>        Structure of 4th order FDM matrices
      type(fdm_matrices), save :: r_4th
!
!>        Structure of 1st order FDM matrices on element
      type(fdm_matrices), save :: r_2nd_ele
!
      end module m_fdm_coefs
