!>@file   m_sph_trans_parameter.f90
!!@brief  module m_sph_trans_parameter
!!
!!@author H. Matsui
!!@date Programmed in July, 2007 
!
!>@brief  Work parameters for parallelization of spherical harmonics
!!
!!@verbatim
!!      subroutine set_local_sph_dimension(my_rank)
!!@endverbatim
!
      module m_sph_trans_parameter
!
      use m_precision
!
      implicit none
!
!
!
      integer(kind = kint) :: ltr
      integer(kind = kint) :: nr_oc, nr_ic, nr_mt, ntheta, nphi
!
      integer(kind = kint) :: ndomain_r, ndomain_r2, ndomain_r3
      integer(kind = kint) :: ndomain_t, ndomain_t2, ndomain_t3
      integer(kind = kint) :: ndomain_p, ndomain_p2, ndomain_p3
!
      integer(kind = kint) :: nro_l, nro_l2, nro_l3
      integer(kind = kint) :: nri_l, nri_l2, nri_l3
      integer(kind = kint) :: nrm_l, nrm_l2, nrm_l3
      integer(kind = kint) :: nth_l, nth_l2, nth_l3
      integer(kind = kint) :: nph_l, nph_l2, nph_l3
!
      integer(kind = kint) :: idomain_r, idomain_r2, idomain_r3
      integer(kind = kint) :: idomain_t, idomain_t2, idomain_t3
      integer(kind = kint) :: idomain_p, idomain_p2, idomain_p3
!
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_local_sph_dimension(my_rank)
!
!     ndomain_p = 1
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      nro_l = nr_oc /  ndomain_r
      nri_l = nr_ic /  ndomain_r
      nrm_l = nr_mt /  ndomain_r
      nth_l = ntheta / ndomain_t
      nph_l = nphi /   ndomain_p
!
      idomain_r = mod(my_rank,ndomain_r)
      idomain_t = (my_rank-idomain_r) / ndomain_r
      idomain_t = mod(idomain_t,ndomain_t)
      idomain_p = ( my_rank - idomain_r - idomain_t*ndomain_r )         &
     &           / (ndomain_r*ndomain_t)
      idomain_r = idomain_r + 1
      idomain_t = idomain_t + 1
      idomain_p = idomain_p + 1
!
      end subroutine set_local_sph_dimension
!
! -----------------------------------------------------------------------
!
      end module m_sph_trans_parameter
