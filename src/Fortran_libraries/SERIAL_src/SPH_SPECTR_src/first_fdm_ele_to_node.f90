!>@file   first_fdm_ele_to_node.f90
!!@brief  module first_fdm_ele_to_node
!!
!!@author H. Matsui
!!@date Programmed in Jan, 2010
!
!>@brief  First order FDM from element to node
!!
!!@verbatim
!!      subroutine const_first_fdm_ele_to_node(sph_rj, fdm_1st_nod)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fdm_matrices), intent(inout) :: fdm_1st_nod
!!      subroutine cal_first_fdm_ele_to_node(i_th, kr_in, kr_out,       &
!!     &          sph_rj, fdm_1st_nod, dele_rj, dnod_dr)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        integer(kind = kint), intent(in) :: i_th, kr_in, kr_out
!!        real(kind = kreal), intent(in) :: dele_rj(sph_rj%nnod_rj)
!!        type(fdm_matrices), intent(in) :: fdm_1st_nod
!!        real(kind = kreal), intent(inout) :: dnod_dr(sph_rj%nnod_rj)
!! ----------------------------------------------------------------------
!!      Coeeficients for derivatives by 1d finite difference method
!!
!!     define of elemental field
!!       r_ele(k) = half *(r_nod(k-1) + r_nod(k))
!!       d_ele(k) = half *(d_nod(k-1) + d_nod(k))
!!
!!    derivatives on node by element field
!!      dfdr_nod(k) =    fdm_1st_nod%fdm(1)%dmat(k,0) * d_ele(k  )
!!                     + fdm_1st_nod%fdm(1)%dmat(k,1) * d_ele(k+1)
!!
!!    fdm_1st_nod%fdm(1)%dmat = d1nod_mat_fdm_2e
!!
!! ----------------------------------------------------------------------
!!      Work array to obtain 1d FDM
!!
!!    derivatives on node by element field
!!      dfdr_nod(k) = fdm_1st_nod%wk_mat(2,1) * d_ele(k)
!!                  + fdm_1st_nod%wk_mat(2,2) * d_ele(k+1)
!! ----------------------------------------------------------------------
!!     Numbering of node and element
!!      n_k-1 e_k  n_k e_k+1 n_k+1
!!        +----x----+----x----+---.....
!!     r(k-1)      r(k)     r(k+1)
!!@endverbatim
!!
!!@n @param nri    number of radial grid points
!!@n @param r(nri) radius
!
      module first_fdm_ele_to_node
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_spheric_parameter
      use t_fdm_coefs
!
      implicit none
!
      private :: set_first_fdm_ele_to_node, copy_first_fdm_ele_to_node
      private :: cal_sph_vect_dr_nod_1
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine const_first_fdm_ele_to_node(sph_rj, fdm_1st_nod)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
!
      type(fdm_matrices), intent(inout) :: fdm_1st_nod
!
      real(kind = kreal), allocatable :: mat_fdm(:,:,:)
!
!
      call alloc_nod_fdm_matrices                                       &
     &   (sph_rj%nidx_rj(1), izero, ione, izero, ione, fdm_1st_nod)
!
      allocate(mat_fdm(2,2,sph_rj%nidx_rj(1)))
      mat_fdm(1:2,1:2,1:sph_rj%nidx_rj(1)) = 0.0d0
!
      call set_first_fdm_ele_to_node                                    &
     &   (sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r, mat_fdm)
!
      call copy_first_fdm_ele_to_node                                   &
     &   (sph_rj%nidx_rj(1), mat_fdm, fdm_1st_nod%fdm)
      deallocate(mat_fdm)
!
      if(iflag_debug .gt. 0) then
          write(*,*) 'check First order FDM from element'
        call check_fdm_coefs                                            &
     &     (sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r, fdm_1st_nod)
      end if
!
      end subroutine const_first_fdm_ele_to_node
!
! -----------------------------------------------------------------------
!
      subroutine cal_first_fdm_ele_to_node(i_th, kr_in, kr_out,         &
     &          sph_rj, fdm_1st_nod, dele_rj, dnod_dr)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer(kind = kint), intent(in) :: i_th, kr_in, kr_out
      real(kind = kreal), intent(in) :: dele_rj(sph_rj%nnod_rj)
      type(fdm_matrices), intent(in) :: fdm_1st_nod
!
      real(kind = kreal), intent(inout) :: dnod_dr(sph_rj%nnod_rj)
!
      call cal_sph_vect_dr_nod_1(kr_in, kr_out, sph_rj,                 &
     &    fdm_1st_nod%fdm(i_th), dele_rj, dnod_dr)
!
      end subroutine cal_first_fdm_ele_to_node
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_first_fdm_ele_to_node(nri, r, mat_fdm)
!
      use cal_inverse_small_matrix
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: r(nri)
      real(kind = kreal), intent(inout) :: mat_fdm(2,2,nri)
!
      integer(kind = kint) :: kr, ierr
!
      real(kind = kreal) :: mat_taylor_2(2,2)
!
!
      do kr = 1, nri
        mat_taylor_2(1,1) = one
        if (kr .eq. 1) then
          mat_taylor_2(1,2) = - half * r(1)
        else
          mat_taylor_2(1,2) = - half * (r(kr) - r(kr-1))
        end if
!
        mat_taylor_2(2,1) = one
        if (kr .eq. nri) then
          mat_taylor_2(2,2) = half * (r(kr) - r(kr-1))
        else
          mat_taylor_2(2,2) = half * (r(kr+1) - r(kr))
        end if
!
        call cal_inverse_22_matrix                                      &
     &     (mat_taylor_2, mat_fdm(1,1,kr), ierr)
      end do
!
      end subroutine set_first_fdm_ele_to_node
!
! -----------------------------------------------------------------------
!
      subroutine copy_first_fdm_ele_to_node(nri, mat_fdm, fdm)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: mat_fdm(2,2,nri)
      type(fdm_matrix), intent(inout) :: fdm(0:1)
!
      integer(kind= kint) :: k
!
!
!$omp parallel do private (k)
      do k = 1, nri-1
        fdm(0)%dmat(k, 0) = mat_fdm(1,1,k)
        fdm(0)%dmat(k, 1) = mat_fdm(1,2,k)
!
        fdm(1)%dmat(k, 0) = mat_fdm(2,1,k)
        fdm(1)%dmat(k, 1) = mat_fdm(2,2,k)
      end do
!$omp end parallel do
!
      end subroutine copy_first_fdm_ele_to_node
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sph_vect_dr_nod_1(kr_in, kr_out, sph_rj,           &
     &                                 fdm1, dele_rj, dnod_dr)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrix), intent(in) :: fdm1
      integer(kind = kint), intent(in) :: kr_in, kr_out
      real(kind = kreal), intent(in) :: dele_rj(sph_rj%nnod_rj)
!
      real(kind = kreal), intent(inout) :: dnod_dr(sph_rj%nnod_rj)
!
      integer(kind = kint) :: inod, i_p1, j, k
      integer(kind = kint) :: ist, ied
!
!
      ist = kr_in * sph_rj%nidx_rj(2) + 1
      ied = kr_out * sph_rj%nidx_rj(2)
!$omp parallel do private(inod,i_p1,j,k)
      do inod = ist, ied
        i_p1 = inod + sph_rj%nidx_rj(2)
        j = mod((inod-1),sph_rj%nidx_rj(2)) + 1
        k = 1 + (inod- j) / sph_rj%nidx_rj(2)
!
        dnod_dr(inod) =  fdm1%dmat(k, 0) * dele_rj(inod)                &
     &                 + fdm1%dmat(k, 1) * dele_rj(i_p1)
      end do
!$omp end parallel do
!
      end subroutine cal_sph_vect_dr_nod_1
!
! -----------------------------------------------------------------------
!
      end module first_fdm_ele_to_node
