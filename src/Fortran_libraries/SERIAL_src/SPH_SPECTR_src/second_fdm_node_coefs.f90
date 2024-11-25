!>@file   second_fdm_node_coefs.f90
!!@brief  module second_fdm_node_coefs
!!
!!@author H. Matsui
!!@date Programmed in Jan, 2020
!
!>@brief  Second order FDM on nodes
!!
!!@verbatim
!!      subroutine const_second_fdm_coefs(sph_params, sph_rj, fdm_2nd)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fdm_matrices), intent(inout) :: fdm_2nd
!!      subroutine cal_second_fdm_node(i_th, kr_in, kr_out,             &
!!     &          sph_rj, fdm_2nd, d_rj, dfdr_rj)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        integer(kind = kint), intent(in) :: i_th, kr_in, kr_out
!!        real(kind = kreal), intent(in) :: d_rj(sph_rj%nnod_rj)
!!        type(fdm_matrices), intent(in) :: fdm_2nd
!!        real(kind = kreal), intent(inout) :: dfdr_rj(sph_rj%nnod_rj)
!! ----------------------------------------------------------------------
!!      Coeeficients for derivatives by 1d finite difference method
!!
!!    derivatives on node by element field
!!      dfdr_rj(k) =    fdm_2nd%fdm(1)%dmat(k,-1) * d_nod(k-1)
!!                    + fdm_2nd%fdm(1)%dmat(k, 0) * d_nod(k  )
!!                    + fdm_2nd%fdm(1)%dmat(k, 1) * d_nod(k+1)
!!      d2fdr2_rj(k) =  fdm_2nd%fdm(2)%dmat(k,-1) * d_nod(k-1)
!!                    + fdm_2nd%fdm(2)%dmat(k, 0) * d_nod(k  )
!!                    + fdm_2nd%fdm(2)%dmat(k, 1) * d_nod(k+1)
!!
!! ----------------------------------------------------------------------
!!      Work array to obtain 1d FDM
!!
!!    derivatives on node by element field
!!      dfdr_rj(k) =  fdm_2nd%wk_mat(2,1) * d_ele(k-1)
!!                  + fdm_2nd%wk_mat(2,2) * d_ele(k  )
!!                  + fdm_2nd%wk_mat(2,3) * d_ele(k+1)
!! ----------------------------------------------------------------------
!!     Numbering of node and element
!!      n_k-1  e_k   n_k  e_k+1 n_k+1 
!!     ...+-----x-----+-----x-----+.....
!!      r(k-1)       r(k)       r(k+1)     r(k+2)
!!@endverbatim
!!
!!@n @param nri    number of radial grid points
!!@n @param r(nri) radius
!
      module second_fdm_node_coefs
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_spheric_parameter
      use t_fdm_coefs
!
      implicit none
!
      private :: copy_second_fdm_node
      private :: cal_sph_vect_second_dxr_node
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine const_second_fdm_coefs(sph_params, sph_rj, fdm_2nd)
!
      use set_radius_func_noequi
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
!
      type(fdm_matrices), intent(inout) :: fdm_2nd
!
      real(kind = kreal), allocatable :: mat_fdm(:,:,:)
!
!
      call alloc_nod_fdm_matrices                                       &
     &   (sph_rj%nidx_rj(1), ione, itwo, ione, ione, fdm_2nd)
!
      allocate(mat_fdm(3,3,sph_rj%nidx_rj(1)))
      mat_fdm(1:3,1:3,1:sph_rj%nidx_rj(1)) = 0.0d0
!
      call nod_r_2nd_fdm_coefs_nonequi(sph_params%nlayer_ICB,           &
     &    sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r, mat_fdm)
!
      call copy_second_fdm_node                                         &
     &   (sph_rj%nidx_rj(1), mat_fdm, fdm_2nd%fdm)
      deallocate(mat_fdm)
!
      if(iflag_debug .gt. 0) then
        write(*,*) 'check Second order FDM'
        call check_fdm_coefs                                            &
     &     (sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r, fdm_2nd)
      end if
!
      end subroutine const_second_fdm_coefs
!
! -----------------------------------------------------------------------
!
      subroutine cal_second_fdm_node(i_th, kr_in, kr_out,               &
     &          sph_rj, fdm_2nd, d_rj, dfdr_rj)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer(kind = kint), intent(in) :: i_th, kr_in, kr_out
      real(kind = kreal), intent(in) :: d_rj(sph_rj%nnod_rj)
      type(fdm_matrices), intent(in) :: fdm_2nd
!
      real(kind = kreal), intent(inout) :: dfdr_rj(sph_rj%nnod_rj)
!
!
      call cal_sph_vect_second_dxr_node(kr_in, kr_out, sph_rj,          &
     &    fdm_2nd%fdm(i_th), d_rj, dfdr_rj)
!
      end subroutine cal_second_fdm_node
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_second_fdm_node(nri, mat_fdm, fdm)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: mat_fdm(3,3,nri)
      type(fdm_matrix), intent(inout) :: fdm(2)
!
      integer(kind= kint) :: i, k
!
!
!$omp parallel private(i)
      do i = 1, 2
!$omp do private (k)
        do k = 1, nri
          fdm(i)%dmat(k,-1) = mat_fdm(i+1,3,k)
          fdm(i)%dmat(k, 0) = mat_fdm(i+1,1,k)
          fdm(i)%dmat(k, 1) = mat_fdm(i+1,2,k)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine copy_second_fdm_node
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sph_vect_second_dxr_node(kr_in, kr_out, sph_rj,    &
     &                                        fdm, d_rj, dfdr_rj)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrix), intent(in) :: fdm
      integer(kind = kint), intent(in) :: kr_in, kr_out
      real(kind = kreal), intent(in) :: d_rj(sph_rj%nnod_rj)
!
      real(kind = kreal), intent(inout) :: dfdr_rj(sph_rj%nnod_rj)
!
      integer(kind = kint) :: inod, i_n1, i_p1, j, k
      integer(kind = kint) :: ist, ied
!
!
      ist = (kr_in-1) * sph_rj%nidx_rj(2) + 1
      ied = kr_out * sph_rj%nidx_rj(2)
!$omp parallel do private(inod,i_n1,i_p1,j,k)
      do inod = ist, ied
        i_n1 = inod - sph_rj%nidx_rj(2)
        i_p1 = inod + sph_rj%nidx_rj(2)
        j = mod((inod-1),sph_rj%nidx_rj(2)) + 1
        k = 1 + (inod- j) / sph_rj%nidx_rj(2)
!
        dfdr_rj(inod) =  fdm%dmat(k,-1) * d_rj(i_n1)                    &
     &                 + fdm%dmat(k, 0) * d_rj(inod)                    &
     &                 + fdm%dmat(k, 1) * d_rj(i_p1)
      end do
!$omp end parallel do
!
      end subroutine cal_sph_vect_second_dxr_node
!
! -----------------------------------------------------------------------
!
      end module second_fdm_node_coefs
