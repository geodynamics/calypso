!>@file   third_fdm_node_to_ele.f90
!!@brief  module third_fdm_node_to_ele
!!
!!@author H. Matsui
!!@date Programmed in Jan, 2020
!
!>@brief  Third order FDM from node to element
!!
!!@verbatim
!!      subroutine const_third_fdm_node_to_ele(sph_rj, fdm_3rd_ele)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(fdm_matrices), intent(inout) :: fdm_3rd_ele
!!      subroutine cal_third_fdm_node_to_ele(i_th, kr_in, kr_out,       &
!!     &          sph_rj, fdm_3rd_ele, d_rj, dele_dr)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        integer(kind = kint), intent(in) :: i_th, kr_in, kr_out
!!        real(kind = kreal), intent(in) :: d_rj(sph_rj%nnod_rj)
!!        type(fdm_matrices), intent(in) :: fdm_3rd_ele
!!        real(kind = kreal), intent(inout) :: dele_dr(sph_rj%nnod_rj)
!! ----------------------------------------------------------------------
!!      Coeeficients for derivatives by 1d finite difference method
!!
!!     define of elemental field
!!       r_ele(k) = half *(r_nod(k-1) + r_nod(k))
!!       d_ele(k) = half *(d_nod(k-1) + d_nod(k))
!!
!!    derivatives on node by element field
!!      dfdr_ele(k) =    fdm_3rd_ele%fdm(1)%dmat(k,-2) * d_nod(k-2)
!!                     + fdm_3rd_ele%fdm(1)%dmat(k,-1) * d_nod(k-1)
!!                     + fdm_3rd_ele%fdm(1)%dmat(k, 0) * d_nod(k  )
!!                     + fdm_3rd_ele%fdm(1)%dmat(k, 1) * d_nod(k+1)
!!      d2fdr2_ele(k) =  fdm_3rd_ele%fdm(2)%dmat(k,-2) * d_nod(k-2)
!!                     + fdm_3rd_ele%fdm(2)%dmat(k,-1) * d_nod(k-1)
!!                     + fdm_3rd_ele%fdm(2)%dmat(k, 0) * d_nod(k  )
!!                     + fdm_3rd_ele%fdm(2)%dmat(k, 1) * d_nod(k+1)
!!      d3fdr3_ele(k) =  fdm_3rd_ele%fdm(3)%dmat(k,-2) * d_nod(k-2)
!!                     + fdm_3rd_ele%fdm(3)%dmat(k,-1) * d_nod(k-1)
!!                     + fdm_3rd_ele%fdm(3)%dmat(k, 0) * d_nod(k  )
!!                     + fdm_3rd_ele%fdm(3)%dmat(k, 1) * d_nod(k+1)
!!
!!    fdm_3rd_ele%fdm(1)%dmat = d1nod_mat_fdm_2e
!!
!! ----------------------------------------------------------------------
!!      Work array to obtain 1d FDM
!!
!!    derivatives on node by element field
!!      dfdr_ele(k) = fdm_3rd_ele%wk_mat(2,1) * d_ele(k-1)
!!                  + fdm_3rd_ele%wk_mat(2,2) * d_ele(k  )
!! ----------------------------------------------------------------------
!!     Numbering of node and element
!!      n_k-2 e_k-1 n_k-1  e_k   n_k  e_k+1 n_k+1 
!!     ...+-----x-----+-----x-----+-----x-----+----.....
!!     r(k-2)       r(k-1)       r(k)       r(k+1)   
!!@endverbatim
!!
!!@n @param nri    number of radial grid points
!!@n @param r(nri) radius
!
      module third_fdm_node_to_ele
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_spheric_parameter
      use t_fdm_coefs
!
      implicit none
!
      private :: set_third_fdm_node_to_ele, copy_third_fdm_node_to_ele
      private :: cal_sph_vect_dxr_ele
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine const_third_fdm_node_to_ele(sph_rj, fdm_3rd_ele)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
!
      type(fdm_matrices), intent(inout) :: fdm_3rd_ele
!
      real(kind = kreal), allocatable :: mat_fdm(:,:,:)
!
!
      call alloc_nod_fdm_matrices                                       &
     &   (sph_rj%nidx_rj(1), izero, ithree, itwo, ione, fdm_3rd_ele)
!
      allocate(mat_fdm(4,4,sph_rj%nidx_rj(1)))
      mat_fdm(1:4,1:4,1:sph_rj%nidx_rj(1)) = 0.0d0
!
      call set_third_fdm_node_to_ele                                    &
     &   (sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r, mat_fdm)
!
      call copy_third_fdm_node_to_ele                                   &
     &   (sph_rj%nidx_rj(1), mat_fdm, fdm_3rd_ele%fdm)
      deallocate(mat_fdm)
!
      if(iflag_debug .gt. 0) then
          write(*,*) 'check Third order FDM on element'
        call check_fdm_coefs                                            &
     &     (sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r, fdm_3rd_ele)
      end if
!
      end subroutine const_third_fdm_node_to_ele
!
! -----------------------------------------------------------------------
!
      subroutine cal_third_fdm_node_to_ele(i_th, kr_in, kr_out,         &
     &          sph_rj, fdm_3rd_ele, d_rj, dele_dr)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      integer(kind = kint), intent(in) :: i_th, kr_in, kr_out
      real(kind = kreal), intent(in) :: d_rj(sph_rj%nnod_rj)
      type(fdm_matrices), intent(in) :: fdm_3rd_ele
!
      real(kind = kreal), intent(inout) :: dele_dr(sph_rj%nnod_rj)
!
!
      call cal_sph_vect_dxr_ele(kr_in, kr_out, sph_rj,                  &
     &    fdm_3rd_ele%fdm(i_th), d_rj, dele_dr)
!
      end subroutine cal_third_fdm_node_to_ele
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_third_fdm_node_to_ele(nri, r, mat_fdm)
!
      use cal_inverse_small_matrix
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: r(nri)
      real(kind = kreal), intent(inout) :: mat_fdm(4,4,nri)
!
      integer(kind = kint) :: kr, ierr
!
      real(kind = kreal) :: mat_taylor_4(4,4)
      real(kind = kreal) :: dr_n2, dr_1, dr_p2
      real(kind = kreal), parameter :: asix = 1.0d0 / 6.0d0
!
!
      do kr = 1, nri
        if (kr .eq. 1) then
          dr_n2 = r(1)
          dr_1 =  half * r(1)
          dr_p2 = r(2) - half * r(1)
        else if(kr .eq. 2) then
          dr_n2 = half*(r(2) + r(1))
          dr_1 =  half*(r(2) - r(1))
          dr_p2 = r(3) - half*(r(2) + r(1))
        else if(kr .eq. nri) then
          dr_n2 = half*(r(nri) + r(nri-1)) - r(nri-2)
          dr_1 =  half*(r(nri) - r(nri-1))
          dr_p2 = r(nri) - r(nri-1)
        else
          dr_n2 = half*(r(kr) + r(kr-1)) - r(kr-2)
          dr_1 =  half*(r(kr) - r(kr-1))
          dr_p2 = r(kr+1) - half*(r(kr) + r(kr-1))
        end if
!
        mat_taylor_4(1,1) =  one
        mat_taylor_4(1,2) = -dr_n2
        mat_taylor_4(1,3) =  half * dr_n2**2
        mat_taylor_4(1,4) = -asix * dr_n2**3
!
        mat_taylor_4(2,1) =  one
        mat_taylor_4(2,2) = -dr_1
        mat_taylor_4(2,3) =  half * dr_1**2
        mat_taylor_4(2,4) = -asix * dr_1**3
!
        mat_taylor_4(3,1) =  one
        mat_taylor_4(3,2) =  dr_1
        mat_taylor_4(3,3) =  half * dr_1**2
        mat_taylor_4(3,4) =  asix * dr_1**3
!
        mat_taylor_4(4,1) =  one
        mat_taylor_4(4,2) =  dr_p2
        mat_taylor_4(4,3) =  half * dr_p2**2
        mat_taylor_4(4,4) =  asix * dr_p2**3
!
        call cal_inverse_44_matrix                                      &
     &     (mat_taylor_4, mat_fdm(1,1,kr), ierr)
      end do
!
      end subroutine set_third_fdm_node_to_ele
!
! -----------------------------------------------------------------------
!
      subroutine copy_third_fdm_node_to_ele(nri, mat_fdm, fdm)
!
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: mat_fdm(4,4,nri)
      type(fdm_matrix), intent(inout) :: fdm(0:3)
!
      integer(kind= kint) :: i, k
!
!
!$omp parallel do private (i,k)
      do i = 0, 3
        do k = 1, nri-1
          fdm(i)%dmat(k,-2) = mat_fdm(i+1,1,k)
          fdm(i)%dmat(k,-1) = mat_fdm(i+1,2,k)
          fdm(i)%dmat(k, 0) = mat_fdm(i+1,3,k)
          fdm(i)%dmat(k, 1) = mat_fdm(i+1,4,k)
        end do
      end do
!$omp end parallel do
!
      end subroutine copy_third_fdm_node_to_ele
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sph_vect_dxr_ele(kr_in, kr_out, sph_rj,            &
     &                                fdm, d_rj, dele_dr)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(fdm_matrix), intent(in) :: fdm
      integer(kind = kint), intent(in) :: kr_in, kr_out
      real(kind = kreal), intent(in) :: d_rj(sph_rj%nnod_rj)
!
      real(kind = kreal), intent(inout) :: dele_dr(sph_rj%nnod_rj)
!
      integer(kind = kint) :: inod, i_n2, i_n1, i_p1, j, k
      integer(kind = kint) :: ist, ied
!
!
      ist = kr_in * sph_rj%nidx_rj(2) + 1
      ied = kr_out * sph_rj%nidx_rj(2)
!$omp parallel do private(inod,i_n2,i_n1,i_p1,j,k)
      do inod = ist, ied
        i_n1 = inod - sph_rj%nidx_rj(2)
        i_n2 = i_n1 - sph_rj%nidx_rj(2)
        i_p1 = inod + sph_rj%nidx_rj(2)
        j = mod((inod-1),sph_rj%nidx_rj(2)) + 1
        k = 1 + (inod- j) / sph_rj%nidx_rj(2)
!
        dele_dr(inod) =  fdm%dmat(k,-2) * d_rj(i_n2)                    &
     &                 + fdm%dmat(k,-1) * d_rj(i_n1)                    &
     &                 + fdm%dmat(k, 0) * d_rj(inod)                    &
     &                 + fdm%dmat(k, 1) * d_rj(i_p1)
      end do
!$omp end parallel do
!
      end subroutine cal_sph_vect_dxr_ele
!
! -----------------------------------------------------------------------
!
      end module third_fdm_node_to_ele
