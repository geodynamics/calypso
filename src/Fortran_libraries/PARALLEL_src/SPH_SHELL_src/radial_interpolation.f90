!>@file   radial_interpolation.f90
!!@brief  module radial_interpolation
!!
!!@author H. Matsui
!!@date Programmed in March, 2012
!
!>@brief Subroutines for radial interpolation
!!
!!@verbatim
!!      logical function check_sph_same_radial_grid                     &
!!     &               (nri_org, r_org, nri_new, r_new)
!!      subroutine cal_radial_interpolation_coef                        &
!!     &         (nri_org, r_org, nri_new, r_new,                       &
!!     &          kr_inner_source, kr_outer_source,                     &
!!     &          k_old2new_in, k_old2new_out, coef_old2new_in)
!!      subroutine r_interpolate_sph_vector                             &
!!     &         (nidx_rj, kr_inside, kr_outside, nri_new,              &
!!     &          k_old2new_in, k_old2new_out, coef_old2new_in,         &
!!     &          n_rj_org, d_rj_org, ncomp, n_point, d_rj)
!!      subroutine interpolate_radial_field                             &
!!     &         (nri_new, k_old2new_in, k_old2new_out, coef_old2new_in,&
!!     &          ncomp, n_rj_org, d_IO, d_r)
!!
!!      subroutine set_org_rj_phys_data_from_IO                         &
!!     &          (j_fld, fld_IO, n_rj_org, d_rj_org)
!!      subroutine set_org_radius_data_from_IO                          &
!!     &          (j_fld, fld_IO, n_rj_org, d_rj_org)
!!@endverbatim
!
      module radial_interpolation
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      implicit  none
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      logical function check_sph_same_radial_grid                       &
     &               (nri_org, r_org, nri_new, r_new)
!
      integer(kind = kint), intent(in) :: nri_org, nri_new
      real(kind = kreal), intent(in) :: r_org(nri_org)
      real(kind = kreal), intent(in) :: r_new(nri_org)
!
      logical :: flag
!
      integer(kind = kint) :: k
!
!
      flag = .TRUE.
      if(nri_org .ne. nri_new) then
        flag =  .FALSE.
      else
        do k = 1, nri_new
          if(abs(r_new(k) - r_org(k)) .gt. 1.0E-12) then
            flag = .FALSE.
            exit
          end if
        end do
      end if
      check_sph_same_radial_grid = flag
!
      end function check_sph_same_radial_grid
!
! -----------------------------------------------------------------------
!
      subroutine cal_radial_interpolation_coef                          &
     &         (nri_org, r_org, nri_new, r_new,                         &
     &          kr_inner_source, kr_outer_source,                       &
     &          k_old2new_in, k_old2new_out, coef_old2new_in)
!
      integer(kind = kint), intent(in) :: nri_org, nri_new
      real(kind = kreal), intent(in) :: r_org(nri_org)
      real(kind = kreal), intent(in) :: r_new(nri_org)
!
      integer(kind = kint), intent(inout) :: kr_inner_source
      integer(kind = kint), intent(inout) :: kr_outer_source
      integer(kind = kint), intent(inout) :: k_old2new_in(nri_new)
      integer(kind = kint), intent(inout) :: k_old2new_out(nri_new)
      real (kind=kreal), intent(inout) :: coef_old2new_in(nri_new)
!
      integer(kind = kint) :: k, kr_org
      real(kind = kreal) :: r_in, r_out
!
!
      do k = 1, nri_new
        if(abs(r_new(k) - r_org(1)) .lt. TINY) then
          k_old2new_in(k) =    1
          k_old2new_out(k) =   2
          coef_old2new_in(k) = 1.0d0
        else if(abs(r_new(k) - r_org(nri_org)) .lt. TINY) then
          k_old2new_in(k) =    nri_org - 1
          k_old2new_out(k) =   nri_org
          coef_old2new_in(k) = 0.0d0
        else if(r_new(k) .lt. r_org(1)) then
          k_old2new_in(k) =    0
          k_old2new_out(k) =   1
          coef_old2new_in(k) = -1.0d0
        else if(r_new(k) .gt. r_org(nri_org)) then
          k_old2new_in(k) =    nri_org
          k_old2new_out(k) =   nri_org + 1
          coef_old2new_in(k) = -1.0d0
        else
          do kr_org = 1, nri_org
            r_in =  r_org(kr_org-1)
            r_out = r_org(kr_org  )
            if(r_new(k) .ge. r_in  .and. r_new(k) .lt. r_out) then
              k_old2new_in(k) =  kr_org - 1
              k_old2new_out(k) = kr_org
              coef_old2new_in(k) = (r_out - r_new(k)) / (r_out - r_in)
              exit
            end if
          end do
        end if
      end do
!
      kr_inner_source = 1
      do k = 1, nri_new
        if(abs(r_new(k) - r_org(1)) .lt. TINY) then
          kr_inner_source = k
          exit
        end if
      end do
      kr_outer_source = nri_new
      do k = nri_new, 1, -1
        if(abs(r_new(k) - r_org(nri_org)) .lt. TINY) then
          kr_outer_source = k
          exit
        end if
      end do
!
      end subroutine cal_radial_interpolation_coef
!
! -----------------------------------------------------------------------
!
      subroutine r_interpolate_sph_vector                               &
     &         (nidx_rj, kr_inside, kr_outside, nri_new,                &
     &          k_old2new_in, k_old2new_out, coef_old2new_in,           &
     &          n_rj_org, d_rj_org, ncomp, n_point, d_rj)
!
      integer(kind = kint), intent(in) :: ncomp
      integer(kind = kint), intent(in) :: nidx_rj(2)
!
      integer(kind = kint), intent(in) :: kr_inside, kr_outside
      integer(kind = kint), intent(in) :: nri_new, n_rj_org
      integer(kind = kint), intent(in) :: k_old2new_in(nri_new)
      integer(kind = kint), intent(in) :: k_old2new_out(nri_new)
      real (kind=kreal), intent(in) :: coef_old2new_in(nri_new)
      real (kind=kreal), intent(in) :: d_rj_org(n_rj_org,6)
      integer(kind = kint), intent(in) :: n_point
!
      real (kind=kreal), intent(inout) :: d_rj(n_point,ncomp)
!
      integer(kind = kint) :: ist, ied, inod
      integer(kind = kint) :: k, j, nd, i1, i2
!
!
!$omp parallel private(nd,ist,ied,inod)
      do nd = 1, ncomp
        ist = 1
        ied = (kr_inside-1) * nidx_rj(2)
!$omp do private(inod)
        do inod = ist, ied
            d_rj(inod,nd) = zero
        end do
!$omp end do nowait
!
!$omp do private(inod,k,j,i1,i2)
        do k = kr_inside, kr_outside
          do j = 1, nidx_rj(2)
            inod = j + (k-1) * nidx_rj(2)
            i1 = j + (k_old2new_in(k)- 1) * nidx_rj(2)
            i2 = j + (k_old2new_out(k)-1) * nidx_rj(2)
            d_rj(inod,nd) = coef_old2new_in(k)*d_rj_org(i1,nd)          &
     &                   + (one - coef_old2new_in(k))*d_rj_org(i2,nd)
          end do
        end do
!$omp end do nowait
!
        ist = 1 + kr_outside * nidx_rj(2)
        ied = nidx_rj(1) * nidx_rj(2)
!$omp do private(inod)
        do inod = ist, ied
          d_rj(inod,nd) = zero
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine r_interpolate_sph_vector
!
! -------------------------------------------------------------------
!
      subroutine interpolate_radial_field                               &
     &         (nri_new, k_old2new_in, k_old2new_out, coef_old2new_in,  &
     &          ncomp, n_rj_org, d_IO, d_r)
!
      integer(kind = kint), intent(in) :: ncomp
!
      integer(kind = kint), intent(in) :: nri_new, n_rj_org
      integer(kind = kint), intent(in) :: k_old2new_in(nri_new)
      integer(kind = kint), intent(in) :: k_old2new_out(nri_new)
      real (kind=kreal), intent(in) :: coef_old2new_in(nri_new)
      real (kind=kreal), intent(in) :: d_IO(n_rj_org,ncomp)
!
      real (kind=kreal), intent(inout) :: d_r(nri_new,ncomp)
!
      integer(kind = kint) :: k, j, nd, i1, i2
!
!
!$omp parallel private(nd)
      do nd = 1, ncomp
!$omp do private(k,j,i1,i2)
        do k = 1, nri_new
          do j = 1, 1
            i1 = k_old2new_in(k)
            i2 = k_old2new_out(k)
            d_r(k,nd) = coef_old2new_in(k) * d_IO(i1,nd)                &
     &                 + (one - coef_old2new_in(k)) * d_IO(i2,nd)
          end do
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine interpolate_radial_field
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine set_org_rj_phys_data_from_IO                           &
     &          (j_fld, fld_IO, n_rj_org, d_rj_org)
!
      use t_field_data_IO
!
      integer(kind = kint), intent(in) :: j_fld
      type(field_IO), intent(in) :: fld_IO
!
      integer(kind = kint), intent(in) :: n_rj_org
      real (kind=kreal), intent(inout) :: d_rj_org(n_rj_org,6)
!
      integer(kind = kint) :: jst, nd
!
!
      jst = fld_IO%istack_comp_IO(j_fld-1)
      if(fld_IO%num_comp_IO(j_fld) .eq. 3) then
!$omp parallel workshare
        d_rj_org(1:n_rj_org,1) = fld_IO%d_IO(1:n_rj_org,jst+1)
        d_rj_org(1:n_rj_org,2) = fld_IO%d_IO(1:n_rj_org,jst+3)
        d_rj_org(1:n_rj_org,3) = fld_IO%d_IO(1:n_rj_org,jst+2)
!$omp end parallel workshare
      else
        do nd = 1, fld_IO%num_comp_IO(j_fld)
!$omp parallel workshare
          d_rj_org(1:n_rj_org,nd) = fld_IO%d_IO(1:n_rj_org,jst+nd)
!$omp end parallel workshare
        end do
      end if
!
      end subroutine set_org_rj_phys_data_from_IO
!
! -------------------------------------------------------------------
!
      subroutine set_org_radius_data_from_IO                            &
     &          (j_fld, fld_IO, n_rj_org, d_rj_org)
!
      use t_field_data_IO
!
      integer(kind = kint), intent(in) :: j_fld
      type(field_IO), intent(in) :: fld_IO
!
      integer(kind = kint), intent(in) :: n_rj_org
      real (kind=kreal), intent(inout) :: d_rj_org(n_rj_org,6)
!
      integer(kind = kint) :: jst, nd
!
!
      jst = fld_IO%istack_comp_IO(j_fld-1)
      do nd = 1, fld_IO%num_comp_IO(j_fld)
!$omp parallel workshare
        d_rj_org(1:n_rj_org,nd) = fld_IO%d_IO(1:n_rj_org,jst+nd)
!$omp end parallel workshare
      end do
!
      end subroutine set_org_radius_data_from_IO
!
! -------------------------------------------------------------------
!
      end module radial_interpolation
