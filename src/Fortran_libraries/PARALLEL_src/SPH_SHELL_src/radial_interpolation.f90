!radial_interpolation.f90
!     module radial_interpolation
!
!      Written by H. Matsui on Sep., 2011
!
!      subroutine deallocate_original_sph_data
!
!!      subroutine const_radial_itp_table                               &
!!     &         (nri_org, r_org, nri_new, radius_1d_rj_r,              &
!!     &          kr_inside, kr_outside, k_inter, rcoef_inter)
!!      subroutine r_interpolate_sph_vector(i_fld, nidx_rj, nnod_rj,    &
!!     &          num_phys_rj, ntot_phys_rj, istack_phys_comp_rj,       &
!!     &          kr_inside, kr_outside, nri_org, k_inter, rcoef_inter, &
!!     &          n_rj_org, d_rj_org, d_rj)
!!
!!      subroutine set_org_rj_phys_data_from_IO                         &
!!     &          (j_fld, fld_IO, n_rj_org, d_rj_org)
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
      subroutine const_radial_itp_table                                 &
     &         (nri_org, r_org, nri_new, radius_1d_rj_r,                &
     &          kr_inside, kr_outside, k_inter, rcoef_inter)
!
      integer(kind = kint), intent(in) :: nri_new
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nri_new)
      integer(kind = kint), intent(in) :: nri_org
      real(kind = kreal), intent(in) :: r_org(nri_org)
!
      integer(kind = kint), intent(inout) :: kr_inside, kr_outside
      integer(kind = kint), intent(inout) :: k_inter(nri_org,2)
      real (kind=kreal), intent(inout) :: rcoef_inter(nri_org,2)
!
      integer(kind = kint) :: kst, k1, k2
!
!
      kst = 1
      do k1 = 1, nri_new
        if(radius_1d_rj_r(k1) .lt. r_org(1)) then
          kr_inside = k1+1
          k_inter(k1,1) = 0
          k_inter(k1,2) = 0
          rcoef_inter(k1,1) = zero
          rcoef_inter(k1,2) = one
        else if(radius_1d_rj_r(k1) .gt. r_org(nri_org)) then
          kr_outside = k1-1
          exit
        else
          do k2 = kst, nri_org-1
            if(radius_1d_rj_r(k1).ge.r_org(k2)                          &
     &        .and. radius_1d_rj_r(k1).le.r_org(k2+1)) then
              k_inter(k1,1) = k2
              k_inter(k1,2) = k2 + 1
              rcoef_inter(k1,1) = (radius_1d_rj_r(k1) - r_org(k2))      &
     &                           / (r_org(k2+1) - r_org(k2))
              rcoef_inter(k1,2) = (r_org(k2+1) - radius_1d_rj_r(k1))    &
     &                           / (r_org(k2+1) - r_org(k2))
              kst = k2
              exit
            end if
          end do
        end if
      end do
      do k1 = kr_outside+1, nri_new
         k_inter(k1,1) = nri_org+1
         k_inter(k1,2) = nri_org+1
         rcoef_inter(k1,1) = one
         rcoef_inter(k1,2) = zero
      end do
!
      end subroutine const_radial_itp_table
!
!  -------------------------------------------------------------------
!
      subroutine r_interpolate_sph_vector(i_fld, nidx_rj, nnod_rj,      &
     &          num_phys_rj, ntot_phys_rj, istack_phys_comp_rj,         &
     &          kr_inside, kr_outside, nri_org, k_inter, rcoef_inter,   &
     &          n_rj_org, d_rj_org, d_rj)
!
      integer(kind = kint), intent(in) :: i_fld
      integer(kind = kint), intent(in) :: nnod_rj
      integer(kind = kint), intent(in) :: nidx_rj(2)
      integer(kind = kint), intent(in) :: num_phys_rj, ntot_phys_rj
      integer(kind = kint), intent(in)                                  &
     &                  :: istack_phys_comp_rj(0:num_phys_rj)
!
      integer(kind = kint), intent(in) :: kr_inside, kr_outside
      integer(kind = kint), intent(in) :: nri_org, n_rj_org
      integer(kind = kint), intent(in) :: k_inter(nri_org,2)
      real (kind=kreal), intent(in) :: rcoef_inter(nri_org,2)
      real (kind=kreal), intent(in) :: d_rj_org(n_rj_org,6)
!
      real (kind=kreal), intent(inout) :: d_rj(nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) :: ncomp, i_comp, ist, ied, inod
      integer(kind = kint) :: k, j, nd, i1, i2
!
!
      ncomp = istack_phys_comp_rj(i_fld) - istack_phys_comp_rj(i_fld-1)
!$omp parallel private(nd,i_comp,ist,ied,inod)
      do nd = 1, ncomp
        i_comp = nd + istack_phys_comp_rj(i_fld-1)
        ist = 1
        ied = (kr_inside-1) * nidx_rj(2)
!$omp do private(inod)
        do inod = 1, kr_inside-1
            d_rj(inod,i_comp) = zero
        end do
!$omp end do nowait
!
!$omp do private(inod,k,j,i1,i2)
        do k = kr_inside, kr_outside
          do j = 1, nidx_rj(2)
            inod = j + (k-1) * nidx_rj(2)
            i1 = j + (k_inter(k,1)-1) * nidx_rj(2)
            i2 = j + (k_inter(k,2)-1) * nidx_rj(2)
            d_rj(i_comp,i_comp) = rcoef_inter(k,1)*d_rj_org(i1,nd)      &
     &                         +  rcoef_inter(k,1)*d_rj_org(i2,nd)
          end do
        end do
!$omp end do nowait
!
        ist = 1 + kr_outside * nidx_rj(2)
        ied = nidx_rj(1) * nidx_rj(2)
!$omp do private(inod)
        do inod = 1, kr_inside-1
          d_rj(inod,i_comp) = zero
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine r_interpolate_sph_vector
!
!  -------------------------------------------------------------------
! -------------------------------------------------------------------
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
        d_rj_org(1:n_rj_org,1) = fld_IO%d_IO(1:n_rj_org,jst+1)
        d_rj_org(1:n_rj_org,2) = fld_IO%d_IO(1:n_rj_org,jst+3)
        d_rj_org(1:n_rj_org,3) = fld_IO%d_IO(1:n_rj_org,jst+2)
      else
        do nd = 1, fld_IO%num_comp_IO(j_fld)
          d_rj_org(1:n_rj_org,nd) = fld_IO%d_IO(1:n_rj_org,jst+nd)
        end do
      end if
!
      end subroutine set_org_rj_phys_data_from_IO
!
! -------------------------------------------------------------------
!
      end module radial_interpolation
