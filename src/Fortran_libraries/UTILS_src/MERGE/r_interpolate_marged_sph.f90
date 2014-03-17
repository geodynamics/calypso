!>@file   r_interpolate_marged_sph.f90
!!@brief  module r_interpolate_marged_sph
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2014
!
!>@brief Radial interpolation for assemble program
!!
!!@verbatim
!!      subroutine deallocate_radial_itp_tbl
!!      subroutine sph_radial_interpolation_coef(nri_org, r_org)
!!      subroutine itp_rj_merged_phys_from_IO(nnod_org,                 &
!!     &          nri_org, jmax_org, idx_gl_1d_j_org, d_rj_IO)
!!      subroutine extend_potential_magne
!!      subroutine extend_inner_core_temp
!!      subroutine extend_inner_core_light
!!@endverbatim
!!
!!@param   nnod_org  Number of spectr data for original data
!!@param   nri_org   Number of radial grid for original data
!!@param   jmax_org  Number of harmonics modes for original data
!!@param   r_org     Position of radial grid for original data
!!@param   idx_gl_1d_j_org(jmax_org,3)
!!                  List of spherical harmonics modes for original data
!!@param   d_rj_IO   Read harmonics data data
!
      module r_interpolate_marged_sph
!
      use m_precision
      use m_spheric_parameter
!
      implicit none
!
!>      Integer flag if radial grid is same
      integer(kind = kint) :: iflag_same_rgrid =  1
!
!>      Number of radial grids for new spectr data
      integer(kind = kint) :: nri_old2new =  0
!>      Inner radial grid ID for interpolation
      integer(kind = kint), allocatable :: k_old2new_in(:)
!>      Outer radial grid ID for interpolation
      integer(kind = kint), allocatable :: k_old2new_out(:)
!>      Coefficient for Inner grid data for interpolation
      real(kind = kreal), allocatable :: coef_old2new_in(:)
!
!>      Innermost new radial ID within the original domain
      integer(kind = kint) :: kr_inner_domain =  0
!>      Outmost new radial ID within the original domain
      integer(kind = kint) :: kr_outer_domain = 0
!
      private :: nri_old2new, k_old2new_in, k_old2new_out
      private :: coef_old2new_in
      private :: allocate_radial_itp_tbl, extend_inner_core_scalar
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_radial_itp_tbl
!
!
      nri_old2new = nidx_rj(1)
      allocate(k_old2new_in(nri_old2new))
      allocate(k_old2new_out(nri_old2new))
      allocate(coef_old2new_in(nri_old2new))
!
      k_old2new_in =    0
      k_old2new_out =   0
      coef_old2new_in = 0.0d0
!
      end subroutine allocate_radial_itp_tbl
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_radial_itp_tbl
!
!
      deallocate(k_old2new_in, k_old2new_out)
      deallocate(coef_old2new_in)
!
      end subroutine deallocate_radial_itp_tbl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sph_radial_interpolation_coef(nri_org, r_org)
!
      integer(kind = kint), intent(in) :: nri_org
      real(kind = kreal), intent(in) :: r_org(nri_org)
!
      integer(kind = kint) :: k, kr_org
      real(kind = kreal) :: r_in, r_out
!
!
      iflag_same_rgrid = 1
      if(nri_org .ne. nidx_rj(1)) then
        iflag_same_rgrid =  0
      else
        do k = 1, nidx_rj(1)
          if(abs(radius_1d_rj_r(k) - r_org(k)) .gt. 1.0E-12) then
            iflag_same_rgrid = 0
            exit
          end if
        end do
      end if
!
      write(*,*) 'iflag_same_rgrid', iflag_same_rgrid
      if(iflag_same_rgrid .ne. 0) return
!
      call allocate_radial_itp_tbl
!
      do k = 1, nidx_rj(1)
        if(radius_1d_rj_r(k) .lt. r_org(1)) then
          k_old2new_in(k) =    0
          k_old2new_out(k) =   1
          coef_old2new_in(k) = -1.0d0
        else if(radius_1d_rj_r(k) .eq. r_org(1)) then
          k_old2new_in(k) =    1
          k_old2new_out(k) =   2
          coef_old2new_in(k) = 1.0d0
        else if(radius_1d_rj_r(k) .eq. r_org(nri_org)) then
          k_old2new_in(k) =    nri_org - 1
          k_old2new_out(k) =   nri_org
          coef_old2new_in(k) = 0.0d0
        else if(radius_1d_rj_r(k) .gt. r_org(nri_org)) then
          k_old2new_in(k) =    nri_org
          k_old2new_out(k) =   nri_org + 1
          coef_old2new_in(k) = -1.0d0
        else
          do kr_org = 1, nri_org
            r_in =  r_org(kr_org-1)
            r_out = r_org(kr_org  )
            if(      radius_1d_rj_r(k) .ge. r_in                        &
     &         .and. radius_1d_rj_r(k) .lt. r_out) then
              k_old2new_in(k) =  kr_org - 1
              k_old2new_out(k) = kr_org
              coef_old2new_in(k) = (r_out - radius_1d_rj_r(k))          &
     &                            / (r_out - r_in)
              exit
            end if
          end do
        end if
      end do
!
      kr_inner_domain = 0
      do k = nidx_rj(1), 1, -1
        if(radius_1d_rj_r(k) .lt. r_org(1)) then
          kr_inner_domain = k + 1
          exit
        end if
      end do
      kr_outer_domain = nidx_rj(1)
      do k = 1, nidx_rj(1)
        if(radius_1d_rj_r(k) .gt. r_org(nri_org)) then
          kr_outer_domain = k - 1
          exit
        end if
      end do
!
      write(*,*) 'kr_inner_domain', kr_inner_domain
      write(*,*) 'kr_outer_domain', kr_outer_domain
!      do k = 1, nidx_rj(1)
!        write(*,'(i5,1pe16.8,2i5,1p3e16.8)') k, radius_1d_rj_r(k),    &
!     &         k_old2new_in(k), k_old2new_out(k),                     &
!     &         r_org(k_old2new_in(k)),  r_org(k_old2new_out(k)),      &
!     &         coef_old2new_in(k)
!      end do
!
      end subroutine sph_radial_interpolation_coef
!
! -----------------------------------------------------------------------
!
      subroutine itp_rj_merged_phys_from_IO(nnod_org, jmax_org,         &
     &          idx_gl_1d_j_org, d_rj_IO)
!
      use m_sph_spectr_data
!
      integer(kind = kint), intent(in) :: nnod_org, jmax_org
      integer(kind = kint), intent(in) :: idx_gl_1d_j_org(jmax_org,3)
      real(kind = kreal), intent(in) :: d_rj_IO(nnod_org,ntot_phys_rj)
!
      integer(kind = kint) :: nd, j, j_gl, kr
      integer(kind = kint) :: inod_gl, inod_in, inod_out
!
!
!!$omp parallel private(nd)
      do nd = 1, ntot_phys_rj
!!$omp do private(j,j_gl,kr,inod_in,inod_out,inod_gl)
        do j = 1, jmax_org
          j_gl = idx_gl_1d_j_org(j,1)
!
          if(j_gl .lt. nidx_rj(2)) then
!
            do kr = kr_inner_domain, kr_outer_domain
              inod_gl = 1 + j_gl + (kr - 1) * nidx_rj(2)
              inod_in =  j + (k_old2new_in(kr) - 1) *  jmax_org
              inod_out = j + (k_old2new_out(kr) - 1) * jmax_org
              d_rj(inod_gl,nd)                                          &
     &           = coef_old2new_in(kr) * d_rj_IO(inod_in,nd)            &
     &          + (1.0d0 - coef_old2new_in(kr)) * d_rj_IO(inod_out,nd)
            end do
          end if
        end do
!!$omp end do
      end do
!!$omp end parallel
!
      end subroutine itp_rj_merged_phys_from_IO
!
! -----------------------------------------------------------------------
!
      subroutine extend_potential_magne
!
      use extend_potential_field
!
      use m_phys_labels
      use m_sph_spectr_data
!
      integer(kind = kint) :: is_magne
      integer(kind = kint) :: i
!
!
      is_magne = 0
      do i = 1, num_phys_rj
        if(phys_name_rj(i) .eq. fhd_magne) then
          is_magne = istack_phys_comp_rj(i-1) + 1
          exit
        end if
      end do
      if(is_magne .eq. 0) return
!
      if(kr_outer_domain .lt. nidx_rj(1)) then
        call ext_outside_potential(is_magne, kr_outer_domain)
      end if
      if(kr_inner_domain .gt. 1) then
        call ext_inside_potential(is_magne, kr_inner_domain)
      end if
!
      end subroutine extend_potential_magne
!
! -----------------------------------------------------------------------
!
      subroutine extend_inner_core_temp
!
      use m_phys_labels
!
!
      call extend_inner_core_scalar(fhd_temp)
!
      end subroutine extend_inner_core_temp
!
! -----------------------------------------------------------------------
!
      subroutine extend_inner_core_light
!
      use m_phys_labels
!
!
      call extend_inner_core_scalar(fhd_light)
!
      end subroutine extend_inner_core_light
!
! -----------------------------------------------------------------------
!
      subroutine extend_inner_core_scalar(field_name)
!
      use extend_potential_field
!
      use m_sph_spectr_data
!
      character(len = kchara), intent(in) :: field_name
!
      integer(kind = kint) :: is_field
      integer(kind = kint) :: i
!
!
      is_field = 0
      do i = 1, num_phys_rj
        if(phys_name_rj(i) .eq. field_name) then
          is_field = istack_phys_comp_rj(i-1) + 1
          exit
        end if
      end do
      if(is_field .eq. 0) return
!
      if(kr_inner_domain .gt. 1) then
        call ext_inside_scalar(is_field, kr_inner_domain)
      end if
!
      end subroutine extend_inner_core_scalar
!
! -----------------------------------------------------------------------
!
      end module r_interpolate_marged_sph
