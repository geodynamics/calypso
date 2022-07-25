!>@file   reference_sources_from_d_rj.f90
!!@brief  module reference_sources_from_d_rj
!!
!!@author H. Matsui
!!@date Programmed in June., 2022
!
!>@brief  Set reference sources from field data
!!
!!@verbatim
!!      subroutine cal_ref_sources_from_d_rj(sph, ipol, rj_fld, refs)
!!        type(sph_grids), intent(in) :: sph
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_data), intent(in) :: rj_fld
!!        type(radial_reference_field), intent(inout) :: refs
!!@endverbatim
!
      module reference_sources_from_d_rj
!
      use m_precision
      use calypso_mpi
!
      use t_spheric_parameter
      use t_phys_address
      use t_phys_data
      use t_radial_reference_field
!
!
      implicit none
!
      private :: set_reference_source_from_d_rj
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_ref_sources_from_d_rj(sph, ipol, rj_fld, refs)
!
      type(sph_grids), intent(in) :: sph
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
!
      type(radial_reference_field), intent(inout) :: refs
!
      integer(kind = kint) :: icomp, jcomp
      real(kind = kreal), allocatable :: ref_local(:)
!
!
      allocate(ref_local(0:sph%sph_rj%nidx_rj(1)))
!
      icomp = ipol%base%i_heat_source
      jcomp = refs%iref_base%i_heat_source
      call set_reference_source_from_d_rj                               &
     &   (sph%sph_rj, rj_fld%d_fld(1,icomp),                            &
     &    refs%ref_field%d_fld(1,jcomp), ref_local(0))
!
      icomp = ipol%base%i_light_source
      jcomp = refs%iref_base%i_light_source
      call set_reference_source_from_d_rj                               &
     &   (sph%sph_rj, rj_fld%d_fld(1,icomp),                            &
     &    refs%ref_field%d_fld(1,jcomp), ref_local(0))
!
      deallocate(ref_local)
!
      end subroutine cal_ref_sources_from_d_rj
!
! -----------------------------------------------------------------------
!
      subroutine set_reference_source_from_d_rj                         &
     &         (sph_rj, d_rj, ref_source, ref_local)
!
      use calypso_mpi_real
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      real(kind = kreal), intent(in) :: d_rj(1:sph_rj%nnod_rj)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: ref_source(0:sph_rj%nidx_rj(1))
      real(kind = kreal), intent(inout)                                 &
     &                   :: ref_local(0:sph_rj%nidx_rj(1))
!
      integer(kind = kint) :: k, j, i
      integer(kind = kint_gl) :: num64
!
!
      do k = 0, sph_rj%nidx_rj(1)
        ref_local(k) = 0.0d0
      end do
!
      if(sph_rj%idx_rj_degree_zero .gt. 0) then
        j = sph_rj%idx_rj_degree_zero
!$omp parallel do private(k,i)
        do k = 1, sph_rj%nidx_rj(1)
          i = 1 + (k-1) * sph_rj%istep_rj(1)                            &
     &          + (j-1) * sph_rj%istep_rj(2)
          ref_local(k) = d_rj(i)
        end do
!$omp end parallel do
!
        if(sph_rj%inod_rj_center .gt. 0) then
          i = sph_rj%inod_rj_center
          ref_local(0) = d_rj(i)
        else
          ref_local(0) = ref_local(1)
        end if
      end if
!
      num64 = sph_rj%nidx_rj(1) + 1
      call calypso_mpi_allreduce_real(ref_local(0), ref_source(0),      &
     &                                num64, MPI_SUM)
!
      end subroutine set_reference_source_from_d_rj
!
! -----------------------------------------------------------------------
!
      end module reference_sources_from_d_rj
