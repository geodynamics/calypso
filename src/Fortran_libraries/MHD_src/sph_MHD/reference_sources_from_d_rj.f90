!>@file   reference_sources_from_d_rj.f90
!!@brief  module reference_sources_from_d_rj
!!
!!@author H. Matsui
!!@date Programmed in June., 2022
!
!>@brief  Set reference sources from field data
!!
!!@verbatim
!!      subroutine set_reference_source_from_rst                        &
!!     &         (sph_rj, ipol_source, rj_fld, iref_source, ref_field)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(phys_data), intent(in) :: rj_fld
!!        integer(kind = kint), intent(in) :: ipol_source, iref_source
!!        type(phys_data), intent(inout) :: ref_field
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
      subroutine set_reference_source_from_rst                          &
     &         (sph_rj, ipol_source, rj_fld, iref_source, ref_field)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(in) :: rj_fld
      integer(kind = kint), intent(in) :: ipol_source, iref_source
!
      type(phys_data), intent(inout) :: ref_field
!
!
      if(ref_field%iflag_update(iref_source) .gt. 0) return
      if((ipol_source*iref_source) .eq. 0) return
        call set_reference_source_from_d_rj                             &
     &     (sph_rj, rj_fld%d_fld(1,ipol_source),                        &
     &      ref_field%d_fld(1,iref_source))
        ref_field%iflag_update(iref_source) = 1
!
      end subroutine set_reference_source_from_rst
!
! -----------------------------------------------------------------------
!
      subroutine set_reference_source_from_d_rj                         &
     &         (sph_rj, d_rj, ref_global)
!
      use calypso_mpi_real
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      real(kind = kreal), intent(in) :: d_rj(1:sph_rj%nnod_rj)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: ref_global(0:sph_rj%nidx_rj(1))
!
      real(kind = kreal), allocatable :: ref_local(:)
      integer(kind = kint) :: k, i
      integer(kind = kint_gl) :: num64
!
!
      allocate(ref_local(0:sph_rj%nidx_rj(1)))
      do k = 0, sph_rj%nidx_rj(1)
        ref_local(k) = 0.0d0
      end do
!
      if(sph_rj%idx_rj_degree_zero .gt. 0) then
!$omp parallel do private(k,i)
        do k = 1, sph_rj%nidx_rj(1)
          i = (k-1) * sph_rj%nidx_rj(2) + sph_rj%idx_rj_degree_zero
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
      call calypso_mpi_allreduce_real(ref_local(0), ref_global(0),      &
     &                                num64, MPI_SUM)
      deallocate(ref_local)
!
      end subroutine set_reference_source_from_d_rj
!
! -----------------------------------------------------------------------
!
      end module reference_sources_from_d_rj
