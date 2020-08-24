!>@file   t_convert_from_rayleigh.f90
!!@brief  module t_convert_from_rayleigh
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2018
!
!>@brief  Main loop to assemble spectr data
!!
!!@verbatim
!!      subroutine alloc_work_rayleigh_restart                          &
!!     &         (nri_org, r_itp, rayleigh_WK)
!!        type(sph_radial_itp_data), intent(in) :: r_itp
!!        type(work_rayleigh_checkpoint), intent(inout) :: rayleigh_WK
!!      subroutine init_fftpack_4_cheby(nri_tgt, fcheby_WK, ierr)
!!        type(work_fftpack_chebyshev), intent(inout) :: fcheby_WK
!!
!!      subroutine dealloc_work_rayleigh_restart(rayleigh_WK)
!!        type(work_rayleigh_checkpoint), intent(inout) :: rayleigh_WK
!!      subroutine dealloc_fftpack_4_cheby(fcheby_WK)
!!        type(work_fftpack_chebyshev), intent(inout) :: fcheby_WK
!!
!!      subroutine init_rayleigh_restart_params                         &
!!     &         (istep_start, org_fld_file, ra_rst)
!!      subroutine copy_rayleigh_radial_data(ra_rst, org_sph_mesh)
!!        type(rayleigh_restart), intent(in) :: ra_rst
!!        type(sph_mesh_data), intent(inout) :: org_sph_mesh
!!      subroutine chebyshev_fwd_mat_4_rayleigh                         &
!!     &         (new_sph_mesh, r_itp, ra_rst)
!!        type(sph_mesh_data), intent(in) :: new_sph_mesh
!!        type(sph_radial_itp_data), intent(in) :: r_itp
!!        type(rayleigh_restart), intent(inout) :: ra_rst
!!
!!      subroutine rescaling_from_rayleigh(l, m, nri_org, rayleigh_in)
!!      subroutine rescaling_for_chebyshev_FFT                          &
!!     &         (nri_org, rayleigh_in, nri_tgt, rayleigh_tg)
!!      subroutine radial_interpolation_rayleigh                        &
!!     &         (r_itp, nri_org, rayleigh_in, nri_tgt, rayleigh_tg)
!!      subroutine copy_from_chebyshev_trans(sph_rj, r_itp, j, i_comp,  &
!!     &          nri_tgt, rayleigh_tg, new_sph_phys)
!!@endverbatim
!
      module t_convert_from_rayleigh
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use m_machine_parameter
      use m_file_format_switch
!
      use r_interpolate_marged_sph
      use t_SPH_mesh_field_data
      use t_time_data
      use t_field_data_IO
      use t_control_data_4_merge
      use t_control_param_assemble
      use t_spectr_data_4_assemble
!
      use new_SPH_restart
      use parallel_assemble_sph
      use copy_rj_phys_data_4_IO
      use assemble_sph_fields
      use set_control_newsph
      use rayleigh_restart_IO
      use field_IO_select
!
      implicit none
!
      type work_fftpack_chebyshev
        integer(kind = kint) :: LENSAV
        real(kind = kreal), allocatable :: WSAVE(:)
        real(kind = kreal), allocatable :: WORK(:)
      end type work_fftpack_chebyshev
!
      type work_rayleigh_checkpoint
        integer(kind = kint) :: nri_tgt
        real(kind = kreal), allocatable :: rayleigh_in(:,:)
        real(kind = kreal), allocatable :: rayleigh_tg(:,:)
        real(kind = kreal), allocatable :: rayleigh_fd(:,:)
      end type work_rayleigh_checkpoint
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_work_rayleigh_restart                            &
     &         (nri_org, r_itp, rayleigh_WK)
!
      integer(kind = kint), intent(in) :: nri_org
      type(sph_radial_itp_data), intent(in) :: r_itp
      type(work_rayleigh_checkpoint), intent(inout) :: rayleigh_WK
!
!
      rayleigh_WK%nri_tgt = r_itp%kr_outer_domain                       &
     &                     - r_itp%kr_inner_domain + 1
      allocate(rayleigh_WK%rayleigh_in(nri_org,2))
      allocate(rayleigh_WK%rayleigh_tg(rayleigh_WK%nri_tgt+1,1))
      if(nri_org .gt. 0) rayleigh_WK%rayleigh_in = 0.0d0
      rayleigh_WK%rayleigh_tg = 0.0d0
!
      allocate(rayleigh_WK%rayleigh_fd(nri_org,1))
      if(nri_org .gt. 0) rayleigh_WK%rayleigh_fd = 0.0d0
!
      end subroutine alloc_work_rayleigh_restart
!
! ----------------------------------------------------------------------
!
      subroutine init_fftpack_4_cheby(nri_tgt, fcheby_WK, ierr)
!
      integer(kind = kint), intent(in) :: nri_tgt
!
      type(work_fftpack_chebyshev), intent(inout) :: fcheby_WK
      integer(kind = kint), intent(inout) :: ierr
!
      fcheby_WK%LENSAV = 2*(nri_tgt+1) + int(log(dble(nri_tgt+1)))+8
      allocate(fcheby_WK%WSAVE(fcheby_WK%LENSAV))
      allocate(fcheby_WK%WORK(nri_tgt+1))
!
!      write(*,*) 'COST1I fcheby_WK%LENSAV', fcheby_WK%LENSAV, nri_tgt
      call COST1I(nri_tgt, fcheby_WK%WSAVE, fcheby_WK%LENSAV, ierr)
!
      end subroutine init_fftpack_4_cheby
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_work_rayleigh_restart(rayleigh_WK)
!
      type(work_rayleigh_checkpoint), intent(inout) :: rayleigh_WK
!
!
        deallocate(rayleigh_WK%rayleigh_in, rayleigh_WK%rayleigh_tg)
        deallocate(rayleigh_WK%rayleigh_fd)
!
      end subroutine dealloc_work_rayleigh_restart
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_fftpack_4_cheby(fcheby_WK)
!
      type(work_fftpack_chebyshev), intent(inout) :: fcheby_WK
!
!
      deallocate(fcheby_WK%WSAVE, fcheby_WK%WORK)
!
      end subroutine dealloc_fftpack_4_cheby
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine init_rayleigh_restart_params                           &
     &         (istep_start, org_fld_file, ra_rst)
!
      use rayleigh_restart_IO
      use MPI_read_rayleigh_restart
!
      integer(kind = kint), intent(in) :: istep_start
      type(field_IO_params), intent(in) :: org_fld_file
!
      type(rayleigh_restart), intent(inout) :: ra_rst
!
!
      if(my_rank .eq. 0) then
        call sel_read_rayleigh_rst_params                               &
     &     (org_fld_file%file_prefix, istep_start, ra_rst)
      end if
      call calypso_mpi_barrier
!
      call bcast_rayleigh_restart_param(ra_rst)
!
      end subroutine init_rayleigh_restart_params
!
! -----------------------------------------------------------------------
!
      subroutine copy_rayleigh_radial_data(ra_rst, org_sph_mesh)
!
      type(rayleigh_restart), intent(in) :: ra_rst
      type(sph_mesh_data), intent(inout) :: org_sph_mesh
!
      integer(kind = kint) :: k, kr
!
!
      org_sph_mesh%sph%sph_rj%nidx_rj(1) = ra_rst%nri_org
      org_sph_mesh%sph%sph_rj%nidx_rj(2) = 1
      call alloc_sph_1d_index_rj(org_sph_mesh%sph%sph_rj)
      do k = 1, org_sph_mesh%sph%sph_rj%nidx_rj(1)
        kr = ra_rst%nri_org-k+1
        org_sph_mesh%sph%sph_rj%radius_1d_rj_r(k) = ra_rst%r_org(kr)
      end do
!
      end subroutine copy_rayleigh_radial_data
!
! -----------------------------------------------------------------------
!
      subroutine chebyshev_fwd_mat_4_rayleigh                           &
     &         (new_sph_mesh, r_itp, ra_rst)
!
      use calypso_mpi_real
!
      type(sph_mesh_data), intent(in) :: new_sph_mesh
      type(sph_radial_itp_data), intent(in) :: r_itp
!
      type(rayleigh_restart), intent(inout) :: ra_rst
!
      real(kind = kreal), allocatable :: theta_org(:)
!
      integer(kind = kint) :: k1, k2
      integer(kind = kint) :: k_ICB
      integer(kind = kint_gl) :: nmat
      real(kind = kreal) :: r_ICB, r_norm
!
!
      allocate(ra_rst%Cheby_fwd(ra_rst%nri_org,ra_rst%nri_org))
!
      if(my_rank .eq. 0) then
        allocate(theta_org(ra_rst%nri_org))
!
        do k2 = 1, ra_rst%nri_org
          k_ICB = r_itp%kr_inner_domain
          r_ICB = new_sph_mesh%sph%sph_rj%radius_1d_rj_r(k_ICB)
          r_norm = two * (ra_rst%r_org(k2) - r_ICB) - one
          if(r_norm .gt.  one) r_norm =  one
          if(r_norm .lt. -one) r_norm = -one
          theta_org(k2) = acos(r_norm)
        end do
!
        write(*,*) 'k2, theta_org(k2)'
        do k2 = 1, ra_rst%nri_org
          write(*,*) k2, ra_rst%r_org(k2),  theta_org(k2)
        end do
!
        do k1 = 1, ra_rst%nri_org
          do k2 = 1, ra_rst%nri_org
            ra_rst%Cheby_fwd(k2,k1) = cos(dble(k1-1) * theta_org(k2))
          end do
        end do
!
        deallocate(theta_org)
      end if
!
      nmat = ra_rst%nri_org * ra_rst%nri_org
      call calypso_mpi_bcast_real(ra_rst%Cheby_fwd, nmat, 0)
!
      end subroutine chebyshev_fwd_mat_4_rayleigh
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine rescaling_from_rayleigh(l, m, nri_org, rayleigh_in)
!
      use m_precision
      use m_constants
      implicit none
!
      integer(kind = kint), intent(in) :: l, m
      integer(kind = kint), intent(in) :: nri_org
      real(kind = kreal), intent(inout) :: rayleigh_in(nri_org,2)
!
      real(kind= kreal) :: pi
!
!
      pi = four * atan(one)
!
!       Transfer to Schinidt normalization
      if(m .eq. 0) then
        rayleigh_in(1:nri_org,1)                                        &
     &               = rayleigh_in(1:nri_org,1) * sqrt(two)
      else if(m .lt. 0) then
        rayleigh_in(1:nri_org,1) = -rayleigh_in(1:nri_org,2)
!      else if(m .gt. 0) then
!        rayleigh_in(1:nri_org,1) =  rayleigh_in(1:nri_org,1)
      end if
!
      rayleigh_in(1:nri_org,1) = rayleigh_in(1:nri_org,1)               &
     &                            * sqrt(dble(2*l+1) / (two*pi))
!
      end subroutine rescaling_from_rayleigh
!
! -----------------------------------------------------------------------
!
      subroutine rescaling_for_chebyshev_FFT                            &
     &         (nri_org, rayleigh_in, nri_tgt, rayleigh_tg)
!
      integer(kind = kint), intent(in) :: nri_org, nri_tgt
      real(kind = kreal), intent(in) :: rayleigh_in(nri_org)
      real(kind = kreal), intent(inout) :: rayleigh_tg(nri_tgt+1)
!
      integer(kind = kint) :: k, nri_min
!
!
      nri_min = min(nri_org, nri_tgt+1)
!
!   Normalize for Chebyshev mode 0
      rayleigh_tg(1) = half * half* rayleigh_in(1)
      do k = 2, nri_min
        rayleigh_tg(k) = half * (-one)**(k-1) * rayleigh_in(k)
      end do
      do k = nri_min+1, nri_tgt+1
        rayleigh_tg(k) = 0.0d0
      end do
!
      end subroutine rescaling_for_chebyshev_FFT
!
! -----------------------------------------------------------------------
!
      subroutine radial_interpolation_rayleigh                          &
     &         (r_itp, nri_org, rayleigh_in, nri_tgt, rayleigh_tg)
!
      type(sph_radial_itp_data), intent(in) :: r_itp
      integer(kind = kint), intent(in) :: nri_org, nri_tgt
      real(kind = kreal), intent(in) :: rayleigh_in(nri_org)
      real(kind = kreal), intent(inout) :: rayleigh_tg(nri_tgt+1)
!
      integer(kind = kint) :: k, kr, kr_in, kr_out
!
!
      do kr = r_itp%kr_inner_domain, r_itp%kr_outer_domain
        k = kr - r_itp%kr_inner_domain + 1
        kr_in =  nri_org - r_itp%k_old2new_in(kr) +  1
        kr_out = nri_org - r_itp%k_old2new_out(kr) + 1
        rayleigh_tg(k) = r_itp%coef_old2new_in(kr) * rayleigh_in(kr_in) &
     &                + (1.0d0 - r_itp%coef_old2new_in(kr))             &
     &                * rayleigh_in(kr_out)
      end do
!
      end subroutine radial_interpolation_rayleigh
!
! -----------------------------------------------------------------------
!
      subroutine copy_from_chebyshev_trans(sph_rj, r_itp, j, i_comp,    &
     &          nri_tgt, rayleigh_tg, new_sph_phys)
!
      use t_spheric_rj_data
      use t_phys_data
      use r_interpolate_marged_sph
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_radial_itp_data), intent(in) :: r_itp
      integer(kind = kint), intent(in) :: i_comp, j
      integer(kind = kint), intent(in) :: nri_tgt
      real(kind = kreal), intent(in) :: rayleigh_tg(nri_tgt+1,1)
!
      type(phys_data), intent(inout) :: new_sph_phys
!
      integer(kind = kint) :: k, kr, inod
!
!
      do k = 1, nri_tgt
        kr = r_itp%kr_inner_domain + k - 1
        inod = j + (kr-1) * sph_rj%nidx_rj(2)
        new_sph_phys%d_fld(inod,i_comp) = rayleigh_tg(k,1)
      end do
!
      end subroutine copy_from_chebyshev_trans
!
! -----------------------------------------------------------------------
!
      end module t_convert_from_rayleigh
