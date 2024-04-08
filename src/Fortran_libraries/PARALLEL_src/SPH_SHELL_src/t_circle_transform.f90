!>@file   t_circle_transform.f90
!!@brief  module t_circle_transform
!!
!!@author H. Matsui
!!@date Programmed on June., 2013
!
!>@brief  spherical transform at a specific circle at @f$(r, theta)@f$
!!
!!@verbatim
!!      subroutine init_legendre_on_circle(sph, comms_sph, trans_p,     &
!!     &                                   leg_circ, SR_sig, SR_r)
!!        type(sph_grids), intent(in) ::  sph
!!        type(sph_comm_tables), intent(in) :: comms_sph
!!        type(parameters_4_sph_trans), intent(in) :: trans_p
!!        type(circle_transform_spectr), intent(inout) :: leg_circ
!!        type(send_recv_status), intent(inout) :: SR_sig
!!        type(send_recv_real_buffer), intent(inout) :: SR_r
!!
!!      subroutine alloc_circle_transform(ltr, leg_circ)
!!      subroutine alloc_legendre_on_circ_rj(sph_rj, leg_circ)
!!      subroutine alloc_work_circle_transform(d_circle, leg_circ)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(phys_data), intent(in) :: d_circle
!!        type(circle_transform_spectr), intent(inout) :: leg_circ
!!      subroutine dealloc_circle_transform(leg_circ)
!!      subroutine dealloc_legendre_on_circ_rj(leg_circ)
!!      subroutine dealloc_work_circle_transform(leg_circ)
!!        type(circle_transform_spectr), intent(inout) :: leg_circ
!!@endverbatim
!!
!!@n @param  ltr      Truncation of spherical harmonics
!!@n @param  jmax     Number of modes for harmonincs except for 0 degree
!!@n @param  numdir   Number of components of field
!!@n @param v_rtp_circle(mphi_circle,numdir)  Field along circle
!
      module t_circle_transform
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_phys_data
!
      implicit none
!
!
      type circle_transform_spectr
!>        Truncation level for spherical transform at equator
        integer(kind = kint) :: ltr_circle
!>        end address of SMP parallelization for scalar Fourier transform
        integer(kind = kint), allocatable :: istack_circfft_smp(:)
!
!>        Radius for specific circle
        real(kind = kreal) :: r_circle
!>        @f$ 1/ r @f$ for specific circle
        real(kind = kreal) :: ar_circle
!>        @f$ 1/ r^{2} @f$ for specific circle
        real(kind = kreal) :: ar2_circle
!
!>        colatitude for specific circle
        real(kind = kreal) :: theta_circle
!>        Legendre polynomial of the circle
        real(kind = kreal), allocatable :: P_circ(:)
!>        difference of the Legendre polynomial of the circle
        real(kind = kreal), allocatable :: dPdt_circ(:)
!
!>        global sphrical harmonics corfs on circle
        real(kind = kreal), allocatable :: d_circ_gl(:,:)
!>        Local sphrical harmonics corfs on circle
        real(kind = kreal), allocatable :: d_circ_lc(:,:)
!
!>        Spectr data for circle point collected to 0 process
        real(kind = kreal), allocatable :: vrtm_mag(:,:)
!>        Spectr data for circle point collected to 0 process
        real(kind = kreal), allocatable :: vrtm_phase(:,:)
      end type circle_transform_spectr
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_legendre_on_circle(sph, comms_sph, trans_p,       &
     &                                   leg_circ, SR_sig, SR_r)
!
      use calypso_mpi
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_work_4_sph_trans
      use t_solver_SR
      use const_equator_legendres_rj
!
      type(sph_grids), intent(in) ::  sph
      type(sph_comm_tables), intent(in) :: comms_sph
      type(parameters_4_sph_trans), intent(in) :: trans_p
!
      type(circle_transform_spectr), intent(inout) :: leg_circ
      type(send_recv_status), intent(inout) :: SR_sig
      type(send_recv_real_buffer), intent(inout) :: SR_r
!
!
      call alloc_legendre_on_circ_rj(sph%sph_rj, leg_circ)
      call s_const_equator_legendres_rj(leg_circ%theta_circle,          &
     &    sph%sph_params, sph%sph_rj, sph%sph_rlm, sph%sph_rtm,         &
     &    comms_sph, trans_p, leg_circ%P_circ, leg_circ%dPdt_circ,      &
     &    SR_sig, SR_r)
!
!      write(*,*) ' check_legendre_on_circ_rj ', my_rank
      if(i_debug .gt. 0) then
        call check_legendre_on_circ_rj(sph%sph_rj, leg_circ)
      end if
!
      end subroutine init_legendre_on_circle
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine alloc_circle_transform(leg_circ)
!
      type(circle_transform_spectr), intent(inout) :: leg_circ
!
!
      allocate(leg_circ%istack_circfft_smp(0:np_smp))
      leg_circ%istack_circfft_smp(0) =        0
      leg_circ%istack_circfft_smp(1:np_smp) = 1
!
      end subroutine alloc_circle_transform
!
! ----------------------------------------------------------------------
!
      subroutine alloc_legendre_on_circ_rj(sph_rj, leg_circ)
!
      use t_spheric_rj_data
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(circle_transform_spectr), intent(inout) :: leg_circ
!
!
      allocate(leg_circ%P_circ(sph_rj%nidx_rj(2)))
      allocate(leg_circ%dPdt_circ(sph_rj%nidx_rj(2)))
!$omp parallel workshare
      leg_circ%P_circ(1:sph_rj%nidx_rj(2)) =    0.0d0
      leg_circ%dPdt_circ(1:sph_rj%nidx_rj(2)) = 0.0d0
!$omp end parallel workshare
!
      end subroutine alloc_legendre_on_circ_rj
!
! ----------------------------------------------------------------------
!
      subroutine alloc_work_circle_transform(d_circle, leg_circ)
!
      type(phys_data), intent(in) :: d_circle
      type(circle_transform_spectr), intent(inout) :: leg_circ
!
      integer(kind = kint) :: ltr, ntot_comp
!
!
      ntot_comp = d_circle%ntot_phys
      ltr =       leg_circ%ltr_circle
      allocate(leg_circ%d_circ_gl(-ltr:ltr, ntot_comp))
      allocate(leg_circ%d_circ_lc(-ltr:ltr, ntot_comp))
!
      allocate(leg_circ%vrtm_mag(0:ltr,ntot_comp))
      allocate(leg_circ%vrtm_phase(0:ltr,ntot_comp))
!
!
      if((ltr*ntot_comp) .le. 0) return
!
!$omp parallel workshare
      leg_circ%d_circ_gl(-ltr:ltr, 1:ntot_comp) = 0.0d0
      leg_circ%d_circ_lc(-ltr:ltr, 1:ntot_comp) = 0.0d0
!$omp end parallel workshare
!
!$omp parallel workshare
      leg_circ%vrtm_mag(0:ltr,1:ntot_comp) =   0.0d0
      leg_circ%vrtm_phase(0:ltr,1:ntot_comp) = 0.0d0
!$omp end parallel workshare
!
      end subroutine alloc_work_circle_transform
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_circle_transform(leg_circ)
!
      type(circle_transform_spectr), intent(inout) :: leg_circ
!
!
      deallocate(leg_circ%istack_circfft_smp)
!
      end subroutine dealloc_circle_transform
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_legendre_on_circ_rj(leg_circ)
!
      type(circle_transform_spectr), intent(inout) :: leg_circ
!
!
      deallocate(leg_circ%P_circ, leg_circ%dPdt_circ)
!
      end subroutine dealloc_legendre_on_circ_rj
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_work_circle_transform(leg_circ)
!
      type(circle_transform_spectr), intent(inout) :: leg_circ
!
!
      if(allocated(leg_circ%vrtm_mag) .eqv. .FALSE.) return
      deallocate(leg_circ%vrtm_mag, leg_circ%vrtm_phase)
      deallocate(leg_circ%d_circ_gl, leg_circ%d_circ_lc)
!
      end subroutine dealloc_work_circle_transform
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine check_legendre_on_circ_rj(sph_rj, leg_circ)
!
      use calypso_mpi
      use t_spheric_rj_data
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(circle_transform_spectr), intent(in) :: leg_circ
!
      integer(kind = kint) :: ip, j
!
      do ip = 1, nprocs
        call calypso_mpi_barrier
        if(ip-1 .ne. my_rank) cycle
        open(80,file='eq_leg.dat', position='APPEND')
        if(ip.eq. 1) then
           write(80,*) 'my_rank, j_local, j, l, m, Pvec_1, Pvec_2',     &
     &                leg_circ%theta_circle
        end if
        do j = 1, sph_rj%nidx_rj(2)
          write(80,*) my_rank, j, sph_rj%idx_gl_1d_rj_j(j,1:3),         &
     &              leg_circ%P_circ(j), leg_circ%dPdt_circ(j)
        end do
        close(80)
      end do
!
      end subroutine check_legendre_on_circ_rj
!
! ----------------------------------------------------------------------
!
     end module t_circle_transform
