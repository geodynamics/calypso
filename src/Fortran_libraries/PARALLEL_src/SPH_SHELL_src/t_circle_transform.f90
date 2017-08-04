!>@file   t_circle_transform.f90
!!@brief  module t_circle_transform
!!
!!@author H. Matsui
!!@date Programmed on June., 2013
!
!>@brief  spherical transform at a specific circle at @f$(r, theta)@f$
!!
!!@verbatim
!!      subroutine alloc_circle_transform(ltr, circ_spec)
!!      subroutine alloc_circle_field(mphi_rtp, nidx_global_jmax,       &
!!     &          circle, d_circle)
!!      subroutine dealloc_circle_transform
!!      subroutine dealloc_circle_field(circle, d_circle)
!!@endverbatim
!!
!!@n @param  ltr      Truncation of spherical harmonics
!!@n @param  jmax     Number of modes for harmonincs except for 0 degree
!!@n @param  d_rj_circle(0:jmax,3)   Spectr field data
!!@n @param  numdir   Number of components of field
!!@n @param v_rtp_circle(mphi_circle,numdir)  Field along circle
!!@n @param vrtm_mag(0:mphi_circle,numdir)  Amplitude of spectrum data
!!                                        along with the circle
!!@n @param vrtm_phase(0:mphi_circle,numdir)    Phase of spectrum data
!!                                        along with the circle
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
!>      Use spherical coordinate for circle data
      integer(kind = kint), parameter :: iflag_circle_sph = 1
!>      Use Cylindrical coordinate for circle data
      integer(kind = kint), parameter :: iflag_circle_cyl = 2
!
!
      type circle_transform_spetr
!>        Truncation level for spherical transform at equator
        integer(kind = kint) :: ltr_circle
!>        Number of modes for spherical transform at equator
        integer(kind = kint) :: jmax_circle
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
!
!>        associated Lagender polynomial at circle
        real(kind = kreal), allocatable :: P_circle(:)
!>         @f$ dP_{l}^{m}/ d\theta @f$ at circle
        real(kind = kreal), allocatable :: dPdt_circle(:)
!
!>        spectr data for Fourier transform at a circle
        real(kind = kreal), allocatable :: vcirc_rtm(:,:)
      end type circle_transform_spetr
!
!>      Structure to make fields on circle
      type fields_on_circle
!>        Flag for coordinate system for circle data
        integer(kind = kint) :: iflag_circle_coord = iflag_circle_sph
!
!>        file name for field data on a circle
        character(len=kchara) :: fname_circle_fld = 'circle_field.dat'
!>        file name for spectr power data on a circle
        character(len=kchara) :: fname_circle_mag                       &
     &                        = 'circle_spec_mag.dat'
!>        file name for spectr phase data on a circle
        character(len=kchara) :: fname_circle_phs                       &
     &                        = 'circle_spec_phase.dat'
!
!>        Number of gird points for a circle
        integer(kind = kint) :: mphi_circle
!>        cylindrical radius for a circle to pick
        real(kind = kreal) :: s_circle
!>        vartical position for a circle to pick
        real(kind = kreal) :: z_circle
!
!>        Inner closest point of circle point of fluid shell
        integer(kind = kint) :: kr_gl_rcirc_in
!>        Outer closest point of circle point of fluid shell
        integer(kind = kint) :: kr_gl_rcirc_out
!>        Inner closest radius of circle point of fluid shell
        real(kind = kreal) :: coef_gl_rcirc_in
!>        Outer closest radius of circle point of fluid shell
        real(kind = kreal) :: coef_gl_rcirc_out
!
!>        Spectr data for circle point for each domain
        real(kind = kreal), allocatable :: d_rj_circ_lc(:,:)
!>        Spectr data for circle point collected to 0 process
        real(kind = kreal), allocatable :: d_rj_circle(:,:)
!
!>        Field data for circle point at equator
        real(kind = kreal), allocatable :: v_rtp_circle(:,:)
!
!>        Spectr data for circle point collected to 0 process
        real(kind = kreal), allocatable :: vrtm_mag(:,:)
!>        Spectr data for circle point collected to 0 process
        real(kind = kreal), allocatable :: vrtm_phase(:,:)
      end type fields_on_circle
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_circle_transform(ltr, circ_spec)
!
      integer(kind = kint), intent(in) :: ltr
      type(circle_transform_spetr), intent(inout) :: circ_spec
!
!
      circ_spec%ltr_circle =  ltr
      circ_spec%jmax_circle = ltr*(ltr+2)
!
      allocate(circ_spec%P_circle(0:circ_spec%jmax_circle))
      allocate(circ_spec%dPdt_circle(0:circ_spec%jmax_circle))
      circ_spec%P_circle =    zero
      circ_spec%dPdt_circle = zero
!
      allocate(circ_spec%vcirc_rtm(-ltr:ltr,3))
      circ_spec%vcirc_rtm = zero
!
      allocate(circ_spec%istack_circfft_smp(0:np_smp))
      circ_spec%istack_circfft_smp(0) =        0
      circ_spec%istack_circfft_smp(1:np_smp) = 1
!
      end subroutine alloc_circle_transform
!
! ----------------------------------------------------------------------
!
      subroutine alloc_circle_field(mphi_rtp, nidx_global_jmax,         &
     &          circle, d_circle)
!
      use calypso_mpi
!
      integer(kind = kint), intent(in) :: mphi_rtp, nidx_global_jmax
      type(fields_on_circle), intent(inout) :: circle
      type(phys_data), intent(inout) :: d_circle
!
      integer(kind = kint) :: jmax_gl, ntot
!
!
      jmax_gl = nidx_global_jmax
      ntot = d_circle%ntot_phys
!
      if(circle%mphi_circle .le. izero) then
        circle%mphi_circle = mphi_rtp
      end if
!
      allocate(circle%v_rtp_circle(circle%mphi_circle,6))
      circle%v_rtp_circle = 0.0d0
!
      allocate( circle%vrtm_mag(0:circle%mphi_circle,ntot) )
      allocate( circle%vrtm_phase(0:circle%mphi_circle,ntot) )
      circle%vrtm_mag = 0.0d0
      circle%vrtm_phase = 0.0d0
!
      allocate( circle%d_rj_circ_lc(0:jmax_gl,ntot) )
      circle%d_rj_circ_lc = 0.0d0
!
      if(my_rank .eq. 0) then
        allocate(circle%d_rj_circle(0:jmax_gl,ntot) )
!
        circle%d_rj_circle = 0.0d0
      end if
!
      call alloc_phys_data_type(circle%mphi_circle, d_circle)
!
      end subroutine alloc_circle_field
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_circle_transform(circ_spec)
!
      type(circle_transform_spetr), intent(inout) :: circ_spec
!
!
      deallocate(circ_spec%P_circle, circ_spec%dPdt_circle)
      deallocate(circ_spec%vcirc_rtm)
      deallocate(circ_spec%istack_circfft_smp)
!
      end subroutine dealloc_circle_transform
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_circle_field(circle, d_circle)
!
      use calypso_mpi
!
      type(fields_on_circle), intent(inout) :: circle
      type(phys_data), intent(inout) :: d_circle
!
!
      deallocate(circle%vrtm_mag, circle%vrtm_phase)
      deallocate(circle%d_rj_circ_lc)
      if(my_rank .eq. 0) then
        deallocate(circle%d_rj_circle, circle%v_rtp_circle)
      end if
!
      call dealloc_phys_data_type(d_circle)
      call dealloc_phys_name_type(d_circle)
!
      end subroutine dealloc_circle_field
!
! ----------------------------------------------------------------------
!
     end module t_circle_transform
