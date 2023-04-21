!>@file   t_read_sph_spectra.f90
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Time average spherical harmonics spectrum data
!!
!!@verbatim
!!      subroutine alloc_sph_espec_name(sph_IN)
!!      subroutine alloc_sph_spectr_data(ltr, sph_IN)
!!      subroutine dealloc_sph_espec_data(sph_IN)
!!        integer(kind = kint), intent(in) :: ltr
!!        type(read_sph_spectr_data), intent(inout) :: sph_IN
!!
!!      subroutine copy_read_ene_params_4_sum(sph_IN, sph_OUT)
!!      subroutine copy_read_ene_step_data(sph_IN, sph_OUT)
!!        integer(kind = kint), intent(in) :: nri_dat, ltr_sph
!!        integer(kind = kint), intent(in) :: ncomp
!!        real(kind = kreal), intent(in)                                &
!!     &                   :: spectr_l(ncomp, 0:ltr_sph, nri_dat)
!!        type(read_sph_spectr_data), intent(inout) :: sph_OUT
!!
!!      subroutine check_sph_spectr_name(sph_IN)
!!        type(read_sph_spectr_data), intent(in) :: sph_IN
!!@endverbatim
      module t_read_sph_spectra
!
      use m_precision
      use m_constants
      use t_sph_spectr_head_labels
!
      implicit none
!
!
      type read_sph_spectr_data
        integer(kind = kint) :: nfield_sph_spec
        integer(kind = kint) :: ntot_sph_spec
        integer(kind = kint) :: num_time_labels
        integer(kind = kint) :: num_labels
        integer(kind = kint), allocatable :: ncomp_sph_spec(:)
        character(len = kchara), allocatable :: ene_sph_spec_name(:)
!
        integer(kind = kint) :: ltr_sph, nri_sph, nri_dat
        integer(kind = kint) :: kr_ICB, kr_CMB
        integer(kind = kint) :: kr_inner, kr_outer
        real(kind = kreal) :: r_inner, r_outer
!
        integer(kind = kint), allocatable :: i_mode(:)
        integer(kind = kint), allocatable :: kr_sph(:)
        real(kind = kreal), allocatable :: r_sph(:)
!
        integer(kind = kint) :: i_step
        real(kind = kreal) :: time
      end type read_sph_spectr_data
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine alloc_sph_espec_name(sph_IN)
!
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
!
      sph_IN%num_labels = sph_IN%ntot_sph_spec + sph_IN%num_time_labels
      allocate( sph_IN%ene_sph_spec_name(sph_IN%num_labels) )
      allocate( sph_IN%ncomp_sph_spec(sph_IN%nfield_sph_spec))
      sph_IN%ncomp_sph_spec = 0
!
      end subroutine alloc_sph_espec_name
!
!   --------------------------------------------------------------------
!
      subroutine alloc_sph_spectr_data(ltr, sph_IN)
!
      integer(kind = kint), intent(in) :: ltr
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
      integer(kind = kint) :: ncomp
!
      integer(kind = kint) :: l
!
      allocate(sph_IN%kr_sph(sph_IN%nri_dat))
      allocate(sph_IN%r_sph(sph_IN%nri_dat))
!
      ncomp = sph_IN%ntot_sph_spec
      allocate(sph_IN%i_mode(0:ltr))
!
!$omp parallel do
      do l = 0, ltr
        sph_IN%i_mode(l) = l
      end do
!$omp end parallel do
!$omp parallel workshare
      sph_IN%kr_sph(1:sph_IN%nri_dat) = izero
      sph_IN%r_sph(1:sph_IN%nri_dat) = zero
!$omp end parallel workshare
!
      end subroutine alloc_sph_spectr_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine dealloc_sph_espec_name(sph_IN)
!
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
!
      deallocate(sph_IN%ene_sph_spec_name, sph_IN%ncomp_sph_spec)
!
      end subroutine dealloc_sph_espec_name
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_sph_espec_data(sph_IN)
!
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
!
      deallocate(sph_IN%kr_sph, sph_IN%r_sph, sph_IN%i_mode)
!
      end subroutine dealloc_sph_espec_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine copy_read_ene_head_params(sph_IN, sph_OUT)
!
      type(read_sph_spectr_data), intent(in) :: sph_IN
      type(read_sph_spectr_data), intent(inout) :: sph_OUT
!
!
      sph_OUT%ltr_sph = sph_IN%ltr_sph
      sph_OUT%nri_sph = sph_IN%nri_sph
      sph_OUT%nri_dat = sph_IN%nri_dat
      sph_OUT%kr_ICB = sph_IN%kr_ICB
      sph_OUT%kr_CMB = sph_IN%kr_CMB
      sph_OUT%kr_inner = sph_IN%kr_inner
      sph_OUT%kr_outer = sph_IN%kr_outer
      sph_OUT%r_inner = sph_IN%r_inner
      sph_OUT%r_outer = sph_IN%r_outer
      sph_OUT%nfield_sph_spec = sph_IN%nfield_sph_spec
      sph_OUT%ntot_sph_spec = sph_IN%ntot_sph_spec
      sph_OUT%num_time_labels = sph_IN%num_time_labels
!
      end subroutine copy_read_ene_head_params
!
!   --------------------------------------------------------------------
!
      subroutine copy_read_ene_name_params(n_fld, ntot_spec,            &
     &          num_time_labels, sph_IN, sph_OUT)
!
      integer(kind= kint), intent(in) :: n_fld, ntot_spec
      integer(kind= kint), intent(in) :: num_time_labels
      type(read_sph_spectr_data), intent(in) :: sph_IN
      type(read_sph_spectr_data), intent(inout) :: sph_OUT
!
      integer(kind = kint) :: i
!
!
      sph_OUT%ncomp_sph_spec(1:n_fld) = sph_IN%ncomp_sph_spec(1:n_fld)
      sph_OUT%ene_sph_spec_name(1:num_time_labels)                      &
     &         = sph_IN%ene_sph_spec_name(1:num_time_labels)
!
      do i = 1, ntot_spec
        sph_OUT%ene_sph_spec_name(i+num_time_labels)                    &
     &          = sph_IN%ene_sph_spec_name(i+num_time_labels)
      end do
!
      end subroutine copy_read_ene_name_params
!
!   --------------------------------------------------------------------
!
      subroutine copy_read_ene_params_4_sum(sph_IN, sph_OUT)
!
      type(read_sph_spectr_data), intent(in) :: sph_IN
      type(read_sph_spectr_data), intent(inout) :: sph_OUT
!
!
      call copy_read_ene_head_params(sph_IN, sph_OUT)
      sph_OUT%num_time_labels = sph_OUT%num_time_labels - 1
!
      call alloc_sph_espec_name(sph_OUT)
      call copy_read_ene_name_params                                    &
     &   (sph_OUT%nfield_sph_spec, sph_OUT%ntot_sph_spec,               &
     &    sph_OUT%num_time_labels, sph_IN, sph_OUT)
!
      call alloc_sph_spectr_data(izero, sph_OUT)
!
      sph_OUT%kr_sph(1:sph_OUT%nri_dat)                                 &
     &                 = sph_IN%kr_sph(1:sph_OUT%nri_dat)
      sph_OUT%r_sph(1:sph_OUT%nri_dat)                                  &
     &                 = sph_IN%r_sph(1:sph_OUT%nri_dat)
      sph_OUT%i_mode(0:sph_OUT%ltr_sph)                                 &
     &                 = sph_IN%i_mode(0:sph_OUT%ltr_sph)
!
      end subroutine copy_read_ene_params_4_sum
!
!   --------------------------------------------------------------------
!
      subroutine copy_read_ene_step_data(sph_IN, sph_OUT)
!
      type(read_sph_spectr_data), intent(in) :: sph_IN
      type(read_sph_spectr_data), intent(inout) :: sph_OUT
!
!
      sph_OUT%time = sph_IN%time
      sph_OUT%i_step = sph_IN%i_step
!$omp parallel workshare
      sph_OUT%kr_sph(1:sph_OUT%nri_dat)                                 &
     &      = sph_IN%kr_sph(1:sph_OUT%nri_dat)
      sph_OUT%r_sph(1:sph_OUT%nri_dat)                                  &
     &      = sph_IN%r_sph(1:sph_OUT%nri_dat)
!$omp end parallel workshare
!$omp parallel workshare
      sph_OUT%i_mode(0:sph_OUT%ltr_sph)                                 &
     &                 = sph_IN%i_mode(0:sph_OUT%ltr_sph)
!$omp end parallel workshare
!
      end subroutine copy_read_ene_step_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine check_sph_spectr_name(sph_IN)
!
      type(read_sph_spectr_data), intent(in) :: sph_IN
      integer(kind = kint) :: i
!
!
      write(*,*) "Number of components: ", sph_IN%ntot_sph_spec
      do i = sph_IN%num_time_labels+1, sph_IN%num_labels
        write(*,*) trim(sph_IN%ene_sph_spec_name(i))
      end do
!
      end subroutine check_sph_spectr_name
!
!   --------------------------------------------------------------------
!
      end module t_read_sph_spectra
