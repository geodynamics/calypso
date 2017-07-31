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
!!
!!      subroutine copy_read_ene_params_4_sum(sph_IN, sph_OUT)
!!      subroutine copy_read_ene_step_data(sph_IN, sph_OUT)
!!      subroutine copy_ene_spectr_data_to_IO(nri_sph, ltr_sph, ncomp,  &
!!     &          spectr_l, sph_OUT)
!!
!!      subroutine select_sph_ene_spec_data_file(sph_IN, input_header)
!!@endverbatim
!
      module t_read_sph_spectra
!
      use m_precision
      use m_constants
!
      implicit none
!
!
      type read_sph_spectr_data
        integer(kind = kint) :: iflag_vol_ave
        integer(kind = kint) :: iflag_spectr
        integer(kind = kint) :: iflag_old_fmt
!
        integer(kind = kint) :: nfield_sph_spec
        integer(kind = kint) :: ntot_sph_spec
        integer(kind = kint) :: num_time_labels
        integer(kind = kint) :: num_labels
        integer(kind = kint), allocatable :: ncomp_sph_spec(:)
        character(len = kchara), allocatable :: ene_sph_spec_name(:)
!
        integer(kind = kint) :: ltr_sph, nri_sph
        integer(kind = kint) :: kr_ICB, kr_CMB
        integer(kind = kint) :: kr_inner, kr_outer
        integer(kind = kint), allocatable :: kr_sph(:)
        real(kind = kreal), allocatable :: r_sph(:)
        real(kind = kreal) :: r_inner, r_outer
!
        integer(kind = kint) :: i_step
        real(kind = kreal) :: time
        real(kind = kreal), allocatable :: spectr_IO(:,:,:)
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
!
      allocate( sph_IN%kr_sph(sph_IN%nri_sph) )
      allocate( sph_IN%r_sph(sph_IN%nri_sph) )
!
      ncomp = sph_IN%ntot_sph_spec
      allocate( sph_IN%spectr_IO(ncomp,0:ltr,sph_IN%nri_sph) )
!
      sph_IN%kr_sph = izero
      sph_IN%r_sph = zero
      sph_IN%spectr_IO =  zero
!
      end subroutine alloc_sph_spectr_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine dealloc_sph_espec_data(sph_IN)
!
      type(read_sph_spectr_data), intent(inout) :: sph_IN
!
!
      deallocate(sph_IN%ene_sph_spec_name, sph_IN%ncomp_sph_spec)
      deallocate(sph_IN%kr_sph, sph_IN%r_sph, sph_IN%spectr_IO)
!
      end subroutine dealloc_sph_espec_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine copy_read_ene_params_4_sum(sph_IN, sph_OUT)
!
      type(read_sph_spectr_data), intent(in) :: sph_IN
      type(read_sph_spectr_data), intent(inout) :: sph_OUT
!
      integer(kind = kint) :: i
!
      sph_OUT%iflag_vol_ave = sph_IN%iflag_vol_ave
      sph_OUT%iflag_old_fmt = 0
      sph_OUT%ltr_sph = sph_IN%ltr_sph
      sph_OUT%nri_sph = sph_IN%nri_sph
      sph_OUT%kr_ICB = sph_IN%kr_ICB
      sph_OUT%kr_CMB = sph_IN%kr_CMB
      sph_OUT%kr_inner = sph_IN%kr_inner
      sph_OUT%kr_outer = sph_IN%kr_outer
      sph_OUT%r_inner = sph_IN%r_inner
      sph_OUT%r_outer = sph_IN%r_outer
      sph_OUT%nfield_sph_spec = sph_IN%nfield_sph_spec
      sph_OUT%ntot_sph_spec = sph_IN%ntot_sph_spec
      sph_OUT%num_time_labels = sph_IN%num_time_labels - 1
!
      call alloc_sph_espec_name(sph_OUT)
      call alloc_sph_spectr_data(izero, sph_OUT)
!
      sph_OUT%kr_sph(1:sph_OUT%nri_sph)                                 &
     &                 = sph_IN%kr_sph(1:sph_OUT%nri_sph)
      sph_OUT%r_sph(1:sph_OUT%nri_sph)                                  &
     &                 = sph_IN%r_sph(1:sph_OUT%nri_sph)
      sph_OUT%ncomp_sph_spec(1:sph_OUT%nfield_sph_spec)                 &
     &            = sph_IN%ncomp_sph_spec(1:sph_OUT%nfield_sph_spec)
      sph_OUT%ene_sph_spec_name(1:sph_OUT%num_time_labels)              &
     &         = sph_IN%ene_sph_spec_name(1:sph_OUT%num_time_labels)
!
      do i = 1, sph_OUT%num_time_labels
        sph_OUT%ene_sph_spec_name(i) = sph_IN%ene_sph_spec_name(i)
      end do
      do i = 1, sph_OUT%ntot_sph_spec
        sph_OUT%ene_sph_spec_name(i+sph_OUT%num_time_labels)            &
     &          = sph_IN%ene_sph_spec_name(i+sph_IN%num_time_labels)
      end do
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
      sph_OUT%kr_sph(1:sph_OUT%nri_sph)                                 &
     &      = sph_IN%kr_sph(1:sph_OUT%nri_sph)
      sph_OUT%r_sph(1:sph_OUT%nri_sph)                                  &
     &      = sph_IN%r_sph(1:sph_OUT%nri_sph)
!
      end subroutine copy_read_ene_step_data
!
!   --------------------------------------------------------------------
!
      subroutine copy_ene_spectr_data_to_IO(nri_sph, ltr_sph, ncomp,    &
     &          spectr_l, sph_OUT)
!
      integer(kind = kint), intent(in) :: nri_sph, ltr_sph
      integer(kind = kint), intent(in) :: ncomp
      real(kind = kreal), intent(in)                                    &
     &                   :: spectr_l(ncomp, 0:ltr_sph, nri_sph)
      type(read_sph_spectr_data), intent(inout) :: sph_OUT
!
!
!$omp parallel workshare
      sph_OUT%spectr_IO(1:ncomp,0:ltr_sph,1:nri_sph)                    &
     &        = spectr_l(1:ncomp,0:ltr_sph,1:nri_sph)
!$omp end parallel workshare
!
      end subroutine copy_ene_spectr_data_to_IO
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine select_sph_ene_spec_data_file(sph_IN, input_header)
!
      type(read_sph_spectr_data), intent(inout) :: sph_IN
      character(len = kchara), intent(inout) :: input_header
!
!
      write(*,*) ' Choose data type to take average'
      write(*,*)  ' 0: spectrum on each layer  '
      write(*,*)  ' 1: volume average spectrum '
      read(*,*) sph_IN%iflag_vol_ave
!
      write(*,*) ' Is data has old format?'
      write(*,*)  ' 1: yes '
      write(*,*)  ' 0: no  '
      read(*,*) sph_IN%iflag_old_fmt
!
      write(*,*) 'enter file header for averaging'
      read(*,*) input_header
!
      end subroutine select_sph_ene_spec_data_file
!
!   --------------------------------------------------------------------
!
      end module t_read_sph_spectra
