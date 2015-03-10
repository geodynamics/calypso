!>@file   m_tave_sph_ene_spectr.f90
!!        module m_tave_sph_ene_spectr
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Time average spherical harmonics spectrum data
!!
!!@verbatim
!!      subroutine allocate_tave_sph_espec_data
!!      subroutine deallocate_tave_sph_espec_data
!!
!!      subroutine reset_tave_sph_espec_data
!!      subroutine reset_tsigma_sph_espec_data
!!
!!      subroutine open_tave_ene_spec_data
!!      subroutine open_tsigma_ene_spec_data
!!
!!      subroutine write_tave_vol_sph_data(nri, ltr, ncomp,             &
!!     &          spec, spec_l, spec_m, spec_lm)
!!      subroutine write_tave_layer_sph_data(nri, ltr, ncomp,           &
!!     &          spec, spec_l, spec_m, spec_lm)
!!@endverbatim
!
      module m_tave_sph_ene_spectr
!
      use m_precision
      use m_constants
!
      implicit none
!
!
      real(kind = kreal), allocatable :: ave_spec_t(:,:)
      real(kind = kreal), allocatable :: ave_spec_l(:,:,:)
      real(kind = kreal), allocatable :: ave_spec_m(:,:,:)
      real(kind = kreal), allocatable :: ave_spec_lm(:,:,:)
!
      real(kind = kreal), allocatable :: sigma_spec_t(:,:)
      real(kind = kreal), allocatable :: sigma_spec_l(:,:,:)
      real(kind = kreal), allocatable :: sigma_spec_m(:,:,:)
      real(kind = kreal), allocatable :: sigma_spec_lm(:,:,:)
!
!     data file ID
!
      integer(kind = kint), parameter :: id_tave_rms_l =    31
      integer(kind = kint), parameter :: id_tave_rms_m =    32
      integer(kind = kint), parameter :: id_tave_rms_lm =   33
      integer(kind = kint), parameter :: id_tave_rms =      34
!
      private :: write_average_ene_sph_head
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine allocate_tave_sph_espec_data
!
      use m_sph_ene_spectra
!
      allocate( ave_spec_t(ncomp_sph_spec,nri_sph) )
      allocate( ave_spec_l(ncomp_sph_spec,0:ltr_sph,nri_sph) )
      allocate( ave_spec_m(ncomp_sph_spec,0:ltr_sph,nri_sph) )
      allocate( ave_spec_lm(ncomp_sph_spec,0:ltr_sph,nri_sph) )
!
      allocate( sigma_spec_t(ncomp_sph_spec,nri_sph) )
      allocate( sigma_spec_l(ncomp_sph_spec,0:ltr_sph,nri_sph) )
      allocate( sigma_spec_m(ncomp_sph_spec,0:ltr_sph,nri_sph) )
      allocate( sigma_spec_lm(ncomp_sph_spec,0:ltr_sph,nri_sph) )
!
      ave_spec_t =  0.0d0
      ave_spec_l =  0.0d0
      ave_spec_m =  0.0d0
      ave_spec_lm = 0.0d0
!
      sigma_spec_t =  0.0d0
      sigma_spec_l =  0.0d0
      sigma_spec_m =  0.0d0
      sigma_spec_lm = 0.0d0
!
      end subroutine allocate_tave_sph_espec_data
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_tave_sph_espec_data
!
      deallocate(ave_spec_t, ave_spec_l, ave_spec_m, ave_spec_lm)
      deallocate(sigma_spec_l, sigma_spec_m)
      deallocate(sigma_spec_t, sigma_spec_lm)
!
      end subroutine deallocate_tave_sph_espec_data
!
!   --------------------------------------------------------------------
!
      subroutine reset_tave_sph_espec_data
!
      use m_sph_ene_spectra
!
!
      sigma_spec_t =  0.0d0
      sigma_spec_l =  0.0d0
      sigma_spec_m =  0.0d0
      sigma_spec_lm = 0.0d0
!
      end subroutine reset_tave_sph_espec_data
!
!   --------------------------------------------------------------------
!
      subroutine reset_tsigma_sph_espec_data
!
!
      sigma_spec_t =  0.0d0
      sigma_spec_l =  0.0d0
      sigma_spec_m =  0.0d0
      sigma_spec_lm = 0.0d0
!
      end subroutine reset_tsigma_sph_espec_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine open_tave_ene_spec_data
!
      use m_sph_ene_spectra
!
      character(len = kchara) :: fname_tave_rms_l
      character(len = kchara) :: fname_tave_rms_m
      character(len = kchara) :: fname_tave_rms_lm
      character(len = kchara) :: fname_tave_rms
!
!
      write(fname_tave_rms_l, '(a6,a)')                                 &
     &        't_ave_', trim(fname_org_rms_l)
      write(fname_tave_rms_m, '(a6,a)')                                 &
     &        't_ave_', trim(fname_org_rms_m)
      write(fname_tave_rms_lm,'(a6,a)')                                 &
     &        't_ave_', trim(fname_org_rms_lm)
      write(fname_tave_rms,   '(a6,a)')                                 &
     &        't_ave_', trim(fname_org_rms)
!
      open(id_tave_rms,   file=fname_tave_rms)
      open(id_tave_rms_l, file=fname_tave_rms_l)
      open(id_tave_rms_m, file=fname_tave_rms_m)
      open(id_tave_rms_lm,file=fname_tave_rms_lm)
!
      call write_average_ene_sph_head
!
      end subroutine open_tave_ene_spec_data
!
!   --------------------------------------------------------------------
!
      subroutine open_tsigma_ene_spec_data
!
      use m_sph_ene_spectra
!
      character(len = kchara) :: fname_tave_rms_l
      character(len = kchara) :: fname_tave_rms_m
      character(len = kchara) :: fname_tave_rms_lm
      character(len = kchara) :: fname_tave_rms
!
!
      write(fname_tave_rms_l, '(a8,a)')                                 &
     &        't_sigma_', trim(fname_org_rms_l)
      write(fname_tave_rms_m, '(a8,a)')                                 &
     &        't_sigma_', trim(fname_org_rms_m)
      write(fname_tave_rms_lm,'(a8,a)')                                 &
     &        't_sigma_', trim(fname_org_rms_lm)
      write(fname_tave_rms,   '(a8,a)')                                 &
     &        't_sigma_', trim(fname_org_rms)
!
      open(id_tave_rms,   file=fname_tave_rms)
      open(id_tave_rms_l, file=fname_tave_rms_l)
      open(id_tave_rms_m, file=fname_tave_rms_m)
      open(id_tave_rms_lm,file=fname_tave_rms_lm)
!
      call write_average_ene_sph_head
!
      end subroutine open_tsigma_ene_spec_data
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine write_average_ene_sph_head
!
      use m_sph_ene_spectra
      use write_field_labels
!
      integer(kind = kint) :: num
!
!
      num = ncomp_sph_spec + num_time_labels
      write(ene_sph_spec_name(num_time_labels),'(a)') 'degree'
      call write_multi_labels(id_tave_rms, num, ene_sph_spec_name)
      call write_multi_labels(id_tave_rms_l, num, ene_sph_spec_name)
!
      write(ene_sph_spec_name(num_time_labels),'(a)') 'order'
      call write_multi_labels(id_tave_rms_m, num, ene_sph_spec_name)
!
      write(ene_sph_spec_name(num_time_labels),'(a)')                   &
     &                                           'diff_deg_order'
      call write_multi_labels(id_tave_rms_lm, num, ene_sph_spec_name)
!
      write(id_tave_rms,*   )
      write(id_tave_rms_l,* )
      write(id_tave_rms_m,* )
      write(id_tave_rms_lm,*)
!
      end subroutine write_average_ene_sph_head
!
!   --------------------------------------------------------------------
!
      subroutine write_tave_vol_sph_data(nri, ltr, ncomp,               &
     &          spec, spec_l, spec_m, spec_lm)
!
      use m_sph_ene_spectra
!
      integer(kind = kint), intent(in) :: nri, ltr, ncomp
      real(kind = kreal), intent(inout) :: spec(ncomp,nri)
      real(kind = kreal), intent(inout) :: spec_l(ncomp,0:ltr,nri)
      real(kind = kreal), intent(inout) :: spec_m(ncomp,0:ltr,nri)
      real(kind = kreal), intent(inout) :: spec_lm(ncomp,0:ltr,nri)
!
      integer(kind = kint) :: lth
!
!
      write(id_tave_rms,'(i16,1pE25.15e3,i16,1p255E25.15e3)')           &
     &         ied_true, time_sph, izero, spec(1:ncomp,1)
!
      do lth = 0, ltr
        write(id_tave_rms_l,'(i16,1pE25.15e3,i16,1p255E25.15e3)')       &
     &         ied_true, time_sph, lth, spec_l(1:ncomp,lth,1)
        write(id_tave_rms_m,'(i16,1pE25.15e3,i16,1p255E25.15e3)')       &
     &         ied_true, time_sph, lth, spec_m(1:ncomp,lth,1)
        write(id_tave_rms_lm,'(i16,1pE25.15e3,i16,1p255E25.15e3)')      &
     &         ied_true, time_sph, lth, spec_lm(1:ncomp,lth,1)
      end do
!
      end subroutine write_tave_vol_sph_data
!
!   --------------------------------------------------------------------
!
      subroutine write_tave_layer_sph_data(nri, ltr, ncomp,             &
     &          spec, spec_l, spec_m, spec_lm)
!
      use m_sph_ene_spectra
!
      integer(kind = kint), intent(in) :: nri, ltr, ncomp
      real(kind = kreal), intent(inout) :: spec(ncomp,nri)
      real(kind = kreal), intent(inout) :: spec_l(ncomp,0:ltr,nri)
      real(kind = kreal), intent(inout) :: spec_m(ncomp,0:ltr,nri)
      real(kind = kreal), intent(inout) :: spec_lm(ncomp,0:ltr,nri)
!
      integer(kind = kint) :: kr, lth
!
!
      do kr = 1, nri
        write(id_tave_rms,'(i16,1pE25.15e3,2i16,1p255E25.15e3)')        &
     &         ied_true, time_sph, kr_sph(kr), izero,                   &
     &         spec(1:ncomp,kr)
!
        do lth = 0, ltr
          write(id_tave_rms_l,'(i16,1pE25.15e3,2i16,1p255E25.15e3)')    &
     &         ied_true, time_sph, kr_sph(kr), lth,                     &
     &         spec_l(1:ncomp,lth,kr)
          write(id_tave_rms_m,'(i16,1pE25.15e3,2i16,1p255E25.15e3)')    &
     &         ied_true, time_sph, kr_sph(kr), lth,                     &
     &         spec_m(1:ncomp,lth,kr)
          write(id_tave_rms_lm,'(i16,1pE25.15e3,2i16,1p255E25.15e3)')   &
     &         ied_true, time_sph, kr_sph(kr), lth,                     &
     &         spec_lm(1:ncomp,lth,kr)
        end do
      end do
!
      end subroutine write_tave_layer_sph_data
!
!   --------------------------------------------------------------------
!
      end module m_tave_sph_ene_spectr
